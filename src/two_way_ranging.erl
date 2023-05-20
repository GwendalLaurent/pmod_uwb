-module(two_way_ranging).

-include("mac_layer.hrl").

-export([ds_initiator/0, ds_responder/0]).
-export([ss_initiator/1, ss_responder/1]).
-export([ss_initiator/0, ss_responder/0]).

%-define(TU, 1.565444993393822e-11). % 1 t.u. is ~1.5654e-11 s
-define(TU, 15.65e-12).
-define(C, 299792458). % Speed of light
% https://forum.qorvo.com/t/sample-programs/788/3
-define(UUS_TO_DWT_TIME, 65536). % in one UWB µs, there are 65536 t.u (UWB µs are special µs ???)
-define(FREQ_OFFSET_MULTIPLIER, 1/( 131072 * 2 * (1024/998.4e6))).
-define(HERTZ_TO_PPM_MUL, 1.0e-6/6489.6e6).

%--- Double-sided two-way ranging -------------------------------------------------------------------
ds_initiator() ->
    % Set the antenna delay -> !! the values should be calibrated
    TX_ANTD = 16436, pmod_uwb:write(tx_antd, #{tx_antd => TX_ANTD}),
    RX_ANTD = 16435, pmod_uwb:write(lde_if, #{lde_rxantd => RX_ANTD}),
    pmod_uwb:set_frame_timeout(16#FFFF),
    % ? Set preamble timeout too ?
    ds_initiator_loop(50, TX_ANTD, {0,0,[],0}).

ds_initiator_loop(0, _, {Succeeded, Errors, _Measures, Total}) ->
    SuccessRate = Succeeded / Total,
    ErrorRate = Errors / Total,
    io:format("-------------------------------- Summary --------------------------------~n"),
    io:format("Sent ~w request - ratio: ~w/~w - Success rate: ~w - Error rate: ~w~n",[Total, Succeeded, Total, SuccessRate, ErrorRate]),
    io:format("-------------------------------------------------------------------------~n");
ds_initiator_loop(Left, TX_ANTD, {Succeeded, Errors, Measures, Total}) ->
    case ds_initiator_protocol(TX_ANTD) of
        ok -> ds_initiator_loop(Left - 1, TX_ANTD, {Succeeded+1, Errors, Measures, Total+1});
        error -> ds_initiator_loop(Left, TX_ANTD, {Succeeded, Errors+1, Measures, Total+1}) % No response to Poll message -> try again
    end.

ds_initiator_protocol(TX_ANTD) ->
    % Sending the first frame
    FrameControl = #frame_control{pan_id_compr = ?ENABLED},
    MacHeader = #mac_header{seqnum = 0, dest_pan = <<16#FFFF:16>>, dest_addr = <<16#FFFF:16>>, src_addr = <<16#FFFF:16>>},
    % enabling wait4resp to avoid an early timeout. Here we know that resp will take at least 10000 µs
    mac_layer:mac_send_data(FrameControl, MacHeader, <<"DS_INIT">>, #tx_opts{wait4resp = ?ENABLED, w4r_tim = 20000}),
    io:format("Poll message is sent~n"),
    case  mac_layer:mac_receive(true) of
        {_, _, <<"Resp_TX">>} -> #{tx_stamp := PollTXTimestamp} = pmod_uwb:read(tx_time),
                         #{rx_stamp := RespRXTimestamp} = pmod_uwb:read(rx_time),
                         FinalTXTime = RespRXTimestamp + (30000 * ?UUS_TO_DWT_TIME),
                         pmod_uwb:write(dx_time, #{dx_time => FinalTXTime}),
                         FinalTXTimestamp = FinalTXTime + TX_ANTD,
                         Message = <<PollTXTimestamp:40, RespRXTimestamp:40, FinalTXTimestamp:40>>,
                         % For now, same MAC header but seqnum should increase
                         pmod_uwb:write(sys_status, #{txfcg => 2#1}),
                         mac_layer:mac_send_data(FrameControl, MacHeader, Message, #tx_opts{txdlys = ?ENABLED, tx_delay = FinalTXTime}),
                         io:format("Final message sent~n"),
                         io:format("PollTX: ~w - RespRX ~w - FinalTX ~w~n", [PollTXTimestamp, RespRXTimestamp, FinalTXTimestamp]),
                         io:format("Data sent~n"),
                         timer:sleep(100);
        Err -> io:format("Reception error: ~w~n", [Err]),
               error
    end.


ds_responder() ->
    % Set the antenna delay -> !! the values should be calibrated
    TX_ANTD = 16436, pmod_uwb:write(tx_antd, #{tx_antd => TX_ANTD}),
    RX_ANTD = 16435, pmod_uwb:write(lde_if, #{lde_rxantd => RX_ANTD}),
    % ? Set preamble timeout too ?
    % Disabling wait t.o. for Poll frame
    ds_responder_loop(50, TX_ANTD, {0, 0, [], 0}).

ds_responder_loop(0, _, {Succeeded, Errors, Measures, Total}) ->
    SuccessRate = Succeeded/Total,
    ErrorRate = Errors/Total,
    MeasureAVG = lists:sum(Measures)/Succeeded,
    StdDev = std_dev(Measures, MeasureAVG, Succeeded, 0),
    io:format("-------------------------------- Summary --------------------------------~n"),
    io:format("Received ~w request - ratio: ~w/~w - Success rate: ~w - Error rate: ~w~n",[Total, Succeeded, Total, SuccessRate, ErrorRate]),
    io:format("Average distance measured: ~w - standard deviation: ~w ~n", [MeasureAVG, StdDev]),
    io:format("-------------------------------------------------------------------------~n");
ds_responder_loop(N, TX_ANTD, {Succeeded, Errors, Measures, Total}) ->
    pmod_uwb:write(sys_cfg, #{rxwtoe  => 2#0}),
    case ds_responder_protocol(TX_ANTD) of
        error_rx_poll -> ds_responder_loop(N, TX_ANTD, {Succeeded, Errors+1, Measures, Total+1});
        error -> ds_responder_loop(N-1, TX_ANTD, {Succeeded, Errors+1, Measures, Total+1});
        Distance -> ds_responder_loop(N-1, TX_ANTD, {Succeeded+1, Errors, [Distance | Measures], Total+1})
    end.

ds_responder_protocol(_TX_ANTD) ->
    case mac_layer:mac_receive() of
        {FrameControl, MacHeader, <<"DS_INIT">>} -> 
            io:format("Poll message received~n"),
            pmod_uwb:set_frame_timeout(16#FFFF), % TODO set back again later
            #{rx_stamp := PollRXTimestamp} = pmod_uwb:read(rx_time),
            RespTXTime = PollRXTimestamp + (20000 * ?UUS_TO_DWT_TIME),
            pmod_uwb:write(dx_time, #{dx_time => RespTXTime}),
            RespMacHeader = #mac_header{src_addr = <<16#FFFF:16>>, dest_pan = MacHeader#mac_header.src_pan, dest_addr = MacHeader#mac_header.src_addr, seqnum = MacHeader#mac_header.seqnum},
            mac_layer:mac_send_data(FrameControl, RespMacHeader, <<"Resp_TX">>, #tx_opts{wait4resp = ?ENABLED, w4r_tim = 20000}),
            io:format("Response message sent~n"),
            case mac_layer:mac_receive(true)of
                {_, _, <<PollTXTimestamp:40, RespRXTimestamp:40, FinalTXTimestamp:40>>} ->
                    #{tx_stamp := RespTXTimestamp} = pmod_uwb:read(tx_time),
                    #{rx_stamp := FinalRXTimestamp} = pmod_uwb:read(rx_time),

                    TRound1 = RespRXTimestamp - PollTXTimestamp,
                    TRound2 = FinalRXTimestamp - RespTXTimestamp,
                    TReply1 = RespTXTimestamp - PollRXTimestamp,
                    TReply2 = FinalTXTimestamp - RespRXTimestamp,

                    TProp = (TRound1 * TRound2 - TReply1 * TReply2)/(TRound1 + TRound2 + TReply1 + TReply2),
                    io:format("TProp: ~w~n", [TProp]),
                    TOF = TProp * ?TU,
                    Distance = TOF * ?C,
                    io:format("PollRX: ~w - RespTX ~w - FinalRX ~w~n", [PollRXTimestamp, RespTXTimestamp, FinalRXTimestamp]),
                    io:format("TRound1: ~w - TRound2 ~w - TReply1 ~w - TReply2 ~w ~n", [TRound1, TRound2, TReply1, TReply2]),
                    io:format("Computed distance: ~w~n", [Distance]),
                    Distance;
                Err -> io:format("Reception error: ~w~n", [Err]),
                       error
            end;
        Err -> io:format("Receiving error: ~w~n", [Err]), 
               error_rx_poll
    end.


%--- Single-sided two-way ranging -------------------------------------------------------------------

ss_initiator(0) -> ok;
ss_initiator(N) ->
    io:format("~w~n", [ss_initiator()]),
    timer:sleep(100),
    ss_initiator(N-1).

ss_initiator() ->
    % Setup 
    pmod_uwb:write(tx_antd, #{tx_antd => 16436}), % ! this value is not correct - the devices should be calibrated
    pmod_uwb:write(lde_if, #{lde_rxantd => 16436}),
    % TODO: can save energy by setting W4R_TIME (delay RX activation)

    % 2-way ranging
    FrameControl = #frame_control{pan_id_compr = ?ENABLED, dest_addr_mode = ?SHORT_ADDR, src_addr_mode = ?SHORT_ADDR},
    MacHeader = #mac_header{seqnum = 0, dest_pan = <<16#FFFF:16>>, dest_addr = <<16#FFFF:16>>, src_addr = <<16#FFFF:16>>},
    MacMessage = mac_layer:mac_message(FrameControl, MacHeader, <<"Wave">>),
    Options = #tx_opts{wait4resp = ?ENABLED, w4r_tim = 0},
    mac_layer:mac_send_data(FrameControl, MacHeader, MacMessage, Options),
    {_, _, Data} = mac_layer:mac_receive(true),
    io:format("Received data: ~w~n", [Data]),
    #{tx_stamp := PollTXTimestamp} = pmod_uwb:read(tx_time),
    #{rx_stamp := RespRXTimestamp} = pmod_uwb:read(rx_time),
    {PollRXTimestamp, RespTXTimestamp} = get_resp_ts(Data),
    io:format("PollTX: ~w - PollRX: ~w - RespTX: ~w - RespRX: ~w~n", [PollTXTimestamp, PollRXTimestamp, RespTXTimestamp, RespRXTimestamp]),
    Rtd_init = RespRXTimestamp - PollTXTimestamp,
    Rtd_resp = RespTXTimestamp - PollRXTimestamp,
    #{drx_car_int := DRX_CAR_INT} = pmod_uwb:read(drx_conf),
    ClockOffsetRatio = (DRX_CAR_INT * ?FREQ_OFFSET_MULTIPLIER *  ?HERTZ_TO_PPM_MUL),
    TimeOfFlight = ( (Rtd_init - Rtd_resp) * ((1-ClockOffsetRatio)/2) ) * ?TU, % TODO include the antenna delay
    TimeOfFlight * ?C.

get_resp_ts(Data) ->
    <<PollRXTimestamp:40, RespTXTimestamp:32, _:8>> = Data,
    {PollRXTimestamp, RespTXTimestamp}.

ss_responder(0) -> ok;
ss_responder(N) ->
    TX_ANTD = 16436,
    pmod_uwb:write(tx_antd, #{tx_antd => TX_ANTD}), % ! this value is not correct - the devices should be calibrated
    pmod_uwb:write(lde_if, #{lde_rxantd => 16436}),
    #{pan_id := PANID, short_addr := Addr} = pmod_uwb:read(panadr),
    {FrameControl, MacHeader, _} = mac_layer:mac_receive(),
    #{rx_stamp := PollRXTimestamp} = pmod_uwb:read(rx_time),
    RespTXTimestamp_ = PollRXTimestamp + (10000 * ?UUS_TO_DWT_TIME), % TODO bitshift ??
    pmod_uwb:write(dx_time, #{dx_time => RespTXTimestamp_}),
    RespTXTimestamp = RespTXTimestamp_ + TX_ANTD,
    TXData = <<PollRXTimestamp:40, RespTXTimestamp:40>>, % ! Antenna delay not included right now
    TXMacHeader = #mac_header{seqnum = MacHeader#mac_header.seqnum+1, dest_pan = MacHeader#mac_header.src_pan, dest_addr = MacHeader#mac_header.src_addr, src_pan = <<PANID:16>>, src_addr = <<Addr:16>>},
    Options = #tx_opts{txdlys = ?ENABLED, tx_delay = RespTXTimestamp},
    mac_layer:mac_send_data(FrameControl, TXMacHeader, TXData, Options),
    ss_responder(N-1).

ss_responder() ->
    ss_responder(1).



%--- Tool functions for stats -------------------------------------------------------------------

-spec std_dev(Measures :: list(), Mean :: number(), N :: number(), Acc :: number()) -> number().
std_dev([], _, N, Acc) ->
    math:sqrt(Acc/N);
std_dev([H | T], Mean, N, Acc) ->
    std_dev(T, Mean, N, Acc + math:pow(H-Mean, 2)).
