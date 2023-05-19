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
    ds_initiator_protocol(50, TX_ANTD).

ds_initiator_protocol(0, _) -> ok;
ds_initiator_protocol(N, TX_ANTD) ->
    % Sending the first frame
    FrameControl = #frame_control{pan_id_compr = ?ENABLED},
    MacHeader = #mac_header{seqnum = 0, dest_pan = <<16#FFFF:16>>, dest_addr = <<16#FFFF:16>>, src_addr = <<16#FFFF:16>>},
    % enabling wait4resp to avoid an early timeout. Here we know that resp will take at least 10000 µs
    mac_layer:mac_send_data(FrameControl, MacHeader, <<"DS_INIT">>, #tx_opts{wait4resp = ?ENABLED, w4r_tim = 10000}),
    case  mac_layer:mac_receive(true) of
        {_, _, _Data} -> #{tx_stamp := PollTXTimestamp} = pmod_uwb:read(tx_time),
                         #{rx_stamp := RespRXTimestamp} = pmod_uwb:read(rx_time),
                         FinalTXTime = RespRXTimestamp + (30000 * ?UUS_TO_DWT_TIME),
                         pmod_uwb:write(dx_time, #{dx_time => FinalTXTime}),
                         FinalTXTimestamp = FinalTXTime + TX_ANTD,
                         Message = <<PollTXTimestamp:40, RespRXTimestamp:40, FinalTXTimestamp:40>>,
                         % For now, same MAC header but seqnum should increase
                         pmod_uwb:write(sys_status, #{txfcg => 2#1}),
                         mac_layer:mac_send_data(FrameControl, MacHeader, Message, #tx_opts{txdlys = ?ENABLED, tx_delay = FinalTXTime}),
                         io:format("PollTX: ~w - RespRX ~w - FinalTX ~w~n", [PollTXTimestamp, RespRXTimestamp, FinalTXTimestamp]),
                         io:format("Data sent~n");
        Err -> io:format("Reception error: ~w~n", [Err]),
               ok
    end,
    ds_initiator_protocol(N-1, TX_ANTD).



ds_responder() ->
    % Set the antenna delay -> !! the values should be calibrated
    TX_ANTD = 16436, pmod_uwb:write(tx_antd, #{tx_antd => TX_ANTD}),
    RX_ANTD = 16435, pmod_uwb:write(lde_if, #{lde_rxantd => RX_ANTD}),
    % ? Set preamble timeout too ?
    pmod_uwb:write(sys_cfg, #{rxwtoe  => 2#0}),
    ds_responder_protocol(50, TX_ANTD, RX_ANTD).

ds_responder_protocol(0, _, _) -> ok;
ds_responder_protocol(N, TX_ANTD, RX_ANTD) ->
    % Disabling wait t.o. for Poll frame
    case mac_layer:mac_receive() of
        {FrameControl, MacHeader, _} -> 
            pmod_uwb:set_frame_timeout(16#FFFF), % TODO set back again later
            #{rx_stamp := PollRXTimestamp} = pmod_uwb:read(rx_time),
            RespTXTime = PollRXTimestamp + (20000 * ?UUS_TO_DWT_TIME),
            pmod_uwb:write(dx_time, #{dx_time => RespTXTime}),
            RespMacHeader = #mac_header{src_addr = <<16#FFFF:16>>, dest_pan = MacHeader#mac_header.src_pan, dest_addr = MacHeader#mac_header.src_addr, seqnum = MacHeader#mac_header.seqnum},
            mac_layer:mac_send_data(FrameControl, RespMacHeader, <<(RespTXTime + TX_ANTD):40>>, #tx_opts{wait4resp = ?ENABLED, w4r_tim = 20000}),
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
                    ds_responder_protocol(N-1, TX_ANTD, RX_ANTD);
                Err -> io:format("Reception error: ~w~n", [Err]),
                       ds_responder_protocol(N-1, TX_ANTD, RX_ANTD)
            end;
        Err -> io:format("Receiving error: ~w~n", [Err]), 
               ds_responder_protocol(N-1, TX_ANTD, RX_ANTD)
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
