% @doc robot public API.
-module(robot).

-behavior(application).

-include("mac_layer.hrl").

% Callbacks
-export([start/2]).
-export([stop/1]).
-export([ss_initiator/0, ss_responder/0]).

%-define(TU, 1.565444993393822e-11). % 1 t.u. is ~1.5654e-11 s
-define(TU, 15.65e-12).
-define(C, 299792458). % Speed of light
% https://forum.qorvo.com/t/sample-programs/788/3
-define(UUS_TO_DWT_TIME, 65536). % in one UWB µs, there are 65536 t.u (UWB µs are special µs ???)
-define(FREQ_OFFSET_MULTIPLIER, 1/( 131072 * 2 * (1024/998.4e6))).
-define(HERTZ_TO_PPM_MUL, 1.0e-6/6489.6e6).

-define(NBR_MEASUREMENTS, 250).
-define(TX_ANTD, 23500).
-define(RX_ANTD, 23500).

%--- Single-sided two-way ranging -------------------------------------------------------------------

ss_initiator() ->
    pmod_uwb:write(tx_antd, #{tx_antd => ?TX_ANTD}), % ! this value is not correct - the devices should be calibrated
    pmod_uwb:write(lde_if, #{lde_rxantd => ?RX_ANTD}),
    ss_initiator(?NBR_MEASUREMENTS, []).

ss_initiator(0, Measurements) -> 
    Total = length(Measurements),
    MeasureAVG = lists:sum(Measurements)/Total,
    StdDev = std_dev(Measurements, MeasureAVG, Total, 0),
    io:format("-------------------------------- Summary --------------------------------~n"),
    io:format("Sent ~w request -~n",[Total]),
    io:format("Average distance measured: ~w - standard deviation: ~w ~n", [MeasureAVG, StdDev]),
    io:format("Min: ~w - Max ~w~n", [lists:min(Measurements), lists:max(Measurements)]),
    io:format("-------------------------------------------------------------------------~n"),
    Measurements;
ss_initiator(N, Measurements) ->
    Measure = ss_initiator_loop(),
    io:format("~w~n", [Measure]),
    timer:sleep(100),
    case Measure of 
        error -> ss_initiator(N-1, Measurements);
        _ -> ss_initiator(N-1, [Measure | Measurements])
    end.

ss_initiator_loop() ->
    % Builds the MAC frame for Poll message
    FrameControl = #frame_control{pan_id_compr = ?ENABLED, dest_addr_mode = ?SHORT_ADDR, src_addr_mode = ?SHORT_ADDR},
    MacHeader = #mac_header{seqnum = 0, dest_pan = <<16#FFFF:16>>, dest_addr = <<16#FFFF:16>>, src_addr = <<16#FFFF:16>>},
    Options = #tx_opts{wait4resp = ?ENABLED, w4r_tim = 0},

    mac_layer:mac_send_data(FrameControl, MacHeader, <<"GRiSP">>, Options),

    {_, _, Data} = mac_layer:mac_receive(true), % Reception of Resp
    %io:format("Received data: ~w~n", [Data]),

    % Getting the timestamps of the TX of Poll and of the RX of Resp
    #{tx_stamp := PollTXTimestamp} = pmod_uwb:read(tx_time),
    #{rx_stamp := RespRXTimestamp} = pmod_uwb:read(rx_time),

    {PollRXTimestamp, RespTXTimestamp} = get_resp_ts(Data), % Getting the timestamps sent by the responder
    % io:format("PollTX: ~w - PollRX: ~w - RespTX: ~w - RespRX: ~w~n", [PollTXTimestamp, PollRXTimestamp, RespTXTimestamp, RespRXTimestamp]),

    TRound = RespRXTimestamp - PollTXTimestamp,
    TResp = RespTXTimestamp - PollRXTimestamp,

    % Getting the clock offset ratio
    #{drx_car_int := DRX_CAR_INT} = pmod_uwb:read(drx_conf),
    ClockOffsetRatio = (DRX_CAR_INT * ?FREQ_OFFSET_MULTIPLIER *  ?HERTZ_TO_PPM_MUL),
    
    io:format("PollTX ~w - RespRX ~w - PollRX ~w - RespTX ~w~n", [PollTXTimestamp, RespRXTimestamp, PollRXTimestamp, RespTXTimestamp]),
    TimeOfFlight = ( (TRound - TResp) * ((1-ClockOffsetRatio)/2) ) * ?TU, 
    if
        (RespRXTimestamp >= PollTXTimestamp) and (RespTXTimestamp >= PollRXTimestamp) -> TimeOfFlight * ?C;
        true -> error
    end.

get_resp_ts(Data) ->
    <<PollRXTimestamp:40, RespTXTimestamp:40>> = Data,
    {PollRXTimestamp, RespTXTimestamp}.

ss_responder() ->
    pmod_uwb:write(tx_antd, #{tx_antd => ?TX_ANTD}), 
    pmod_uwb:write(lde_if, #{lde_rxantd => ?RX_ANTD}),
    #{pan_id := PANID, short_addr := Addr} = pmod_uwb:read(panadr),
    ss_responder_loop(?NBR_MEASUREMENTS, PANID, Addr).

ss_responder_loop(0, _, _) -> ok;
ss_responder_loop(N, PANID, Addr) ->
    {FrameControl, MacHeader, _} = mac_layer:mac_receive(),
    #{rx_stamp := PollRXTimestamp} = pmod_uwb:read(rx_time),

    RespTXTimestamp_ = PollRXTimestamp + (20000 * ?UUS_TO_DWT_TIME), 
    pmod_uwb:write(dx_time, #{dx_time => RespTXTimestamp_}),

    RespTXTimestamp = RespTXTimestamp_ + ?TX_ANTD,
    TXData = <<PollRXTimestamp:40, RespTXTimestamp:40>>, 
    TXMacHeader = #mac_header{seqnum = MacHeader#mac_header.seqnum+1, dest_pan = MacHeader#mac_header.src_pan, dest_addr = MacHeader#mac_header.src_addr, src_pan = <<PANID:16>>, src_addr = <<Addr:16>>},
    Options = #tx_opts{txdlys = ?ENABLED, tx_delay = RespTXTimestamp},

    mac_layer:mac_send_data(FrameControl, TXMacHeader, TXData, Options),
    ss_responder_loop(N-1, PANID, Addr).

%--- Tool functions for stats -------------------------------------------------------------------

-spec std_dev(Measures :: list(), Mean :: number(), N :: number(), Acc :: number()) -> number().
std_dev([], _, N, Acc) ->
    math:sqrt(Acc/N);
std_dev([H | T], Mean, N, Acc) ->
    std_dev(T, Mean, N, Acc + math:pow(H-Mean, 2)).


%--- Callbacks -----------------------------------------------------------------

% @private
start(_Type, _Args) -> 
    {ok, Supervisor} = robot_sup:start_link(),
    grisp:add_device(spi2, pmod_uwb),
    % Res = pmod_uwb:read(dev_id),
    {ok, Supervisor}.

% @private
stop(_State) -> ok.
