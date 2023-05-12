-module(two_way_ranging).

-include("mac_layer.hrl").

-export([initiator/1, responder/1]).
-export([initiator/0, responder/0]).

%-define(TU, 1.565444993393822e-11). % 1 t.u. is ~1.5654e-11 s
-define(TU, 15.65e-12).
-define(C, 299792458). % Speed of light
% https://forum.qorvo.com/t/sample-programs/788/3
-define(UUS_TO_DWT_TIME, 65536). % in one UWB µs, there are 65536 t.u (UWB µs are special µs ???)

initiator(0) -> ok;
initiator(N) ->
    io:format("~w~n", [initiator()]),
    timer:sleep(500),
    initiator(N-1).

initiator() ->
    % Setup 
    pmod_uwb:write(tx_antd, #{tx_antd => 16436}), % ! this value is not correct - the devices should be calibrated
    pmod_uwb:write(lde_if, #{lde_rxantd => 16436}),
    % TODO: can save energy by setting W4R_TIME (delay RX activation)
    % TODO: Set RX TO ? Already quite big rn

    % 2-way ranging
    FrameControl = #frame_control{pan_id_compr = ?ENABLED, dest_addr_mode = ?SHORT_ADDR, src_addr_mode = ?SHORT_ADDR},
    MacHeader = #mac_header{seqnum = 0, dest_pan = <<16#FFFF:16>>, dest_addr = <<16#FFFF:16>>, src_addr = <<16#FFFF:16>>},
    MacMessage = mac_layer:mac_message(FrameControl, MacHeader, <<"Wave">>),
    Options = #tx_opts{wait4resp = ?ENABLED, w4r_tim = 0},
    {_, _, Data} = mac_layer:mac_send_data(FrameControl, MacHeader, MacMessage, Options),
    io:format("Received data: ~w~n", [Data]),
    #{tx_stamp := PollTXTimestamp} = pmod_uwb:read(tx_time),
    #{rx_stamp := RespRXTimestamp} = pmod_uwb:read(rx_time),
    {PollRXTimestamp, RespTXTimestamp} = get_resp_ts(Data),
    io:format("PollTX: ~w - PollRX: ~w - RespTX: ~w - RespRX: ~w~n", [PollTXTimestamp, PollRXTimestamp, RespTXTimestamp, RespRXTimestamp]),
    Rtd_init = RespRXTimestamp - PollTXTimestamp,
    Rtd_resp = RespTXTimestamp - PollRXTimestamp,
    TimeOfFlight = ( (Rtd_init - Rtd_resp) * (1/2) ) * ?TU, % TODO include the antenna delay
    TimeOfFlight * ?C.

get_resp_ts(Data) ->
    <<PollRXTimestamp:40, RespTXTimestamp:40>> = Data,
    {PollRXTimestamp, RespTXTimestamp}.

responder(0) -> ok;
responder(N) ->
    responder(),
    responder(N-1).

responder() ->
    #{pan_id := PANID, short_addr := Addr} = pmod_uwb:read(panadr),
    {FrameControl, MacHeader, _} = mac_layer:mac_receive(),
    #{rx_stamp := PollRXTimestamp} = pmod_uwb:read(rx_time),
    #{sys_time := SYS_TIME} = pmod_uwb:read(sys_time),
    RespTXTimestamp = PollRXTimestamp + (50000 * ?UUS_TO_DWT_TIME),
    io:format("RespTXTS: ~w - PollRXTS: ~w - Sys_time: ~w~n", [RespTXTimestamp, PollRXTimestamp, SYS_TIME]),
    pmod_uwb:write(dx_time, #{dx_time => RespTXTimestamp}),
    io:format("TX timestamp: ~w~n", [RespTXTimestamp]),
    TXData = <<PollRXTimestamp:40, (RespTXTimestamp bsr 8):32, 0:8>>, % ! Antenna delay not included right now
    io:format("Data sent: ~w~n", [TXData]),
    TXMacHeader = #mac_header{seqnum = MacHeader#mac_header.seqnum+1, dest_pan = MacHeader#mac_header.src_pan, dest_addr = MacHeader#mac_header.src_addr, src_pan = <<PANID:16>>, src_addr = <<Addr:16>>},
    Options = #tx_opts{txdlys = ?ENABLED, tx_delay = RespTXTimestamp},
    mac_layer:mac_send_data(FrameControl, TXMacHeader, TXData, Options),
    io:format("~w~n", [pmod_uwb:read(sys_state)]).
    % mac_layer:delayed_mac_send_data(FrameControl, TXMacHeader, TXData, RespTXTimestamp).
