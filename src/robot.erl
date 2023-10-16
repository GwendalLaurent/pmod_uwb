-module(robot).

-behaviour(application).

-include("mac_layer.hrl").

-export([tx/0]).
-export([rx/0]).

-export([tx_ack/0]).
-export([rx_ack/0]).
% Callbacks
-export([start/2]).
-export([stop/1]).

tx() ->
    FrameControl = #frame_control{},
    MacHeader = #mac_header{},
    ieee802154:transmition(FrameControl, MacHeader, <<"Test">>).

rx() ->
    ieee802154:reception().

tx_ack() ->
    pmod_uwb:write(panadr, #{pan_id => 16#BEEF, short_addr => 16#0001}),
    pmod_uwb:write(rx_fwto, #{rxfwto => 16#FFFF}),
    pmod_uwb:write(sys_cfg, #{rxwtoe => 2#1}), 
    #{short_addr := SrcAddr} = pmod_uwb:read(panadr),
    pmod_uwb:write(sys_cfg, #{ffaa => 2#1, autoack => 2#1}), % allow ACK and data frame reception and enable autoack
    pmod_uwb:write(sys_cfg, #{ffen => 2#1}), % enable frame filtering and allow ACK frame reception and enable autoack
    FrameControl = #frame_control{ack_req = ?ENABLED, pan_id_compr = ?ENABLED},
    MacHeader = #mac_header{seqnum = 0, dest_pan = <<16#BEEF:16>>, dest_addr = <<16#0002:16>>, src_addr = <<SrcAddr:16>>},
    % io:format("Sending message #~w with seqnum ~w~n", [Cnt, Seqnum]),
    mac_layer:send_data(FrameControl, MacHeader, <<"Hello">>, #tx_opts{wait4resp = ?ENABLED, w4r_tim = 0}),
    case  mac_layer:reception(true) of
        {#frame_control{frame_type = ?FTYPE_ACK} = _RxFrameControl, #mac_header{seqnum = Seqnum} = _RxMacHeader, _RxData} -> ok;
        _ -> io:format("Reception error. Trying again...~n"),
               pmod_uwb:write(sys_status, #{rxfto => 2#1})% reset rxfto to avoid false t.o.
    end.

rx_ack() -> 
    pmod_uwb:write(panadr, #{pan_id => 16#BEEF, short_addr => 16#0002}),
    pmod_uwb:write(sys_cfg, #{ffad => 2#1, autoack => 2#1}), % allow ACK and data frame reception and enable autoack
    pmod_uwb:write(sys_cfg, #{ffen => 2#1}), % enable frame filtering and allow ACK frame reception and enable autoack
    case mac_layer:reception(false) of
        {#frame_control{frame_type = ?FTYPE_DATA, pan_id_compr = ?ENABLED} = _FrameControl, MacHeader, _Data} -> 
            io:format("Received data from ~w with seqnum ~w~n", [MacHeader#mac_header.src_addr, MacHeader#mac_header.seqnum]),
            pmod_uwb:wait_for_transmission();
            % pmod_uwb:write(sys_status, #{txfrs => 2#1});
        {_, _, _} -> io:format("Received unexpected frame~n");
        Err -> io:format("Reception error: ~w~n", [Err])
    end.

start(_Type, _Args) -> 
    {ok, Supervisor} = robot_sup:start_link(),
    grisp:add_device(spi2, pmod_uwb),
    ieee802154:start_link(#{mac_layer => mac_layer}),
    {ok, Supervisor}.

% @private
stop(_State) -> ok.
