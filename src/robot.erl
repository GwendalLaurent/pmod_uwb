-module(robot).

-behaviour(application).

-include("mac_layer.hrl").

-export([tx/0]).
-export([rx/0]).

-export([tx/1]).
-export([rx_loop/0]).

-export([tx/2]).
-export([rx_ack/0]).

% Callbacks
-export([start/2]).
-export([stop/1]).

% Sends/receive only 1 frame
tx() ->
    FrameControl = #frame_control{},
    MacHeader = #mac_header{},
    ieee802154:transmition(FrameControl, MacHeader, <<"Test">>).

rx() ->
    ieee802154:reception().

% Sends/receive multiple frames
tx(0) -> ok;
tx(N) -> 
    FrameControl = #frame_control{},
    MacHeader = #mac_header{seqnum = N},
    ieee802154:transmition(FrameControl, MacHeader, <<"Test">>),
    tx(N-1).

rx_loop() ->
    case ieee802154:reception() of
        {_, MacHeader, Payload} -> io:format("Received: ~w - With Seqnum: ~w~n", [Payload, MacHeader#mac_header.seqnum]);
        Err -> io:format("Err: ~w~n", [Err])
    end,
    rx_loop().

% Data/ACK exchange

tx(_, 0) -> ok;
tx(Count, Left) ->
    FrameControl = #frame_control{ack_req = ?ENABLED},
    MacHeader = #mac_header{seqnum = Count},
    ieee802154:transmition(FrameControl, MacHeader, <<"TX ACK trial">>),
    case ieee802154:reception() of
        {#frame_control{frame_type = ?FTYPE_ACK}, #mac_header{seqnum = Count}, _} -> tx(Count+1, Left-1);
        Err -> io:format("Error: ~w - retry ...~n", [Err]), tx(Count, Left)
    end.

rx_ack() ->
    case ieee802154:reception() of
        {#frame_control{frame_type = ?FTYPE_DATA, ack_req = ?ENABLED}, #mac_header{seqnum = Seqnum}, Payload} ->
            io:format("Received data frame with seqnum: ~w~n", [Seqnum]),
            ieee802154:transmition(#frame_control{frame_type = ?FTYPE_ACK}, #mac_header{seqnum = Seqnum}, <<>>);
        _ -> ok
    end,
    rx_ack().


start(_Type, _Args) -> 
    {ok, Supervisor} = robot_sup:start_link(),
    grisp:add_device(spi2, pmod_uwb),
    ieee802154:start_link(#{mac_layer => mac_layer}),
    % pmod_uwb:write(rx_fwto, #{rxfwto => 16#FFFF}),
    % pmod_uwb:write(sys_cfg, #{rxwtoe => 2#1}), 
    {ok, Supervisor}.

% @private
stop(_State) -> ok.
