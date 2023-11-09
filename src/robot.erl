-module(robot).

-behaviour(application).

-include("mac_frame.hrl").
-include("ieee802154.hrl").

-export([tx/0]).
-export([rx_on/0]).
-export([rx_off/0]).
-export([tx/1]).

% Callbacks
-export([start/2]).
-export([stop/1]).

% Sends/receive only 1 frame
tx() ->
    % FrameControl = #frame_control{ack_req = ?ENABLED},
    FrameControl = #frame_control{},
    MacHeader = #mac_header{},
    ieee802154:transmition(FrameControl, MacHeader, <<"Test">>).

rx_callback({_FrameControl, MacHeader, Payload}) ->
    io:format("Received frame with seqnum: ~w - Payload: ~w ~n", [MacHeader#mac_header.seqnum, Payload]).

rx_on() -> ieee802154:rx_on().
rx_off() -> ieee802154:rx_off().

tx(0) -> ok;
tx(N) ->
    ieee802154:transmition(#frame_control{ack_req = ?ENABLED}, #mac_header{seqnum = N}, <<16#F:(111*8)>>),
    tx(N-1).

start(_Type, _Args) -> 
    {ok, Supervisor} = robot_sup:start_link(),
    grisp:add_device(spi2, pmod_uwb),
    ieee802154:start_link(#ieee_parameters{mac_layer = mac_layer, input_callback = fun rx_callback/1}),
    % pmod_uwb:write(rx_fwto, #{rxfwto => 16#FFFF}),
    % pmod_uwb:write(sys_cfg, #{rxwtoe => 2#1}), 
    ieee802154:rx_on(),
    {ok, Supervisor}.

% @private
stop(_State) -> ok.
