-module(robot).

-behaviour(application).

-include("mac_layer.hrl").

-export([tx/0]).
-export([rx/0]).
% Callbacks
-export([start/2]).
-export([stop/1]).

tx() ->
    FrameControl = #frame_control{},
    MacHeader = #mac_header{},
    ieee802154:transmition(FrameControl, MacHeader, <<"Test">>).

rx() ->
    ieee802154:reception().

start(_Type, _Args) -> 
    {ok, Supervisor} = robot_sup:start_link(),
    {ok, _NetworkSup} = network_sup:start_link(),
    % grisp:add_device(spi2, pmod_uwb),
    ieee802154:create_stack(#{}, {mac_layer, {}, #{}}, {pmod_uwb, {}, #{}}),
    {ok, Supervisor}.

% @private
stop(_State) -> ok.
