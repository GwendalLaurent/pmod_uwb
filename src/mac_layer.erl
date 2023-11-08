-module(mac_layer).
-behaviour(gen_mac_layer).

-include("gen_mac_layer.hrl").

%% This module implmements the gen_mac_layer behaviour.
%% This file can be taken as an example when you want to build a mock-up for the MAC layer

%%% gen_mac_layer callbacks
-export([init/1]).
-export([tx/4]).
-export([rx/1]).
-export([rx_on/2]).
-export([rx_off/1]).        
-export([terminate/2]).

%--- mac_layer_behaviour callback functions ---------------------------------------------

% @doc Initialize the MAC layer
% 
% By default, it activate fthe frame filtering with auto-acknowledgment
% It also set the frame waiting time out to MACACKWAITDURATION. However, the timeout isn't activated
% If you want to use it, it has to be activated later on.
% 
% Params is a map that can contain different elements:
% * `phy_layer': specifying which driver it should use. By default it uses the driver of the pmod_uwb
% @end
-spec init(Params::list()) -> {ok, State :: term()}.
init([PhyMod, DutyCycleMod]) ->
    PhyMod:write(rx_fwto, #{rxfwto => ?MACACKWAITDURATION}),
    PhyMod:write(sys_cfg, #{ffab => 1, ffad => 1, ffaa => 1, ffam => 1, ffen => 1, autoack => 1}),
    DutyCycleState = gen_duty_cycle:start(DutyCycleMod, PhyMod),
    #{phy_layer => PhyMod, duty_cycle => DutyCycleState, retries => 0}. 

% @doc transmits a frame using the physical layer
% @end
tx(#{duty_cycle := DutyCycleState} = State, FrameControl, MacHeader, Payload) ->
    case gen_duty_cycle:tx_request(DutyCycleState, mac_frame:encode(FrameControl, MacHeader, Payload)) of
        {ok, NewDutyCycleState} -> {ok, State#{duty_cycle => NewDutyCycleState}};
        {error, NewDutyCycleState, Error} -> {error, State#{duty_cycle => NewDutyCycleState}, Error}
    end.

% @doc Performs the reception of a single packet
% 
% @param State: The state of the module
% @end
rx(#{duty_cycle := DutyCycleState} = State) ->
    case gen_duty_cycle:rx_request(DutyCycleState) of
        {ok, NewDutyCycleState, Frame} -> {ok, State#{duty_cycle => NewDutyCycleState}, mac_frame:decode(Frame)};
        {error, NewDutyCycleState, Error} -> {error, State#{duty_cycle => NewDutyCycleState}, Error}
    end.

% @doc Turns on the continuous reception
% 
% When a packet is received, it will call the function specified in the parameter `Callback'
% If the received packet is a data packet requesting an ACL and the autoack is turned on (by default)
% the ACK is handled by the hardware before calling the callback
% 
% @end
rx_on(#{duty_cycle := DutyCycleState} = State, Callback) ->
    NewDutyCycleState = gen_duty_cycle:turn_on(DutyCycleState, fun(Frame) -> Callback(mac_frame:decode(Frame)) end),
    {ok, State#{duty_cycle => NewDutyCycleState}}.

% @doc Turns off the continuous reception
% @end
rx_off(#{duty_cycle := DutyCycleState} = State) ->
    NewDutyCycleState = gen_duty_cycle:turn_off(DutyCycleState),
    {ok, State#{duty_cycle => NewDutyCycleState}}.

% @doc clean up function to stop the mac layer
% @end
terminate(#{duty_cycle := DutyCycleState}, Reason) -> 
    gen_duty_cycle:stop(DutyCycleState, Reason).
