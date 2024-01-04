-module(mac_layer).
-behaviour(gen_mac_layer).

-include("gen_mac_layer.hrl").
-include("mac_frame.hrl").
-include("ieee802154.hrl").

%% This module implmements the gen_mac_layer behaviour.
%% This file can be taken as an example when you want to build a mock-up for the MAC layer

%%% gen_mac_layer callbacks
-export([init/1]).
-export([tx/5]).
-export([rx/1]).
-export([rx_on/3]).
-export([rx_off/1]).
-export([get/2]).
-export([set/3]).
-export([terminate/2]).

%--- Types ---------------------------------------------------------------------
-type csma_attributes() :: #{cw0 := cw0(), mac_max_BE := mac_max_BE(), mac_min_BE := mac_max_BE(), mac_max_csma_backoffs := mac_max_csma_backoff()}.
-type state() :: #{phy_layer := module(), duty_cycle := gen_duty_cycle:state(), retries := integer(), attributes := csma_attributes()}.

%--- mac_layer_behaviour callback functions ------------------------------------

% @doc Initialize the MAC layer
%
% By default, it activate fthe frame filtering with auto-acknowledgment
% It also set the frame waiting time out to MACACKWAITDURATION. However, the timeout isn't activated
% If you want to use it, it has to be activated later on.
%
% Params is a map that can contain different elements:
% * `phy_layer': specifying which driver it should use. By default it uses the driver of the pmod_uwb
% @end
-spec init(Params) -> State when
      Params :: #{phy_layer := module(), duty_cycle := module()},
      State  :: state().
init(#{phy_layer := PhyMod, duty_cycle := DutyCycleMod}) ->
    PhyMod:write(rx_fwto, #{rxfwto => ?MACACKWAITDURATION}),
    PhyMod:write(sys_cfg, #{ffab => 1, ffad => 1, ffaa => 1, ffam => 1, ffen => 1, autoack => 1, rxwtoe => 1}),
    DutyCycleState = gen_duty_cycle:start(DutyCycleMod, PhyMod),
    #{phy_layer => PhyMod,
      duty_cycle => DutyCycleState,
      retries => 0,
      attributes => default_attribute_values()}.

% @doc transmits a frame using the physical layer
% @end
-spec tx(State, FrameControl, MacHeader, Payload, Ranging) -> Result when
      State :: map(),
      FrameControl :: frame_control(),
      MacHeader :: mac_header(),
      Payload :: binary(),
      Ranging :: ranging_tx(),
      Result :: {ok, State} | {error, State, Error},
      Error :: tx_error().
tx(State, FrameControl, MacHeader, Payload, Ranging) ->
    DCState = maps:get(duty_cycle, State),
    Attributes = maps:get(attributes, State),
    CsmaParams = get_csma_params(Attributes),
    EncFrame = mac_frame:encode(FrameControl, MacHeader, Payload),
    case gen_duty_cycle:tx_request(DCState, EncFrame, CsmaParams, Ranging) of
        {ok, NewDutyCycleState} ->
            {ok, State#{duty_cycle => NewDutyCycleState}};
        {error, NewDutyCycleState, Error} ->
            {error, State#{duty_cycle => NewDutyCycleState}, Error}
    end.

% @doc Performs the reception of a single packet
%
% @param State: The state of the module
% @end
-spec rx(State) -> {ok, State, Frame} | {error, State, Error} when
      State :: state(),
      Frame :: frame(),
      Error :: atom().
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
-spec rx_on(State, Callback, RangingFlag) -> {ok, State} | {error, State, Error} when
      State :: map(),
      Callback :: input_callback(),
      RangingFlag :: pmod_uwb:flag(),
      Error       :: atom().
rx_on(#{duty_cycle := DutyCycleState} = State, Callback, RangingFlag) ->
    case gen_duty_cycle:turn_on(DutyCycleState, fun(Frame, LQI, _PRF, Security, _PreambleRep, _DataRate, Ranging) -> Callback(mac_frame:decode(Frame), LQI, Security, Ranging) end, RangingFlag) of
        {ok, NewDutyCycleState} ->
            {ok, State#{duty_cycle => NewDutyCycleState, ranging => RangingFlag}};
        {error, NewDutyCycleState, Error} ->
            {error, State#{duty_cycle => NewDutyCycleState, ranging => RangingFlag}, Error}
    end.

% @doc Turns off the continuous reception
% @end
rx_off(#{duty_cycle := DutyCycleState} = State) ->
    NewDutyCycleState = gen_duty_cycle:turn_off(DutyCycleState),
    {ok, State#{duty_cycle => NewDutyCycleState}}.

-spec get(State::term(), Attribute::gen_mac_layer:pibAttribute()) -> {ok, State::term(), Value::term()} | {error, State::term(), unsupported_attribute}.
get(#{phy_layer := PhyMod} = State, mac_extended_address) ->
    #{eui := EUI} = PhyMod:read(eui),
    {ok, State, EUI};
get(#{phy_layer := PhyMod} = State, mac_short_address) ->
    #{short_addr := ShortAddr} = PhyMod:read(panadr),
    {ok, State, ShortAddr};
get(#{phy_layer := PhyMod} = State, mac_pan_id) ->
    #{pan_id := PanId} = PhyMod:read(panadr),
    {ok, State, PanId};
get(#{attributes := Attributes} = State, Attribute) when is_map_key(Attribute, Attributes) ->
    {ok, State, maps:get(Attribute, Attributes)};
get(State, _) ->
    {error, State, unsupported_attribute}.

-spec set(State::term(), Attribute::gen_mac_layer:pibAttribute(), Value::term()) -> {ok, State::term()} | {error, State::term(), gen_mac_layer:pibSetError()}.
set(#{phy_layer := PhyMod} = State, mac_extended_address, Value) ->
    PhyMod:write(eui, #{eui => Value}), % TODO check the range/type/value given
    {ok, State};
set(#{phy_layer := PhyMod} = State, mac_short_address, Value) ->
    PhyMod:write(panadr, #{short_addr => Value}),
    {ok, State};
set(#{phy_layer := PhyMod} = State, mac_pan_id, Value) ->
    PhyMod:write(panadr, #{pan_id => Value}),
    {ok, State};
set(#{attributes := Attributes} = State, Attribute, Value) when is_map_key(Attribute, Attributes) ->
    {ok, State#{attributes => maps:update(Attribute, Value, Attributes)}};
set(State, _Attribute, _) ->
    {error, State, unsupported_attribute}. % TODO detect if PIB is a read only attribute

% @doc clean up function to stop the mac layer
% @end
terminate(#{duty_cycle := DutyCycleState}, Reason) ->
    gen_duty_cycle:stop(DutyCycleState, Reason).

%--- Internal ------------------------------------------------------------------

default_attribute_values() ->
    #{
      cw0 => 2, % cf. p.22 standard
      mac_max_BE => 5,
      mac_max_csma_backoffs => 4,
      mac_min_BE => 3
     }.

-spec get_csma_params(Attributes) -> CsmaParams when
      Attributes :: #{mac_min_BE := _,
                      mac_max_BE := _,
                      mac_max_csma_backoff := _,
                      cw0 := _},
      CsmaParams :: csma_params().
get_csma_params(Attributes) ->
    #csma_params{mac_min_BE = maps:get(mac_min_BE, Attributes),
                 mac_max_BE = maps:get(mac_max_BE, Attributes),
                 mac_max_csma_backoff = maps:get(mac_max_csma_backoffs,
                                                 Attributes),
                 cw0 = maps:get(cw0, Attributes)}.
