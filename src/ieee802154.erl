-module(ieee802154).
-behaviour(gen_statem).


% API
-export([start_link/1]).
-export([stop_link/0]).

-export([transmition/3]).
-export([reception/0]).

-export([rx_on/0]).
-export([rx_off/0]).

-export([get_mac_extended_address/0]).
-export([set_mac_extended_address/1]).
-export([get_mac_short_address/0]).
-export([set_mac_short_address/1]).
-export([get_pan_id/0]).
-export([set_pan_id/1]).

% gen_statem callbacks
-export([init/1]).
-export([callback_mode/0]).
-export([terminate/3]).
-export([code_change/4]).

-export([idle/3]).
-export([rx/3]).
-export([tx/3]).

% Includes
-include_lib("eunit/include/eunit.hrl").

-include("ieee802154.hrl").
-include("mac_frame.hrl").

%--- API --------------------------------------------------------------------------------

%% @doc Starts the IEEE 812.15.4 stack and creates a link
%% 
%% The parameter map has to be composed of at least:
%% * mac_layer: The module that has to be used for the mac_layer 
%%
%% ```
%% The following code will start the stack using the mac_layer module
%% 1> ieee802154:start_link(#{mac_layer => mac_layer}).
%% 
%% Starting using a mock layer
%% 2> ieee802154:start_link(#mac_layer => mock_mac}).
%% '''
%%
%% @param Params: A map containing the parameters of the IEEE stack
%%
%% @end
-spec start_link(Params::#ieee_parameters{}) -> {ok, pid()} | {error, any()}.
start_link(Params) -> gen_statem:start_link({local, ?MODULE}, ?MODULE, Params, []).

stop_link() ->
    gen_statem:stop(?MODULE).

-spec transmition(FrameControl :: #frame_control{}, FrameHeader :: #mac_header{}, Payload :: bitstring()) -> ok.
transmition(FrameControl, FrameHeader, Payload) -> gen_statem:call(?MODULE, {tx, FrameControl, FrameHeader, Payload}, infinity).

%% @doc Wait for the reception of a frame and returns its content
%% @end
-spec reception() -> {FrameControl :: #frame_control{}, FrameHeader :: #mac_header{}, Payload :: bitstring()}.
reception() -> 
    gen_statem:call(?MODULE, rx, infinity).

%% @doc Turns on the continuous reception 
%% @end
rx_on() ->
    gen_statem:call(?MODULE, rx_on).

%% @doc Turns off the continuous reception 
%% @end
rx_off() ->
    gen_statem:call(?MODULE, rx_off).

%% @doc Gets the extended address of the device stored in the PIB
%% The function returns the address as a bitstring
-spec get_mac_extended_address() -> bitstring().
get_mac_extended_address() -> 
    gen_statem:call(?MODULE, {get, mac_extended_address}).

%% @doc Sets the mac extended address of the device and update the PIB
%% The Value is a bitstring with a size of 64 bits
-spec set_mac_extended_address(Value::bitstring()) -> ok.
set_mac_extended_address(Value) ->
    gen_statem:call(?MODULE, {set, mac_extended_address, Value}).

%% @doc Gets the short address of the device stored in the PIB
%% The function returns the address as a bitstring
-spec get_mac_short_address() -> bitstring().
get_mac_short_address() ->
    gen_statem:call(?MODULE, {get, mac_short_address}).

%% @doc Sets the mac short address of the device and update the PIB
%% The Value is a bitstring with a size of 16 bits
-spec set_mac_short_address(Value::bitstring()) -> ok.
set_mac_short_address(Value) ->
    gen_statem:call(?MODULE, {set, mac_short_address, Value}).

%% @doc Gets the PAN ID of the device stored in the PIB
%% The function returns the PAN ID as a bitstring
-spec get_pan_id() -> bitstring().
get_pan_id() ->
    gen_statem:call(?MODULE, {get, mac_pan_id}).

%% @doc Sets the PAN ID of the device and update the PIB
%% The Value is a bitstring with a size of 16 bits
-spec set_pan_id(bitstring()) -> ok.
set_pan_id(Value) ->
    gen_statem:call(?MODULE, {set, mac_pan_id, Value}).

%--- gen_statem callbacks --------------------------------------------------------------

init(Params) ->
    MacState = gen_mac_layer:start(Params#ieee_parameters.mac_layer, Params#ieee_parameters.mac_parameters),
    Data = #{cache => #{tx => [], rx => []}, mac_layer => MacState, input_callback => Params#ieee_parameters.input_callback},
    {ok, idle, Data}.

callback_mode() ->
    [state_functions].
    % [state_enter, state_functions].

terminate(Reason, _State, #{mac_layer := MacLayerState}) ->
    gen_mac_layer:stop(MacLayerState, Reason).

code_change(_, _, _, _) ->
    error(not_implemented).

%--- Idle State
idle({call, From}, rx_on, #{mac_layer := MacState, input_callback := Callback} = Data) -> 
    {ok, NewMacState} = gen_mac_layer:turn_on_rx(MacState, Callback),
    {next_state, rx, Data#{mac_layer => NewMacState}, {reply, From, ok}}; 

idle({call, From}, {tx, FrameControl, FrameHeader, Payload}, Data) -> 
    {next_state, tx, Data, [{next_event, internal, {tx, idle, FrameControl, FrameHeader, Payload, From}}]};

idle({call, From}, rx, #{mac_layer := MacState} = Data) -> % simple RX doesn't goes in RX state
    case gen_mac_layer:rx(MacState) of
        {ok, NewMacState, {FrameControl, FrameHeader, Payload}} -> {keep_state, Data#{mac_layer => NewMacState}, [{reply, From, {FrameControl, FrameHeader, Payload}}]};
        {error, Err, NewMacState} -> {keep_state, Data#{mac_layer => NewMacState}, [{reply, From, Err}]}
    end;

idle(EventType, EventContent, Data) -> handle_event(EventType, EventContent, idle, Data).

%---  RX State 
rx(enter, _OldState, Data) ->
    {next_state, rx, Data};

rx({call, From}, rx_on, Data) -> 
    {keep_state, Data, {reply, From, ok}};

rx({call, From}, rx_off, #{mac_layer := MacState} = Data) ->
    {ok, NewMacState} = gen_mac_layer:turn_off_rx(MacState), 
    {next_state, idle, Data#{mac_layer => NewMacState}, {reply, From, ok}};

rx({call, From}, {tx, FrameControl, FrameHeader, Payload}, Data)->
    % {ok, NewMacState} = gen_mac_layer:turn_off_rx(MacState),
    % {next_state, tx, Data#{mac_layer => NewMacState}, [{next_event, internal, {tx, rx, FrameControl, FrameHeader, Payload, From}}]}; 
    {next_state, tx, Data, [{next_event, internal, {tx, rx, FrameControl, FrameHeader, Payload, From}}]}; 

rx(_EventType, {rx, From}, #{mac_layer := MacState} = Data) ->
    case gen_mac_layer:rx(MacState) of
        {ok, NewMacState, {FrameControl, FrameHeader, Payload}} -> {next_state, idle, Data#{mac_layer => NewMacState}, [{reply, From, {FrameControl, FrameHeader, Payload}}]};
        {error, Err, NewMacState} -> {next_state, idle, Data#{mac_layer => NewMacState}, [{reply, From, Err}]}
    end;

rx(EventType, EventContent, Data) -> handle_event(EventType, EventContent, rx, Data).

%---  TX State
tx(enter, _OldState, Data) ->
    {next_state, tx, Data};

tx(_EventType, {tx, OldState, FrameControl, MacHeader, Payload, From}, #{mac_layer := MacLayerState} = Data) -> 
    case gen_mac_layer:tx(MacLayerState, FrameControl, MacHeader, Payload) of
        {ok, NewMacState} -> {next_state, OldState, Data#{mac_layer => NewMacState}, [{reply, From, ok}]};
        {error, NewMacState, Err} -> {next_state, OldState, Data#{mac_layer => NewMacState}, [{reply, From, {error, Err}}]} 
    end;

tx(EventType, EventContent, Data) -> handle_event(EventType, EventContent, tx, Data).

%--- Handle event
handle_event({call, From}, {get, Attribute}, _State, #{mac_layer := MacState} = Data) -> 
    case gen_mac_layer:get(MacState, Attribute) of
        {ok, NewMacState, Value} -> {keep_state, Data#{mac_layer => NewMacState}, [{reply, From, Value}]};
        {error, NewMacState, Error} -> {keep_state, Data#{mac_layer => NewMacState}, [{reply, From, {error, Error}}]}
    end;
handle_event({call, From}, {set, Attribute, Value}, _State, #{mac_layer := MacState} = Data) -> 
    case gen_mac_layer:set(MacState, Attribute, Value) of
        {ok, NewMacState} -> {keep_state, Data#{mac_layer => NewMacState}, [{reply, From, ok}]};
        {error, NewMacState, Error} -> {keep_state, Data#{mac_layer => NewMacState}, [{reply, From, {error, Error}}]}
    end;
handle_event({call, From}, _EventContent, _State, Data) -> {keep_state, Data, [{reply, From, {error, unknown_request}}]}.
