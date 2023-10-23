-module(ieee802154).
-behaviour(gen_statem).

-include_lib("eunit/include/eunit.hrl").

-include("mac_layer.hrl").

-export([start_link/1]).
-export([stop_link/0]).

-export([transmition/3]).
-export([reception/0]).

%%% gen_statem callbacks
-export([init/1]).
-export([callback_mode/0]).
-export([terminate/3]).
-export([code_change/4]).

-export([idle/3]).
-export([rx/3]).
-export([tx/3]).


% --- API --------------------------------------------------------------------------------

%% ---------------------------------------------------------------------------------------
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
%% ---------------------------------------------------------------------------------------
-spec start_link(Params::map()) -> {ok, pid()} | {error, any()}.
start_link(Params) -> gen_statem:start_link({local, ?MODULE}, ?MODULE, Params, []).

stop_link() ->
    gen_statem:stop(?MODULE).

-spec transmition(FrameControl :: #frame_control{}, FrameHeader :: #mac_header{}, Payload :: bitstring()) -> ok.
transmition(FrameControl, FrameHeader, Payload) -> gen_statem:call(?MODULE, {tx, FrameControl, FrameHeader, Payload}, infinity).

reception() -> 
    gen_statem:call(?MODULE, rx, infinity).

% --- gen_statem callbacks --------------------------------------------------------------
init(#{mac_layer := MAC}) ->
    MacState = gen_mac_layer:init(MAC, #{}),
    Data = #{cache => #{tx => [], rx => []}, mac_layer => MacState},
    {ok, idle, Data}.

callback_mode() ->
    state_functions.

idle({call, From}, {tx, FrameControl, FrameHeader, Payload}, Data) -> 
    {next_state, tx, Data, [{next_event, internal, {tx, FrameControl, FrameHeader, Payload, From}}]};

idle({call, From}, rx, Data) -> 
    {next_state, rx, Data, [{next_event, internal, {rx, From}}]}.

rx(_EventType, {rx, From}, #{mac_layer := MacState} = Data) ->
    case gen_mac_layer:rx(MacState) of
        {ok, NewMacState, {FrameControl, FrameHeader, Payload}} -> {next_state, idle, Data#{mac_layer => NewMacState}, [{reply, From, {FrameControl, FrameHeader, Payload}}]};
        {error, Err, NewMacState} -> {next_state, idle, Data#{mac_layer => NewMacState}, [{reply, From, Err}]}
    end.

tx(_EventType, {tx,FrameControl, MacHeader, Payload, From}, #{mac_layer := MacLayerState} = Data) -> 
    case gen_mac_layer:tx(MacLayerState, FrameControl, MacHeader, Payload) of
        {ok, NewMacState} -> {next_state, idle, Data#{mac_layer => NewMacState}, [{reply, From, ok}]};
        {error, Err, NewMacState} -> {next_state, idle, Data#{mac_layer => NewMacState}, [{reply, From, {error, Err}}]} 
    end.

terminate(Reason, _State, #{mac_layer := MacLayerState}) ->
    gen_mac_layer:stop(MacLayerState, Reason).

code_change(_, _, _, _) ->
    error(not_implemented).
