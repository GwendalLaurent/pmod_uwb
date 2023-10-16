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


%%% gen_statem callbacks
init(#{mac_layer := MAC}) ->
    MAC:start_link(#{}, #{}),
    Data = #{cache => #{tx => [], rx => []}, mac_layer => MAC},
    {ok, idle, Data}.

callback_mode() ->
    state_functions.

idle({call, From}, {tx, FrameControl, FrameHeader, Payload}, Data) -> 
    {next_state, tx, Data, [{next_event, internal, {tx, FrameControl, FrameHeader, Payload, From}}]};

idle({call, From}, rx, Data) -> 
    {next_state, rx, Data, [{next_event, internal, {rx, From}}]}.

rx(_EventType, {rx, From}, #{mac_layer := MacLayer} = Data) ->
    case MacLayer:reception() of
        {FrameControl, FrameHeader, Payload} -> {next_state, idle, Data, [{reply, From, {FrameControl, FrameHeader, Payload}}]};
        Err -> {next_state, idle, Data, [{reply, From, Err}]}
    end.

tx(_EventType, {tx,FrameControl, FrameHeader, Payload, From}, #{mac_layer := MacLayer} = Data) -> 
    case MacLayer:send_data(FrameControl, FrameHeader, Payload) of
        ok -> {next_state, idle, Data, [{reply, From, ok}]};
        Err -> {next_state, idle, Data, [{reply, From, Err}]}
    end.

terminate(_Reason, _State, Data) ->
    #{mac_layer := MAC} = Data,
    MAC:stop_link().

code_change(_, _, _, _) ->
    error(not_implemented).


