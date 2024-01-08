-module(ieee802154).
-behaviour(gen_server).

%%% @headerfile "ieee802154.hrl"

% API
-export([start_link/1]).
-export([start/1]).
-export([stop_link/0]).
-export([stop/0]).

-export([transmission/1]).
-export([transmission/2]).
-export([reception/0]).

-export([rx_on/0]).
-export([rx_on/1]).
-export([rx_off/0]).

-export([get_mac_extended_address/0]).
-export([set_mac_extended_address/1]).
-export([get_mac_short_address/0]).
-export([set_mac_short_address/1]).
-export([get_pan_id/0]).
-export([set_pan_id/1]).

% gen_server callbacks
-export([init/1]).
-export([terminate/2]).
-export([code_change/4]).
-export([handle_call/3]).
-export([handle_cast/2]).


% Includes
-include_lib("eunit/include/eunit.hrl").

-include("ieee802154.hrl").
-include("mac_frame.hrl").

%--- Types ---------------------------------------------------------------------
-type state() :: #{mac_layer := gen_mac_layer:state(),
                   input_callback := input_callback(),
                   _:=_}.

%--- API -----------------------------------------------------------------------

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
-spec start_link(Params) -> {ok, pid()} | {error, any()} when
      Params :: ieee_parameters().
start_link(Params) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Params, []).

start(Params) ->
    gen_server:start({local, ?MODULE}, ?MODULE, Params, []).

stop_link() ->
    gen_server:stop(?MODULE).

stop() -> gen_server:stop(?MODULE).

%% @doc
%% No timestamp because its value is the same as Ranging counter start
-spec transmission(Frame) -> Result when
      Frame        :: frame(),
      Result       :: {ok, Ranging} | {error, Error},
      Ranging      :: ranging_informations(),
      Error        :: atom().
transmission(Frame) ->
    transmission(Frame, ?NON_RANGING).

-spec transmission(Frame, Ranging) -> Result when
      Frame        :: frame(),
      Ranging      :: ranging_tx(),
      Result       :: {ok, Ranging} | {error, Error},
      Ranging      :: ranging_informations(),
      Error        :: atom().
transmission(Frame, Ranging) ->
    gen_server:call(?MODULE,
                    {tx, Frame, Ranging},
                    infinity).

%% @doc Wait for the reception of a frame and returns its content
%% @end
-spec reception() -> {FrameControl, FrameHeader, Payload} when
      FrameControl :: frame_control(),
      FrameHeader  :: mac_header(),
      Payload      :: binary().
reception() ->
    gen_server:call(?MODULE, {rx}, infinity).

%% @doc Turns on the continuous reception
%% @end
-spec rx_on() -> Result when
      Result :: ok | {error, atom()}.
rx_on() ->
    gen_server:call(?MODULE, {rx_on, ?DISABLED}).

-spec rx_on(Ranging) -> ok when
      Ranging :: ?DISABLED | ?ENABLED.
rx_on(Ranging) ->
    gen_server:call(?MODULE, {rx_on, Ranging}).

%% @doc Turns off the continuous reception
%% @end
rx_off() ->
    gen_server:call(?MODULE, {rx_off}).

%% @doc Gets the extended address of the device stored in the PIB
%% The function returns the address as a bitstring
-spec get_mac_extended_address() -> bitstring().
get_mac_extended_address() ->
    gen_server:call(?MODULE, {get, mac_extended_address}).

%% @doc Sets the mac extended address of the device and update the PIB
%% The Value is a bitstring with a size of 64 bits
-spec set_mac_extended_address(Value::bitstring()) -> ok.
set_mac_extended_address(Value) ->
    gen_server:call(?MODULE, {set, mac_extended_address, Value}).

%% @doc Gets the short address of the device stored in the PIB
%% The function returns the address as a bitstring
-spec get_mac_short_address() -> bitstring().
get_mac_short_address() ->
    gen_server:call(?MODULE, {get, mac_short_address}).

%% @doc Sets the mac short address of the device and update the PIB
%% The Value is a bitstring with a size of 16 bits
-spec set_mac_short_address(Value::bitstring()) -> ok.
set_mac_short_address(Value) ->
    gen_server:call(?MODULE, {set, mac_short_address, Value}).

%% @doc Gets the PAN ID of the device stored in the PIB
%% The function returns the PAN ID as a bitstring
-spec get_pan_id() -> bitstring().
get_pan_id() ->
    gen_server:call(?MODULE, {get, mac_pan_id}).

%% @doc Sets the PAN ID of the device and update the PIB
%% The Value is a bitstring with a size of 16 bits
-spec set_pan_id(bitstring()) -> ok.
set_pan_id(Value) ->
    gen_server:call(?MODULE, {set, mac_pan_id, Value}).

%--- gen_statem callbacks ------------------------------------------------------

-spec init(Params) -> {ok, State} when
      Params :: ieee_parameters(),
      State  :: state().
init(Params) ->
    MacState = gen_mac_layer:start(Params#ieee_parameters.mac_layer,
                                   Params#ieee_parameters.mac_parameters),
    Data = #{cache => #{tx => [], rx => []},
             mac_layer => MacState,
             input_callback => Params#ieee_parameters.input_callback},
    {ok, Data}.

-spec terminate(Reason, State) -> ok when
      Reason :: term(),
      State :: map().
terminate(Reason, #{mac_layer := MacLayerState}) ->
    gen_mac_layer:stop(MacLayerState, Reason).

code_change(_, _, _, _) ->
    error(not_implemented).

-spec handle_call(_, _, State) -> Result when
      State   :: state(),
      Result  :: {reply, term(), State}.
handle_call({rx_on, Ranging}, _From, State) ->
    #{mac_layer := MacState, input_callback := Callback} = State,
    case gen_mac_layer:turn_on_rx(MacState, Callback, Ranging) of
        {ok, NewMacState} ->
            {reply, ok, State#{mac_layer => NewMacState}};
        {error, NewMacState, Error} ->
            {reply, {error, Error}, State#{mac_layer => NewMacState}}
    end;
handle_call({rx_off}, _From, #{mac_layer := MacState} = State) ->
    {ok, NewMacState} = gen_mac_layer:turn_off_rx(MacState),
    {reply, ok, State#{mac_layer => NewMacState}};
handle_call({tx, Frame, Ranging}, _From, State) ->
    #{mac_layer := MacState} = State,
    case gen_mac_layer:tx(MacState, Frame, Ranging) of
        {ok, NewMacState, RangingInfo} ->
            % Ret = {ok, RangingInfo},
            {reply, {ok, RangingInfo}, State#{mac_layer => NewMacState}};
        {error, NewMacState, Error} ->
            {reply, {error, Error}, State#{mac_layer => NewMacState}}
    end;
handle_call({rx}, _From, #{mac_layer := MacState} = State) ->
    case gen_mac_layer:rx(MacState) of
        {ok, NewMacState, Frame} ->
            {reply, {ok, Frame}, State#{mac_layer => NewMacState}};
        {error, NewMacState, Error} ->
            {reply, {error, Error}, State#{mac_layer => NewMacState}}
    end;
handle_call({get, Attribute}, _From, #{mac_layer := MacState} = State) ->
    case gen_mac_layer:get(MacState, Attribute) of
        {ok, NewMacState, Value} ->
            {reply, Value, State#{mac_layer => NewMacState}};
        {error, NewMacState, Error} ->
            {keep_state, {error, Error}, State#{mac_layer => NewMacState}}
    end;
handle_call({set, Attribute, Value}, _From, #{mac_layer := MacState} = State) ->
    case gen_mac_layer:set(MacState, Attribute, Value) of
        {ok, NewMacState} ->
            {reply, ok, State#{mac_layer => NewMacState}};
        {error, NewMacState, Error} ->
            {reply, {error, Error}, State#{mac_layer => NewMacState}}
    end;
handle_call(_Request, _From, _State) ->
    error(call_not_recognized).

handle_cast(_, _) ->
    error(not_implemented).
