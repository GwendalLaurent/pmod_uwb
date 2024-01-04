%% @doc Behaviour for frame encoding/decoding tasks
%% It should include the MLME/MPDU functions
%% @end
-module(gen_mac_layer).

% API
-export([start/2]).
-export([tx/5]).
-export([rx/1]).
-export([turn_on_rx/3]).
-export([turn_off_rx/1]).
-export([get/2]).
-export([set/3]).
-export([stop/2]).

%--- Includes ------------------------------------------------------------------
-include("ieee802154.hrl").
-include("mac_frame.hrl").
-include("gen_mac_layer.hrl").

%--- Callbacks -----------------------------------------------------------------
-callback init(Params) -> State when
      Params :: map(),
      State  :: term().
-callback tx(State, FrameControl, MacHeader, Payload, Ranging) -> Result when
      State :: term(),
      FrameControl :: frame_control(),
      MacHeader    :: mac_header(),
      Payload      :: binary(),
      Ranging      :: ranging_tx(),
      Result       :: {ok, State}
                      | {error, State, Error},
      Error        :: tx_error().
-callback rx(State) -> Result when
      State  :: term(),
      Result :: {ok, State, Frame}
                | {error, State, Error},
      Frame  :: frame(),
      Error  :: atom().
-callback rx_on(State, Callback, Ranging) -> Result when
      State    :: term(),
      Callback :: ieee802154:input_callback(),
      Ranging  :: flag(),
      Result   :: {ok, State} | {error, State, Error},
      Error    :: atom().
-callback rx_off(State::term()) -> {ok, State::term()}.
-callback get(State, Attribute) -> Result when
      State :: term(),
      Attribute :: pibAttribute(),
      Result    :: {ok, State, Value}
                   | {error, State, unsupported_attribute},
      Value     :: term().
-callback set(State, Attribute, Value) -> Result when
      State     :: term(),
      Attribute :: pibAttribute(),
      Value     :: term(),
      Result    :: {ok, State}
                   | {error, State, Error},
      Error     :: pibSetError().
-callback terminate(State :: term(), Reason :: term()) -> ok.

%--- Types ----------------------------------------------------------------
-export_type([pibAttribute/0, pibSetError/0, state/0]).

-type pibAttribute() :: mac_extended_address
                        | mac_short_address
                        | mac_pand_id
                        | atom().
-type pibSetError() :: read_only | unsupported_attribute | invalid_parameter.
-type state() :: {module(), term()}.

%--- API ------------------------------------------------------------------

%% @doc Initialize the MAC layer using the Module given in the arguments
%% Module has to implement the gen_mac_layer behaviour
%% The parameters are encapsulated in a list.
%% This avoids having to specify the whole stack when using a mockup
%% @end
-spec start(Module::module(), Params::map()) -> State::state().
start(Module, Params) ->
    {Module, Module:init(Params)}.

%% @doc Transmission request to the MAC layer of a MAC frame
%% @end
-spec tx(State, FrameControl, MacHeader, Payload, Ranging) -> Result when
      State :: state(),
      FrameControl :: frame_control(),
      MacHeader :: mac_header(),
      Payload :: bitstring(),
      Ranging :: ranging_tx(),
      Result  :: {ok, State}
                 | {error, State, Error},
      Error :: tx_error().
tx(State, #frame_control{dest_addr_mode=?NONE, src_addr_mode=?NONE}, _, _, _) ->
    {error, State, invalid_address};
tx({Mod, Sub}, FrameControl, MacHeader, Payload, Ranging) ->
    case Mod:tx(Sub, FrameControl, MacHeader, Payload, Ranging) of
        {ok, Sub2} -> {ok, {Mod, Sub2}};
        {error, Sub2, Err} -> {error, {Mod, Sub2}, Err}
    end.

%% @doc Transmission request to the MAC layer of a MAC frame
%% @end
-spec rx(State) -> {ok, State, frame()} | {error, State, Error::atom()} when
      State :: state().
rx({Mod, Sub}) ->
    case Mod:rx(Sub) of
        {ok, Sub2, {FrameControl, MacHeader, Payload}} ->
            {ok, {Mod, Sub2}, {FrameControl, MacHeader, Payload}};
        {error, Sub2, Error} ->
            {error, {Mod, Sub2}, Error}
    end.

%% @doc Turns on the continuous reception
%% It can be turned of using `turn_off_rx/1'
%% Note: if the receiption is already enable, nothing happens
%% @param State: The state of the Mac layer
%% @param Callback: the callback fx which is going to be called at frame rx.
%% @end
-spec turn_on_rx(State, Callback, Ranging) -> Result when
      State :: term(),
      Callback :: ieee802154:input_callback(),
      Ranging :: flag(),
      Result :: {ok, State} | {error, State, Error},
      Error :: atom().
turn_on_rx({Mod, Sub}, Callback, Ranging) ->
    case Mod:rx_on(Sub, Callback, Ranging) of
        {ok, Sub2} -> {ok, {Mod, Sub2}};
        {error, Sub2, Error} -> {error, {Mod, Sub2}, Error}
    end.

-spec get(State, Attribute) -> Result when
      State     :: state(),
      Attribute :: pibAttribute(),
      Result    :: {ok, State, Value}
                   | {error, State, unsupported_attribute},
      Value     :: term().
get({Mod, Sub}, Attribute) ->
    {Status, Sub2, Ret} = Mod:get(Sub, Attribute),
    {Status, {Mod, Sub2}, Ret}.

-spec set(State, Attribute, Value) -> Result when
      State     :: state(),
      Attribute :: pibAttribute(),
      Value     :: term(),
      Result    :: {ok, State}
                   | {error, State, Error},
      Error     :: pibSetError().
set({Mod, Sub}, Attribute, Value) ->
    case Mod:set(Sub, Attribute, Value) of
        {ok, Sub2} -> {ok, {Mod, Sub2}};
        {error, Sub2, Error} -> {error, {Mod, Sub2}, Error}
    end.

%% @doc Turns off the continuous reception
%% Note: if the reception is already off, noting happens
%% @end
-spec turn_off_rx(State) -> {ok, State} when
      State :: state().
turn_off_rx({Mod, Sub}) ->
    {ok, Sub2} = Mod:rx_off(Sub),
    {ok, {Mod, Sub2}}.

stop({Mod, Sub}, Reason) ->
    Mod:terminate(Sub, Reason).
