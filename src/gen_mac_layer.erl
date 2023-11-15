%% @doc The modules implementing this behaviour is responsible for the frame encoding/decoding
%% It should include the MLME/MPDU functions
%% @end
-module(gen_mac_layer).

% API
-export([start/2]).
-export([tx/4]).
-export([rx/1]).
-export([turn_on_rx/2]).
-export([turn_off_rx/1]).
-export([get/2]).
-export([set/3]).
-export([stop/2]).

% Includes
-include("mac_frame.hrl").
-include("gen_mac_layer.hrl").

% Callbacks
-callback init(Params::map()) -> State :: term().
-callback tx(State::term(), FrameControl::#frame_control{}, MacHeader::#mac_header{}, Payload::bitstring()) -> {ok, State::term()} | {ok, State::term()} | {error, State::term(), Error::tx_error()}.
-callback rx(State::term()) -> {ok, State::term(), {FrameControl::#frame_control{}, MacHeader::#mac_header{}, Payload::bitstring()}} | {error, State::term(), Error::atom()}.
-callback rx_on(State::term(), Callback::function()) -> {ok, State::term()}.
-callback rx_off(State::term()) -> {ok, State::term()}.
-callback get(State::term(), Attribute::pibAttribute()) -> {ok, State::term(), Value::term()} | {error, State::term(), unsupported_attribute}.
-callback set(State::term(), Attribute::pibAttribute(), Value::term()) -> {ok, State::term()} | {error, State::term(), pibSetError()}.
-callback terminate(State :: term(), Reason :: term()) -> ok.

%--- Types ----------------------------------------------------------------
-export_type([pibAttribute/0, pibSetError/0]).

-type pibAttribute() :: mac_extended_address | mac_short_address | mac_pand_id | atom().
-type pibSetError() :: read_only | unsupported_attribute | invalid_parameter.

%--- API ------------------------------------------------------------------

%% @doc Initialize the MAC layer using the Module given in the arguments
%% Module has to implement the gen_mac_layer behaviour
%% The parameters are encapsulated in a list. This avoids having to specify the whole stack when using a mockup
%% @end
-spec start(Module::module(), Params::map()) -> State::term().
start(Module, Params) ->
    {Module, Module:init(Params)}.

%% @doc Transmission request to the MAC layer of a MAC frame
%% @end
-spec tx(State::term(), FrameControl::#frame_control{}, MacHeader::#mac_header{}, Payload::bitstring()) -> {ok, State::term()} | {error, State::term(), Error::tx_error()}.
tx({Mod, Sub}, #frame_control{dest_addr_mode = ?NONE, src_addr_mode = ?NONE}, _, _) ->
    {error, {Mod, Sub}, invalid_address};
tx({Mod, Sub}, FrameControl, MacHeader, Payload) ->
    case Mod:tx(Sub, FrameControl, MacHeader, Payload) of
        {ok, Sub2} -> {ok, {Mod, Sub2}};
        {error, Sub2, Err} -> {error, {Mod, Sub2}, Err}
    end.

%% @doc Transmission request to the MAC layer of a MAC frame
%% @end
-spec rx(State::term()) -> {ok, State::term(), {FrameControl::#frame_control{}, MacHeader::#mac_header{}, Payload::bitstring()}} | {error, State::term(), Error::atom()}.
rx({Mod, Sub}) ->
    case Mod:rx(Sub) of
        {ok, Sub2, {FrameControl, MacHeader, Payload}} -> {ok, {Mod, Sub2}, {FrameControl, MacHeader, Payload}};
        {error, Sub2, Error} -> {error, {Mod, Sub2}, Error}
    end.

%% @doc Turns on the continuous reception
%% It can be turned of using `turn_off_rx/1'
%% Note: if the receiption is already enable, nothing happens
%% @param State: The state of the Mac layer
%% @param Callback: the callback function which is going to be called when a frame is received.
%%                  The callback function must have only arameter: the frame put in a tuple
%% @end
-spec turn_on_rx(State::term(), Callback::function()) -> {ok, State::term()}.
turn_on_rx({Mod, Sub}, Callback) ->
    {ok, Sub2} = Mod:rx_on(Sub, Callback),
    {ok, {Mod, Sub2}}.

-spec get(State::term(), Attribute::pibAttribute()) -> {ok, State::term(), Value::term()} | {error, State::term(), unsupported_attribute}.
get({Mod, Sub}, Attribute) ->
    {Status, Sub2, Ret} = Mod:get(Sub, Attribute),
    {Status, {Mod, Sub2}, Ret}.

-spec set(State::term(), Attribute::pibAttribute(), Value::term()) -> {ok, State::term()} | {error, State::term(), Error::pibSetError()}.
set({Mod, Sub}, Attribute, Value) ->
    case Mod:set(Sub, Attribute, Value) of
        {ok, Sub2} -> {ok, {Mod, Sub2}};
        {error, Sub2, Error} -> {error, {Mod, Sub2}, Error}
    end.

%% @doc Turns off the continuous reception
%% Note: if the reception is already off, noting happens
%% @end
-spec turn_off_rx(State::term()) -> {ok, State::term()}.
turn_off_rx({Mod, Sub}) ->
    {ok, Sub2} = Mod:rx_off(Sub), 
    {ok, {Mod, Sub2}}.

stop({Mod, Sub}, Reason) ->
    Mod:terminate(Sub, Reason).
