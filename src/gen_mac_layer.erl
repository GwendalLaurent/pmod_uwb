-module(gen_mac_layer).

-include("mac_layer.hrl").

-callback init(Params::term()) -> State :: term().
-callback tx(State::term(), FrameControl::#frame_control{}, MacHeader::#mac_header{}, Payload::bitstring()) -> {ok, State::term()} | {error, State::term(), Error::atom()}.
-callback rx(State::term()) -> {ok, State::term(), {FrameControl::#frame_control{}, MacHeader::#mac_header{}, Payload::bitstring()}} | {error, State::term(), Error::atom()}.
-callback terminate(State :: term(), Reason :: term()) -> ok.

-export([init/2]).
-export([tx/4]).
-export([rx/1]).
-export([stop/2]).

% ---------------------------------------------------------------------
% @doc Initialize the MAC layer using the Module given in the arguments
% Module has to implement the gen_mac_layer behaviour
% @end
% ---------------------------------------------------------------------
-spec init(Module::module(), Params::map()) -> State::term().
init(Module, Params) ->
    {Module, Module:init(Params)}.

% ---------------------------------------------------------------------
% @doc Transmission request to the MAC layer of a MAC frame
% @end
% ---------------------------------------------------------------------
-spec tx(State::term(), FrameControl::#frame_control{}, MacHeader::#mac_header{}, Payload::bitstring()) -> {ok, State::term()} | {error, State::term(), Error::atom()}.
tx({Mod, Sub}, FrameControl, MacHeader, Payload) ->
    case Mod:tx(Sub, FrameControl, MacHeader, Payload) of
        {ok, Sub2} -> {ok, {Mod, Sub2}};
        {error, Sub2, Err} -> {error, {Mod, Sub2}, Err}
    end.

% ---------------------------------------------------------------------
% @doc Transmission request to the MAC layer of a MAC frame
% @end
% ---------------------------------------------------------------------
-spec rx(State::term()) -> {ok, State::term(), {FrameControl::#frame_control{}, MacHeader::#mac_header{}, Payload::bitstring()}} | {error, State::term(), Error::atom()}.
rx({Mod, Sub}) ->
    case Mod:rx(Sub) of
        {ok, Sub2, {FrameControl, MacHeader, Payload}} -> {ok, {Mod, Sub2}, {FrameControl, MacHeader, Payload}};
        {error, Sub2, Error} -> {error, {Mod, Sub2}, Error}
    end.

stop({Mod, Sub}, Reason) ->
    Mod:terminate(Sub, Reason).
