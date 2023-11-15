-module(mock_mac_network).

-include("../src/mac_frame.hrl").
-include("../src/gen_mac_layer.hrl").

-behaviour(gen_mac_layer).

-include_lib("common_test/include/ct.hrl").

%%% gen_mac_layer callbacks
-export([init/1]).
-export([tx/4]).
-export([rx/1]).
-export([rx_on/2]).
-export([rx_off/1]).
-export([get/2]).
-export([set/3]).
-export([terminate/2]).

%%% gen_mac_layer callbacks
-spec init(Params::list()) -> State :: term().
init(#{network := NetworkNode}) ->
    {network_loop, NetworkNode} ! {register, node()},
    PhyPid = mock_phy_network:start(),
    #{network => NetworkNode, phy => PhyPid, pib => #{mac_extended_address => <<16#FFFFFFFFFFFFFFFF:64>>, mac_short_address => <<16#FFFF:16>>, mac_pan_id => <<16#FFFF:16>>}};
init(_) ->
    #{pib => #{mac_extended_address => <<16#FFFFFFFFFFFFFFFF:64>>, mac_short_address => <<16#FFFF:16>>, mac_pan_id => <<16#FFFF:16>>}}.

-spec tx(State::term(), FrameControl::#frame_control{}, MacHeader::#mac_header{}, Payload::bitstring()) -> {ok, State::term()} | {ok, State::term()} | {error, State::term(), Error::tx_error()}.
tx(#{network := NetworkNode} = State, FrameControl, MacHeader, Payload) ->
    {network_loop, NetworkNode} ! {tx, mac_frame:encode(FrameControl, MacHeader, Payload)},
    {ok, State};
tx(State, _, _, _) ->
    {ok, State}.

-spec rx(State::term()) -> {ok, State::term(), {FrameControl::#frame_control{}, MacHeader::#mac_header{}, Payload::bitstring()}} | {error, State::term(), Error::atom()}.
rx(#{pib := PIB, phy := PhyPid} = State) ->
    case mock_phy_network:reception(PhyPid, get_pib(PIB, mac_extended_address)) of
        {error, Error} -> {error, State, Error};
        Frame -> {ok, State, mac_frame:decode(Frame)}
    end. 

-spec rx_on(State::term(), Callback::function()) -> {ok, State::term()}.
rx_on(_, _) ->
  error(not_implemented).

-spec rx_off(State::term()) -> {ok, State::term()}.
rx_off(_) ->
  error(not_implemented).

-spec get(State::term(), Attribute::gen_mac_layer:pibAttribute()) -> {ok, State::term(), Value::term()} | {error, State::term(), unsupported_attribute}.
get(#{pib := PIB} = State, Attribute) ->
    {ok, State, maps:get(Attribute, PIB)}.

-spec set(State::term(), Attribute::gen_mac_layer:pibAttribute(), Value::term()) -> {ok, State::term()} | {error, State::term(), gen_mac_layer:pibSetError()}.
set(#{pib := PIB} = State, Attribute, Value) ->
    {ok, State#{pib => maps:update(Attribute, Value, PIB)}}.

-spec terminate(State :: term(), Reason :: term()) -> ok.
terminate(_, _) -> ok.

%--- Internal --------------------------------------------

get_pib(PIB, Attribute) -> maps:get(Attribute, PIB).
