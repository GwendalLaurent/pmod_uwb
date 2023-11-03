-module(lossy_mock_mac).

-include("../src/mac_frame.hrl").
-include("../src/gen_mac_layer.hrl").

-behaviour(gen_mac_layer).

%%% gen_mac_layer callbacks
-export([init/1]).
-export([tx/4]).
-export([rx/2]).
-export([rx_on/2]).
-export([rx_off/1]).
-export([terminate/2]).

%%% gen_mac_layer callbacks
-spec init(Params::term()) -> State :: term().
init(Params) ->
    Params.

-spec tx(State::term(), FrameControl::#frame_control{}, MacHeader::#mac_header{}, Payload::bitstring()) -> {ok, State::term()} | {error, State::term(), Error::tx_error()}.
tx(State, #frame_control{ack_req = ?ENABLED}=FrameControl, MacHeader, Payload) ->
    Frame = mac_frame:encode(FrameControl, MacHeader, Payload),
    transmit(State, Frame);
tx(State, _, _, _) ->
    {ok, State}.

-spec rx(State::term(), RxEnabled::binary()) -> {ok, State::term(), {FrameControl::#frame_control{}, MacHeader::#mac_header{}, Payload::bitstring()}} | {error, State::term(), Error::atom()}.
rx(_, _) ->
  error(not_implemented).

-spec rx_on(State::term(), Callback::function()) -> {ok, State::term()}.
rx_on(_, _) ->
  error(not_implemented).

-spec rx_off(State::term()) -> {ok, State::term()}.
rx_off(_) ->
  error(not_implemented).

-spec terminate(State :: term(), Reason :: term()) -> ok.
terminate(_, _) ->
  ok.

%--- Internal --------------------------------------------

transmit(State, Frame) when (byte_size(Frame) < 125) ->
    {error, State, no_ack}. 

