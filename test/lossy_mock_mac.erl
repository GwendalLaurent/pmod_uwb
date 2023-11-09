-module(lossy_mock_mac).

-include("../src/mac_frame.hrl").
-include("../src/gen_mac_layer.hrl").

-behaviour(gen_mac_layer).

%%% gen_mac_layer callbacks
-export([init/1]).
-export([tx/4]).
-export([rx/1]).
-export([rx_on/2]).
-export([rx_off/1]).
-export([terminate/2]).

%%% gen_mac_layer callbacks
init(Params) ->
    Params.

tx(State, #frame_control{ack_req = ?ENABLED}=FrameControl, MacHeader, Payload) ->
    Frame = mac_frame:encode(FrameControl, MacHeader, Payload),
    transmit(State, Frame);
tx(State, _, _, _) ->
    {ok, State}.

rx(_) ->
  error(not_implemented).

rx_on(_, _) ->
  error(not_implemented).

rx_off(_) ->
  error(not_implemented).

terminate(_, _) ->
  ok.

%--- Internal --------------------------------------------

transmit(State, Frame) when (byte_size(Frame) < 125) ->
    {error, State, no_ack}. 

