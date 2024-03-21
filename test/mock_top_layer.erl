-module(mock_top_layer).

-behaviour(gen_server).

-include("../src/mac_frame.hrl").

-include_lib("common_test/include/ct.hrl").

%%% EXPORT %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% API functions
-export([start/0]).
-export([rx_frame/4]).
-export([dump/0]).
-export([stop/0]).

%%% gen_server callbacks
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).

%%% MACROS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 

-define(RCVR_ADDR, <<16#CAFEDECA00000002:64>>).
-define(MDL_ADDR, <<16#CAFEDECA00000003:64>>).

%%% API %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 
start() ->
    gen_server:start({local, ?MODULE}, ?MODULE, [], []).

rx_frame(Frame, _, _, _) ->
    gen_server:cast(?MODULE, {rx, Frame}).

dump() ->
    gen_server:call(?MODULE, {dump}).

stop() ->
    gen_server:stop(?MODULE).

%%% gen_server callbacks %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
init(_) ->
    {ok, #{received => []}}.

handle_call({dump}, _From, #{received := Received} = State) ->
    {reply, {ok, lists:reverse(Received)}, State#{received => []}};
handle_call(_, _, _) ->
  error(not_implemented).

handle_cast({rx, Frame}, #{received := Received} = State) ->
    {FC, MH, Payload} = Frame,
    case MH#mac_header.dest_addr of
        ?MDL_ADDR ->
            NewMH = MH#mac_header{src_addr = ?MDL_ADDR, dest_addr = ?RCVR_ADDR},
            NewFrame = {FC, NewMH, Payload},
            ieee802154:transmission(NewFrame);
        _ ->
            ok
    end,
    {noreply, State#{received => [Frame | Received]}};
handle_cast(_, _) ->
  error(not_implemented).

handle_info(_, _) ->
  error(not_implemented).

terminate(_, _) ->
    ok.
