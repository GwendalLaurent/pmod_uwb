-module(network_simulation).

-behaviour(application).

-include_lib("common_test/include/ct.hrl").

%%% application callbacks
-export([start/2]).
-export([stop/1]).

%%% application callbacks
start(_, _) ->
    io:format("Starting"),
    LoopPid = spawn(fun() -> loop(#{nodes => []}) end),
    register(network_loop, LoopPid),
    {ok, LoopPid}.

stop(_) ->
    network_loop ! {stop},
    unregister(network_loop).

%--- Internal -----------------------------

loop(#{nodes := Nodes} = State) ->
    io:format("Looping"),
    receive
        {ping, Pid, Node} -> {Pid, Node} ! pong, loop(State);
        {register, Name} -> loop(State#{nodes => [Name | Nodes]});
        {unreg, Name} -> loop(State#{nodes => lists:filter(fun(Elem) -> Elem =/= Name end, Nodes)});
        {tx, From, Frame} -> broadcast(Nodes, From, Frame), loop(State);
        {stop} -> ok
    end.

broadcast([], _, _) -> 
    ok;
broadcast([From | T], From, Frame) -> 
    broadcast(T, From, Frame);
broadcast([Node | T], From, Frame) -> 
    {mock_phy_network, Node} ! {frame, Frame},
    broadcast(T, From, Frame).
