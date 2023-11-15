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
        {tx, Frame} -> broadcast(Nodes, Frame), loop(State);
        {stop} -> ok
    end.

broadcast(Nodes, Frame) -> 
    io:format("Broadcasting"),
    [{phy, Node} ! {rx, Frame} || Node <- Nodes].
