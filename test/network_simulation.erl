-module(network_simulation).

-behaviour(application).

-include_lib("common_test/include/ct.hrl").

%%% EXPORTS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% application callbacks
-export([start/2]).
-export([stop/1]).

%%% application callbacks %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec start(_, Args) -> {ok, pid()} when
      Args :: #{loss => boolean()}.
start(_, Args) ->
    Loss = maps:get(loss, Args, false),
    StartData = #{nodes => #{}, loss => Loss},
    LoopPid = spawn(fun() -> loop(ready, StartData) end),
    register(network_loop, LoopPid),
    {ok, LoopPid}.

stop(_) ->
    network_loop ! {stop},
    unregister(network_loop).

%%%% Internal %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec loop(State, Data) -> ok when
      State :: ready | blocked,
      Data  :: #{nodes := NodeMap, loss := boolean()},
      NodeMap :: #{node() => {non_neg_integer(), non_neg_integer()}}.
loop(ready, #{nodes := Nodes} = Data) ->
    receive
        {ping, Pid, Node} ->
            {Pid, Node} ! pong, loop(ready, Data);
        {register, Name} ->
            NewNodes = update_nodes(Name, Nodes),
            loop(ready, Data#{nodes => NewNodes});
        {tx, From, Frame} ->
            do_broadcast(Data, From, Frame);
        {stop} -> ok
    end;
loop(blocked, #{nodes := Nodes} = Data) ->
    receive 
        {register, _Name} ->
            loop(ready, Data);
        {tx, From, Frame} -> % Once a frame has been blocked => pass through
            broadcast(maps:keys(Nodes), From, Frame),
            loop(blocked, Data);
        {stop} ->
            ok;
        OtherEvent ->
            error(wrong_event_in_blocked, OtherEvent)
    end.

update_nodes(Name, Nodes) ->
    case maps:is_key(Name, Nodes) of
        true -> Nodes;
        false -> maps:put(Name, {0, 0}, Nodes)
    end.

do_broadcast(#{loss := false, nodes := Nodes} = Data, From, Frame) ->
    broadcast(maps:keys(Nodes), From, Frame),
    loop(ready, Data);
do_broadcast(#{loss := true, nodes := Nodes} = Data, From, Frame) ->
    case maps:get(From, Nodes) of
        {MemorySeen, MemorySeen} ->
            NewNodes = maps:update(From, {0, MemorySeen+1}, Nodes),
            loop(blocked, Data#{nodes => NewNodes});
        {RndSeen, MemorySeen} ->
            broadcast(maps:keys(Nodes), From, Frame),
            NewNodes = maps:update(From, {RndSeen+1, MemorySeen}, Nodes),
            loop(ready, Data#{nodes => NewNodes})
    end.

broadcast([], _, _) -> 
    ok;
broadcast([From | T], From, Frame) -> 
    broadcast(T, From, Frame);
broadcast([Node | T], From, Frame) -> 
    {mock_phy_network, Node} ! {frame, Frame},
    broadcast(T, From, Frame).
