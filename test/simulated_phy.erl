-module(simulated_phy).

-include("../src/mac_frame.hrl").

-export([start/0]).
-export([reception/2]).

start() ->
    Pid = spawn(fun() -> loop([]) end),
    register(phy, Pid),
    Pid.


reception(Pid, Address) -> 
    Ref = erlang:monitor(process, Pid),
    Pid ! {reception, self(), Ref, Address},
    receive 
        {Ref, Frame} -> 
            erlang:demonitor(Ref), 
            Frame
    after 5000 -> {error, timeout}
    end.

%--- Internal -----------------

loop(Waiting) ->
    receive 
        {reception, Pid, Ref, Address} -> loop([{Pid, Ref, Address} | Waiting]);
        {rx, Frame} -> NewWaiting = reply(Waiting, Frame), loop(NewWaiting)
    end.

reply([], _) -> [];
reply([{Pid, Ref, Address} | T] = Waiting, Frame) ->
    {_, MacHeader, _} = mac_frame:decode(Frame),
    case MacHeader#mac_header.dest_addr of
        Address -> Pid ! {Ref, Frame}, T;
        _ ->  Waiting
    end.
