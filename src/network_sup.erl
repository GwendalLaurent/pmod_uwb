-module(network_sup).

-include_lib("eunit/include/eunit.hrl").

-behaviour(supervisor).

-export([start_link/0]).
-export([start_child/3]).
-export([terminate_child/1]).
-export([delete_child/1]).

%%% supervisor callbacks
-export([init/1]).


start_link() -> supervisor:start_link({local, ?MODULE}, ?MODULE, []).

start_child(ChildID, Module, Params) -> 
    ?debugMsg("Creating child"),
    {ok, Pid} = supervisor:start_child(?MODULE, #{id => ChildID, start => {Module, start_link, Params}}),
    ?debugMsg("Child created"),
    Pid.

terminate_child(ChildID) ->
    supervisor:terminate_child(?MODULE, ChildID).

delete_child(ChildID) ->
    supervisor:delete_child(?MODULE, ChildID).

%%% supervisor callbacks
init(_) ->
    {ok, { {one_for_all, 0, 1}, []} }.

