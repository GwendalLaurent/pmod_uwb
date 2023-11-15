-module(ieee_node).

-export([boot_node/1]).
-export([get_project_cwd/0]).

-include_lib("common_test/include/ct.hrl").

-define(ROBOT_REL_DIR, "/_build/default/rel/robot").

boot_node(Name) ->
    ProjectCWD = get_project_cwd(),
    Flags = ["-pa", ProjectCWD ++ ?ROBOT_REL_DIR ++ "/lib/robot-0.1.0/ebin"],
    {ok, Pid, NodeName} = ?CT_PEER(#{name => Name, args => Flags}),
    unlink(Pid),
    NodeName.

get_project_cwd() -> 
    {ok, Path} = file:get_cwd(),
    filename:dirname(filename:dirname(filename:dirname(filename:dirname(Path)))).
