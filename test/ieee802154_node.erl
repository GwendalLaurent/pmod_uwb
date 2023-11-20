-module(ieee802154_node).

-export([boot_network_node/0, stop_network_node/2]).
-export([boot_ieee802154_node/4, stop_ieee802154_node/2]).
-export([boot_ieee802154_node/5]).
-export([boot_node/1]).
-export([get_project_cwd/0]).

-include_lib("common_test/include/ct.hrl").

-include("../src/ieee802154.hrl").

% -define(ROBOT_REL_DIR, "/_build/default/rel/robot").
-define(ROBOT_LIB_DIR, "/_build/default/lib").

-spec boot_network_node() -> node().
boot_network_node() ->
    {Pid, Network} = boot_node(network),
    erpc:call(Network, network_simulation, start, [{}, {}]),
    ping_node(network_loop, Network),
    {Pid, Network}.

-spec stop_network_node(Network::node(), NetPid::pid()) -> ok.
stop_network_node(Network, NetPid) ->
    erpc:call(Network, network_simulation, stop, [{}]),
    peer:stop(NetPid).

-spec boot_ieee802154_node(Name::atom(), Network::node(), AddressType::mac_extended_address|mac_short_address, Address::bitstring()) -> node().
boot_ieee802154_node(Name, Network, AddressType, Address)           ->
    boot_ieee802154_node(Name, Network, AddressType, Address, fun() -> ok end).

-spec boot_ieee802154_node(Name::atom(), Network::node(), AddressType::mac_extended_address|mac_short_address, Address::bitstring(), Callback::function()) -> {pid(), node()}.
boot_ieee802154_node(Name, Network, AddressType, Address, Callback) ->
    {Pid, Node} = boot_node(Name),
    erpc:call(Node, mock_phy_network, start, [spi2, #{network => Network}]), % Starting the the mock driver/physical layer
    erpc:call(Node, ieee802154, start, [#ieee_parameters{mac_layer = mac_layer, mac_parameters = #{phy_layer => mock_phy_network, duty_cycle => duty_cycle_non_beacon}, input_callback = Callback }]),
    case AddressType of
        mac_extended_address -> erpc:call(Node, ieee802154, set_mac_extended_address, [Address]);
        mac_short_address -> erpc:call(Node, ieee802154, set_mac_short_address, [Address])
    end,
    {Pid, Node}.

-spec stop_ieee802154_node(Node::node(), NodePid::pid()) -> ok.
stop_ieee802154_node(Node, NodePid) ->
    erpc:call(Node, ieee802154, stop, []),
    peer:stop(NodePid).

-spec boot_node(Name::atom()) -> {pid(), node()}.
boot_node(Name) ->
    ProjectCWD = get_project_cwd(),
    %Flags = ["-pa", ProjectCWD ++ ?ROBOT_REL_DIR ++ "/lib/robot-0.1.0/ebin"],
    Flags = ["-pa", ProjectCWD ++ ?ROBOT_LIB_DIR ++ "/robot/ebin"],
    {ok, Pid, NodeName} = ?CT_PEER(#{name => Name, args => Flags}),
    unlink(Pid),
    {Pid, NodeName}.

-spec get_project_cwd() -> string().
get_project_cwd() -> 
    {ok, Path} = file:get_cwd(),
    filename:dirname(filename:dirname(filename:dirname(filename:dirname(Path)))).

ping_node(RegisteredName, Node) ->
    register(ping, self()),
    {RegisteredName, Node} ! {ping, ping, node()},
    receive pong -> ct:pal("Node: ~w says pong", [Node])
    after 2000 -> error(network_node_not_started)
    end,
    unregister(ping).
