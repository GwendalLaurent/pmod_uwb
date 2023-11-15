-module(sender_receiver_simple_SUITE).

-include("../src/ieee802154.hrl").
-include("../src/mac_frame.hrl").

-include_lib("common_test/include/ct.hrl").

-export([all/0, groups/0, init_per_suite/1, end_per_suite/1, init_per_group/2, end_per_group/2, init_per_testcase/2, end_per_testcase/2]).
-export([sender/1]).
-export([receiver/1]).

all() -> [{group, simple_tx_rx}].

groups() -> [{simple_tx_rx, [parallel], [sender, receiver]}].

init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    ok.

init_per_group(simple_tx_rx, Config) ->
    FrameControl = #frame_control{src_addr_mode = ?EXTENDED, dest_addr_mode = ?EXTENDED}, 
    MacHeader = #mac_header{src_addr = <<16#CAFEDECA00000001:64>>, dest_addr = <<16#CAFEDECA00000002:64>>}, 
    Payload = <<"Test">>,
    Network = ieee_node:boot_network_node(),
    [{network, Network}, {frame_control, FrameControl}, {mac_header, MacHeader}, {payload, Payload} | Config];
init_per_group(_, Config) ->
    Config.

end_per_group(_, Config) ->
    Network = ?config(network, Config),
    ieee_node:stop_network_node(Network).

init_per_testcase(sender, Config) ->
    Network = ?config(network, Config),
    Node = ieee_node:boot_ieee802154_node(sender, Network, mac_extended_address, <<16#CAFEDECA00000001:64>>),
    [{sender, Node} | Config];
init_per_testcase(receiver, Config) ->
    Network = ?config(network, Config),
    Node = ieee_node:boot_ieee802154_node(receiver, Network, mac_extended_address, <<16#CAFEDECA00000002:64>>),
    [{receiver, Node} | Config];
init_per_testcase(_, Config) ->
    Config.

end_per_testcase(sender, Config) ->
    SenderNode = ?config(sender, Config),
    ieee_node:stop_ieee802154_node(SenderNode);
end_per_testcase(receiver, Config) ->
    ReceiverNode = ?config(receiver, Config),
    ieee_node:stop_ieee802154_node(ReceiverNode);
end_per_testcase(_, _Config) ->
    ok.

%--- Test cases -----------------------------------------------------------------------------

sender(Config) ->
    ct:sleep(100),
    Node = ?config(sender, Config),
    {FrameControl, MacHeader, Payload} = get_expected_frame(Config),
    erpc:call(Node, ieee802154, transmission, [FrameControl, MacHeader, Payload]).

receiver(Config) ->
    Node = ?config(receiver, Config),
    {FrameControl, MacHeader, Payload} = get_expected_frame(Config),
    {ok, {FrameControl, MacHeader, Payload}} = erpc:call(Node, ieee802154, reception, []).

get_expected_frame(Config) ->
    {?config(frame_control, Config), ?config(mac_header, Config), ?config(payload, Config)}.
