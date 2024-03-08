-module(ieee802154_unreliable_network_SUITE).

-include("../src/mac_frame.hrl").

-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").

%--- Exports -------------------------------------------------------------------

% Common test exports
-export([all/0]).
-export([groups/0]).
-export([init_per_suite/1]).
-export([end_per_suite/1]).
-export([init_per_group/2]).
-export([end_per_group/2]).
-export([init_per_testcase/2]).
-export([end_per_testcase/2]).

% Test cases
-export([sender/1]).
-export([receiver/1]).

%--- Macros --------------------------------------------------------------------

-define(SENDER_ADDR, <<16#CAFEDECA00000001:64>>).
-define(RCVR_ADDR, <<16#CAFEDECA00000002:64>>).

%--- Common test callbacks -----------------------------------------------------

all() -> [{group, unreliable_ack_exchange}].

groups() -> [{unreliable_ack_exchange, [sequential] , [{group, single_exchange}]},
             {single_exchange, [parallel, {repeat, 2}], [sender, receiver]}
            ].

%--- Suite setup/teardown
init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    ok.

%--- Group setup/teardown
init_per_group(unreliable_ack_exchange, Config) ->
    TemplateFC = #frame_control{frame_type = ?FTYPE_DATA,
                                src_addr_mode = ?EXTENDED,
                                dest_addr_mode = ?EXTENDED,
                                ack_req = ?ENABLED},
    TemplateMH = #mac_header{src_pan = <<16#CAFEDECA:16>>,
                             dest_pan = <<16#DECACAFE:16>>,
                             src_addr = ?SENDER_ADDR,
                             dest_addr = ?RCVR_ADDR},
    Payload = <<"Payload">>,
    {NetPid, Network} = ieee802154_node:boot_network_node(#{loss => true}),
    [{fc, TemplateFC},
     {mh, TemplateMH},
     {payload, Payload},
     {net_pid, NetPid},
     {network, Network} | Config];
init_per_group(_, Config) ->
    Config.

end_per_group(unreliable_ack_exchange, Config) ->
    Network = ?config(network, Config),
    NetPid = ?config(net_pid, Config),
    ieee802154_node:stop_network_node(Network, NetPid);
end_per_group(_, _Config) ->
    ok.

%--- Test cases setup/teardown
init_per_testcase(sender, Config) ->
    Network = ?config(network, Config),
    NodeRef = ieee802154_node:boot_ieee802154_node(sender, Network, mac_extended_address, ?SENDER_ADDR),
    [{sender, NodeRef} | Config];
init_per_testcase(receiver, Config) ->
    Network = ?config(network, Config),
    NodeRef = ieee802154_node:boot_ieee802154_node(receiver, Network, mac_extended_address, ?RCVR_ADDR),
    [{receiver, NodeRef} | Config].

end_per_testcase(Name, Config) ->
    {NodePid, Node} = ?config(Name, Config),
    ieee802154_node:stop_ieee802154_node(Node, NodePid).

%--- Test cases ----------------------------------------------------------------
sender(Config) ->
   {_, Node} = ?config(sender, Config),
   FC = ?config(fc, Config),
   TemplateMH = ?config(mh, Config),
   MH = TemplateMH#mac_header{seqnum = 7},
   Payload = ?config(payload, Config),
   timer:sleep(100),
   TxRet = erpc:call(Node, ieee802154, transmission, [{FC, MH, Payload}]),
   ?assertMatch({ok, _}, TxRet, "The transmission didn't succeed").

receiver(Config) ->
   {_, Node} = ?config(receiver, Config),
   ok = erpc:call(Node, ieee802154, rx_on, []),
   timer:sleep(10000),
   % TODO check if all received correctly,
   ok.

