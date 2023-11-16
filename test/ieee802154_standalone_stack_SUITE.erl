-module(ieee802154_standalone_stack_SUITE).

-include_lib("common_test/include/ct.hrl").

-export([all/0, init_per_testcase/2, end_per_testcase/2]).
-export([stack_test/1]).
-export([stack_read_write_test/1]).
-export([double_on_off/1]).

-include("../src/mac_frame.hrl").
-include("../src/ieee802154.hrl").

all() -> [stack_test, stack_read_write_test, double_on_off].

init_per_testcase(_, Config) ->
    ct:pal("~w", [Config]),
    MockPhyPid = mock_phy:start_link(spi2, {perfect, #{}}),
    IEEE = ieee802154:start_link(#ieee_parameters{mac_parameters = #{phy_layer => mock_phy, duty_cycle => duty_cycle_non_beacon}}),
    [{ieee_pid, IEEE}, {phy_pid, MockPhyPid} | Config].

end_per_testcase(_, _Config) ->
    ieee802154:stop_link(),
    mock_phy:stop_link().

%--- Test cases -------------------------------------------------
%
stack_test(_Config) -> 
    ieee802154:rx_on(),
    ieee802154:transmission(#frame_control{}, #mac_header{}, <<"Simple Frame">>),
    ieee802154:transmission(#frame_control{ack_req = ?ENABLED}, #mac_header{}, <<"AR Frame">>),
    ieee802154:rx_off(),
    ieee802154:transmission(#frame_control{}, #mac_header{}, <<"Simple Frame">>),
    ieee802154:transmission(#frame_control{ack_req = ?ENABLED}, #mac_header{}, <<"AR Frame">>).

stack_read_write_test(_Config) ->
    ieee802154:get_mac_extended_address(),
    ieee802154:set_mac_extended_address(<<16#DECACAFEDECACAFE:64>>),
    ieee802154:get_pan_id(),
    ieee802154:set_pan_id(<<16#CAFE:16>>),
    ieee802154:get_mac_short_address(),
    ieee802154:set_mac_short_address(<<16#DECA:16>>).

double_on_off(_Config) ->
    ieee802154:rx_on(),
    ieee802154:rx_on(),
    ieee802154:rx_off(),
    ieee802154:rx_off().
