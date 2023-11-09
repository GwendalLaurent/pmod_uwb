-module(ieee802154_stack_SUITE).

-include_lib("common_test/include/ct.hrl").

-export([all/0, init_per_testcase/2, end_per_testcase/2]).
-export([stack_test/1]).

-include("../src/mac_frame.hrl").
-include("../src/ieee802154.hrl").

all() -> [stack_test].

init_per_testcase(stack_test, Config) ->
    MockPhyPid = mock_phy:start_link(spi2, {perfect, #{}}),
    IEEE = ieee802154:start_link(#ieee_parameters{mac_parameters = [mock_phy, duty_cycle_non_beacon]}),
    [{ieee_pid, IEEE}, {phy_pid, MockPhyPid} | Config].

end_per_testcase(stack_test, _Config) ->
    ieee802154:stop_link(),
    mock_phy:stop_link().

stack_test(_Config) -> 
    ieee802154:rx_on(),
    ct:pal("~w~n", [sys:get_state(ieee802154)]),
    ieee802154:transmition(#frame_control{}, #mac_header{}, <<"Simple Frame">>),
    ieee802154:transmition(#frame_control{ack_req = ?ENABLED}, #mac_header{}, <<"AR Frame">>),
    ieee802154:rx_off().
