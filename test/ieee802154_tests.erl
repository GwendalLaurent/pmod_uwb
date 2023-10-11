-module(ieee802154_tests).

-include_lib("eunit/include/eunit.hrl").

-include("../src/mac_layer.hrl").

%--- Setup ---------------------------------------------------------------------
setup() ->
    {ok, NetworkSup} = network_sup:start_link(),
    ?debugMsg("Creating the network stack"),
    % TODO Use a mock mac layer
    ieee802154:create_stack(#{mac_layer => #{module => mac_layer, state => {}, parameters => #{phy => mock_phy}}}),
    NetworkSup.

teardown(NetworkSup) ->
    ?debugFmt("~w~n", [NetworkSup]),
    network_sup:terminate_child(ieee802154), % TODO create a kill stack function
    network_sup:delete_child(ieee802154),
    exit(NetworkSup, normal),
    Ref = monitor(process, NetworkSup),
    receive
        {'DOWN', Ref, process, NetworkSup, _Reason} -> ok;
        _ -> ok
    end.



mac_test_() ->
    {setup, fun setup/0, fun teardown/1, [
        % Encode and decode test functions
        [fun reception_/0
        ]
        % Transmission and reception test functions
%         [fun transmission_/0]
     ]}.

%--- Tests ---------------------------------------------------------------------

reception_() ->
    ?assertEqual(rx_content, ieee802154:reception()).
