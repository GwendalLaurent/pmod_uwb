-module(ieee802154_tests).

-include_lib("eunit/include/eunit.hrl").

-include("../src/mac_layer.hrl").

%--- Setup ---------------------------------------------------------------------
setup() ->
    {ok, NetworkSup} = network_sup:start_link(),
    % TODO Use a mock mac layer
    ieee802154:create_stack(#{}, {mock_mac, {}, #{}}),
    NetworkSup.

teardown(NetworkSup) ->
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
        [fun reception_/0,
         fun transmission_/0,
         fun ar_tx_/0]
     ]}.

%--- Tests ---------------------------------------------------------------------

transmission_() ->
    FrameControl = #frame_control{pan_id_compr = ?ENABLED, frame_version = 2#00},
    MacHeader = #mac_header{seqnum = 0, dest_pan = <<16#DECA:16>>, dest_addr = <<"RX">>, src_pan = <<16#DECA:16>>, src_addr = <<"TX">>},
    Payload = <<"Hello">>,
    ?assertEqual(ok, ieee802154:transmition(FrameControl, MacHeader, Payload)).

reception_() ->
    FrameControl = #frame_control{pan_id_compr = ?ENABLED, frame_version = 2#00},
    MacHeader = #mac_header{seqnum = 0, dest_pan = <<16#DECA:16>>, dest_addr = <<"RX">>, src_pan = <<16#DECA:16>>, src_addr = <<"TX">>},
    ?assertEqual({FrameControl, MacHeader, <<"Hello">>}, ieee802154:reception()).

ar_tx_() ->
    FrameControl = #frame_control{ack_req = ?ENABLED, pan_id_compr = ?ENABLED},
    MacHeader = #mac_header{seqnum = 54, dest_pan = <<16#DECA:16>>, dest_addr = <<"RX">>, src_pan = <<16#DECA:16>>, src_addr = <<"TX">>},
    ?assertEqual(ok, ieee802154:transmition(FrameControl, MacHeader, <<"Hello">>)),
    AckFrameControl = #frame_control{frame_type = ?FTYPE_ACK},
    AckMacHeader = #mac_header{seqnum = 54},
    ?assertEqual({AckFrameControl, AckMacHeader, <<>>}, ieee802154:reception()).
