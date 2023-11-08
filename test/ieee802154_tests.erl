-module(ieee802154_tests).

-include_lib("eunit/include/eunit.hrl").

-include("../src/mac_frame.hrl").
-include("../src/ieee802154.hrl").

%--- Common teardown -----------------------------------------------------------------
teardown(_Ieee) ->
    ieee802154:stop_link().

%--- Perfect MAC ---------------------------------------------------------------------
setup() ->
    ieee802154:start_link(#ieee_parameters{mac_layer = mock_mac}).

mac_test_() ->
    {setup, fun setup/0, fun teardown/1, 
        [{"Reception", fun reception_/0},
         {"Transmission" , fun transmission_/0},
         {"Acknowledgment request", fun ar_tx_/0},
         {inorder, [fun rx_on_/0, fun rx_off_/0, fun rx_on_tx_on_/0, fun double_rx_on_/0]},
         {"invalid address error", fun invalid_addr_error_/0}
     ]}.

%--- Perfect MAC Tests --------------------------------------------------------------

transmission_() ->
    ?assertMatch({idle, _}, sys:get_state(ieee802154)),
    FrameControl = #frame_control{pan_id_compr = ?ENABLED, frame_version = 2#00},
    MacHeader = #mac_header{seqnum = 0, dest_pan = <<16#DECA:16>>, dest_addr = <<"RX">>, src_pan = <<16#DECA:16>>, src_addr = <<"TX">>},
    Payload = <<"Hello">>,
    ?assertEqual(ok, ieee802154:transmition(FrameControl, MacHeader, Payload)),
    ieee802154:transmition(FrameControl, MacHeader, Payload),
    ?assertMatch({idle, _}, sys:get_state(ieee802154)).

reception_() ->
    ?assertMatch({idle, _}, sys:get_state(ieee802154)),
    FrameControl = #frame_control{pan_id_compr = ?ENABLED, frame_version = 2#00},
    MacHeader = #mac_header{seqnum = 0, dest_pan = <<16#DECA:16>>, dest_addr = <<"RX">>, src_pan = <<16#DECA:16>>, src_addr = <<"TX">>},
    ?assertEqual({FrameControl, MacHeader, <<"Hello">>}, ieee802154:reception()),
    ?assertMatch({idle, _}, sys:get_state(ieee802154)).

ar_tx_() ->
    FrameControl = #frame_control{ack_req = ?ENABLED, pan_id_compr = ?ENABLED},
    MacHeader = #mac_header{seqnum = 54, dest_pan = <<16#DECA:16>>, dest_addr = <<"RX">>, src_pan = <<16#DECA:16>>, src_addr = <<"TX">>},
    ?assertEqual(ok, ieee802154:transmition(FrameControl, MacHeader, <<"Hello">>)).

rx_on_() ->
    ?assertEqual(ok, ieee802154:rx_on()),
    ?assertMatch({rx, _}, sys:get_state(ieee802154)).

rx_off_() ->
    ?assertEqual(ok, ieee802154:rx_off()),
    ?assertMatch({idle, _}, sys:get_state(ieee802154)).

rx_on_tx_on_() ->
    ?assertMatch({idle, _}, sys:get_state(ieee802154)),
    ieee802154:rx_on(),
    ?assertMatch({rx, _}, sys:get_state(ieee802154)),
    ?assertMatch({_, #{mac_layer := {_, #{rx := on}}}}, sys:get_state(ieee802154)),

    FrameControl = #frame_control{pan_id_compr = ?ENABLED, frame_version = 2#00},
    MacHeader = #mac_header{seqnum = 0, dest_pan = <<16#DECA:16>>, dest_addr = <<"RX">>, src_pan = <<16#DECA:16>>, src_addr = <<"TX">>},
    Payload = <<"Hello">>,
    ?assertEqual(ok, ieee802154:transmition(FrameControl, MacHeader, Payload)),

    ?assertMatch({rx, _}, sys:get_state(ieee802154)),
    ?assertMatch({_, #{mac_layer := {_, #{rx := on}}}}, sys:get_state(ieee802154)),
    ieee802154:rx_off(),

    ?assertMatch({idle, _}, sys:get_state(ieee802154)),
    ?assertMatch({_, #{mac_layer := {_, #{rx := off}}}}, sys:get_state(ieee802154)).

% Testing id turning on twice the reception doesn't crash
double_rx_on_() ->
    ?assertMatch({idle, _}, sys:get_state(ieee802154)),

    ?assertEqual(ok, ieee802154:rx_on()),
    ?assertMatch({rx, _}, sys:get_state(ieee802154)),

    ?assertEqual(ok, ieee802154:rx_on()),
    ?assertMatch({rx, _}, sys:get_state(ieee802154)),

    ?assertEqual(ok, ieee802154:rx_off()),
    ?assertMatch({idle, _}, sys:get_state(ieee802154)).

invalid_addr_error_() ->
    FrameControl = #frame_control{src_addr_mode = ?NONE, dest_addr_mode = ?NONE},
    FrameHeader = #mac_header{},
    Payload = <<"Invalid addr">>,
    ?assertMatch({error, invalid_address}, ieee802154:transmition(FrameControl, FrameHeader, Payload)).

%--- Lossy MAC ----------------------------------------------------------------------
setup_lossy() ->
    ieee802154:start_link(#ieee_parameters{mac_layer = lossy_mock_mac}).

lossy_mac_test_() ->
    {setup, fun setup_lossy/0, fun teardown/1,
     [{"Lossy AR", fun ar_tx_lossy_/0}]}.

%--- Lossy MAC Tests --Perfect --------------------------------------------------------------
ar_tx_lossy_() ->
    FrameControl = #frame_control{ack_req = ?ENABLED},
    MacHeader = #mac_header{seqnum = 0, dest_pan = <<16#DECA:16>>, dest_addr = <<"RX">>, src_pan = <<16#DECA:16>>, src_addr = <<"TX">>},
    Payload = <<"Hello">>,

    ?assertMatch({error, no_ack}, ieee802154:transmition(FrameControl, MacHeader, Payload)).
