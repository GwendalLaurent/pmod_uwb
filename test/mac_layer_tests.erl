-module(mac_layer_tests).

-include_lib("eunit/include/eunit.hrl").

-include("../src/mac_layer.hrl").

%--- Setup ---------------------------------------------------------------------

%--- Tests ---------------------------------------------------------------------

mac_message_from_api_test() ->
    FrameControl = #frame_control{ack_req = ?ENABLED, pan_id_compr = ?ENABLED, frame_version = 2#00},
    MacHeader = #mac_header{seqnum = 0, dest_pan = <<16#DECA:16>>, dest_addr = <<"RX">>, src_addr = <<"TX">>},
    ?assertEqual(<<16#6188:16, 0:8, 16#CADE:16, "XR", "XT", "Hello", 0, 0>>, 
                 mac_layer:mac_message(FrameControl, MacHeader, <<"Hello">>)).

mac_message_pan_id_not_compressed_test() ->
    FrameControl = #frame_control{ack_req = ?ENABLED, pan_id_compr = ?DISABLED, frame_version = 2#00},
    MacHeader = #mac_header{seqnum = 0, dest_pan = <<16#DECA:16>>, dest_addr = <<"RX">>, src_pan = <<16#DECA:16>>, src_addr = <<"TX">>},
    ?assertEqual(<<16#2188:16, 0:8, 16#CADE:16, "XR", 16#CADE:16, "XT", "Hello", 0, 0>>,
                 mac_layer:mac_message(FrameControl, MacHeader, <<"Hello">>)).

mac_message_broadcast_test() ->
    FrameControl = #frame_control{ack_req = ?ENABLED, pan_id_compr = ?DISABLED, frame_version = 2#00},
    MacHeader = #mac_header{seqnum = 0, dest_pan = <<16#FFFF:16>>, dest_addr = <<16#FFFF:16>>, src_pan = <<16#FFFF:16>>, src_addr = <<16#FFFF:16>>},
    ?assertEqual(<<16#2188:16, 0:8, 16#FFFF:16, 16#FFFF:16, 16#FFFF:16, 16#FFFF:16, "Hello", 0, 0>>, 
                 mac_layer:mac_message(FrameControl, MacHeader, <<"Hello">>)).

decode_mac_message_test() ->
    Message = <<16#6188:16, 0:8, 16#CADE:16, "XR", "XT", "Hello", 0, 0>>,
    FrameControl = #frame_control{ack_req = ?ENABLED, pan_id_compr = ?ENABLED, frame_version = 2#00},
    MacHeader = #mac_header{seqnum = 0, dest_pan = <<16#DECA:16>>, dest_addr = <<"RX">>, src_pan = <<16#DECA:16>>, src_addr = <<"TX">>},
    ?assertEqual({FrameControl, MacHeader, <<"Hello">>},
                 mac_layer:mac_decode(Message)).
