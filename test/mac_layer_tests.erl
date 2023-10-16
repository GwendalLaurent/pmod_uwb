-module(mac_layer_tests).

-include_lib("eunit/include/eunit.hrl").

-include("../src/mac_layer.hrl").

%--- Setup ---------------------------------------------------------------------

mac_test_() ->
    {inorder,
     [{generator, fun mac_encode_decode/0},
      {generator, fun mac_perfect_phy/0},
      {generator, fun mac_faulty_phy/0},
      {generator, fun mac_lossy_phy/0}]}.

mac_encode_decode() ->
    {inorder,
     [fun mac_message_from_api_/0,
      fun mac_message_pan_id_not_compressed_/0,
      fun mac_message_broadcast_/0,
      fun decode_mac_message_/0,
      fun decode_mac_message_uncompressed_pan_id_/0,
      fun decode_ack_frame_from_device_/0,
      fun decode_mac_message_no_src_/0,
      fun decode_mac_message_no_src_no_compt_/0]}.

setup_perfect() ->
    mock_phy:start_link(spi2, {perfect, #{}}),
    mac_layer:start_link(#{}, #{phy_layer => mock_phy}).

teardown(_) ->
    mock_phy:stop_link(),
    mac_layer:stop_link().

mac_perfect_phy() ->
    {setup, fun setup_perfect/0, fun teardown/1, [
     fun transmission_/0,
     fun reception_perfect_/0]}.

mac_faulty_setup() ->
    mock_phy:start_link(spi2, {faulty, #{}}),
    mac_layer:start_link(#{}, #{phy_layer => mock_phy}).

mac_faulty_phy() ->
    {setup, fun mac_faulty_setup/0, fun teardown/1, [
     fun reception_faulty/0]}.

mac_lossy_setup() ->
    mock_phy:start_link(spi2, {lossy, #{}}),
    mac_layer:start_link(#{}, #{phy_layer => mock_phy}).

mac_lossy_phy() -> % Note: Not sure if I should keep that kind of tests
    {setup, fun mac_lossy_setup/0, fun teardown/1, [
        {timeout, 60, [fun reception_lossy/0]}
    ]}.

%--- Encode/Decode Tests ---------------------------------------------------------------------

mac_message_from_api_() ->
    FrameControl = #frame_control{ack_req = ?ENABLED, pan_id_compr = ?ENABLED, frame_version = 2#00},
    MacHeader = #mac_header{seqnum = 0, dest_pan = <<16#DECA:16>>, dest_addr = <<"RX">>, src_addr = <<"TX">>},
    ?assertEqual(<<16#6188:16, 0:8, 16#CADE:16, "XR", "XT", "Hello">>, 
                 mac_layer:mac_frame(FrameControl, MacHeader, <<"Hello">>)).

mac_message_pan_id_not_compressed_() ->
    FrameControl = #frame_control{ack_req = ?ENABLED, pan_id_compr = ?DISABLED, frame_version = 2#00},
    MacHeader = #mac_header{seqnum = 0, dest_pan = <<16#DECA:16>>, dest_addr = <<"RX">>, src_pan = <<16#DECA:16>>, src_addr = <<"TX">>},
    ?assertEqual(<<16#2188:16, 0:8, 16#CADE:16, "XR", 16#CADE:16, "XT", "Hello">>,
                 mac_layer:mac_frame(FrameControl, MacHeader, <<"Hello">>)).

mac_message_broadcast_() ->
    FrameControl = #frame_control{ack_req = ?ENABLED, pan_id_compr = ?DISABLED, frame_version = 2#00},
    MacHeader = #mac_header{seqnum = 0, dest_pan = <<16#FFFF:16>>, dest_addr = <<16#FFFF:16>>, src_pan = <<16#FFFF:16>>, src_addr = <<16#FFFF:16>>},
    ?assertEqual(<<16#2188:16, 0:8, 16#FFFF:16, 16#FFFF:16, 16#FFFF:16, 16#FFFF:16, "Hello">>, 
                 mac_layer:mac_frame(FrameControl, MacHeader, <<"Hello">>)).

decode_mac_message_() ->
    Message = <<16#6188:16, 0:8, 16#CADE:16, "XR", "XT", "Hello">>,
    FrameControl = #frame_control{ack_req = ?ENABLED, pan_id_compr = ?ENABLED, frame_version = 2#00},
    MacHeader = #mac_header{seqnum = 0, dest_pan = <<16#DECA:16>>, dest_addr = <<"RX">>, src_pan = <<16#DECA:16>>, src_addr = <<"TX">>},
    ?assertEqual({FrameControl, MacHeader, <<"Hello">>},
                 mac_layer:mac_decode(Message)).

decode_mac_message_uncompressed_pan_id_() ->
    Message = <<16#2188:16, 0:8, 16#CADE:16, "XR", 16#CADE:16, "XT", "Hello">>,
    FrameControl = #frame_control{ack_req = ?ENABLED, frame_version = 2#00},
    MacHeader = #mac_header{seqnum = 0, dest_pan = <<16#DECA:16>>, dest_addr = <<"RX">>, src_pan = <<16#DECA:16>>, src_addr = <<"TX">>},
    ?assertEqual({FrameControl, MacHeader, <<"Hello">>},
                 mac_layer:mac_decode(Message)).

decode_ack_frame_from_device_() ->
    Message = <<16#0200:16, 50:8>>,
    FrameControl = #frame_control{frame_type = ?FTYPE_ACK, src_addr_mode = ?NONE, dest_addr_mode = ?NONE},
    MacHeader = #mac_header{seqnum = 50},
    ?assertEqual({FrameControl, MacHeader, <<>>}, 
                 mac_layer:mac_decode(Message)).

% If Src address mode is zero and frame isn't an ACK. It implies that the frame comes from the PAN coordinator
decode_mac_message_no_src_() -> 
    Message = <<16#4108:16, 22:8, 16#CADE:16, 16#CDAB:16, "Test">>,
    FrameControl = #frame_control{frame_type = ?FTYPE_DATA, pan_id_compr = ?ENABLED, dest_addr_mode = ?SHORT_ADDR, src_addr_mode = ?NONE},
    % SRC addr set to zero because can't imply the addr of the PAN coordinator at this level
    MacHeader = #mac_header{seqnum = 22, dest_pan = <<16#DECA:16>>, dest_addr = <<16#ABCD:16>>, src_pan = <<16#DECA:16>>, src_addr = <<>>},
    ?assertEqual({FrameControl, MacHeader, <<"Test">>},
                 mac_layer:mac_decode(Message)).

decode_mac_message_no_src_no_compt_() -> 
    Message = <<16#0108:16, 22:8, 16#CADE:16, 16#CDAB:16, "Test">>,
    FrameControl = #frame_control{frame_type = ?FTYPE_DATA, pan_id_compr = ?DISABLED, dest_addr_mode = ?SHORT_ADDR, src_addr_mode = ?NONE},
    % SRC addr set to zero because can't imply the addr of the PAN coordinator at this level
    MacHeader = #mac_header{seqnum = 22, dest_pan = <<16#DECA:16>>, dest_addr = <<16#ABCD:16>>, src_pan = <<16#DECA:16>>, src_addr = <<>>},
    ?assertEqual({FrameControl, MacHeader, <<"Test">>},
                 mac_layer:mac_decode(Message)).

%--- perfect phy layer test ---------------------------------------------------------------------
transmission_() ->
    ?assertEqual(ok,
                 mac_layer:send_data(#frame_control{}, #mac_header{}, <<"Test">>)).

reception_perfect_() ->
    FrameControl = #frame_control{ack_req = ?ENABLED, pan_id_compr = ?ENABLED, frame_version = 2#00},
    MacHeader = #mac_header{seqnum = 0, dest_pan = <<16#DECA:16>>, dest_addr = <<"RX">>, src_pan = <<16#DECA:16>>, src_addr = <<"TX">>},
    ?assertEqual({FrameControl, MacHeader, <<"Hello">>}, mac_layer:reception()).

%--- faulty phy layer test ---------------------------------------------------------------------
reception_faulty() ->
    ?assertEqual(rxpto, mac_layer:reception()).

%--- faulty phy layer test ---------------------------------------------------------------------
reception_lossy() ->
    Received = mac_layer:reception(),
    
    FrameControl = #frame_control{ack_req = ?ENABLED, pan_id_compr = ?ENABLED, frame_version = 2#00},
    MacHeader = #mac_header{seqnum = 0, dest_pan = <<16#DECA:16>>, dest_addr = <<"RX">>, src_pan = <<16#DECA:16>>, src_addr = <<"TX">>},

    case Received of
        rxpto -> reception_lossy();
        _ -> ?assertEqual({FrameControl, MacHeader, <<"Hello">>}, Received)
    end.
