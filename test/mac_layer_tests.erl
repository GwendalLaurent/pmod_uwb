-module(mac_layer_tests).

-include_lib("eunit/include/eunit.hrl").

-include("../src/mac_frame.hrl").

%--- Setup ---------------------------------------------------------------------

mac_test_() ->
    {inorder,
     [{generator, fun mac_encode_decode/0},
      {generator, fun mac_perfect_phy/0},
      {generator, fun mac_faulty_phy/0},
      {generator, fun mac_lossy_phy/0}]}.

mac_encode_decode() ->
    {inorder,
     [{"Mac message from API", fun mac_message_from_api_/0},
      {"Mac message PAN ID not compressed", fun mac_message_pan_id_not_compressed_/0},
      {"Mac message broadcast", fun mac_message_broadcast_/0},
      {"Decode MAC message", fun decode_mac_message_/0},
      {"Decode MAC message uncompressed", fun decode_mac_message_uncompressed_pan_id_/0},
      {"Decode ACK frame from device", fun decode_ack_frame_from_device_/0},
      {"Decode Mac message no src", fun decode_mac_message_no_src_/0},
      {"Decode MAC message no src no compt", fun decode_mac_message_no_src_no_compt_/0},
      {"Encode ACK frame", fun encode_ack_frame/0}]}.

setup_perfect() ->
    mock_phy:start_link(spi2, {perfect, #{}}),
    gen_mac_layer:init(mac_layer, #{phy_layer => mock_phy}).

teardown(State) ->
    gen_mac_layer:stop(State, exit),
    mock_phy:stop_link().

mac_perfect_phy() ->
    {setup, fun setup_perfect/0, fun teardown/1, {with, [
     fun transmission_/1,
     fun reception_perfect_/1]}
    }.

mac_faulty_setup() ->
    mock_phy:start_link(spi2, {faulty, #{}}),
    gen_mac_layer:init(mac_layer, #{phy_layer => mock_phy}).

mac_faulty_phy() ->
    {setup, fun mac_faulty_setup/0, fun teardown/1, {with, [
        fun reception_faulty/1]}}.

mac_lossy_setup() ->
    mock_phy:start_link(spi2, {lossy, #{}}),
    gen_mac_layer:init(mac_layer, #{phy_layer => mock_phy}).

mac_lossy_phy() -> % Note: Not sure if I should keep that kind of tests
    {setup, fun mac_lossy_setup/0, fun teardown/1, {with, [
        fun(State) -> {timeout, 60, {with, State, [fun reception_lossy/1]}} end
    ]}}.

%--- Encode/Decode Tests ---------------------------------------------------------------------

mac_message_from_api_() ->
    FrameControl = #frame_control{ack_req = ?ENABLED, pan_id_compr = ?ENABLED, frame_version = 2#00},
    MacHeader = #mac_header{seqnum = 0, dest_pan = <<16#DECA:16>>, dest_addr = <<"RX">>, src_addr = <<"TX">>},
    ?assertEqual(<<16#6188:16, 0:8, 16#CADE:16, "XR", "XT", "Hello">>, 
                 mac_frame:encode(FrameControl, MacHeader, <<"Hello">>)).

mac_message_pan_id_not_compressed_() ->
    FrameControl = #frame_control{ack_req = ?ENABLED, pan_id_compr = ?DISABLED, frame_version = 2#00},
    MacHeader = #mac_header{seqnum = 0, dest_pan = <<16#DECA:16>>, dest_addr = <<"RX">>, src_pan = <<16#DECA:16>>, src_addr = <<"TX">>},
    ?assertEqual(<<16#2188:16, 0:8, 16#CADE:16, "XR", 16#CADE:16, "XT", "Hello">>,
                 mac_frame:encode(FrameControl, MacHeader, <<"Hello">>)).

mac_message_broadcast_() ->
    FrameControl = #frame_control{ack_req = ?ENABLED, pan_id_compr = ?DISABLED, frame_version = 2#00},
    MacHeader = #mac_header{seqnum = 0, dest_pan = <<16#FFFF:16>>, dest_addr = <<16#FFFF:16>>, src_pan = <<16#FFFF:16>>, src_addr = <<16#FFFF:16>>},
    ?assertEqual(<<16#2188:16, 0:8, 16#FFFF:16, 16#FFFF:16, 16#FFFF:16, 16#FFFF:16, "Hello">>, 
                 mac_frame:encode(FrameControl, MacHeader, <<"Hello">>)).

decode_mac_message_() ->
    Message = <<16#6188:16, 0:8, 16#CADE:16, "XR", "XT", "Hello">>,
    FrameControl = #frame_control{ack_req = ?ENABLED, pan_id_compr = ?ENABLED, frame_version = 2#00},
    MacHeader = #mac_header{seqnum = 0, dest_pan = <<16#DECA:16>>, dest_addr = <<"RX">>, src_pan = <<16#DECA:16>>, src_addr = <<"TX">>},
    ?assertEqual({FrameControl, MacHeader, <<"Hello">>},
                 mac_frame:decode(Message)).

decode_mac_message_uncompressed_pan_id_() ->
    Message = <<16#2188:16, 0:8, 16#CADE:16, "XR", 16#CADE:16, "XT", "Hello">>,
    FrameControl = #frame_control{ack_req = ?ENABLED, frame_version = 2#00},
    MacHeader = #mac_header{seqnum = 0, dest_pan = <<16#DECA:16>>, dest_addr = <<"RX">>, src_pan = <<16#DECA:16>>, src_addr = <<"TX">>},
    ?assertEqual({FrameControl, MacHeader, <<"Hello">>},
                 mac_frame:decode(Message)).

decode_ack_frame_from_device_() ->
    Message = <<16#0200:16, 50:8>>,
    FrameControl = #frame_control{frame_type = ?FTYPE_ACK, src_addr_mode = ?NONE, dest_addr_mode = ?NONE},
    MacHeader = #mac_header{seqnum = 50},
    ?assertEqual({FrameControl, MacHeader, <<>>}, 
                 mac_frame:decode(Message)).

% If Src address mode is zero and frame isn't an ACK. It implies that the frame comes from the PAN coordinator
decode_mac_message_no_src_() -> 
    Message = <<16#4108:16, 22:8, 16#CADE:16, 16#CDAB:16, "Test">>,
    FrameControl = #frame_control{frame_type = ?FTYPE_DATA, pan_id_compr = ?ENABLED, dest_addr_mode = ?SHORT_ADDR, src_addr_mode = ?NONE},
    % SRC addr set to zero because can't imply the addr of the PAN coordinator at this level
    MacHeader = #mac_header{seqnum = 22, dest_pan = <<16#DECA:16>>, dest_addr = <<16#ABCD:16>>, src_pan = <<16#DECA:16>>, src_addr = <<>>},
    ?assertEqual({FrameControl, MacHeader, <<"Test">>},
                 mac_frame:decode(Message)).

decode_mac_message_no_src_no_compt_() -> 
    Message = <<16#0108:16, 22:8, 16#CADE:16, 16#CDAB:16, "Test">>,
    FrameControl = #frame_control{frame_type = ?FTYPE_DATA, pan_id_compr = ?DISABLED, dest_addr_mode = ?SHORT_ADDR, src_addr_mode = ?NONE},
    % SRC addr set to zero because can't imply the addr of the PAN coordinator at this level
    MacHeader = #mac_header{seqnum = 22, dest_pan = <<16#DECA:16>>, dest_addr = <<16#ABCD:16>>, src_pan = <<16#DECA:16>>, src_addr = <<>>},
    ?assertEqual({FrameControl, MacHeader, <<"Test">>},
                 mac_frame:decode(Message)).

encode_ack_frame() ->
    FramePendingDisabled = mac_frame:encode_ack(?DISABLED, 42),
    ?assertEqual(<<16#0200:16, 42:8>>, FramePendingDisabled),

    FramePendingEnabled = mac_frame:encode_ack(?ENABLED, 200),
    ?assertEqual(<<16#1200:16, 200:8>>, FramePendingEnabled).

%--- perfect phy layer test ---------------------------------------------------------------------
transmission_(State) ->
    ?assertMatch({ok, _}, gen_mac_layer:tx(State, #frame_control{}, #mac_header{}, <<"Test">>)).

reception_perfect_(State) ->
    FrameControl = #frame_control{ack_req = ?ENABLED, pan_id_compr = ?ENABLED, frame_version = 2#00},
    MacHeader = #mac_header{seqnum = 0, dest_pan = <<16#DECA:16>>, dest_addr = <<"RX">>, src_pan = <<16#DECA:16>>, src_addr = <<"TX">>},
    {ok, _, {ReceivedFC, ReceivedMH, ReceivedPayload}} = gen_mac_layer:rx(State),
    ?assertEqual({FrameControl, MacHeader, <<"Hello">>}, {ReceivedFC, ReceivedMH, ReceivedPayload}).

%--- faulty phy layer test ---------------------------------------------------------------------
reception_faulty(State) ->
    ?assertMatch({error, _, rxpto}, gen_mac_layer:rx(State)).

%--- faulty phy layer test ---------------------------------------------------------------------
reception_lossy(State) ->
    Received = gen_mac_layer:rx(State), 
    
    FrameControl = #frame_control{ack_req = ?ENABLED, pan_id_compr = ?ENABLED, frame_version = 2#00},
    MacHeader = #mac_header{seqnum = 0, dest_pan = <<16#DECA:16>>, dest_addr = <<"RX">>, src_pan = <<16#DECA:16>>, src_addr = <<"TX">>},

    case Received of
        {error, _, rxpto} -> reception_lossy(State);
        {ok, _, {ReceivedFC, ReceivedMH, ReceivedPayload}} -> ?assertEqual({FrameControl, MacHeader, <<"Hello">>}, {ReceivedFC, ReceivedMH, ReceivedPayload})
    end.
