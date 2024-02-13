-module(ieee802154_mac_frame_SUITE).

%--- Includes ------------------------------------------------------------------
-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").

-include("../src/mac_frame.hrl").

%--- Export --------------------------------------------------------------------

-export([init_per_group/2, end_per_group/2]).
-export([all/0, groups/0]).

%--- Export: Data
-export([mac_message_from_api/1]).
-export([mac_message_pan_id_not_compressed/1]).
-export([mac_message_broadcast/1]).
-export([decode_mac_message/1]).
-export([decode_mac_message_uncompressed_pan_id/1]).
-export([decode_ack_frame_from_device/1]).
-export([decode_mac_message_no_src/1]).
-export([decode_mac_message_no_src_no_compt/1]).
-export([encode_ack_frame/1]).
-export([encode_decode_extended_address/1]).
-export([encode_decode_no_payload/1]).
-export([encode_decode_src_pan_coord/1]).
-export([encode_decode_src_pan_coord_pan_id_compr/1]).
-export([encode_decode_dest_pan_coord/1]).
-export([encode_decode_dest_pan_coord_pan_id_compr/1]).
-export([encode_decode_ext_address_pan_id_compr/1]).
-export([encode_decode_short_src_address/1]).
-export([encode_decode_short_src_address_pan_id_compr/1]).
-export([encode_decode_extended_dest_no_src_addr/1]).
-export([encode_decode_short_dest_ext_src_no_compr/1]).
-export([encode_decode_short_dest_ext_src_pan_compr/1]).
-export([encode_decode_no_dest_ext_src/1]).
-export([encode_decode_invalid_header_fields_value/1]).

%--- Export: Beacon
-export([ed_beacon_empty_fields/1]).
-export([ed_beacon_gts_and_pend_addr/1]).
-export([ed_beacon_only_gts/1]).
-export([ed_beacon_only_pend_addr/1]).

-export([decode_beacon_malformed_too_short/1]).

%--- Dialyzer ------------------------------------------------------------------
-compile({nowarn_unused_function, [debug_bitstring_hex/1]}).

%--- Callbacks -----------------------------------------------------------------
all() -> [{group, encode_decode_mac_data},
          {group, encode_decode_mac_beacon}].

groups() -> [{encode_decode_mac_data, [parallel], [
                mac_message_from_api,
                mac_message_pan_id_not_compressed,
                mac_message_broadcast,
                decode_mac_message_uncompressed_pan_id,
                decode_ack_frame_from_device,
                decode_mac_message_no_src,
                decode_mac_message_no_src_no_compt,
                encode_ack_frame,
                encode_decode_extended_address,
                encode_decode_no_payload,
                encode_decode_src_pan_coord,
                encode_decode_src_pan_coord_pan_id_compr,
                encode_decode_dest_pan_coord,
                encode_decode_dest_pan_coord_pan_id_compr,
                encode_decode_ext_address_pan_id_compr,
                encode_decode_short_src_address,
                encode_decode_short_src_address_pan_id_compr,
                encode_decode_extended_dest_no_src_addr,
                encode_decode_short_dest_ext_src_no_compr,
                encode_decode_short_dest_ext_src_pan_compr,
                encode_decode_no_dest_ext_src,
                encode_decode_invalid_header_fields_value]},
             {encode_decode_mac_beacon, [parallel], [
                ed_beacon_empty_fields,
                ed_beacon_gts_and_pend_addr,
                ed_beacon_only_gts,
                ed_beacon_only_pend_addr,
                decode_beacon_malformed_too_short
                ]}].

init_per_group(encode_decode_mac_beacon, Config) ->
    ExpectedFC = #frame_control{frame_type = ?FTYPE_BEACON,
                                dest_addr_mode = ?NONE,
                                src_addr_mode = ?EXTENDED,
                                pan_id_compr = ?DISABLED,
                                ack_req = ?DISABLED,
                                sec_en = ?DISABLED},
    ExpectedMH = #mac_header{seqnum = 0,
                             src_addr = <<16#DECACAFE00000001:64>>,
                             src_pan = <<16#FFFF:16>>,
                             dest_pan = <<>>,
                             dest_addr = <<>>},
    MHR = <<16#00C000FFFF01000000FECACADE:104>>,
    [{fc, ExpectedFC}, {mh, ExpectedMH}, {mhr, MHR}, {payload, <<>>} | Config];
init_per_group(_, Config) ->
    Config.

end_per_group(_, _) -> ok.

%--- Test cases: encode_decode_mac_data group --------------------------------------------------

mac_message_from_api(_Config) ->
    FrameControl = #frame_control{ack_req = ?ENABLED, pan_id_compr = ?ENABLED, frame_version = 2#00},
    MacHeader = #mac_header{seqnum = 0, dest_pan = <<16#DECA:16>>, dest_addr = <<"RX">>, src_addr = <<"TX">>},
    <<16#6188:16, 0:8, 16#CADE:16, "XR", "XT", "Hello">> = mac_frame:encode(FrameControl, MacHeader, <<"Hello">>).

mac_message_pan_id_not_compressed(_Config) ->
    FrameControl = #frame_control{ack_req = ?ENABLED, pan_id_compr = ?DISABLED, frame_version = 2#00},
    MacHeader = #mac_header{seqnum = 0, dest_pan = <<16#DECA:16>>, dest_addr = <<"RX">>, src_pan = <<16#DECA:16>>, src_addr = <<"TX">>},
    <<16#2188:16, 0:8, 16#CADE:16, "XR", 16#CADE:16, "XT", "Hello">> = mac_frame:encode(FrameControl, MacHeader, <<"Hello">>).

mac_message_broadcast(_Config) ->
    FrameControl = #frame_control{ack_req = ?ENABLED, pan_id_compr = ?DISABLED, frame_version = 2#00},
    MacHeader = #mac_header{seqnum = 0, dest_pan = <<16#FFFF:16>>, dest_addr = <<16#FFFF:16>>, src_pan = <<16#FFFF:16>>, src_addr = <<16#FFFF:16>>},
    <<16#2188:16, 0:8, 16#FFFF:16, 16#FFFF:16, 16#FFFF:16, 16#FFFF:16, "Hello">> = mac_frame:encode(FrameControl, MacHeader, <<"Hello">>).

decode_mac_message(_Config) ->
    Message = <<16#6188:16, 0:8, 16#CADE:16, "XR", "XT", "Hello">>,
    FrameControl = #frame_control{ack_req = ?ENABLED, pan_id_compr = ?ENABLED, frame_version = 2#00},
    MacHeader = #mac_header{seqnum = 0, dest_pan = <<16#DECA:16>>, dest_addr = <<"RX">>, src_pan = <<16#DECA:16>>, src_addr = <<"TX">>},
    {FrameControl, MacHeader, <<"Hello">>} = mac_frame:decode(Message).

decode_mac_message_uncompressed_pan_id(_Config) ->
    Message = <<16#2188:16, 0:8, 16#CADE:16, "XR", 16#CADE:16, "XT", "Hello">>,
    FrameControl = #frame_control{ack_req = ?ENABLED, frame_version = 2#00},
    MacHeader = #mac_header{seqnum = 0, dest_pan = <<16#DECA:16>>, dest_addr = <<"RX">>, src_pan = <<16#DECA:16>>, src_addr = <<"TX">>},
    {FrameControl, MacHeader, <<"Hello">>} = mac_frame:decode(Message).

decode_ack_frame_from_device(_Config) ->
    Message = <<16#0200:16, 50:8>>,
    FrameControl = #frame_control{frame_type = ?FTYPE_ACK, src_addr_mode = ?NONE, dest_addr_mode = ?NONE},
    MacHeader = #mac_header{seqnum = 50},
    {FrameControl, MacHeader, <<>>} = mac_frame:decode(Message).

% If Src address mode is zero and frame isn't an ACK. It implies that the frame comes from the PAN coordinator
decode_mac_message_no_src(_Config) -> 
    Message = <<16#4108:16, 22:8, 16#CADE:16, 16#CDAB:16, "Test">>,
    FrameControl = #frame_control{frame_type = ?FTYPE_DATA, pan_id_compr = ?ENABLED, dest_addr_mode = ?SHORT_ADDR, src_addr_mode = ?NONE},
    % SRC addr set to zero because can't imply the addr of the PAN coordinator at this level
    MacHeader = #mac_header{seqnum = 22, dest_pan = <<16#DECA:16>>, dest_addr = <<16#ABCD:16>>, src_pan = <<>>, src_addr = <<>>},
    {FrameControl, MacHeader, <<"Test">>} = mac_frame:decode(Message).

decode_mac_message_no_src_no_compt(_Config) -> 
    Message = <<16#0108:16, 22:8, 16#CADE:16, 16#CDAB:16, "Test">>,
    FrameControl = #frame_control{frame_type = ?FTYPE_DATA, pan_id_compr = ?DISABLED, dest_addr_mode = ?SHORT_ADDR, src_addr_mode = ?NONE},
    % SRC addr set to zero because can't imply the addr of the PAN coordinator at this level
    MacHeader = #mac_header{seqnum = 22, dest_pan = <<16#DECA:16>>, dest_addr = <<16#ABCD:16>>, src_pan = <<>>, src_addr = <<>>},
    {FrameControl, MacHeader, <<"Test">>} = mac_frame:decode(Message).

encode_ack_frame(_Config) ->
    FramePendingDisabled = mac_frame:encode_ack(?DISABLED, 42),
    <<16#0200:16, 42:8>> = FramePendingDisabled,

    FramePendingEnabled = mac_frame:encode_ack(?ENABLED, 200),
    <<16#1200:16, 200:8>> = FramePendingEnabled.

encode_decode_extended_address(_Config) ->
    FrameControl = #frame_control{src_addr_mode = ?EXTENDED, dest_addr_mode = ?EXTENDED},
    MacHeader = #mac_header{src_addr = <<16#DECACAFE0001:64>>, dest_addr = <<16#DECACAFE0002:64>>},
    Payload = <<"Test">>,
    {FrameControl, MacHeader, Payload} = mac_frame:decode(mac_frame:encode(FrameControl, MacHeader, Payload)).

encode_decode_no_payload(_Config) ->
    FrameControl = #frame_control{src_addr_mode = ?EXTENDED, dest_addr_mode = ?EXTENDED},
    MacHeader = #mac_header{src_addr = <<16#DECACAFE0001:64>>, dest_addr = <<16#DECACAFE0002:64>>},
    Encoded = mac_frame:encode(FrameControl, MacHeader), % TODO chce the encoded value
    {FrameControl, MacHeader, <<>>} = mac_frame:decode(Encoded).

encode_decode_src_pan_coord(_Config) ->
    FrameControl = #frame_control{src_addr_mode = ?NONE,  dest_addr_mode = ?SHORT_ADDR},
    MacHeader = #mac_header{dest_addr = <<16#CAFE:16>>, src_addr = <<>>, src_pan = <<>>},
    Payload = <<"This is a frame originating from the PAN coordinator">>,
    Encoded = mac_frame:encode(FrameControl, MacHeader, Payload),
    <<16#0108:16, 0:8, 16#FFFF:16, 16#FECA:16, Payload/bitstring>> = Encoded,
    {FrameControl, MacHeader, Payload} = mac_frame:decode(Encoded).

encode_decode_src_pan_coord_pan_id_compr(_Config) ->
    FrameControl = #frame_control{src_addr_mode = ?NONE,  dest_addr_mode = ?SHORT_ADDR, pan_id_compr = ?ENABLED},
    MacHeader = #mac_header{dest_addr = <<16#CAFE:16>>, src_addr = <<>>, src_pan = <<>>},
    Payload = <<"This is a frame originating from the PAN coordinator">>,
    Encoded = mac_frame:encode(FrameControl, MacHeader, Payload),
    <<16#4108:16, 0:8, 16#FFFF:16, 16#FECA:16, Payload/bitstring>> = Encoded,
    {FrameControl, MacHeader, Payload} = mac_frame:decode(Encoded).

encode_decode_dest_pan_coord(_Config) ->
    FrameControl = #frame_control{dest_addr_mode = ?NONE, src_addr_mode = ?SHORT_ADDR},
    MacHeader = #mac_header{src_addr = <<16#DECA:16>>, dest_addr = <<>>, dest_pan = <<>>}, % Not mandatory to set dest address to <<>> but needed here for the encode/decode check
    Payload = <<"This is a frame for the PAN coordinator">>,
    Encoded = mac_frame:encode(FrameControl, MacHeader, Payload),
    <<16#0180:16, 0:8, 16#FFFF:16, 16#CADE:16, Payload/bitstring>> = Encoded,
    {FrameControl, MacHeader, Payload} = mac_frame:decode(Encoded).

encode_decode_dest_pan_coord_pan_id_compr(_Config) ->
    ExpectedFrameControl = #frame_control{dest_addr_mode = ?NONE, src_addr_mode = ?SHORT_ADDR, pan_id_compr = ?ENABLED},
    ExpectedMacHeader = #mac_header{src_addr = <<16#DECA:16>>, dest_addr = <<>>, dest_pan = <<16#CAFE:16>>, src_pan = <<16#CAFE:16>>}, % Not mandatory to set dest address to <<>> but needed here for the encode/decode check
    ExpectedPayload = <<"This is a frame for the PAN coordinator">>,
    Encoded = mac_frame:encode(ExpectedFrameControl, ExpectedMacHeader, ExpectedPayload),
    ExpectedFrame = <<16#4180:16, 0:8, 16#FECA:16, 16#CADE:16, ExpectedPayload/bitstring>>,
    ?assertEqual(ExpectedFrame, Encoded),
    {FrameControl, MacHeader, Payload} = mac_frame:decode(Encoded),
    ?assertEqual(ExpectedFrameControl, FrameControl),
    ?assertEqual(ExpectedMacHeader, MacHeader),
    ?assertEqual(ExpectedPayload, Payload).

encode_decode_ext_address_pan_id_compr(_Config) -> 
    ExpectedFrameControl = #frame_control{dest_addr_mode = ?EXTENDED, src_addr_mode = ?EXTENDED, pan_id_compr = ?ENABLED},
    ExpectedMacHeader = #mac_header{dest_pan = <<16#CAFE:16>>,
                                    dest_addr = <<16#DECA000000000001:64>>,
                                    src_addr = <<16#DECA000000000002:64>>,
                                    src_pan = <<16#CAFE:16>>},
    ExpectedPayload = <<"Extended address and pan id compression">>,
    Encoded = mac_frame:encode(ExpectedFrameControl, ExpectedMacHeader, ExpectedPayload),
    ExpectedFrame = <<16#41CC:16, 0:8, 16#FECA:16, 16#010000000000CADE:64, 16#020000000000CADE:64, ExpectedPayload/bitstring>>,
    ?assertEqual(ExpectedFrame, Encoded),
    {FrameControl, MacHeader, Payload} = mac_frame:decode(Encoded),
    ?assertEqual(ExpectedFrameControl, FrameControl),
    ?assertEqual(ExpectedMacHeader, MacHeader),
    ?assertEqual(ExpectedPayload, Payload).

encode_decode_short_src_address(_Config) -> 
    FrameControl = #frame_control{dest_addr_mode = ?EXTENDED, src_addr_mode = ?SHORT_ADDR},
    MacHeader = #mac_header{dest_pan = <<16#CAFE:16>>, dest_addr = <<16#DECA000000000001:64>>, src_addr = <<16#DE02:16>>, src_pan = <<16#CAFE:16>>},
    Payload = <<"Extended dest. address and short src. address">>,
    Encoded = mac_frame:encode(FrameControl, MacHeader, Payload),
    <<16#018C:16, 0:8, 16#FECA:16, 16#010000000000CADE:64, 16#FECA:16, 16#02DE:16, Payload/bitstring>> = Encoded,
    {FrameControl, MacHeader, Payload} = mac_frame:decode(Encoded).

encode_decode_short_src_address_pan_id_compr(_Config) -> 
    FrameControl = #frame_control{dest_addr_mode = ?EXTENDED, src_addr_mode = ?SHORT_ADDR, pan_id_compr = ?ENABLED},
    MacHeader = #mac_header{dest_pan = <<16#CAFE:16>>, dest_addr = <<16#DECA000000000001:64>>, src_addr = <<16#DE02:16>>, src_pan = <<16#CAFE:16>>},
    Payload = <<"Extended address and pan id compression">>,
    Encoded = mac_frame:encode(FrameControl, MacHeader, Payload),
    <<16#418C:16, 0:8, 16#FECA:16, 16#010000000000CADE:64, 16#02DE:16, Payload/bitstring>> = Encoded,
    {FrameControl, MacHeader, Payload} = mac_frame:decode(Encoded).

encode_decode_extended_dest_no_src_addr(_Config) ->
    FrameControl = #frame_control{dest_addr_mode = ?EXTENDED, src_addr_mode = ?NONE},
    MacHeader = #mac_header{dest_pan = <<16#CAFE:16>>, dest_addr = <<16#DECA000000000001:64>>, src_addr = <<>>, src_pan = <<>>},
    Payload = <<"Extended address and missing src fields">>,
    Encoded = mac_frame:encode(FrameControl, MacHeader, Payload),
    <<16#010C:16, 0:8, 16#FECA:16, 16#010000000000CADE:64, Payload/bitstring>> = Encoded,
    {FrameControl, MacHeader, Payload} = mac_frame:decode(Encoded).

encode_decode_short_dest_ext_src_no_compr(_Config) ->
    FrameControl = #frame_control{dest_addr_mode = ?SHORT_ADDR, src_addr_mode = ?EXTENDED},
    MacHeader = #mac_header{dest_pan = <<16#CAFE:16>>, dest_addr = <<16#DE01:16>>, src_addr = <<16#DECA000000000001:64>>, src_pan = <<16#BAD0:16>>},
    Payload = <<"Short dest addr - ext src addr">>,
    Encoded = mac_frame:encode(FrameControl, MacHeader, Payload),
    <<16#01C8:16, 0:8, 16#FECA:16, 16#01DE:16, 16#D0BA:16, 16#010000000000CADE:64, Payload/bitstring>> = Encoded,
    {FrameControl, MacHeader, Payload} = mac_frame:decode(Encoded).

encode_decode_short_dest_ext_src_pan_compr(_Config) ->
    FrameControl = #frame_control{dest_addr_mode = ?SHORT_ADDR, src_addr_mode = ?EXTENDED, pan_id_compr = ?ENABLED},
    MacHeader = #mac_header{dest_pan = <<16#CAFE:16>>, dest_addr = <<16#DE01:16>>, src_addr = <<16#DECA000000000001:64>>, src_pan = <<16#CAFE:16>>},
    Payload = <<"Short dest addr - ext src addr - pan compr">>,
    Encoded = mac_frame:encode(FrameControl, MacHeader, Payload),
    <<16#41C8:16, 0:8, 16#FECA:16, 16#01DE:16, 16#010000000000CADE:64, Payload/bitstring>> = Encoded,
    {FrameControl, MacHeader, Payload} = mac_frame:decode(Encoded).

encode_decode_no_dest_ext_src(_Config) ->
    FrameControl = #frame_control{dest_addr_mode = ?NONE, src_addr_mode = ?EXTENDED},
    MacHeader = #mac_header{dest_pan = <<>>, dest_addr = <<>>, src_addr = <<16#DECA000000000001:64>>, src_pan = <<16#CAFE:16>>},
    Payload = <<"no dest addr - ext src addr">>,
    Encoded = mac_frame:encode(FrameControl, MacHeader, Payload),
    <<16#01C0:16, 0:8, 16#FECA:16, 16#010000000000CADE:64, Payload/bitstring>> = Encoded,
    {FrameControl, MacHeader, Payload} = mac_frame:decode(Encoded).

encode_decode_invalid_header_fields_value(_Config) ->
    FrameControl = #frame_control{src_addr_mode = 9, dest_addr_mode = 9},
    MacHeader = #mac_header{dest_pan = <<>>, dest_addr = <<>>, src_addr = <<16#DECA000000000001:64>>, src_pan = <<16#CAFE:16>>},
    Payload = <<"Invalid values">>,
    InvalidFrame = mac_frame:encode(FrameControl, MacHeader, Payload),
    {'EXIT', {internal_decoding_error, _}} = catch mac_frame:decode(InvalidFrame), 
    ok.

%--- Test cases: encode_decode_mac_beacon --------------------------------------
% Test I Should add:
% [x] - Beacon with empty fields (no GTS and no pending addr)
% [x] - Beacon with GTS and pending addr
%       [x] - Multiple GTS & pending addr
%       [x] - Multiple GTS but no pending addr
%       [x] - Multiple pend addr but no GTS
% [x] - Malformed beacon (not enough minimal bits)
% [ ] - Malformed beacon (The number of GTS fields doesn't match the one specified
% [ ] - Malformed beacon (The number of pending addr doesn't match the one specified
% [ ] - Probably other malformed beacons are possible

ed_beacon_empty_fields(Config) ->
    % TODO encoding test
    MHR = ?config(mhr, Config),
    {ExpectedFC, ExpectedMH, ExpectedPayload} = get_expected_mac_struct(Config),
    % Payload:
    % * Beacon order: 15; Superframe order: 15; CAP final slot: 0; BLE: false; PAN coord: true; Ass. perm.: true
    % * GTS fields: all set to zero and rest is null
    % * Pending addresses set to zero
    MacPayload = <<16#C0FF0000:32>>,
    Data = <<MHR:104/bitstring, MacPayload:32/bitstring>>,
    {FC, MH, Metadatas, BeaconPayload} = mac_frame:decode_beacon(Data),
    {SuperFrameSpecs, GTSFields, PendAddr} = Metadatas,
    ExpectedSFrameSpecs = #superframe_specs{beacon_order = 15,
                                            superframe_order = 15,
                                            final_cap_slot = 0,
                                            ble = false,
                                            pan_coord = true,
                                            association_perm = true},
    ExpectedGTS = #gts_fields{gts_descr_cnt = 0,
                              gts_permit = false,
                              gts_direction = <<>>,
                              gts_descr_list = []},
    ExpectedPendAddr = #pending_addr_flds{nbr_short_addr_pending = 0,
                                          nbr_ext_addr_pending = 0,
                                          short_addr_pending = [],
                                          ext_addr_pending = []},
    Expected = {ExpectedFC, ExpectedMH, ExpectedSFrameSpecs, ExpectedGTS, ExpectedPendAddr, ExpectedPayload},
    Actual = {FC, MH, SuperFrameSpecs, GTSFields, PendAddr, BeaconPayload},
    beacon_check_actual_against_expected(Expected, Actual),
    mac_frame:encode_beacon(MHR, Metadatas, BeaconPayload).

ed_beacon_gts_and_pend_addr(Config) ->
    % TODO encoding test
    MHR = ?config(mhr, Config),
    {ExpectedFC, ExpectedMH, ExpectedPayload} = get_expected_mac_struct(Config),
    ExpectedSFrameSpecs = #superframe_specs{beacon_order = 14,
                                            superframe_order = 13,
                                            final_cap_slot = 11,
                                            ble = false,
                                            pan_coord = true,
                                            association_perm = true},
    ExpectedGTSDescrList = [#gts_descr{short_addr = <<16#0001:16>>,
                                       starting_slot = 12,
                                       gts_length = 1},
                            #gts_descr{short_addr = <<16#0002:16>>,
                                       starting_slot = 13,
                                       gts_length = 1}],
    ExpectedGTS = #gts_fields{gts_descr_cnt = 2,
                              gts_permit = true,
                              gts_direction = <<2#0000000:7>>,
                              gts_descr_list = ExpectedGTSDescrList},
    ExpectedPendAddr = #pending_addr_flds{nbr_short_addr_pending = 1,
                                          short_addr_pending = [<<16#0003:16>>],
                                          nbr_ext_addr_pending = 0,
                                          ext_addr_pending = []},
    % Payload:
    % * Beacon order: 14; Superframe order: 13; CAP final: 11; BLE: false; PAN coord: true; Ass. perm.: true
    % * GTS fields: desc. cnt.: 2; GTS perm: true; direction: all tx only; 2 addresses 0x01 at slot 12 and 0x02 at slot 13 both of length 1
    % * Pending addresses: 1 pending short address 0x03; no pending ext. addr.
    MacPayload = <<16#CBDE82001C00011D0002010003:104>>,
    Data = <<MHR:104/bitstring, MacPayload:104/bitstring>>,
    {FC, MH, Metadatas, BeaconPayload} = mac_frame:decode_beacon(Data),
    {SuperFrameSpecs, GTSFields, PendAddr} = Metadatas,
    Expected = {ExpectedFC, ExpectedMH, ExpectedSFrameSpecs, ExpectedGTS, ExpectedPendAddr, ExpectedPayload},
    Actual = {FC, MH, SuperFrameSpecs, GTSFields, PendAddr, BeaconPayload},
    beacon_check_actual_against_expected(Expected, Actual),
    mac_frame:encode_beacon(MHR, Metadatas, BeaconPayload).

ed_beacon_only_gts(Config) ->
    MHR = ?config(mhr, Config),
    {ExpectedFC, ExpectedMH, ExpectedPayload} = get_expected_mac_struct(Config),
    ExpectedSFrameSpecs = #superframe_specs{beacon_order = 14,
                                            superframe_order = 13,
                                            final_cap_slot = 11,
                                            ble = false,
                                            pan_coord = true,
                                            association_perm = true},
    ExpectedGTSDescrList = [#gts_descr{short_addr = <<16#0001:16>>,
                                       starting_slot = 12,
                                       gts_length = 1},
                            #gts_descr{short_addr = <<16#0002:16>>,
                                       starting_slot = 13,
                                       gts_length = 1}],
    ExpectedGTS = #gts_fields{gts_descr_cnt = 2,
                              gts_permit = true,
                              gts_direction = <<2#0000000:7>>,
                              gts_descr_list = ExpectedGTSDescrList},
    ExpectedPendAddr = #pending_addr_flds{nbr_short_addr_pending = 0,
                                          short_addr_pending = [],
                                          nbr_ext_addr_pending = 0,
                                          ext_addr_pending = []},
    % Payload:
    % * Beacon order: 14; Superframe order: 13; CAP final: 11; BLE: false; PAN coord: true; Ass. perm.: true
    % * GTS fields: desc. cnt.: 2; GTS perm: true; direction: all tx only; 2 addresses 0x01 at slot 12 and 0x02 at slot 13 both of length 1
    % * Pending addresses: 1 pending short address 0x03; no pending ext. addr.
    MacPayload = <<16#CBDE82001C00011D000200:88>>,
    Data = <<MHR:104/bitstring, MacPayload:88/bitstring>>,
    {FC, MH, Metadatas, BeaconPayload} = mac_frame:decode_beacon(Data),
    {SuperFrameSpecs, GTSFields, PendAddr} = Metadatas,
    Expected = {ExpectedFC, ExpectedMH, ExpectedSFrameSpecs, ExpectedGTS, ExpectedPendAddr, ExpectedPayload},
    Actual = {FC, MH, SuperFrameSpecs, GTSFields, PendAddr, BeaconPayload},
    beacon_check_actual_against_expected(Expected, Actual),
    mac_frame:encode_beacon(MHR, Metadatas, BeaconPayload).

ed_beacon_only_pend_addr(Config) ->
    MHR = ?config(mhr, Config),
    {ExpectedFC, ExpectedMH, ExpectedPayload} = get_expected_mac_struct(Config),
    ExpectedSFrameSpecs = #superframe_specs{beacon_order = 14,
                                            superframe_order = 13,
                                            final_cap_slot = 13,
                                            ble = false,
                                            pan_coord = true,
                                            association_perm = true},
    ExpectedGTS = #gts_fields{gts_descr_cnt = 0,
                              gts_permit = true,
                              gts_direction = <<>>,
                              gts_descr_list = []},
    ExpectedPendAddr = #pending_addr_flds{nbr_short_addr_pending = 2,
                                          short_addr_pending = [<<16#0001:16>>,
                                                                <<16#0002:16>>],
                                          nbr_ext_addr_pending = 2,
                                          ext_addr_pending = [<<16#CAFEDECA00000001:64>>,
                                                              <<16#CAFEDECA00000002:64>>]},
    % Payload:
    % * Beacon order: 14; Superframe order: 13; CAP final: 11; BLE: false; PAN coord: true; Ass. perm.: true
    % * GTS fields: desc. cnt.: 2; GTS perm: true; direction: all tx only; 2 addresses 0x01 at slot 12 and 0x02 at slot 13 both of length 1
    % * Pending addresses: 1 pending short address 0x03; no pending ext. addr.
    MacPayload = <<16#CDDE802200010002CAFEDECA00000001CAFEDECA00000002:192>>,
    Data = <<MHR:104/bitstring, MacPayload:192/bitstring>>,
    {FC, MH, Metadatas, BeaconPayload} = mac_frame:decode_beacon(Data),
    {SuperFrameSpecs, GTSFields, PendAddr} = Metadatas,
    Expected = {ExpectedFC, ExpectedMH, ExpectedSFrameSpecs, ExpectedGTS, ExpectedPendAddr, ExpectedPayload},
    Actual = {FC, MH, SuperFrameSpecs, GTSFields, PendAddr, BeaconPayload},
    beacon_check_actual_against_expected(Expected, Actual).

decode_beacon_malformed_too_short(Config) ->
    MHR = ?config(mhr, Config),
    MacPayload = <<16#000:28>>,
    Data = <<MHR:104/bitstring, MacPayload:28/bitstring>>,
    ?assertEqual({error, malformed}, mac_frame:decode_beacon(Data)).


%--- Utils ---------------------------------------------------------------------
%% @doc get the expected FC, MH and Playload from the ones defined in the Config
get_expected_mac_struct(Config) ->
    ExpectedFC = ?config(fc, Config),
    ExpectedMH = ?config(mh, Config),
    ExpectedPayload = ?config(payload, Config),
    {ExpectedFC, ExpectedMH, ExpectedPayload}.

beacon_check_actual_against_expected(Expected, Actual) ->
    {ExpectedFC, ExpectedMH, ExpectedSFrameSpecs, ExpectedGTS, ExpectedPendAddr, ExpectedPayload} = Expected,
    {FC, MH, SuperFrameSpecs, GTSFields, PendAddr, BeaconPayload} = Actual,
    ?assertEqual(ExpectedFC, FC),
    ?assertEqual(ExpectedMH, MH),
    ?assertEqual(ExpectedSFrameSpecs, SuperFrameSpecs),
    ?assertEqual(ExpectedGTS, GTSFields),
    ?assertEqual(ExpectedPendAddr, PendAddr),
    ?assertEqual(ExpectedPayload, BeaconPayload).


debug_bitstring_hex(Bitstring) ->
    lists:flatten([io_lib:format("16#~2.16.0B ", [X]) || <<X>> <= Bitstring]).
