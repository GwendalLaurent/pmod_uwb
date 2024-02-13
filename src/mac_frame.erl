-module(mac_frame).

-include("mac_frame.hrl").

-export([encode/2]).
-export([encode/3]).
-export([encode_ack/2]).
-export([encode_beacon_request/0]).
-export([decode/1]).
-export([decode_beacon/1]).

%--- API -----------------------------------------------------------------------
% @doc builds a mac frame without a payload
% @equiv encode(FrameControl, MacHeader, <<>>)
% @end
-spec encode(FrameControl, MacHeader) -> bitstring() when
      FrameControl :: frame_control(),
      MacHeader    :: mac_header().
encode(FrameControl, MacHeader) ->
    encode(FrameControl, MacHeader, <<>>).

% @doc builds a mac frame
% @returns a MAC frame ready to be transmitted in a bitstring (not including
% the CRC automatically added by the DW1000)
% @end
-spec encode(FrameControl, MacHeader, Payload) -> bitstring() when
      FrameControl :: frame_control(),
      MacHeader    :: mac_header(),
      Payload      :: bitstring().
encode(FrameControl, MacHeader, Payload) ->
    Header = build_mac_header(FrameControl, MacHeader),
    <<Header/bitstring, Payload/bitstring>>.

% @doc Builds an ACK frame
% @returns a MAC frame ready to be transmitted in a bitstring
% (not including the CRC automatically added by the DW1000)
% @end
encode_ack(FramePending, Seqnum) ->
    FC = build_frame_control(#frame_control{frame_type = ?FTYPE_ACK,
                                            frame_pending = FramePending,
                                            dest_addr_mode = ?NONE,
                                            src_addr_mode = ?NONE}),
    <<FC/bitstring, Seqnum:8>>.

% Might have to expend that function in the future
-spec encode_beacon_request() -> bitstring().
encode_beacon_request() ->
    Seqnum = rand:uniform(255),
    FrameControl = #frame_control{frame_type = ?FTYPE_MACCOM,
                                  dest_addr_mode = ?SHORT_ADDR,
                                  src_addr_mode = ?NONE,
                                  frame_pending = ?DISABLED,
                                  ack_req = ?DISABLED,
                                  sec_en = ?DISABLED},
    MacHeader = #mac_header{dest_pan = <<16#FFFF:16>>,
                            dest_addr = <<16#FFFF:16>>,
                            seqnum = Seqnum},
    Payload = <<16#06:16>>,
    encode(FrameControl, MacHeader, Payload).

% @doc decodes the MAC frame given in the arguments
% @return A tuple containing the decoded FH, MH and the payload
% @end
-spec decode(Data) -> {FrameControl, MacHeader, Payload} when
      Data         :: binary(),
      FrameControl :: frame_control(),
      MacHeader    :: mac_header(),
      Payload      :: bitstring().
decode(Data) ->
    <<FC:16/bitstring, Seqnum:8, Rest/bitstring>> = Data,
    FrameControl = decode_frame_control(FC),
    decode_rest(FrameControl, Seqnum, Rest).

% @todo check the bit order of each fields
% @todo docs
-spec decode_beacon(Data) -> Result when
      Data          :: binary(),
      Result        :: {FrameControl, MacHeader, Metadatas, BeaconPayload}
                     | {error, malformed},
      FrameControl  :: frame_control(),
      MacHeader     :: mac_header(),
      Metadatas     :: beacon_metadatas(),
      BeaconPayload :: binary().
decode_beacon(Data) ->
    {FrameControl, MacHeader, Payload} = decode(Data),

    ct:log("Payload size: ~w bits ~n", [bit_size(Payload)]),
    case decode_beacon_metadatas(Payload) of
        {ok, Metadatas, BeaconPayload} -> {FrameControl,
                                           MacHeader,
                                           Metadatas,
                                           BeaconPayload};
        Error -> Error
    end.

-spec decode_beacon_metadatas(Payload) -> Result when
      Payload         :: binary(),
      Result          :: {ok, Metadatas, BeaconPayload}  | {error, malformed},
      Metadatas       :: {SuperframeSpecs, Gts, PendAddr},
      SuperframeSpecs :: superframe_specs(),
      Gts             :: gts_fields(),
      PendAddr        :: pending_addr_flds(),
      BeaconPayload   :: binary().
decode_beacon_metadatas(Payload) when bit_size(Payload) < 32 ->
    {error, malformed};
decode_beacon_metadatas(Payload) ->
    <<RawSpecs:16/bitstring, Left/bitstring>> = Payload,
    ct:log("Left size: ~w bits~n", [bit_size(Left)]),
    Specs = superframe_specs(RawSpecs),
    % Next 2 lines are buggy if malfored fields TOFIX
    {GTSFlds, LeftWithPendAddr} = gts(Left),
    {PendAddrs, BeaconPayload} = pending_addr(LeftWithPendAddr),
    Metadatas = {Specs, GTSFlds, PendAddrs},
    {ok, Metadatas, BeaconPayload}.

-spec superframe_specs(RawSpecs) -> superframe_specs() when
      RawSpecs :: <<_:16>>.
superframe_specs(RawSpecs) ->
    <<AssoPerm:1,
      PanCoord:1,
      _:1,
      BLE:1,
      FinalCAPSlot:4,
      SupOrd:4,
      BeacOrd:4>> = RawSpecs,
    #superframe_specs{beacon_order = BeacOrd,
                      superframe_order = SupOrd,
                      final_cap_slot = FinalCAPSlot,
                      ble = to_bool(BLE),
                      pan_coord = to_bool(PanCoord),
                      association_perm = to_bool(AssoPerm)}.

% @TODO what happens if we don't have the exact amount of descr ?
-spec gts(binary()) -> {gts_fields(), binary()}.
gts(LeftoverPayload) ->
      <<GTSPermit:1,
      _:4,
      GTSDescCnt:3/integer,
      Rest/bitstring>> = LeftoverPayload,

    ct:log("Rest size: ~w bits~n", [bit_size(Rest)]),
    ct:log("GTS descriptor count: ~w~n", [GTSDescCnt]),
    {GTSDirection, Descriptors, Left} = gts_fields(GTSDescCnt, Rest),
    GTSFields = #gts_fields{gts_descr_cnt = GTSDescCnt,
                            gts_permit = to_bool(GTSPermit),
                            gts_direction = GTSDirection,
                            gts_descr_list = lists:reverse(Descriptors)},
    {GTSFields, Left}.

-spec gts_fields(GTSDescCnt, Leftover) -> Result when
      GTSDescCnt   :: 0..7,
      Leftover     :: binary(),
      Result       :: {GTSDirection, Descriptors, Left},
      GTSDirection :: <<>> | <<_:7>>,
      Descriptors  :: [gts_descr()],
      Left         :: binary().
gts_fields(0, Leftover) ->
    {<<>>, [], Leftover};
gts_fields(GtsDescCnt, Leftover) ->
    <<DirectionMask:7/bitstring, _:1, Rest/bitstring>> = Leftover,
    {Descriptors, Left} = get_gts_descriptors(GtsDescCnt, Rest, []),
    {DirectionMask, Descriptors, Left}.

-spec get_gts_descriptors(DescLeft, Leftover, Acc) -> {Descriptors, Left} when
      DescLeft    :: 0..7,
      Leftover    :: binary(),
      Acc         :: [gts_descr()],
      Descriptors :: [gts_descr()],
      Left        :: binary().
get_gts_descriptors(0, Leftover, Acc) ->
    {Acc, Leftover};
get_gts_descriptors(DescLeft, Leftover, Acc) ->
    <<Length:4/integer,
      StartingSlot:4/integer,
      ShortAddr:16/bitstring,
      Rest/bitstring>> = Leftover,
    Descr = #gts_descr{short_addr = ShortAddr,
                       starting_slot = StartingSlot,
                       gts_length = Length},
    get_gts_descriptors(DescLeft-1, Rest, [Descr | Acc]).

% @TODO what happens if we don't have the exact amount of pending addr ?
-spec pending_addr(Leftover) -> {pending_addr_flds(), Rest} when
      Leftover :: <<_:8, _:_*8>>,
      Rest     :: binary().
pending_addr(Leftover) ->
    ct:log("Leftover size: ~w bits ~n", [bit_size(Leftover)]),
    <<_:1, NbrExt:3/integer,
      _:1, NbrShort:3/integer,
      Left/bitstring>> = Leftover,
    {ShortAddrs, ExtAddrs, Rest} = get_addrs(NbrShort, NbrExt, Left, {[], []}),
    {#pending_addr_flds{nbr_short_addr_pending = NbrShort,
                         nbr_ext_addr_pending = NbrExt,
                         short_addr_pending = lists:reverse(ShortAddrs),
                         ext_addr_pending = lists:reverse(ExtAddrs)},
     Rest}.

-spec get_addrs(NbrShrAddr, NbrExtAddr, Left, Acc) -> Result when
      NbrShrAddr :: 0..7,
      NbrExtAddr :: 0..7,
      Left       :: binary(),
      Acc        :: {[<<_:16>>], [<<_:64>>]},
      Result     :: {[<<_:16>>], [<<_:64>>], Rest},
      Rest       :: binary().
get_addrs(0, 0, Left, {ShortAddrs, ExtAddrs}) ->
    {ShortAddrs, ExtAddrs, Left};
get_addrs(0, NbrExt, Left, {ShortAddrs, ExtAddrs}) ->
    <<ExtAddr:64/bitstring, Rest/bitstring>> = Left,
    get_addrs(0, NbrExt-1, Rest, {ShortAddrs, [ExtAddr | ExtAddrs]});
get_addrs(NbrShort, NbrExt, Left, {ShortAddrs, ExtAddrs}) ->
    <<ShortAddr:16/bitstring, Rest/bitstring>> = Left,
    get_addrs(NbrShort-1, NbrExt, Rest, {[ShortAddr | ShortAddrs], ExtAddrs}).

%--- Internals -----------------------------------------------------------------

% @doc builds a mac header based on the FC and the MH struct. given in the args.
% <b> The MAC header doesn't support security fields yet </b>
% @returns the MAC header in a bitstring
% @end
-spec build_mac_header(FrameControl, MacHeader) -> binary() when
      FrameControl :: frame_control(),
      MacHeader    :: mac_header().
build_mac_header(FrameControl, MacHeader) ->
    FC = build_frame_control(FrameControl),

    DestPan = reverse_byte_order(MacHeader#mac_header.dest_pan),
    DestAddr= reverse_byte_order(MacHeader#mac_header.dest_addr),
    DestAddrFields = case FrameControl#frame_control.dest_addr_mode of
                         ?NONE -> <<>>;
                         _ -> <<DestPan/bitstring, DestAddr/bitstring>>
                    end,

    SrcPan = reverse_byte_order(MacHeader#mac_header.src_pan),
    SrcAddr= reverse_byte_order(MacHeader#mac_header.src_addr),
    SrcAddrFields = case {FrameControl#frame_control.src_addr_mode,
                          FrameControl#frame_control.pan_id_compr,
                          FrameControl#frame_control.dest_addr_mode} of
                        {?NONE, _, _} -> <<>>;
                        {_, ?DISABLED, _} -> <<SrcPan/bitstring, SrcAddr/bitstring>>; % if no compression is applied on PANID and SRC addr is present
                        {_, ?ENABLED, ?NONE}  -> <<SrcPan/bitstring, SrcAddr/bitstring>>; % if there is a compression of the PANID but the dest addr isn't present
                        {_, ?ENABLED, _} -> <<SrcAddr/bitstring>> % if there is a compression of the PANID and the dest addr is present
                    end,
    <<FC/bitstring,
      (MacHeader#mac_header.seqnum):8,
      DestAddrFields/bitstring,
      SrcAddrFields/bitstring>>.



% @private
% @doc Decodes the remaining sequence of bit the payload after the seqnum
% @end
-spec decode_rest(FrameControl :: frame_control(), Seqnum::integer(), Rest::binary()) -> {FrameControl::frame_control(), MacHeader::mac_header(), Payload::binary()}.  
decode_rest(#frame_control{frame_type = ?FTYPE_ACK} = FrameControl, Seqnum, _Rest) -> % Might cause an issue if piggybacking is used (allowed in IEEE 802.15.4?)
    {FrameControl, #mac_header{seqnum = Seqnum}, <<>>};
decode_rest(FrameControl, Seqnum, Rest) ->
    {DestPAN, DestAddr, SrcPAN, SrcAddr, Payload} = decode_mac_header(FrameControl#frame_control.dest_addr_mode, FrameControl#frame_control.src_addr_mode, FrameControl#frame_control.pan_id_compr, Rest),
    MacHeader = #mac_header{seqnum = Seqnum, dest_pan = reverse_byte_order(DestPAN), dest_addr = reverse_byte_order(DestAddr), src_pan = reverse_byte_order(SrcPAN), src_addr = reverse_byte_order(SrcAddr)},
    {FrameControl, MacHeader, Payload}.

% Note extended addresses and PAN ID are used in the case of inter-PAN comms
% In inter PAN communication, it can be omitted but it's not mandatory
-spec decode_mac_header(DestAddrMode, SrcAddrMode, PanIdCompr, Bits) -> Res when
      DestAddrMode :: flag(),
      SrcAddrMode  :: flag(),
      PanIdCompr   :: flag(),
      Bits         :: bitstring(),
      Res          :: {DestPAN, DestAddr, SrcPAN, SrcAddr, Payload},
      DestPAN      :: binary(),
      DestAddr     :: binary(),
      SrcPAN       :: binary(),
      SrcAddr      :: binary(),
      Payload      :: binary().
decode_mac_header(?EXTENDED, ?EXTENDED, ?DISABLED, <<DestPAN:16/bitstring, DestAddr:64/bitstring, SrcPAN:16/bitstring, SrcAddr:64/bitstring, Payload/bitstring>>) -> 
    {DestPAN, DestAddr, SrcPAN, SrcAddr, Payload};

decode_mac_header(?EXTENDED, ?EXTENDED, ?ENABLED, <<DestPAN:16/bitstring, DestAddr:64/bitstring, SrcAddr:64/bitstring, Payload/bitstring>>) ->
    {DestPAN, DestAddr, DestPAN, SrcAddr, Payload};

decode_mac_header(?EXTENDED, ?SHORT_ADDR, ?DISABLED, <<DestPAN:16/bitstring, DestAddr:64/bitstring, SrcPAN:16/bitstring, SrcAddr:16/bitstring, Payload/bitstring>>) ->
    {DestPAN, DestAddr, SrcPAN, SrcAddr, Payload};

decode_mac_header(?EXTENDED, ?SHORT_ADDR, ?ENABLED, <<DestPAN:16/bitstring, DestAddr:64/bitstring, SrcAddr:16/bitstring, Payload/bitstring>>) ->
    {DestPAN, DestAddr, DestPAN, SrcAddr, Payload};

decode_mac_header(?EXTENDED, ?NONE, _, <<DestPAN:16/bitstring, DestAddr:64/bitstring, Payload/bitstring>>) ->
    {DestPAN, DestAddr, <<>>, <<>>, Payload};

decode_mac_header(?SHORT_ADDR, ?EXTENDED, ?DISABLED, <<DestPAN:16/bitstring, DestAddr:16/bitstring, SrcPAN:16/bitstring, SrcAddr:64/bitstring, Payload/bitstring>>) ->
    {DestPAN, DestAddr, SrcPAN, SrcAddr, Payload};

decode_mac_header(?SHORT_ADDR, ?EXTENDED, ?ENABLED, <<DestPAN:16/bitstring, DestAddr:16/bitstring, SrcAddr:64/bitstring, Payload/bitstring>>) ->
    {DestPAN, DestAddr, DestPAN, SrcAddr, Payload};

decode_mac_header(?SHORT_ADDR, ?SHORT_ADDR, ?DISABLED, <<DestPAN:16/bitstring, DestAddr:16/bitstring, SrcPAN:16/bitstring, SrcAddr:16/bitstring, Payload/bitstring>>) ->
    {DestPAN, DestAddr, SrcPAN, SrcAddr, Payload};

decode_mac_header(?SHORT_ADDR, ?SHORT_ADDR, ?ENABLED, <<DestPAN:16/bitstring, DestAddr:16/bitstring, SrcAddr:16/bitstring, Payload/bitstring>>) ->
    {DestPAN, DestAddr, DestPAN, SrcAddr, Payload};

decode_mac_header(?SHORT_ADDR, ?NONE, _, <<DestPAN:16/bitstring, DestAddr:16/bitstring, Payload/bitstring>>) ->
    {DestPAN, DestAddr, <<>>, <<>>, Payload};

decode_mac_header(?NONE, ?EXTENDED, ?DISABLED, <<SrcPAN:16/bitstring, SrcAddr:64/bitstring, Payload/bitstring>>) ->
    {<<>>, <<>>, SrcPAN, SrcAddr, Payload};

decode_mac_header(?NONE, ?EXTENDED, ?ENABLED, <<SrcPAN:16/bitstring, SrcAddr:64/bitstring, Payload/bitstring>>) ->
    {SrcPAN, <<>>, SrcPAN, SrcAddr, Payload};

decode_mac_header(?NONE, ?SHORT_ADDR, ?DISABLED, <<SrcPAN:16/bitstring, SrcAddr:16/bitstring, Payload/bitstring>>) ->
    {<<>>, <<>>, SrcPAN, SrcAddr, Payload};

decode_mac_header(?NONE, ?SHORT_ADDR, ?ENABLED, <<SrcPAN:16/bitstring, SrcAddr:16/bitstring, Payload/bitstring>>) ->
    {SrcPAN, <<>>, SrcPAN, SrcAddr, Payload};

decode_mac_header(_SrcAddrMode, _DestAddrMode, _PanIdCompr, _Bits) ->
    error(internal_decoding_error).

% @private
% @doc Creates a MAC frame control
% @param FrameType: MAC frame type
% @param AR: ACK request
% @end
-spec build_frame_control(FrameControl) -> <<_:16>> when
      FrameControl :: frame_control().
build_frame_control(FrameControl) ->
    #frame_control{pan_id_compr=PanIdCompr,ack_req=AckReq,frame_pending=FramePending,sec_en=SecEn,
                   frame_type=FrameType,src_addr_mode=SrcAddrMode,frame_version=FrameVersion,dest_addr_mode=DestAddrMode} = FrameControl,
    <<2#0:1, PanIdCompr:1, AckReq:1, FramePending:1, SecEn:1, FrameType:3, SrcAddrMode:2, FrameVersion:2, DestAddrMode:2, 2#0:2>>.


% @private
% @doc Decode the frame control given in a bitstring form in the parameters
% @end
-spec decode_frame_control(FC) -> frame_control() when
      FC :: <<_:16>>.
decode_frame_control(FC) ->
    <<_:1, PanIdCompr:1, AckReq:1, FramePending:1, SecEn:1, FrameType:3, SrcAddrMode:2, FrameVersion:2, DestAddrMode:2, _:2>> = FC,
    #frame_control{frame_type = FrameType, sec_en = SecEn, frame_pending = FramePending, ack_req = AckReq, pan_id_compr = PanIdCompr, dest_addr_mode = DestAddrMode, frame_version = FrameVersion, src_addr_mode = SrcAddrMode}.

%--- Internal: Tool functions
% reverse_byte_order(Bitstring) ->
%     Size = bit_size(Bitstring),
%     <<X:Size/integer-little>> = Bitstring,
%     <<X:Size/integer-big>>.
reverse_byte_order(Bitstring) -> reverse_byte_order(Bitstring, <<>>).
reverse_byte_order(<<>>, Acc) -> Acc;
reverse_byte_order(<<Head:8>>, Acc) ->
    <<Head:8, Acc/bitstring>>;
reverse_byte_order(<<Head:8, Tail/bitstring>>, Acc) ->
    reverse_byte_order(Tail, <<Head:8, Acc/bitstring>>).

-spec to_bool(0 | 1) -> boolean().
to_bool(1) -> true;
to_bool(0) -> false.
