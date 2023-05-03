-module(mac_layer).

-include("mac_layer.hrl").

-export([mac_send_data/3, mac_receive/0]).
-export([mac_message/2, mac_message/3]).

% Not used for now, might use it later for more clarity
% -type decoded_mac() :: {FrameControl :: #frame_control{}, MacHeader :: #mac_header{}, Payload :: bitstring()}.

%--- API -----------------------------------------------------------------------


%-------------------------------------------------------------------------------
% @doc builds a mac message without a payload
% @equiv mac_message(FrameControl, MacHeader, <<>>)
% @end
%-------------------------------------------------------------------------------
-spec mac_message(FrameControl :: #frame_control{}, MacHeader :: #mac_header{}) -> bitstring().
mac_message(FrameControl, MacHeader) ->
    mac_message(FrameControl, MacHeader, <<>>).
%-------------------------------------------------------------------------------
% @doc builds a mac message
% @returns a MAC message ready to send in a bitstring
% @end
%-------------------------------------------------------------------------------
-spec mac_message(FrameControl :: #frame_control{}, MacHeader :: #mac_header{}, Payload :: bitstring()) -> bitstring().
mac_message(FrameControl, MacHeader, Payload) ->
    Header = build_mac_header(FrameControl, MacHeader),
    <<Header/bitstring, Payload/bitstring, 2#0:16>>.


%-------------------------------------------------------------------------------
% @doc Sends a MAC message using the pmod_uwb
%-------------------------------------------------------------------------------
-spec mac_send_data(FrameControl :: #frame_control{}, MacHeader :: #mac_header{}, Payload :: bitstring()) -> ok.
mac_send_data(FrameControl, MacHeader, Payload) ->
    Message = mac_message(FrameControl, MacHeader, Payload),
    pmod_uwb:transmit(Message).

mac_receive() ->
    {_Length, Data} = pmod_uwb:reception(),
    mac_decode(Data),
    ok.


%--- Internal ------------------------------------------------------------------

%-------------------------------------------------------------------------------
% @doc builds a mac header based on the FrameControl and the MacHeader structures 
% given in the args.
% @returns the MAC header in a bitstring
% @end
%-------------------------------------------------------------------------------
-spec build_mac_header(FrameControl :: #frame_control{}, MacHeader :: #mac_header{}) -> bitstring().
build_mac_header(FrameControl, MacHeader) ->
    FC = build_frame_control(FrameControl),

    DestPan = reverse_byte_order(MacHeader#mac_header.dest_pan, <<>>),
    DestAddr= reverse_byte_order(MacHeader#mac_header.dest_addr, <<>>),
    DestAddrFields = case FrameControl#frame_control.dest_addr_mode of
                         ?NONE -> <<>>;
                         _ -> <<DestPan/bitstring, DestAddr/bitstring>> 
                    end,

    SrcPan = reverse_byte_order(MacHeader#mac_header.src_pan, <<>>),
    SrcAddr= reverse_byte_order(MacHeader#mac_header.src_addr, <<>>),
    SrcAddrFields = case {FrameControl#frame_control.src_addr_mode, FrameControl#frame_control.pan_id_compr, FrameControl#frame_control.dest_addr_mode} of
                        {?NONE, _, _} -> <<>>;
                        {_, ?DISABLED, _} -> <<SrcPan/bitstring, SrcAddr/bitstring>>;
                        {_, ?ENABLED, ?NONE}  -> <<SrcPan/bitstring, SrcAddr/bitstring>>;
                        {_, ?ENABLED, _} -> <<SrcAddr/bitstring>>
                    end,
    <<FC/bitstring,  (MacHeader#mac_header.seqnum):8, DestAddrFields/bitstring, SrcAddrFields/bitstring>>.


-spec mac_decode(Data :: bitstring()) -> {FrameControl :: #frame_control{}, MacHeader :: #mac_header{}, Payload :: bitstring()}. 
mac_decode(Data) ->
    <<FC:16, Seqnum:8, Rest/bitstring>> = Data,
    FrameControl = decode_frame_control(FC),
    % TODO: Decode the pan/addrs + the payload 
    % MacHeader = #mac_header{seqnum = Seqnum},
    [DestPan, DestAddr, SrcPan_, SrcAddr, Payload] = lists:flatten(decode_addrs(dest_pan_id, Rest, FrameControl, bit_size(Rest))),
    SrcPan = case {FrameControl#frame_control.pan_id_compr, SrcPan_} of
                 {?ENABLED, <<>>} -> DestPan;
                 _ -> SrcPan_
             end,
    MacHeader = #mac_header{seqnum = Seqnum, dest_pan = DestPan, dest_addr = DestAddr, src_pan = SrcPan, src_addr = SrcAddr},
    {FrameControl, MacHeader, Payload}.

decode_addrs(dest_pan_id, Rest, FrameControl, SizeLeft) ->
    case FrameControl#frame_control.dest_addr_mode of
        ?NONE -> [<<>>, <<>>, decode_addrs(src_pan_id, Rest, FrameControl, SizeLeft)];
        _ -> <<PanId:16, Tail>> = Rest, [PanId, decode_addrs(dest_addr, Tail, FrameControl, SizeLeft-16)]
    end;
decode_addrs(dest_addr, Rest, FrameControl, SizeLeft) ->
    case FrameControl#frame_control.dest_addr_mode of
        ?SHORT_ADDR -> <<Addr:16, Tail>> = Rest, [Addr, decode_addrs(src_pan_id, Tail, FrameControl, SizeLeft-16)];
        ?EXTENDED -> <<Addr:64, Tail>> = Rest, [Addr, decode_addrs(src_pan_id, Tail, FrameControl, SizeLeft-64)]
    end;
decode_addrs(src_pan_id, Rest, FrameControl, SizeLeft) ->
    case {FrameControl#frame_control.pan_id_compr, FrameControl#frame_control.dest_addr_mode} of
        {?DISABLED, _} -> <<PanID:16, Tail>> = Rest, [PanID, decode_addrs(src_addr, Tail, FrameControl, SizeLeft-16)];
        {?ENABLED, ?NONE} -> <<PanID:16, Tail>> = Rest, [PanID, decode_addrs(src_addr, Tail, FrameControl, SizeLeft-16)];
        {?ENABLED, _} -> [<<>>, decode_addrs(src_addr, Rest, FrameControl, SizeLeft)]
    end;
decode_addrs(src_addr_id, Rest, FrameControl, SizeLeft) ->
    case FrameControl#frame_control.src_addr_mode of
        ?NONE -> <<Payload:(SizeLeft-16)/bitstring, _:16>> = Rest, Payload;
        ?SHORT_ADDR -> <<Addr:16, Payload:(SizeLeft-32)/bitstring, _:16>> = Rest, [Addr, Payload];
        ?EXTENDED -> <<Addr:64, Payload:(SizeLeft-80)/bitstring, _:16>> = Rest, [Addr, Payload]
    end.

%-------------------------------------------------------------------------------
% @private
% @doc Creates a MAC frame control 
% @param FrameType: MAC frame type
% @param AR: ACK request
% @end
%-------------------------------------------------------------------------------
-spec build_frame_control(FrameControl :: #frame_control{}) -> bitstring().
build_frame_control(FrameControl) ->
    #frame_control{pan_id_compr=PanIdCompr,ack_req=AckReq,frame_pending=FramePending,sec_en=SecEn,
                   frame_type=FrameType,src_addr_mode=SrcAddrMode,frame_version=FrameVersion,dest_addr_mode=DestAddrMode} = FrameControl,
    % <<16#6188:16>>.
    <<2#0:1, PanIdCompr:1, AckReq:1, FramePending:1, SecEn:1, FrameType:3, SrcAddrMode:2, FrameVersion:2, DestAddrMode:2, 2#0:2>>.


%-------------------------------------------------------------------------------
% @private
% @doc Decode the frame control given in a bitstring form in the parameters
%-------------------------------------------------------------------------------
-spec decode_frame_control(FC :: bitstring) -> #frame_control{}.
decode_frame_control(FC) -> 
    <<_:1, PanIdCompr:1, AckReq:1, FramePending:1, SecEn:1, FrameType:3, SrcAddrMode:2, FrameVersion:2, DestAddrMode:2, _:2>> = FC,
    #frame_control{frame_type = FrameType, sec_en = SecEn, frame_pending = FramePending, ack_req = AckReq, pan_id_compr = PanIdCompr, dest_addr_mode = DestAddrMode, frame_version = FrameVersion, src_addr_mode = SrcAddrMode}.

%--- Tool functions ------------------------------------------------------------

reverse_byte_order(<<Head:8>>, Acc) ->
    <<Head:8, Acc/bitstring>>;
reverse_byte_order(<<Head:8, Tail/bitstring>>, Acc) ->
    reverse_byte_order(Tail, <<Head:8, Acc/bitstring>>). 
