-module(mac_layer).

-include("mac_layer.hrl").

-export([mac_message/2, mac_message/3]).

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
% @end
%-------------------------------------------------------------------------------
-spec mac_message(FrameControl :: #frame_control{}, MacHeader :: #mac_header{}, Payload :: bitstring()) -> bitstring().
mac_message(FrameControl, MacHeader, Payload) ->
    Header = build_mac_header(FrameControl, MacHeader),
    <<Header/bitstring, Payload/bitstring, 2#0:16>>.

%--- Internal ------------------------------------------------------------------

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

reverse_byte_order(<<Head:8>>, Acc) ->
    <<Head:8, Acc/bitstring>>;
reverse_byte_order(<<Head:8, Tail/bitstring>>, Acc) ->
    reverse_byte_order(Tail, <<Head:8, Acc/bitstring>>). 
