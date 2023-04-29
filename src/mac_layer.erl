-module(mac_layer).

-export([mac_message/8]).

-define(FTYPE_BEACON, 2#000).
-define(FTYPE_DATA, 2#001).
-define(FTYPE_ACK, 2#010).
-define(FTYPE_MACCOM, 2#011).

%--- API -----------------------------------------------------------------------

mac_message(data, Data, AR, Seqnum, DestPAN, DestAddr, SrcPAN, SrcAddr) ->
    FC = frame_control(?FTYPE_DATA, AR),
    Header = mac_header(FC, Seqnum, DestPAN, DestAddr, SrcPAN, SrcAddr),
    <<Header/bitstring, (list_to_binary(Data))/bitstring, 2#0:16>>.

%--- Internal ------------------------------------------------------------------

mac_header(FrameControl, Seqnum, DestPAN, DestAddr, _SrcPAN, SrcAddr) ->
    <<FrameControl:16/bitstring, Seqnum:8, DestPAN:16, DestAddr:16, SrcAddr:16>>.

frame_control(FrameType, AR) ->
    % <<FrameType:3, 2#0:1, 2#0:1, AR:1, 2#0:1, 2#0:3, 2#10:2, 2#01:2, 2#10>>.
    % << SrcAddrMode, FrameVer, DestAddrMode, RSV, PANID comp, AR, Frame pending, Sec en, Frame type>>
    % <<2#10:2, 2#00:2, 2#10:2, 2#000:3, 2#0:1, AR:1, 2#0:1, 2#0:1, FrameType:3>>.
    % <<16#8861:16>>.
    <<16#6188:16>>.
