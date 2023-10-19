-module(mac_layer).
-behaviour(gen_mac_layer).

-include("mac_layer.hrl").

%%% gen_mac_layer callbacks
-export([init/1]).
-export([tx/4]).
-export([rx/1]).
-export([terminate/2]).

-export([mac_decode/1]).
-export([mac_frame/2, mac_frame/3]).


%--- mac_layer_behaviour callback functions ---------------------------------------------

-spec init(Params::term()) -> State :: term().
init(Params) ->
    PhyModule = case Params of
              #{phy_layer := PHY} -> PHY;
              _ -> pmod_uwb
          end,
    #{phy_layer => PhyModule}. 

tx(#{phy_layer := PhyModule} = State, FrameControl, MacHeader, Payload) ->
    case PhyModule:transmit(mac_frame(FrameControl, MacHeader, Payload), #tx_opts{}) of
        ok -> {ok, State};
        Error -> {error, Error, State}
    end.

rx(#{phy_layer := PhyModule} = State) ->
    case PhyModule:reception() of
        {_Length, Frame} -> {ok, State, mac_decode(Frame)};
        Err -> {error, Err, State}
    end.

terminate(_State, _Reason) -> ok.

%--- Internal ------------------------------------------------------------------

%-------------------------------------------------------------------------------
% @doc builds a mac frame without a payload
% @equiv mac_frame(FrameControl, MacHeader, <<>>)
% @end
%-------------------------------------------------------------------------------
-spec mac_frame(FrameControl :: #frame_control{}, MacHeader :: #mac_header{}) -> bitstring().
mac_frame(FrameControl, MacHeader) ->
    mac_frame(FrameControl, MacHeader, <<>>).

%-------------------------------------------------------------------------------
% @doc builds a mac frame
% @returns a MAC frame ready to be transmitted in a bitstring (not including the CRC automatically added by the DW1000)
% @end
%-------------------------------------------------------------------------------
-spec mac_frame(FrameControl :: #frame_control{}, MacHeader :: #mac_header{}, Payload :: bitstring()) -> bitstring().
mac_frame(FrameControl, MacHeader, Payload) ->
    Header = build_mac_header(FrameControl, MacHeader),
    <<Header/bitstring, Payload/bitstring>>.

%-------------------------------------------------------------------------------
% @doc builds a mac header based on the FrameControl and the MacHeader structures given in the args.
% <b> The MAC header doesn't support security fields yet </b>
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
                        {_, ?DISABLED, _} -> <<SrcPan/bitstring, SrcAddr/bitstring>>; % if no compression is applied on PANID and SRC addr is present
                        {_, ?ENABLED, ?NONE}  -> <<SrcPan/bitstring, SrcAddr/bitstring>>; % if there is a compression of the PANID but the dest addr isn't present
                        {_, ?ENABLED, _} -> <<SrcAddr/bitstring>> % if there is a compression of the PANID and the dest addr is present
                    end,
    <<FC/bitstring,  (MacHeader#mac_header.seqnum):8, DestAddrFields/bitstring, SrcAddrFields/bitstring>>.


%-------------------------------------------------------------------------------
% @doc decodes the MAC frame given in the arguments
% @return A tuple containing the decoded frame control, the decoded mac header and the payload
% @end
%-------------------------------------------------------------------------------
-spec mac_decode(Data :: bitstring()) -> {FrameControl :: #frame_control{}, MacHeader :: #mac_header{}, Payload :: bitstring()}. 
mac_decode(Data) ->
    <<FC:16/bitstring, Seqnum:8, Rest/bitstring>> = Data,
    FrameControl = decode_frame_control(FC),
    decode_rest(FrameControl, Seqnum, Rest).

%-------------------------------------------------------------------------------
% @private
% @doc Decodes the remaining sequence of bit present in the payload after the seqnum
% @end
%-------------------------------------------------------------------------------
decode_rest(#frame_control{frame_type = ?FTYPE_ACK} = FrameControl, Seqnum, _Rest) ->
    {FrameControl, #mac_header{seqnum = Seqnum}, <<>>};
decode_rest(FrameControl, Seqnum, Rest) ->
    [DestPan_, DestAddr, SrcPan_, SrcAddr, Payload] = lists:flatten(decode_addrs(dest_pan_id, Rest, FrameControl)),
    DestPan = case {DestPan_, FrameControl#frame_control.pan_id_compr, FrameControl#frame_control.frame_type} of 
                  {<<>>, ?ENABLED, _} -> SrcPan_; % Can always deduce if the compression is enabled
                  {<<>>, ?DISABLED, ?FTYPE_ACK} -> <<>>; % if compression isn't enabled and ACK => can't deduce
                  {<<>>, ?DISABLED, ?FTYPE_BEACON} -> <<>>; % if compression isn't enabled and BEACON => can't deduce
                  {<<>>, ?DISABLED, _} -> SrcPan_; % Other wise destination is PAN coord with same PANID as SRC
                  {_, _, _} -> DestPan_
              end,
    SrcPan = case {SrcPan_, FrameControl#frame_control.pan_id_compr, FrameControl#frame_control.frame_type} of
                 {<<>>, ?ENABLED, _} -> DestPan;
                 {<<>>, ?DISABLED, ?FTYPE_ACK} -> <<>>; % if compression is disabled and frame type is an ACK => can't deduce (e.g. ACK comming from outside the PAN
                 {<<>>, ?DISABLED, _} -> DestPan;
                 {_, _, _} -> SrcPan_
             end,
    MacHeader = #mac_header{seqnum = Seqnum, dest_pan = DestPan, dest_addr = DestAddr, src_pan = SrcPan, src_addr = SrcAddr},
    {FrameControl, MacHeader, Payload}.


%-------------------------------------------------------------------------------
% @private
% @doc decode the address fields present in the remaining sequence of bits based on the settings inside Framecontrol
%
% The first parameter is an atom representing the the field that should be parsed next
% @end
%-------------------------------------------------------------------------------
decode_addrs(dest_pan_id, Rest, FrameControl) ->
    case FrameControl#frame_control.dest_addr_mode of
        ?NONE -> [<<>>, <<>>, decode_addrs(src_pan_id, Rest, FrameControl)];
        _ -> <<PanID:16/bitstring, Tail/bitstring>> = Rest, 
             [reverse_byte_order(PanID), decode_addrs(dest_addr, Tail, FrameControl)]
    end;
decode_addrs(dest_addr, Rest, FrameControl) ->
    case FrameControl#frame_control.dest_addr_mode of
        ?SHORT_ADDR -> <<Addr:16/bitstring, Tail/bitstring>> = Rest, 
                       [reverse_byte_order(Addr), decode_addrs(src_pan_id, Tail, FrameControl)];
        ?EXTENDED -> <<Addr:64/bitstring, Tail/bitstring>> = Rest, 
                     [reverse_byte_order(Addr), decode_addrs(src_pan_id, Tail, FrameControl)];
        _ -> io:format("Frame control dest_addr: ~w~n", [FrameControl#frame_control.dest_addr_mode])
    end;
decode_addrs(src_pan_id, Rest, FrameControl) ->
    case {FrameControl#frame_control.pan_id_compr, FrameControl#frame_control.src_addr_mode} of
        {?ENABLED, _} -> [<<>>, decode_addrs(src_addr, Rest, FrameControl)];
        {_, ?NONE} -> [<<>>, <<>>, Rest];
        _ -> <<PanID:16/bitstring, Tail/bitstring>> = Rest, % If compr disabled and src_addr_mode isn't none
             [reverse_byte_order(PanID), decode_addrs(src_addr, Tail, FrameControl)]
    end;
decode_addrs(src_addr, Rest, FrameControl) ->
    case FrameControl#frame_control.src_addr_mode of
        ?NONE -> [<<>>, Rest]; 
        ?SHORT_ADDR -> <<Addr:16/bitstring, Payload/bitstring>> = Rest, 
                       [reverse_byte_order(Addr), Payload];
        ?EXTENDED -> <<Addr:64/bitstring, Payload/bitstring>> = Rest, 
                     [reverse_byte_order(Addr), Payload]
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
    <<2#0:1, PanIdCompr:1, AckReq:1, FramePending:1, SecEn:1, FrameType:3, SrcAddrMode:2, FrameVersion:2, DestAddrMode:2, 2#0:2>>.


%-------------------------------------------------------------------------------
% @private
% @doc Decode the frame control given in a bitstring form in the parameters
% @end
%-------------------------------------------------------------------------------
-spec decode_frame_control(FC :: bitstring) -> #frame_control{}.
decode_frame_control(FC) -> 
    <<_:1, PanIdCompr:1, AckReq:1, FramePending:1, SecEn:1, FrameType:3, SrcAddrMode:2, FrameVersion:2, DestAddrMode:2, _:2>> = FC,
    #frame_control{frame_type = FrameType, sec_en = SecEn, frame_pending = FramePending, ack_req = AckReq, pan_id_compr = PanIdCompr, dest_addr_mode = DestAddrMode, frame_version = FrameVersion, src_addr_mode = SrcAddrMode}.

%--- Tool functions ------------------------------------------------------------

reverse_byte_order(Bitstring) -> reverse_byte_order(Bitstring, <<>>).
reverse_byte_order(<<Head:8>>, Acc) ->
    <<Head:8, Acc/bitstring>>;
reverse_byte_order(<<Head:8, Tail/bitstring>>, Acc) ->
    reverse_byte_order(Tail, <<Head:8, Acc/bitstring>>). 


