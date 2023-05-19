-module(mac_layer).

-include("mac_layer.hrl").

-export([delayed_mac_send_data/4]).
-export([mac_send_data/3, mac_send_data/4, mac_receive/0, mac_receive/1]).
-export([mac_decode/1]).
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
% @returns a MAC message ready to be transmitted in a bitstring (not including the CRC automatically added by the DW1000)
% @end
%-------------------------------------------------------------------------------
-spec mac_message(FrameControl :: #frame_control{}, MacHeader :: #mac_header{}, Payload :: bitstring()) -> bitstring().
mac_message(FrameControl, MacHeader, Payload) ->
    Header = build_mac_header(FrameControl, MacHeader),
    <<Header/bitstring, Payload/bitstring>>.


%-------------------------------------------------------------------------------
% @doc Sends a MAC message using the pmod_uwb without any options
% The 2 bytes CRC are automatically added at the end of the payload and
% must not be included in the Payload given in the arguments
%-------------------------------------------------------------------------------
-spec mac_send_data(FrameControl :: #frame_control{}, MacHeader :: #mac_header{}, Payload :: bitstring()) -> ok.
mac_send_data(FrameControl, MacHeader, Payload) ->
    mac_send_data(FrameControl, MacHeader, Payload, #tx_opts{}).

%-------------------------------------------------------------------------------
% @doc Sends a MAC message using the pmod_uwb using the specified options 
% The 2 bytes CRC are automatically added at the end of the payload and
% must not be included in the Payload given in the arguments
%
% If the option wait4resp is enabled, the function will wait for the reception and return the decoded frame
%-------------------------------------------------------------------------------
-spec mac_send_data(FrameControl :: #frame_control{}, MacHeader :: #mac_header{}, Payload :: bitstring(), Option :: #tx_opts{}) -> ok | {FrameControl :: #frame_control{}, MacHeader :: #mac_header{}, Payload :: bitstring()}.
mac_send_data(FrameControl, MacHeader, Payload, Options) ->
    Message = mac_message(FrameControl, MacHeader, Payload),
    pmod_uwb:transmit(Message, Options).

%-------------------------------------------------------------------------------
% @doc sends a MAC message using the pmod_uwb with some delay
% The 2 bytes CRC are automatically added at the end of the payload and
% must not be included in the Payload given in the arguments
%
% The delay must be exprimed in system time unit. NB: the lower 9 bits are ignored to compute the delay
%-------------------------------------------------------------------------------
-spec delayed_mac_send_data(FrameControl :: #frame_control{}, MacHeader :: #mac_header{}, Payload :: bitstring(), Delay :: integer()) -> ok.
delayed_mac_send_data(FrameControl, MacHeader, Payload, Delay) ->
    Message = mac_message(FrameControl, MacHeader, Payload),
    Options = #tx_opts{txdlys = ?ENABLED, tx_delay = Delay}, 
    pmod_uwb:transmit(Message, Options).

%-------------------------------------------------------------------------------
% @doc Receive a message using the pmod_uwb and decode the message 
%
% @equiv mac_receive(false)
%
% @return the received mac message decoded
%-------------------------------------------------------------------------------
-spec mac_receive() -> {FrameControl :: #frame_control{}, MacHeader :: #mac_header{}, Payload :: bitstring()}.
mac_receive() ->
    mac_receive(false).

%-------------------------------------------------------------------------------
% @doc Receive a message using the pmod_uwb and decode the message 
% @param RXEnab indicates if the reception was already enabled (or is enabled with delay)
% <b>Warning:</b> if this function is called with RXEnab = true and the reception isn't set, the driver will be stuck in a loop without any timeout
% @return the received mac message decoded
%-------------------------------------------------------------------------------
-spec mac_receive(RXEnab :: boolean()) -> {FrameControl :: #frame_control{}, MacHeader :: #mac_header{}, Payload :: bitstring()}.
mac_receive(RXEnab) ->
    io:format("Waiting for reception~n"),
    case pmod_uwb:reception(RXEnab) of
        {_Length, Data} -> mac_decode(Data);
        Err -> Err
    end.

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


%-------------------------------------------------------------------------------
% @doc decodes the MAC message given in the arguments
% @return A tuple containing the decoded frame control, the decoded mac header and the payload
%-------------------------------------------------------------------------------
-spec mac_decode(Data :: bitstring()) -> {FrameControl :: #frame_control{}, MacHeader :: #mac_header{}, Payload :: bitstring()}. 
mac_decode(Data) ->
    <<FC:16/bitstring, Seqnum:8, Rest/bitstring>> = Data,
    FrameControl = decode_frame_control(FC),
    % TODO: Decode the pan/addrs + the payload 
    % MacHeader = #mac_header{seqnum = Seqnum},
    decode_rest(FrameControl, Seqnum, Rest).

decode_rest(#frame_control{frame_type = ?FTYPE_ACK} = FrameControl, Seqnum, _Rest) ->
    {FrameControl, #mac_header{seqnum = Seqnum}, <<>>};
decode_rest(FrameControl, Seqnum, Rest) ->
    [DestPan, DestAddr, SrcPan_, SrcAddr, Payload] = lists:flatten(decode_addrs(dest_pan_id, Rest, FrameControl)),
    SrcPan = case {FrameControl#frame_control.pan_id_compr, SrcPan_} of
                 {?ENABLED, <<>>} -> DestPan;
                 _ -> SrcPan_
             end,
    MacHeader = #mac_header{seqnum = Seqnum, dest_pan = DestPan, dest_addr = DestAddr, src_pan = SrcPan, src_addr = SrcAddr},
    {FrameControl, MacHeader, Payload}.


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
                     [reverse_byte_order(Addr), decode_addrs(src_pan_id, Tail, FrameControl)]
    end;
decode_addrs(src_pan_id, Rest, FrameControl) ->
    case {FrameControl#frame_control.pan_id_compr, FrameControl#frame_control.dest_addr_mode} of
        {?DISABLED, _} -> <<PanID:16/bitstring, Tail/bitstring>> = Rest, 
                          [reverse_byte_order(PanID), decode_addrs(src_addr, Tail, FrameControl)];
        {?ENABLED, ?NONE} -> <<PanID:16/bitstring, Tail/bitstring>> = Rest, 
                             [reverse_byte_order(PanID), decode_addrs(src_addr, Tail, FrameControl)];
        {?ENABLED, _} -> [<<>>, decode_addrs(src_addr, Rest, FrameControl)]
    end;
decode_addrs(src_addr, Rest, FrameControl) ->
    case FrameControl#frame_control.src_addr_mode of
        ?NONE -> Rest; 
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

reverse_byte_order(Bitstring) -> reverse_byte_order(Bitstring, <<>>).
reverse_byte_order(<<Head:8>>, Acc) ->
    <<Head:8, Acc/bitstring>>;
reverse_byte_order(<<Head:8, Tail/bitstring>>, Acc) ->
    reverse_byte_order(Tail, <<Head:8, Acc/bitstring>>). 
