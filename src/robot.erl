% @doc robot public API.
-module(robot).

-behavior(application).

-include("mac_layer.hrl").

-export([test_sys_clock/1]).
-export([send_mac/1, receive_mac/0]).
-export([send_and_wait_ack/1, receive_and_ack/0, receive_data_jitter/0]).
-export([test_receiver/0, test_sender/0, test_sender_ack/2, test_receiver_ack/0]).

% Callbacks
-export([start/2]).
-export([stop/1]).


%--- API -----------------------------------------------------------------------

send_and_wait_ack(Data) ->
    #{pan_id := SrcPAN, short_addr := SrcAddr} = pmod_uwb:read(panadr),
    send_and_wait_ack(Data, SrcPAN, SrcAddr).

send_and_wait_ack(Data, SrcPAN, SrcAddr) ->
    FrameControl = #frame_control{ack_req = ?ENABLED},
    MacHeader = #mac_header{dest_pan = <<16#FFFF:16>>, dest_addr = <<16#FFFF:16>>, src_pan = <<SrcPAN:16>>, src_addr = <<SrcAddr:16>>},
    Mac = mac_layer:mac_frame(FrameControl, MacHeader, Data),
    io:format("~w~n", [Mac]),

    pmod_uwb:write(sys_cfg, #{ffen => 2#1, ffaa => 2#1, autoack => 2#1}), % enable frame filtering and allow ACK frame reception and enable autoack
    pmod_uwb:transmit(Mac),

    io:format("Waiting for ACK~n"),
    {_Length, _Data} = pmod_uwb:reception(),
    io:format("Received ACK ?~n").

receive_and_ack() ->
    pmod_uwb:write(sys_cfg, #{ffad => 2#1, ffaa => 2#1, autoack => 2#1}), % allow ACK and data frame reception and enable autoack
    pmod_uwb:write(sys_cfg, #{ffen => 2#1}), % enable frame filtering and allow ACK frame reception and enable autoack
    receive_data_jitter(). % Here we just want to receive one frame
    
test_receiver() -> receive_data_jitter().

test_sender() -> send_data(0, 100).

test_receiver_ack() ->
    pmod_uwb:write(panadr, #{pan_id => 16#BEEF, short_addr => 16#0002}),
    pmod_uwb:write(sys_cfg, #{ffad => 2#1, autoack => 2#1}), % allow ACK and data frame reception and enable autoack
    pmod_uwb:write(sys_cfg, #{ffen => 2#1}), % enable frame filtering and allow ACK frame reception and enable autoack
    receive_data_jitter().

% @doc Test the sender
% @param NbrFrames => The number of frames to send
% @param FrameSize => The size of the frame to send in bytes
test_sender_ack(NbrFrames, FrameSize) ->
    pmod_uwb:write(panadr, #{pan_id => 16#BEEF, short_addr => 16#0001}),
    pmod_uwb:write(rx_fwto, #{rxfwto => 16#FFFF}),
    pmod_uwb:write(sys_cfg, #{rxwtoe => 2#1}), 
    #{short_addr := SrcAddr} = pmod_uwb:read(panadr),
    pmod_uwb:write(sys_cfg, #{ffaa => 2#1, autoack => 2#1}), % allow ACK and data frame reception and enable autoack
    pmod_uwb:write(sys_cfg, #{ffen => 2#1}), % enable frame filtering and allow ACK frame reception and enable autoack
    Data = <<0:(FrameSize*8)>>,
    Start = os:timestamp(),
    {Success, Error, Total} = send_data_wait_ack(0, NbrFrames, SrcAddr, 10, 10, Data, {0, 0, 0}),
    End = os:timestamp(),
    Time = timer:now_diff(End, Start)/1000000,
    io:format("------------------- Report -------------------~n"),
    io:format("Sent ~w frames - Success rate ~.3f (~w/~w) - Error rate ~.3f (~w/~w)~n", [Total, Success/Total, Success, Total, Error/Total, Error, Total]),
    io:format("Data rate ~.1f b/s - In ~w s ~n", [(bit_size(Data)*NbrFrames)/Time, Time]),
    io:format("----------------------------------------------~n").

send_mac(Data) -> 
    #{pan_id := SrcPAN, short_addr := SrcAddr} = pmod_uwb:read(panadr),
    FrameControl = #frame_control{pan_id_compr = ?ENABLED},
    MacHeader = #mac_header{seqnum = 0, dest_pan = <<SrcPAN:16>>, dest_addr = <<16#FFFF:16>>, src_addr = <<SrcAddr:16>>},
    mac_layer:mac_send_data(FrameControl, MacHeader, <<Data/bitstring>>).

receive_mac() ->
    mac_layer:mac_receive().

test_sys_clock(0) -> ok;
test_sys_clock(N) ->
    #{sys_time := SYS_CLOCK} = pmod_uwb:read(sys_time),
    io:format("~w~n", [SYS_CLOCK]),
    test_sys_clock(N-1).

%--- Private -------------------------------------------------------------------
receive_data_jitter() ->
    case mac_layer:mac_receive(false) of
        {#frame_control{frame_type = ?FTYPE_DATA, pan_id_compr = ?ENABLED} = _FrameControl, MacHeader, Data} -> 
            io:format("Received data from ~w with seqnum ~w~n", [MacHeader#mac_header.src_addr, MacHeader#mac_header.seqnum]),
            % Simulates some delay in the network for every frame out of 4
            case rand:uniform(4) of
                _ -> ok;
                1 -> timer:sleep(200);
                _ -> ok
            end,
            pmod_uwb:wait_for_transmission();
            % pmod_uwb:write(sys_status, #{txfrs => 2#1});
        {_, _, _} -> io:format("Received unexpected frame~n");
        Err -> io:format("Reception error: ~w~n", [Err])
    end,
    receive_data_jitter().

send_data(Max, Max) -> ok;
send_data(Cnt, Max) ->
    pmod_uwb:transmit(<<16#5C, Cnt, (list_to_binary("Data"))/bitstring>>),
    timer:sleep(5000),
    send_data(Cnt+1, Max).


%-------------------------------------------------------------------------------
% @private
% @param Cnt: number of MAC message already sent
% @param Max: total number of MAC message to send
% @param SrcAddr: the address of the device sending the MAC message
% @param TrialsLeft: the number of reception attempts left
% @param TotTrialsAllowed: the maximum number of times we will try to receive a frame after a bad reception
% @TODO use RXAUTR later on
%-------------------------------------------------------------------------------
-spec send_data_wait_ack(Cnt :: integer(), Max :: integer(), SrcAddr :: integer(), TrialsLeft :: integer(), TotTrialsAllowed :: integer(), Data :: bitstring(), _Stats) -> ok | {error, any()}.
send_data_wait_ack(_, _, _, 0, _, _, Stats) -> error({reception_error, "Max trials reached", Stats});
send_data_wait_ack(Max, Max, _, _, _, _, Stats) -> Stats;
send_data_wait_ack(Cnt, Max, SrcAddr, TrialsLeft, TotTrialsAllowed, Data, {Success, Error, TotalFrameSent}) ->
    Seqnum = Cnt rem 16#FF,
    FrameControl = #frame_control{ack_req = ?ENABLED, pan_id_compr = ?ENABLED},
    MacHeader = #mac_header{seqnum = Seqnum, dest_pan = <<16#BEEF:16>>, dest_addr = <<16#0002:16>>, src_addr = <<SrcAddr:16>>},
    io:format("Sending message #~w with seqnum ~w~n", [Cnt, Seqnum]),
    mac_layer:mac_send_data(FrameControl, MacHeader, Data, #tx_opts{wait4resp = ?ENABLED, w4r_tim = 0}),
    case  mac_layer:mac_receive(true) of
        {#frame_control{frame_type = ?FTYPE_ACK} = _RxFrameControl, #mac_header{seqnum = Seqnum} = _RxMacHeader, _RxData} -> io:format("ACK received for frame seqnum ~w~n", [_RxMacHeader#mac_header.seqnum]),
                                                                                                                             send_data_wait_ack(Cnt+1, Max, SrcAddr, TotTrialsAllowed, TotTrialsAllowed, Data, {Success+1, Error, TotalFrameSent+1});
        {_RxFrameControl, _RxMacHeader, RxData} -> io:format("Received MAC frame but not ACK: ~w~n", [RxData]),
                                                   send_data_wait_ack(Cnt, Max, SrcAddr, TrialsLeft, TotTrialsAllowed, Data, {Success, Error, TotalFrameSent});
        _ -> io:format("Reception error. Trying again...~n"),
               pmod_uwb:write(sys_status, #{rxfto => 2#1}), % reset rxfto to avoid false t.o.
               send_data_wait_ack(Cnt, Max, SrcAddr, TrialsLeft-1, TotTrialsAllowed, Data, {Success, Error+1, TotalFrameSent+1})
    end.

%--- Callbacks -----------------------------------------------------------------

% @private
start(_Type, _Args) -> 
    {ok, Supervisor} = robot_sup:start_link(),
    grisp:add_device(spi2, pmod_uwb),
    % Res = pmod_uwb:read(dev_id),
    {ok, Supervisor}.

% @private
stop(_State) -> ok.
