% @doc robot public API.
-module(robot).

-behavior(application).

-include("mac_layer.hrl").

-export([test_sys_clock/1]).
-export([send_mac/1, receive_mac/0]).
-export([send_and_wait_ack/1, receive_and_ack/0, receive_data/0]).
-export([test_receiver/0, test_sender/0, test_sender_ack/0, test_receiver_ack/0]).

% Callbacks
-export([start/2]).
-export([stop/1]).


%--- API -----------------------------------------------------------------------

send_and_wait_ack(Data) ->
    #{pan_id := SrcPAN, short_addr := SrcAddr} = pmod_uwb:read(panadr),

    FrameControl = #frame_control{ack_req = ?ENABLED},
    MacHeader = #mac_header{dest_pan = <<16#FFFF:16>>, dest_addr = <<16#FFFF:16>>, src_pan = <<SrcPAN:16>>, src_addr = <<SrcAddr:16>>},
    Mac = mac_layer:mac_message(FrameControl, MacHeader, Data),
    io:format("~w~n", [Mac]),

    pmod_uwb:write(sys_cfg, #{ffen => 2#1, ffaa => 2#1, autoack => 2#1}), % enable frame filtering and allow ACK frame reception and enable autoack
    pmod_uwb:transmit(Mac),

    io:format("Waiting for ACK~n"),
    {_Length, _Data} = pmod_uwb:reception(),
    io:format("Received ACK ?~n").

receive_and_ack() ->
    pmod_uwb:write(sys_cfg, #{ffad => 2#1, ffaa => 2#1, autoack => 2#1}), % allow ACK and data frame reception and enable autoack
    pmod_uwb:write(sys_cfg, #{ffen => 2#1}), % enable frame filtering and allow ACK frame reception and enable autoack
    receive_data().
    

receive_data() ->
    {Length, Data} = pmod_uwb:reception(),
    io:format("Received data with length: ~w~n", [Length]),
    binary_to_list(Data).


test_receiver() -> receive_data(100).

test_sender() -> send_data(0, 100).

test_receiver_ack() ->
    pmod_uwb:write(sys_cfg, #{ffad => 2#1, autoack => 2#1}), % allow ACK and data frame reception and enable autoack
    pmod_uwb:write(sys_cfg, #{ffen => 2#1}), % enable frame filtering and allow ACK frame reception and enable autoack
    receive_data(100).

test_sender_ack() ->
    #{short_addr := SrcAddr} = pmod_uwb:read(panadr),
    pmod_uwb:write(sys_cfg, #{ffaa => 2#1, autoack => 2#1}), % allow ACK and data frame reception and enable autoack
    pmod_uwb:write(sys_cfg, #{ffen => 2#1}), % enable frame filtering and allow ACK frame reception and enable autoack
    send_data_wait_ack(0, 100, SrcAddr).

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
receive_data(0) -> ok;
receive_data(N) ->
    Received = receive_data(),
    io:format("~w~n", [Received]),
    receive_data(N-1).

send_data(Max, Max) -> ok;
send_data(Cnt, Max) ->
    pmod_uwb:transmit(<<16#5C, Cnt, (list_to_binary("Data"))/bitstring>>),
    timer:sleep(50),
    send_data(Cnt+1, Max).

send_data_wait_ack(Max, Max, _) -> ok;
send_data_wait_ack(Cnt, Max, SrcAddr) ->
    FrameControl = #frame_control{ack_req = ?ENABLED, pan_id_compr = ?ENABLED},
    MacHeader = #mac_header{seqnum = Cnt, dest_pan = <<16#FFFF:16>>, dest_addr = <<16#FFFF:16>>, src_addr = <<SrcAddr:16>>},
    MacMessage = mac_layer:mac_message(FrameControl, MacHeader, <<"Data">>),
    io:format("Sending message #~w~n", [Cnt]),
    pmod_uwb:transmit(MacMessage),
    {_Length, _Data} = pmod_uwb:reception(),
    io:format("Received Ack~n"),
    timer:sleep(50),
    send_data_wait_ack(Cnt+1, Max, SrcAddr).


%--- Callbacks -----------------------------------------------------------------

% @private
start(_Type, _Args) -> 
    {ok, Supervisor} = robot_sup:start_link(),
    grisp:add_device(spi2, pmod_uwb),
    % Res = pmod_uwb:read(dev_id),
    {ok, Supervisor}.

% @private
stop(_State) -> ok.
