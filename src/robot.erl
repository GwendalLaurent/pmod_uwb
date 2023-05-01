% @doc robot public API.
-module(robot).

-behavior(application).

-include("mac_layer.hrl").

-export([send_and_wait_ack/1, receive_and_ack/0, receive_data/0]).
-export([test_receiver/0, test_sender/0]).

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
    pmod_uwb:write(sys_cfg, #{ffad => 2#1, ffaa => 2#1, autoack => 2#1}), % enable frame filtering and allow ACK frame reception and enable autoack
    pmod_uwb:write(sys_cfg, #{ffen => 2#1}), % enable frame filtering and allow ACK frame reception and enable autoack
    receive_data().
    

receive_data() ->
    {Length, Data} = pmod_uwb:reception(),
    io:format("Received data with length: ~w~n", [Length]),
    binary_to_list(Data).


test_receiver() -> receive_data(100).

test_sender() -> send_data(0, 100).

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

%--- Callbacks -----------------------------------------------------------------

% @private
start(_Type, _Args) -> 
    {ok, Supervisor} = robot_sup:start_link(),
    grisp:add_device(spi2, pmod_uwb),
    % Res = pmod_uwb:read(dev_id),
    {ok, Supervisor}.

% @private
stop(_State) -> ok.
