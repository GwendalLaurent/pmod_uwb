% @doc robot public API.
-module(robot).

-behavior(application).

-export([send_and_wait_ack/1, receive_and_ack/0, receive_data/0]).

% Callbacks
-export([start/2]).
-export([stop/1]).


%--- API -----------------------------------------------------------------------

send_and_wait_ack(Data) ->
    #{pan_id := SrcPAN, short_addr := SrcAddr} = pmod_uwb:read(panadr),
    Mac = mac_layer:mac_message(data, Data, 2#1, 0, 16#FFFF, 16#FFFF, SrcPAN, SrcAddr),
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

%--- Callbacks -----------------------------------------------------------------

% @private
start(_Type, _Args) -> 
    {ok, Supervisor} = robot_sup:start_link(),
    grisp:add_device(spi2, pmod_uwb),
    % Res = pmod_uwb:read(dev_id),
    {ok, Supervisor}.

% @private
stop(_State) -> ok.
