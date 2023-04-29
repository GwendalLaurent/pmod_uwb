% @doc robot public API.
-module(robot).

-behavior(application).

-export([test_receiver/0, test_sender/0]).

% Callbacks
-export([start/2]).
-export([stop/1]).


%--- API -----------------------------------------------------------------------

test_receiver() -> receive_data(100).

test_sender() -> send_data(0, 100).

%--- Private -------------------------------------------------------------------
receive_data() ->
    {Length, Data} = pmod_uwb:reception(),
    io:format("Received data with length: ~w~n", [Length]),
    binary_to_list(Data).

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
