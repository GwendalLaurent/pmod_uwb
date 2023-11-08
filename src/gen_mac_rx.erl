-module(gen_mac_rx).

-callback init(Params::tuple()) -> State::term().
-callback rx_on(State::term(), Callback::function()) -> {ok, State::term()} | {error, State::term(), Error::atom()}.
-callback rx_off(State::term()) -> {ok, State::term()} | {error, State::term(), Error::atom()}.
-callback suspend(State::term()) -> {ok, State::term()} | {error, State::term(), Error::atom()}.
-callback resume(State::term()) -> {ok, State::term()} | {error, State::term(), Error::atom()}.
-callback terminate(State::term(), Reason::atom()) -> ok.

-export([start/2]).
-export([turn_on_rx/2]).
-export([turn_off_rx/1]).
-export([suspend_rx/1]).
-export([resume_rx/1]).
-export([stop/2]).

-export([rx/1]).
-export([rx_loop/2]).


% @doc This behaviour is responsible of the continuous rx and of the internal duty cycle/power optimization of the DW1000
% Idially, for each type of power optimization, there should be one implementation of this behaviour
% @end

-spec start(Module::module(), Params::map()) -> State::term().
start(Module, Params) ->
    {Module, Module:init(Params)}.

-spec turn_on_rx(State::term(), Callback::function()) -> {ok, State::term()} | {error, State::term(), Error::atom()}.
turn_on_rx({Mod, Sub}, Callback) ->
    case Mod:rx_on(Sub, Callback) of
        {ok, Sub2} -> {ok, {Mod, Sub2}};
        {error, Sub2, Err} -> {error, {Mod, Sub2}, Err} % Needed ? Can starting the rx loop throw an error ?
    end.

-spec turn_off_rx(State::term()) -> {ok, State::term()} | {error, State::term(), Error::atom()}.
turn_off_rx({Mod, Sub}) ->
    case Mod:rx_off(Sub) of
        {ok, Sub2} -> {ok, {Mod, Sub2}};
        {error, Sub2, Err} -> {error, {Mod, Sub2}, Err} % Needed ? Can starting the rx loop throw an error ?
    end.

-spec suspend_rx(State::term()) -> {ok, State::term()} | {error, State::term(), Error::atom()}.
suspend_rx({Mod, Sub}) ->
    case Mod:suspend(Sub) of
        {ok, Sub2} -> {ok, {Mod, Sub2}};
        {error, Sub2, Err} -> {error, {Mod, Sub2}, Err} % Needed ? Can starting the rx loop throw an error ?
    end.

-spec resume_rx(State::term()) -> {ok, State::term()} | {error, State::term(), Error::atom()}.
resume_rx({Mod, Sub}) ->
    case Mod:resume(Sub) of
        {ok, Sub2} -> {ok, {Mod, Sub2}};
        {error, Sub2, Err} -> {error, {Mod, Sub2}, Err} % Needed ? Can starting the rx loop throw an error ?
    end.

-spec stop(State::term(), Reason::atom()) -> ok.
stop({Mod, Sub}, Reason) ->
    Mod:terminate(Sub, Reason).

% Common RX functions to all behaviour implementations
-spec rx(Phy::module()) -> {ok, Frame::bitstring()} | {error, Error::atom()}. 
rx(Phy) ->
    case Phy:reception() of
        {_Length, Frame} -> {ok, Frame};
        Err -> {error, Err}
    end.

rx_loop(Phy, Callback) -> 
    case rx(Phy) of
        {ok, Frame} -> 
            Callback(Frame), 
            rx_loop(Phy, Callback);
        {error, _Err} -> rx_loop(Phy, Callback) % Log that an error occured ?
    end.
