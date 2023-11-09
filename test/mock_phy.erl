-module(mock_phy).
-behaviour(gen_server).

-export([start_link/2]).
-export([stop_link/0]).

-export([transmit/2]).
-export([reception/0]).
-export([reception/1]).
-export([disable_rx/0]).

-export([read/1]).
-export([write/2]).

%%% gen_server callbacks
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).

-define(NAME, mock_phy).

% --- API -----------------------------------------

start_link(_Connector, {State, Params}) ->
    gen_server:start_link({local, ?NAME}, ?MODULE, {Params, State}, []).

stop_link() ->
    gen_server:stop(?NAME).

transmit(Data, Options) ->
    gen_server:call(?NAME, {transmit, Data, Options}).

reception() ->
    gen_server:call(?NAME, {reception}).

reception(_) ->
    gen_server:call(?NAME, {reception}).

read(Reg) ->
    gen_server:call(?NAME, {read, Reg}).

write(Reg, Val) ->
    gen_server:call(?NAME, {write, Reg, Val}).

disable_rx() ->
    gen_server:call(?NAME, {rx_off}).

%%% gen_server callbacks
init({_Params, State}) ->
    io:format("Mock phy created~n"),
    case State of
        perfect -> {ok, perfect};
        faulty -> {ok, faulty};
        lossy -> {ok, loss}
    end.

handle_call({transmit, Data, Options}, _From, State) -> {reply, tx(Data, Options), State};
handle_call({reception}, _From, perfect) -> {reply, rx(), perfect};
handle_call({reception}, _From, faulty) -> {reply, rx_faulty(), faulty};
handle_call({reception}, _From, loss) ->
    case rand:uniform(2) of
        1 -> {reply, rx_faulty(), loss};
        2 -> {reply, rx(), loss}
    end;
handle_call({read, _Reg, _Val}, _From, State) -> {reply, ok, State};
handle_call({write, _Reg, _Val}, _From, State) -> {reply, ok, State};
handle_call({rx_off}, _From, State) -> {reply, ok, State};
handle_call(_Request, _From, _State) -> error(not_implemented).

handle_cast(_Request, _State) ->
  error(not_implemented).


% --- Internal -----------------------------------------
tx(_Data, _Options) ->
    % TODO
    ok.

rx() ->
    {14, <<16#6188:16, 0:8, 16#CADE:16, "XR", "XT", "Hello">>}.
rx_faulty() ->
    timer:sleep(2000),
    rxpto.
