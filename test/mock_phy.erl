-module(mock_phy).
-behaviour(gen_server).

-export([start_link/2]).
-export([start/2]).
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

start_link(_Connector, Params) ->
    gen_server:start_link({local, ?NAME}, ?MODULE, Params, []).

start(_Connector, Params) ->
    gen_server:start({local, ?NAME}, ?MODULE, Params, []).

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
init(_Params) ->
    {ok, #{regs => pmod_uwb_registers:default()}}.

handle_call({transmit, Data, Options}, _From, State) -> {reply, tx(Data, Options), State};
handle_call({reception}, _From, State) -> {reply, rx(), State};
handle_call({read, Reg}, _From, #{regs := Regs} = State) -> {reply, maps:get(Reg, Regs), State};
handle_call({write, Reg, Val}, _From, #{regs := Regs} = State) -> {reply, ok, State#{regs => pmod_uwb_registers:update_reg(Regs, Reg, Val)}};
handle_call({rx_off}, _From, State) -> {reply, ok, State};
handle_call(_Request, _From, _State) -> error(not_implemented).

handle_cast(_Request, _State) ->
  error(not_implemented).


% --- Internal -----------------------------------------
tx(_Data, _Options) ->
    ok.

rx() ->
    {14, <<16#6188:16, 0:8, 16#CADE:16, "XR", "XT", "Hello">>}.

rx_faulty() ->
    timer:sleep(2000),
    rxpto.
