-module(mock_phy).
-behaviour(gen_server).

-export([start_link/2]).
-export([transmit/2]).

%%% gen_server callbacks
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).

-define(NAME, mock_phy).

% --- API -----------------------------------------

start_link(_Connector, _Opts) ->
    gen_server:start_link({local, ?NAME}, ?MODULE, {}, []).

transmit(Data, Options) ->
    gen_server:call(?NAME, {transmit, Data, Options}).

%%% gen_server callbacks
init(_) ->
    io:format("Mock phy created~n"),
    {ok, #{}}.

handle_call({transmit, Data, Options}, _From, State) -> {reply, tx(Data, Options), State};
handle_call(_Request, _From, _State) -> error(not_implemented).

handle_cast(_Request, _State) ->
  error(not_implemented).


% --- Internal -----------------------------------------
tx(Data, Options) ->
    % TODO
    ok.
