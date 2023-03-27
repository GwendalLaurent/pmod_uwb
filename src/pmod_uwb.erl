% @doc pmod_uwb public API.
-module(pmod_uwb).

-behavior(application).

% Callbacks
-export([start/2]).
-export([stop/1]).

%--- Callbacks -----------------------------------------------------------------

% @private
start(_Type, _Args) -> pmod_uwb_sup:start_link().

% @private
stop(_State) -> ok.
