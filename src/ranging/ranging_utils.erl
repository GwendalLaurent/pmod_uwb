-module(ranging_utils).

-export([do_stats/1]).

%--- Macros --------------------------------------------------------------------


%--- API -----------------------------------------------------------------------
-spec do_stats([number(), ...]) -> ok.
do_stats(Measures) ->
    Nbr = length(Measures),
    Avg = lists:sum(Measures)/Nbr,
    Min = lists:min(Measures),
    Max = lists:max(Measures),
    StdDev = std_dev(Measures, Avg, Nbr, 0),
    io:format("--------------------------- Summary ---------------------------~n"),
    io:format("Average distance measured: ~w - standard deviation: ~w ~n", [Avg, StdDev]),
    io:format("Total: ~w - Min: ~w - Max ~w~n", [Nbr, Min, Max]),
    io:format("---------------------------------------------------------------~n"). 

%--- Internals -----------------------------------------------------------------
-spec std_dev(Measures, Mean, N, Acc) -> Result when
      Measures :: [number()],
      Mean     :: float(),
      N        :: non_neg_integer(),
      Acc      :: number(),
      Result   :: float().
std_dev([], _, N, Acc) ->
    math:sqrt(Acc/N);
std_dev([H | T], Mean, N, Acc) ->
    std_dev(T, Mean, N, Acc + math:pow(H-Mean, 2)).
