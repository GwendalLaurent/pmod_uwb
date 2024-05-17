-define(ENABLED, 2#1).
-define(DISABLED, 2#0).

% w4r_tim is the delay between the tx is done and the moment the rx will be enabled (it is not a timeout)
-record(tx_opts, {wait4resp = ?DISABLED:: flag(), w4r_tim = 0 :: miliseconds(), txdlys = ?DISABLED:: flag(), tx_delay = 300 :: integer(), ranging = ?DISABLED :: flag()}).

-type tx_opts() :: #tx_opts{}.

-type flag() :: ?DISABLED| ?ENABLED.

-type miliseconds() :: integer().
-type microseconds() :: integer().

% map the r/w bit of the transaction header

-type writeOnly() :: tx_buffer.
-type readOnly() :: dev_id | sys_time | rx_finfo | rx_buffer | rx_fqual | rx_ttcki | rx_ttcko | rx_time | tx_time | sys_state | acc_mem.

-type uwb_PRF() :: 0 | 4 | 16 | 64.
-type uwb_preamble_symbol_repetition() :: 0 | 16 | 64 | 1024 | 4096.
-type data_rate() :: 0..4. % Mapping defined in Std. at table 105 p. 206

%% @doc Supported channels by the DW1000
%% @end
-type phy_channel() :: 1 | 2 | 3 | 4 | 5 | 7.

%% @doc Supported preamble codes
%% The sequence 1..12 are the preamble codes for 16MHz PRF
%% The sequence 17..20 are the preamble codes for 64MHz PRF
%% For more informations see sec. 10.5
%% @end
-type preamble_code() ::  1..12 | 17..20.

-define(PCode,#{16 => #{1 => [1, 2],
                        2 => [3, 4],
                        3 => [5, 6],
                        4 => [7, 8],
                        5 => [3, 4],
                        7 => [7, 8]},
                 64 => # {1 => [9, 10, 11, 12],
                          2 => [9, 10, 11, 12],
                          3 => [9, 10, 11, 12],
                          4 => [17, 18, 19, 20],
                          5 => [9, 10, 11, 12],
                          7 => [17, 18, 19, 20]}
               }).

-define(PCode(Channel, PRF), maps:get(Channel, maps:get(PRF, ?PCode))).

-define(SCH(Channel) , if Channel =< 4 -> 1,3335;
                          true -> 1.0 end).
