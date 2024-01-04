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
