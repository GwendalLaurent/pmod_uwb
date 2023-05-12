-define(ENABLED, 2#1).
-define(DISABLED, 2#0).
-type flag() :: ?DISABLED| ?ENABLED.

-type miliseconds() :: integer().
-record(tx_opts, {wait4resp = ?DISABLED:: flag(), w4r_tim = 500 :: miliseconds(), txdlys = ?DISABLED:: flag(), tx_delay = 300 :: integer()}).

% map the r/w bit of the transaction header

-type writeOnly() :: tx_buffer.
-type readOnly() :: dev_id | sys_time | rx_finfo | rx_buffer | rx_fqual | rx_ttcki | rx_ttcko | rx_time | tx_time | sys_state | acc_mem.

