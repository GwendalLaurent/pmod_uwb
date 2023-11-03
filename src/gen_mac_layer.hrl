-type tx_error() :: invalid_address | invalid_gts | transaction_overflow | transaction_expired | no_ack | frame_too_long | channel_access_failure.

-define(MACMAXFRAMERETRIES, 3).
-define(MACACKWAITDURATION, 4000).  % works with 2000 µs but calculations give me 4081µs
% -define(MACACKWAITDURATION, 2000).  % works with 2000 µs but calculations give me 4081µs
