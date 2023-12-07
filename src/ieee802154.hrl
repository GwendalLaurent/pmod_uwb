%--- Types --------------------------------------------------------------------

%--- Record types
-record(ieee_parameters, {mac_layer = mac_layer :: module(),
                          mac_parameters = #{phy_layer => pmod_uwb, duty_cycle => duty_cycle_non_beacon} :: map(),
                          input_callback = fun(_Frame) -> ok end :: function()}).

%--- IEEE 802.15.4 parameter types
-type cw0() :: 1 | 2.
-type mac_max_BE() :: 3..8.
-type max_max_csma_backoff() :: 0..5.
-type mac_min_BE() :: 0..8.

