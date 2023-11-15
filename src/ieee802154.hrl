-record(ieee_parameters, {mac_layer = mac_layer :: module(),
                          mac_parameters = #{phy_layer => pmod_uwb, duty_cycle => duty_cycle_non_beacon} :: map(),
                          input_callback = fun(_Frame) -> ok end :: function()}).
