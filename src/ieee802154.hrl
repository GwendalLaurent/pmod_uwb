-record(ieee_parameters, {mac_layer = mac_layer :: module(),
                          mac_parameters = [pmod_uwb, duty_cycle_non_beacon] :: list(),
                          input_callback = fun(_Frame) -> ok end :: function()}).
