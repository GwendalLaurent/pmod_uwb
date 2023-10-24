-record(ieee_parameters, {mac_layer = mac_layer :: module(),
                          mac_parameters = #{} :: map(),
                          input_callback = fun(_Frame) -> ok end :: function()}).
