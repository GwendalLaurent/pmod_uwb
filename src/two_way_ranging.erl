-module(two_way_ranging).

-include("mac_layer.hrl").

-export([initiator/0, responder/0]).

initiator() ->
    % Setup 
    pmod_uwb:write(tx_antd, #{tx_antd => 16436}), % ! this value is not correct - the devices should be calibrated 
    pmod_uwb:write(lde_if, #{lde_rxantd => 16436}),
    % TODO: can save energy by setting W4R_TIME (delay RX activation)
    % TODO: Set RX TO ? Already quite big rn

    % 2-way ranging
    FrameControl = #frame_control{pan_id_compr = ?ENABLED, dest_addr_mode = ?SHORT_ADDR, src_addr_mode = ?SHORT_ADDR},
    MacHeader = #mac_header{seqnum = 0, dest_pan = <<16#FFFF:16>>, dest_addr = <<16#FFFF:16>>, src_addr = <<16#FFFF:16>>},
    MacMessage = mac_layer:mac_message(FrameControl, MacHeader, <<"Wave">>),
    pmod_uwb:write_tx_data(MacMessage),
    pmod_uwb:write(sys_ctrl, #{txstrt => 2#1, wait4resp => 2#1}),
    {_Length, Data} = pmod_uwb:reception(),
    #{tx_stamp := PollTXTimestamp} = pmod_uwb:read(tx_time),
    #{rx_stamp := RespRXTimestamp} = pmod_uwb:read(rx_time),
    %{PollRXtimestamp, RespTXTimestamp} = get_resp_ts(Data),
    ok.


responder() -> ok.
