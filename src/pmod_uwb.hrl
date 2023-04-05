% map the r/w bit of the transaction header
rw(read) -> 0;
rw(write) -> 1.

-type registerId() :: dev_id | eui | panadr | panadr | sys_cfg | sys_time | tx_fctrl | tx_buffer | dx_time | rx_fwto | sys_ctrl | sys_mask | sys_status | rx_finfo.
% Mapping of the different register IDs to their hexadecimal value
reg(dev_id) -> 16#00;
reg(eui) -> 16#01;
reg(panadr) -> 16#03;
reg(sys_cfg) -> 16#04;
% 0x05 is reserved
reg(sys_time) -> 16#06;
% 0x07 is reserved
reg(tx_fctrl) -> 16#08;
reg(tx_buffer) -> 16#09;
reg(dx_time) -> 16#0A;
% 0x0B is reserved
reg(rx_fwto) -> 16#0C;
reg(sys_ctrl) -> 16#0D;
reg(sys_mask) -> 16#0E;
reg(sys_status) -> 16#0F;
reg(rx_finfo) -> 16#10;
reg(rx_buffer) -> 16#11;
reg(rx_fqual) -> 16#12;
reg(rx_ttcki) -> 16#13;
reg(rx_ttcko) -> 16#14;
reg(rx_time) -> 16#15;
% 0x16 is reserved
reg(tx_time) -> 16#17;
reg(tx_antd) -> 16#18;
reg(sys_state) -> 16#19;
reg(ack_resp_t) -> 16#1A;
% 0x1B is reserved
% 0x1C is reserved
reg(rx_sniff) -> 16#1D;
reg(tx_power) -> 16#1E;
reg(chan_ctrl) -> 16#1F;
% 0x20 is reserved
reg(usr_sfd) -> 16#21;
% 0x22 is reserved
reg(agc_ctrl) -> 16#23;
reg(ext_sync) -> 16#24;
reg(acc_mem) -> 16#25;
reg(gpio_ctrl) -> 16#26;
reg(drx_conf) -> 16#27;
reg(rf_conf) -> 16#28;
% 0x29 is reserved
reg(tx_cal) -> 16#2A;
reg(fs_ctrl) -> 16#2B;
reg(aon) -> 16#2C;
reg(otp_if) -> 16#2D;
reg(lde_ctrl) -> 16#2E; % No size ?
reg(dig_dag) -> 16#2F;
% 0x30 - 0x35 are reserved
reg(pmsc) -> 16#36;
% 0x37 - 0x3F are reserved
reg(RegId) -> error({wrong_register_ID, RegId}).

% Mapping of the size in bytes of the different register IDs
regSize(dev_id) -> 4;
regSize(eui) -> 8;
regSize(panadr) -> 4;
regSize(sys_cfg) -> 4;
regSize(sys_time) -> 5;
regSize(tx_fctrl) -> 5;
regSize(tx_buffer) -> 1024;
regSize(dx_time) -> 5;
regSize(rx_fwto) -> 2; % user manual gives 2 bytes and bits 16-31 are reserved
regSize(sys_ctrl) -> 4;
regSize(sys_mask) -> 4;
regSize(sys_status) -> 5;
regSize(rx_finfo) -> 4;
regSize(rx_buffer) -> 1024;
regSize(rx_fqual) -> 8;
regSize(rx_ttcki) -> 4;
regSize(rx_ttcko) -> 5;
regSize(rx_time) -> 14;
regSize(tx_time) -> 10;
regSize(tx_antd) -> 2;
regSize(sys_state) -> 4;
regSize(ack_resp_t) -> 4;
regSize(rx_sniff) -> 4;
regSize(tx_power) -> 4;
regSize(chan_ctrl) -> 4;
regSize(usr_sfd) -> 41;
regSize(agc_ctrl) -> 33;
regSize(ext_sync) -> 12;
regSize(acc_mem) -> 4064;
regSize(gpio_ctrl) -> 44;
regSize(drx_conf) -> 45; % user manual gives 44 bytes but register description gives 45 bytes
regSize(rf_conf) -> 58;
regSize(tx_cal) -> 13; % user manual gives 52 bytes but sum of all sub regs gives 13 bytes
regSize(fs_ctrl) -> 21;
regSize(aon) -> 12;
regSize(otp_if) -> 19; % user manual gives 18 bytes in regs table but sum of all sub regs is 19 bytes
regSize(lde_ctrl) -> undefined; % No size ?
regSize(dig_dag) -> 38; % user manual gives 41 bytes but sum of all sub regs gives 38 bytes
regSize(pmsc) -> 41. % user manual gives 48 bytes but sum of all sub regs gives 41 bytes