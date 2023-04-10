% map the r/w bit of the transaction header
rw(read) -> 0;
rw(write) -> 1.

-type registerId() :: dev_id | eui | panadr | panadr | sys_cfg | sys_time | tx_fctrl | tx_buffer | dx_time | rx_fwto | sys_ctrl | sys_mask | sys_status | rx_finfo.
-type writeOnly() :: tx_buffer.
-type readOnly() :: dev_id | sys_time | rx_finfo | rx_buffer | rx_fqual | rx_ttcki | rx_ttcko | rx_time | tx_time | sys_state | acc_mem.

-define(READ_ONLY_REG_FILE(RegFileID), RegFileID==dev_id; RegFileID==sys_time; RegFileID==rx_finfo; RegFileID==rx_buffer; RegFileID==rx_fqual; RegFileID==rx_ttcko;
                                 RegFileID==rx_time; RegFileID==tx_time; RegFileID==sys_state; RegFileID==acc_mem).

% -define(READ_ONLY_SUB_REG(SubRegister), )

% ! list isn't complete yet
-define(IS_SPECIAL(RegFileID), RegFileID==agc_ctrl).

% Mapping of the different register IDs to their hexadecimal value
regFile(dev_id) -> 16#00;
regFile(eui) -> 16#01;
regFile(panadr) -> 16#03;
regFile(sys_cfg) -> 16#04;
% 0x05 is reserved
regFile(sys_time) -> 16#06;
% 0x07 is reserved
regFile(tx_fctrl) -> 16#08;
regFile(tx_buffer) -> 16#09;
regFile(dx_time) -> 16#0A;
% 0x0B is reserved
regFile(rx_fwto) -> 16#0C;
regFile(sys_ctrl) -> 16#0D;
regFile(sys_mask) -> 16#0E;
regFile(sys_status) -> 16#0F;
regFile(rx_finfo) -> 16#10;
regFile(rx_buffer) -> 16#11;
regFile(rx_fqual) -> 16#12;
regFile(rx_ttcki) -> 16#13;
regFile(rx_ttcko) -> 16#14;
regFile(rx_time) -> 16#15;
% 0x16 is reserved
regFile(tx_time) -> 16#17;
regFile(tx_antd) -> 16#18;
regFile(sys_state) -> 16#19;
regFile(ack_resp_t) -> 16#1A;
% 0x1B is reserved
% 0x1C is reserved
regFile(rx_sniff) -> 16#1D;
regFile(tx_power) -> 16#1E;
regFile(chan_ctrl) -> 16#1F;
% 0x20 is reserved
regFile(usr_sfd) -> 16#21;
% 0x22 is reserved
regFile(agc_ctrl) -> 16#23;
regFile(ext_sync) -> 16#24;
regFile(acc_mem) -> 16#25;
regFile(gpio_ctrl) -> 16#26;
regFile(drx_conf) -> 16#27;
regFile(rf_conf) -> 16#28;
% 0x29 is reserved
regFile(tx_cal) -> 16#2A;
regFile(fs_ctrl) -> 16#2B;
regFile(aon) -> 16#2C;
regFile(otp_if) -> 16#2D;
regFile(lde_ctrl) -> 16#2E; % No size ?
regFile(dig_dag) -> 16#2F;
% 0x30 - 0x35 are reserved
regFile(pmsc) -> 16#36;
% 0x37 - 0x3F are reserved
regFile(RegId) -> error({wrong_register_ID, RegId}).

% AGC_CTRL
subReg(agc_ctrl1) -> 16#02;
subReg(agc_tune1) -> 16#04;
subReg(agc_tune2) -> 16#0C;
subReg(agc_tune3) -> 16#12;
subReg(agc_stat1) -> 16#1E;
subReg(drx_tune2) -> 16#08;
subReg(ldotune) -> 16#30.

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
regSize(drx_conf) -> 44; % user manual gives 44 bytes but sum of register length gives 45 bytes
regSize(rf_conf) -> 58;
regSize(tx_cal) -> 13; % user manual gives 52 bytes but sum of all sub regs gives 13 bytes
regSize(fs_ctrl) -> 21;
regSize(aon) -> 12;
regSize(otp_if) -> 19; % user manual gives 18 bytes in regs table but sum of all sub regs is 19 bytes
regSize(lde_ctrl) -> undefined; % No size ?
regSize(dig_dag) -> 38; % user manual gives 41 bytes but sum of all sub regs gives 38 bytes
regSize(pmsc) -> 41. % user manual gives 48 bytes but sum of all sub regs gives 41 bytes

%% Gives the size in bytes
subRegSize(agc_ctrl1) -> 2;
subRegSize(agc_tune1) -> 2;
subRegSize(agc_tune2) -> 4;
subRegSize(agc_tune3) -> 2;
subRegSize(agc_stat1) -> 3;
subRegSize(drx_tune2) -> 4;
subRegSize(ldotune) -> 5.
