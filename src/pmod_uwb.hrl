% map the r/w bit of the transaction header
rw(read) -> 0;
rw(write) -> 1.

% type isn't complete yet
-type registerId() :: dev_id | eui | panadr | panadr | sys_cfg | sys_time | tx_fctrl | tx_buffer | dx_time | rx_fwto | sys_ctrl | sys_mask | sys_status | rx_finfo.
-type writeOnly() :: tx_buffer.
-type readOnly() :: dev_id | sys_time | rx_finfo | rx_buffer | rx_fqual | rx_ttcki | rx_ttcko | rx_time | tx_time | sys_state | acc_mem.

-define(READ_ONLY_REG_FILE(RegFileID), RegFileID==dev_id; RegFileID==sys_time; RegFileID==rx_finfo; RegFileID==rx_buffer; RegFileID==rx_fqual; RegFileID==rx_ttcko;
                                       RegFileID==rx_time; RegFileID==tx_time; RegFileID==sys_state; RegFileID==acc_mem).

% ! list isn't complete yet
% The congifurations of the subregisters of these register files are different (some sub-registers are RO, some are RW and some have reserved bytes that can't be written)
% Thus, some registers files require to write their sub-register independently => Write the sub-registers one by one instead of writting the whole register file directly
-define(IS_SRW(RegFileID), RegFileID==agc_ctrl; RegFileID==ext_sync; RegFileID==ec_ctrl; RegFileID==gpio_ctrl; RegFileID==drx_conf; RegFileID==rf_conf; RegFileID==tx_cal; 
                           RegFileID==fs_ctrl; RegFileID==aon; RegFileID==otp_if; RegFileID==dig_dag; RegFileID==pmsc).

% ! list isn't complete yet
 -define(READ_ONLY_SUB_REG(SubRegister), SubRegister==irqs; SubRegister==agc_stat1; SubRegister==ec_rxtc; SubRegister==ec_glop; SubRegister==drx_car_int; 
                                         SubRegister==rf_status; SubRegister==tc_sarl; SubRegister==sarw; SubRegister==tc_pg_status; SubRegister==evc_phe; 
                                         SubRegister==evc_rse; SubRegister==evc_fcg; SubRegister==evc_fce; SubRegister==evc_ffr; SubRegister==evc_ovr; 
                                         SubRegister==evc_sto; SubRegister==evc_pto; SubRegister==evc_fwto; SubRegister==evc_txfs; SubRegister==evc_hpw; 
                                         SubRegister==evc_tpw).

% Mapping of the different register IDs to their hexadecimal value
regFile(dev_id) -> 16#00;
regFile(eui) -> 16#01;
% 0x02 is reserved
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
regFile(dig_diag) -> 16#2F;
% 0x30 - 0x35 are reserved
regFile(pmsc) -> 16#36;
% 0x37 - 0x3F are reserved
regFile(RegId) -> error({wrong_register_ID, RegId}).

% Only the writtable subregisters in SRW register files are present here
% AGC_CTRL
subReg(agc_ctrl1) -> 16#02;
subReg(agc_tune1) -> 16#04;
subReg(agc_tune2) -> 16#0C;
subReg(agc_tune3) -> 16#12;
subReg(agc_stat1) -> 16#1E;
subReg(ec_ctrl) -> 16#00;
subReg(gpio_mode) -> 16#00;
subReg(gpio_dir) -> 16#08;
subReg(gpio_dout) -> 16#0C;
subReg(gpio_irqe) -> 16#10;
subReg(gpio_isen) -> 16#14;
subReg(gpio_imode) -> 16#18;
subReg(gpio_ibes) -> 16#1C;
subReg(gpio_iclr) -> 16#20;
subReg(gpio_idbe) -> 16#24;
subReg(gpio_raw) -> 16#28;
subReg(drx_tune0b) -> 16#02;
subReg(drx_tune1a) -> 16#04;
subReg(drx_tune1b) -> 16#06;
subReg(drx_tune2) -> 16#08;
subReg(drx_sfdtoc) -> 16#20;
subReg(drx_pretoc) -> 16#24;
subReg(drx_tune4h) -> 16#26;
subReg(rf_conf) -> 16#00;
subReg(rf_rxctrlh) -> 16#0B;
subReg(rf_txctrl) -> 16#0C;
subReg(ldotune) -> 16#30;
subReg(tc_sarc) -> 16#00;
subReg(tc_pg_ctrl) -> 16#08;
subReg(tc_pgdelay) -> 16#0B;
subReg(tc_pgtest) -> 16#0C;
subReg(fs_pllcfg) -> 16#07;
subReg(fs_plltune) -> 16#0B;
subReg(fs_xtalt) -> 16#0E;
subReg(aon_wcfg) -> 16#00;
subReg(aon_ctrl) -> 16#02;
subReg(aon_rdat) -> 16#03;
subReg(aon_addr) -> 16#04;
subReg(aon_cfg0) -> 16#06;
subReg(aon_cfg1) -> 16#0A;
subReg(otp_wdat) -> 16#00;
subReg(otp_addr) -> 16#04;
subReg(otp_ctrl) -> 16#06;
subReg(otp_stat) -> 16#08;
subReg(otp_rdat) -> 16#0A;
subReg(otp_srdat) -> 16#0E;
subReg(otp_sf) -> 16#12;
subReg(evc_ctrl) -> 16#00;
subReg(diag_tmc) -> 16#24;
subReg(pmsc_ctrl0) -> 16#00;
subReg(pmsc_ctrl1) -> 16#04;
subReg(pmsc_snozt) -> 16#0C;
subReg(pmsc_txfseq) -> 16#26;
subReg(pmsc_ledc) -> 16#28.


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
regSize(rf_conf) -> 58; % user manual gives 58 but sum of all its register gives 53 => Placeholder for the remaining 8 bytes
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
subRegSize(ec_ctrl) -> 4;
subRegSize(gpio_mode) -> 4;
subRegSize(gpio_dir) -> 4;
subRegSize(gpio_dout) -> 4;
subRegSize(gpio_irqe) -> 4;
subRegSize(gpio_isen) -> 4;
subRegSize(gpio_imode) -> 4;
subRegSize(gpio_ibes) -> 4;
subRegSize(gpio_iclr) -> 4;
subRegSize(gpio_idbe) -> 4;
subRegSize(gpio_raw) -> 4;
subRegSize(drx_tune0b) -> 2;
subRegSize(drx_tune1a) -> 2;
subRegSize(drx_tune1b) -> 2;
subRegSize(drx_tune2) -> 4;
subRegSize(drx_sfdtoc) -> 2;
subRegSize(drx_pretoc) -> 2;
subRegSize(drx_tune4h) -> 2;
subRegSize(rf_conf) -> 4;
subRegSize(rf_rxctrlh) -> 1;
subRegSize(rf_txctrl) -> 4; % ! table in user manual gives 3 but details gives 4
subRegSize(ldotune) -> 5;
subRegSize(tc_sarc) -> 2;
subRegSize(tc_pg_ctrl) -> 1;
subRegSize(tc_pgdelay) -> 1;
subRegSize(tc_pgtest) -> 1;
subRegSize(fs_pllcfg) -> 4;
subRegSize(fs_plltune) -> 1;
subRegSize(fs_xtalt) -> 1;
subRegSize(aon_wcfg) -> 2;
subRegSize(aon_ctrl) -> 1;
subRegSize(aon_rdat) -> 1;
subRegSize(aon_addr) -> 1;
subRegSize(aon_cfg0) -> 4;
subRegSize(aon_cfg1) -> 2;
subRegSize(otp_wdat) -> 4;
subRegSize(otp_addr) -> 2;
subRegSize(otp_ctrl) -> 2;
subRegSize(otp_stat) -> 2;
subRegSize(otp_rdat) -> 4;
subRegSize(otp_srdat) -> 4;
subRegSize(otp_sf) -> 1;
subRegSize(evc_ctrl) -> 4;
subRegSize(diag_tmc) -> 2;
subRegSize(pmsc_ctrl0) -> 4;
subRegSize(pmsc_ctrl1) -> 4;
subRegSize(pmsc_snozt) -> 1;
subRegSize(pmsc_txfseq) -> 2;
subRegSize(pmsc_ledc) -> 4;
subRegSize(_) -> error({error}). % TODO: remove or make a better error
