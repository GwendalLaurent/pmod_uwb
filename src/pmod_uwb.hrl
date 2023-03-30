% map the r/w bit of the transaction header
rw(read) -> 0;
rw(write) -> 1.

% Mapping of the different register IDs to their hexadecimal value
reg(dev_id) -> 16#00;
reg(eui) -> 16#01;
reg(panadr) -> 16#03;
reg(sys_cfg) -> 16#04;
% 0x05 is reserved
reg(sys_time) -> 16#06;
% 0x07 is reserved
reg(tx_ctrl) -> 16#08;
reg(tx_buffer) -> 16#09.

% Mapping of the size in bytes of the different register IDs
regSize(dev_id) -> 4;
regSize(eui) -> 8;
regSize(panadr) -> 4;
regSize(sys_cfg) -> 4;
regSize(sys_time) -> 5;
regSize(tx_ctrl) -> 5;
regSize(tx_buffer) -> 1024.