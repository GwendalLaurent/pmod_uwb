%%% Careful, this is only pseudocode to help me getting everything in my head
%%% This should not be used like this in the code
%%% You should use the gen_server synthax 

-define(SPI_MODE, #{clock:={low, leading}}).

regFileID(dev_id) -> 16#00;
regFileID(eui) -> 16#01.

regFileSize(dev_id) -> 4.

% Format: {reg-file-id, r/w, size (in bits), [{sub-field-1, size (in bits), map/raw}]}]
reg(dev_id) -> {16#00, read, 24, [
    {rid_tag, 16, raw}, %16#DECA
    {model, 8, raw},
    {ver, 4, raw},
    {rev, 4, raw}]};
reg(eui) -> {16#01, read_write, 64, [
    {extension_identifier, 40, raw},
    {oui, 24, raw}]}.


rw(read) -> 0;
rw(write) -> 1.

% TODO:
% See when those values are used
% See how a value is written (pmod_nav and pmod_dio)
% See how a value is read and understood by the driver

% Create the header of a transaction
header(Op, RegFileID) -> 
    % <<(rw(Op)):1, 2#0:1, regFileID(RegFileID):6>>;
    <<(rw(Op)):1, 2#0:1, RegFileID:6>>;
header(Op, RegFileID, <<SubAddr:7>>) ->
    <<(rw(Op)):1, 2#1:1, RegFileID:6, 
        2#0:1, SubAddr:7>>;
header(Op, RegFileID, <<SubAddr1:7, SubAddr2:8>>) ->
    <<(rw(Op)):1, 2#1:1, RegFileID:6,
        2#1:1, SubAddr1:7,
        SubAddr2:8 >>.

%--- API -----------------------------------------------------------------------


read(Bus, RegFileID) ->
    Header = header(Op, RegFileID),
    grisp_spi:transfer(Bus, {?SPI_MODE, Header, 1, regFileLength(RegFileID)}).

write(Bus, RegFileID, Content) when byte_size(Content) == regFileSize(RegFileID) ->
    Header = header(Op, RegFileID),
    RegSize = regFileSize(RegFileID),
    Request = <<Header:1, Content:RegSize>>,
    grisp_spi:transfer(Bus, {?SPI_MODE, Request, 1+RegSize, 0}).