-module(pmod_uwb).
-behaviour(gen_server).


%% API
-export([start_link/2]).
-export([init/1, handle_call/3, handle_cast/2]).
-export([read/1, read/2, write/2, write_tx_data/1]).

% Define the polarity and the phase of the clock
-define(SPI_MODE, #{clock => {low, leading}}).

% Include for the record "device"
-include("grisp.hrl").

-include("pmod_uwb.hrl").


%--- API -----------------------------------------------------------------------
% TODO: Document the public API of the pmod

start_link(Connector, _Opts) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Connector, []).

% TODO: update the spec
-spec read(RegFileID :: registerId()) -> map() | {error, any()}.
read(RegFileID) -> call({read, RegFileID}).
read(RegFileID, SubRegister) -> call({read, RegFileID, SubRegister}).

%% ---------------------------------------------------------------------------------------
%% @doc Write values in a register
%% 
%% === Examples ===
%% To write in a simple register file (i.e. a register without any sub-register):
%% ```
%% 1> pmod_uwb:write(eui, #{eui => 16#AAAAAABBBBBBBBBB}).
%% ok
%% ''' 
%% To write in one sub-register of a register file:
%% ```
%% 2> pmod_uwb:write(panadr, #{pan_id => 16#AAAA}). 
%% ok
%% '''
%% The previous code will only change the values inside the sub-register PAN_ID
%%
%% To write in multiple sub-register of a register file in the same burst:
%% ```
%% 3> pmod_uwb:write(panadr, #{pan_id => 16#AAAA, short_addr => 16#BBBB}).
%% ok
%% '''
%% ---------------------------------------------------------------------------------------
write(RegFileID, Value) when ?READ_ONLY_REG_FILE(RegFileID) ->
    error({write_on_read_only_register, RegFileID, Value});
write(RegFileID, Value) when is_map(Value) ->
    call({write, RegFileID, Value}).

write_tx_data(Value) -> call({write_tx, Value}).

%--- Callbacks -----------------------------------------------------------------

init(Slot) ->
    % Verify the slot used
    case {grisp_hw:platform(), Slot} of
        {grisp2, spi2} -> ok;
        {P, S} -> error({incompatible_slot, P, S})
    end,
    grisp_devices:register(Slot, ?MODULE),
    Bus = grisp_spi:open(Slot),
    % Verify the dev_id
    case verify_id(Bus) of
        ok -> {ok, #{bus => Bus}};
        Val -> error({dev_id_no_match, Val})
    end.
    % TODO reset the DW1000 like in the code example

verify_id(Bus) ->
    #{ridtag := Val} = read_reg(Bus, dev_id),
    case Val of
        <<16#DE:8, 16#CA:8>> -> ok;
        _ -> Val
    end.

handle_call({read, RegFileID}, _From, #{bus := Bus} = State) -> {reply, read_reg(Bus, RegFileID), State};
handle_call({read, RegFileID, SubRegister}, _From, #{bus := Bus} = State) -> {reply, read_sub_reg(Bus, RegFileID, SubRegister), State};
handle_call({write, RegFileID, Value}, _From, #{bus := Bus} = State) -> {reply, write_reg(Bus, RegFileID, Value), State};
handle_call({write_tx, Value}, _From, #{bus := Bus} = State) -> {reply, write_tx_data(Bus, Value), State};
handle_call(Request, _From, _State) -> error({unknown_call, Request}).

handle_cast(Request, _State) -> error({unknown_cast, Request}).

%--- Internal ------------------------------------------------------------------

call(Call) ->
    Dev = grisp_devices:default(?MODULE),
    gen_server:call(Dev#device.pid, Call).

% Reverse the response of the pmod
% TODO: document this
reverse(Bin) -> reverse(Bin, <<>>).
reverse(<<Bin:8>>, Acc) -> 
    <<Bin, Acc/binary>>;
reverse(<<Bin:8, Rest/bitstring>>, Acc) -> 
    reverse(Rest, <<Bin, Acc/binary>>).

% Create the header of the operation
% Op: atom - either read or write
% RegFileID: atom - identifier of the register file id according to the data sheet
% returns the corresponding header in binary format (1 byte long) 
% TODO: document this
header(Op, RegFileID) ->
    <<(rw(Op)):1, 2#0:1, (regFile(RegFileID)):6>>.
header(Op, RegFileID, SubRegister) ->
    << (rw(Op)):1, 2#1:1, (regFile(RegFileID)):6,
        2#0:1, (subReg(SubRegister)):7 >>.

read_reg(Bus, tx_buffer) -> error({read_write_only, Bus, tx_buffer});
read_reg(Bus, RegFileID) ->
    Header = header(read, RegFileID),
    [Resp] = grisp_spi:transfer(Bus, [{?SPI_MODE, Header, 1, regSize(RegFileID)}]),
    debug_read(RegFileID, Resp),
    reg(decode, RegFileID, Resp).

read_sub_reg(Bus, RegFileID, SubRegister) ->
    Header = header(read, RegFileID, SubRegister),
    [Resp] = grisp_spi:transfer(Bus, [{?SPI_MODE, Header, 2, subRegSize(SubRegister)}]),
    debug_read(RegFileID, Resp),
    reverse(Resp).

% TODO: have a function that encodes the fields (e.g. be able to pass 'enable' as value and have automatic translation)
% TODO: check that user isn't trying to write reserved bits by passing res, res1, ... in the map fields
%% ---------------------------------------------------------------------------------------
%% write_reg/3 is used to write the values in the map given in the Value argument
%% ---------------------------------------------------------------------------------------
write_reg(Bus, RegFileID, Value) when ?IS_SPECIAL(RegFileID) ->
    maps:map(
        fun(SubRegister, Val) ->
            Header = header(write, RegFileID, SubRegister),
            io:format("~s~n", [atom_to_list(SubRegister)]),
            CurrVal = maps:get(SubRegister, read_reg(Bus, RegFileID)), % ? can the read be done before ? Maybe but not assured that no values changes after a write in the register
            io:format("~w~n", [CurrVal]),
            Body = case CurrVal of
                        V when is_map(V) -> reg(encode, SubRegister, maps:merge_with(fun(_Key, _Old, New) -> New end, CurrVal, Val));
                        _ -> reg(encode, SubRegister, #{SubRegister => Val})
                   end,
            debug_write(RegFileID, Body),
            _ = grisp_spi:transfer(Bus, [{?SPI_MODE, <<Header/binary, Body/binary>>, 2+subRegSize(SubRegister), 0}])
        end,
        Value),
    ok;
write_reg(Bus, RegFileID, Value) ->
    io:format("~w~n", [Value]),
    Header = header(write, RegFileID),
    CurrVal = read_reg(Bus, RegFileID),
    % TODO: Check that a field isn't a read only sub-register
    ValuesToWrite = maps:merge_with(fun(_Key, _Value1, Value2) -> Value2 end, CurrVal, Value),
    Body = reg(encode, RegFileID, ValuesToWrite),
    debug_write(RegFileID, Body),
    _ = grisp_spi:transfer(Bus, [{?SPI_MODE, <<Header/binary, Body/binary>>, 1+regSize(RegFileID), 0}]),
    ok.

%% ---------------------------------------------------------------------------------------
%% @doc This function is used to write values to single sub-registers if they are mapped
%% 
%% Some Register don't allow writting in the reserved fields and, 
%% thus the writting needs to be done sub-registers by sub-registers
%% 
%% Careful however, the sub-register must be mapped by itself
%% ---------------------------------------------------------------------------------------
write_sub_reg(Bus, RegFileID, SubRegister, Value) ->
    Header = header(write, RegFileID, SubRegister),
    Body = reg(encode, SubRegister, #{SubRegister => Value}),
    _ = grisp_spi:transfer(Bus, [{?SPI_MODE, <<Header/binary, Body/binary>>, 2+regSize(SubRegister), 0}]),
    ok.

%% ---------------------------------------------------------------------------------------
%% @doc write_tx_data/2 sends data (Value) in the register tx_buffer
%% @param Value is the data to be written. It must be a binary and have a size of maximum 1024 bits
%% ---------------------------------------------------------------------------------------
write_tx_data(Bus, Value) when is_binary(Value), (bit_size(Value) < 1025) ->
    Header = header(write, tx_buffer),
    Body = reverse(Value),
    Length = byte_size(Value),
    debug_write(tx_buffer, Body),
    _ = grisp_spi:transfer(Bus, [{?SPI_MODE, <<Header/binary, Body/binary>>, 1+Length, 0}]),
    ok.

%% ---------------------------------------------------------------------------------------
%% @doc Used to either decode the data returned by the pmod or to encode to data that will be sent to the pmod
%% 
%% The transmission on the MISO line is done byte by byte starting from the lowest rank byte to the highest rank
%% Example: dev_id value is 0xDECA0130 but 0x3001CADE is transmitted over the MISO line
%% ---------------------------------------------------------------------------------------
reg(decode, dev_id, Resp) -> 
    << 
        Ver:4/integer, Rev:4/integer, Model:8/integer, RIDTAG2:8, RIDTAG1:8
    >> = Resp,
    #{
        ridtag => <<RIDTAG1, RIDTAG2>>, model => Model, ver => Ver, rev => Rev
    };
reg(decode, eui, Resp) ->
    #{
        eui => reverse(Resp)
    };
reg(encode, eui, Val) ->
    #{
        eui:= EUI
    } = Val,
    reverse(<<
        EUI:64
    >>);
reg(decode, panadr, Resp) ->
    <<
        PanId:16, ShortAddr:16
    >> = reverse(Resp),
    #{
        panid => PanId, shortaddr => ShortAddr
    };
reg(encode, panadr, Val) ->
    #{
        panid := PanId, shortaddr := ShortAddr
    } = Val,
    reverse(<<
        PanId:16, ShortAddr:16
    >>);
reg(decode, sys_cfg, Resp) ->
    << 
        FFA4:1, FFAR:1, FFAM:1, FFAA:1, FFAD:1, FFAB:1, FFBC:1, FFEN:1, % bits 7-0
        FCS_INIT2F:1, DIS_RSDE:1, DIS_PHE:1, DIS_DRXB:1, DIS_FCE:1, SPI_EDGE:1, HIRQ_POL:1, FFA5:1, % bits 15-8
        _:1, RXM110K:1, _:3, DIS_STXP:1, PHR_MODE:2, % bits 23-16
        AACKPEND:1, AUTOACK:1, RXAUTR:1, RXWTOE:1, _:4 % bits 31-24
    >> = Resp,
    #{
        aackpend => AACKPEND, autoack => AUTOACK, rxautr => RXAUTR, rxwtoe => RXWTOE, 
        rxm110k => RXM110K, dis_stxp => DIS_STXP, phr_mode => PHR_MODE, 
        fcs_init2F => FCS_INIT2F, dis_rsde => DIS_RSDE, dis_phe => DIS_PHE, dis_drxb => DIS_DRXB, dis_fce => DIS_FCE, spi_edge => SPI_EDGE, hirq_pol => HIRQ_POL, ffa5 => FFA5,
        ffa4 => FFA4, ffar => FFAR, ffam => FFAM, ffaa => FFAA, ffad => FFAD, ffab => FFAB, ffbc => FFBC, ffen => FFEN
    };
reg(encode, sys_cfg, Val) ->
    #{
        aackpend := AACKPEND, autoack := AUTOACK, rxautr := RXAUTR, rxwtoe := RXWTOE, 
        rxm110k := RXM110K, dis_stxp := DIS_STXP, phr_mode := PHR_MODE, 
        fcs_init2F := FCS_INIT2F, dis_rsde := DIS_RSDE, dis_phe := DIS_PHE, dis_drxb := DIS_DRXB, dis_fce := DIS_FCE, spi_edge := SPI_EDGE, hirq_pol := HIRQ_POL, ffa5 := FFA5,
        ffa4 := FFA4, ffar := FFAR, ffam := FFAM, ffaa := FFAA, ffad := FFAD, ffab := FFAB, ffbc := FFBC, ffen := FFEN
    } = Val,
    <<
        FFA4:1, FFAR:1, FFAM:1, FFAA:1, FFAD:1, FFAB:1, FFBC:1, FFEN:1, % bits 7-0
        FCS_INIT2F:1, DIS_RSDE:1, DIS_PHE:1, DIS_DRXB:1, DIS_FCE:1, SPI_EDGE:1, HIRQ_POL:1, FFA5:1, % bits 15-8
        2#0:1, RXM110K:1, 2#0:3, DIS_STXP:1, PHR_MODE:2, % bits 23-16
        AACKPEND:1, AUTOACK:1, RXAUTR:1, RXWTOE:1, 2#0:4 % bits 31-24
    >>;
reg(decode, sys_time, Resp) -> 
    <<
        SysTime:40
    >> = reverse(Resp),
    #{
        sys_time => SysTime
    };
reg(decode, tx_fctrl, Resp) -> 
    <<
        IFSDELAY:8, TXBOFFS:10, PE:2, TXPSR:2, TXPRF:2, TR:1, TXBR:2, R:3, TFLE:3, TFLEN:7
    >> = reverse(Resp),
    #{
        ifsdelay => IFSDELAY, txboffs => TXBOFFS, pe => PE, txpsr => TXPSR, txprf => TXPRF, tr => TR, txbr => TXBR, r => R, tfle => TFLE, tflen => TFLEN
    };
reg(encode, tx_fctrl, Val) ->
    #{
        ifsdelay := IFSDELAY, txboffs := TXBOFFS, pe := PE, txpsr := TXPSR, txprf := TXPRF, tr := TR, txbr := TXBR, r := R, tfle := TFLE, tflen := TFLEN
    } = Val,
    reverse(<<
        IFSDELAY:8, TXBOFFS:10, PE:2, TXPSR:2, TXPRF:2, TR:1, TXBR:2, R:3, TFLE:3, TFLEN:7
    >>);
% TX_BUFFER is write only => no decode
reg(decode, dx_time, Resp) ->
    #{
        dx_time => reverse(Resp)
    };
reg(encode, tx_time, Val) ->
    #{
        dx_time := DX_TIME
    } = Val,
    reverse(<<
        DX_TIME:40
    >>);
reg(decode, rx_fwto, Resp) ->
    <<
        RXFWTO:16
    >> = reverse(Resp),
    #{
        rxfwto => RXFWTO
    };
reg(encode, rx_fwto, Val) ->
    #{
        rxfwto := RXFWTO
    } = Val,
    reverse(<<
        RXFWTO:16
    >>);
reg(decode, sys_ctrl, Resp) ->
    <<
        WAIT4RESP:1, TRXOFF:1, _:2, CANSFCS:1, TXDLYS:1, TXSTRT:1, SFCST:1, % bits 7-0
        _:6, RXDLYE:1, RXENAB:1, % bits 15-8
        _:8, % bits 23-16
        _:7, HRBPT:1 % bits 31-24
    >> = Resp,
    #{
        sfcst => SFCST, txstrt => TXSTRT, txdlys => TXDLYS, cansfcs => CANSFCS, trxoff => TRXOFF, wait4resp => WAIT4RESP, 
        rxenab => RXENAB, rxdlye => RXDLYE, 
        hrbpt => HRBPT
    };
reg(encode, sys_ctrl, Val) ->
    #{
        sfcst := SFCST, txstrt := TXSTRT, txdlys := TXDLYS, cansfcs := CANSFCS, trxoff := TRXOFF, wait4resp := WAIT4RESP, 
        rxenab := RXENAB, rxdlye := RXDLYE, 
        hrbpt := HRBPT
    } = Val,
    <<
        WAIT4RESP:1, TRXOFF:1, 2#0:2, CANSFCS:1, TXDLYS:1, TXSTRT:1, SFCST:1, % bits 7-0
        2#0:6, RXDLYE:1, RXENAB:1, % bits 15-8
        2#0:8, % bits 23-16
        2#0:7, HRBPT:1 % bits 31-24
    >>;
reg(decode, sys_mask, Resp) ->
    <<
        MTXFRS:1, MTXPHS:1, MTXPRS:1, MTXFRB:1, MAAT:1, MESYNCR:1, MCPLOCK:1, Reserved0:1, % bits 7-0
        MRXFCE:1, MRXFCG:1, MRXDFR:1, MRXPHE:1, MRXPHD:1, MLDEDON:1, MRXSFDD:1, MRXPRD:1, % bits 15-8
        MSLP2INIT:1, MGPIOIRQ:1, MRXPTO:1, MRXOVRR:1, Reserved1:1, MLDEERR:1, MRXRFTO:1, MRXRFSL:1, % bits 23-16
        Reserved2:2, MAFFREJ:1, MTXBERR:1, MHPDDWAR:1, MPLLHILO:1, MCPLLLL:1, MRFPLLLL:1 % bits 31-24
    >> = Resp,
    #{
        mtxfrs => MTXFRS, mtxphs => MTXPHS, mtxprs => MTXPRS, mtxfrb => MTXFRB, maat => MAAT, mesyncr => MESYNCR, mcplock => MCPLOCK, res0 => Reserved0, % bits 7-0
        mrxfce => MRXFCE, mrxfcg => MRXFCG, mrxdfr => MRXDFR, mrxphe => MRXPHE, mrxphd => MRXPHD, mldeon => MLDEDON, mrxsfdd => MRXSFDD, mrxprd => MRXPRD, % bits 15-8
        mslp2init => MSLP2INIT, mgpioirq => MGPIOIRQ, mrxpto => MRXPTO, mrxovrr => MRXOVRR, res1 => Reserved1, mldeerr => MLDEERR, mrxrfto => MRXRFTO, mrxrfsl => MRXRFSL, % bits 23-16
        res2 => Reserved2, maffrej => MAFFREJ, mtxberr => MTXBERR, mhpddwar => MHPDDWAR, mpllhilo => MPLLHILO, mcpllll => MCPLLLL, mrfpllll => MRFPLLLL % bits 31-24
    };
reg(encode, sys_mask, Val) ->
    #{
        mtxfrs := MTXFRS, mtxphs := MTXPHS, mtxprs := MTXPRS, mtxfrb := MTXFRB, maat := MAAT, mesyncr := MESYNCR, mcplock := MCPLOCK, res0 := Reserved0, % bits 7-0
        mrxfce := MRXFCE, mrxfcg := MRXFCG, mrxdfr := MRXDFR, mrxphe := MRXPHE, mrxphd := MRXPHD, mldeon := MLDEDON, mrxsfdd := MRXSFDD, mrxprd := MRXPRD, % bits 15-8
        mslp2init := MSLP2INIT, mgpioirq := MGPIOIRQ, mrxpto := MRXPTO, mrxovrr := MRXOVRR, res1 := Reserved1, mldeerr := MLDEERR, mrxrfto := MRXRFTO, mrxrfsl := MRXRFSL, % bits 23-16
        res2 := Reserved2, maffrej := MAFFREJ, mtxberr := MTXBERR, mhpddwar := MHPDDWAR, mpllhilo := MPLLHILO, mcpllll := MCPLLLL, mrfpllll := MRFPLLLL % bits 31-24
    } = Val,
    <<
        MTXFRS:1, MTXPHS:1, MTXPRS:1, MTXFRB:1, MAAT:1, MESYNCR:1, MCPLOCK:1, Reserved0:1, % bits 7-0
        MRXFCE:1, MRXFCG:1, MRXDFR:1, MRXPHE:1, MRXPHD:1, MLDEDON:1, MRXSFDD:1, MRXPRD:1, % bits 15-8
        MSLP2INIT:1, MGPIOIRQ:1, MRXPTO:1, MRXOVRR:1, Reserved1:1, MLDEERR:1, MRXRFTO:1, MRXRFSL:1, % bits 23-16
        Reserved2:2, MAFFREJ:1, MTXBERR:1, MHPDDWAR:1, MPLLHILO:1, MCPLLLL:1, MRFPLLLL:1 % bits 31-24
    >>;
reg(decode, sys_status, Resp) ->
    <<
        TXFRS:1, TXPHS:1, TXPRS:1, TXFRB:1, AAT:1, ESYNCR:1, CPLOCK:1, IRQS:1, % bits 7-0
        RXFCE:1, RXFCG:1, RXDFR:1, RXPHE:1, RXPHD:1, LDEDONE:1, RXSFDD:1, RXPRD:1, % bits 15-8
        SPL2INIT:1, GPIOIRQ:1, RXPTO:1, RXOVRR:1, Reserved0:1, LDEERR:1, RXRFTO:1, RXRFLS:1, % bits 23-16
        ICRBP:1, HSRBP:1, AFFREJ:1, TXBERR:1, HPDWARN:1, RXSFDTO:1, CLCKPLL_LL:1, RFPLL_LL:1, % bits 31-24
        Reserved1:5, TXPUTE:1, RXPREJ:1, RXRSCS:1 % bits 39-32
    >> = Resp,
    #{
        txfrs => TXFRS, txphs => TXPHS, txprs => TXPRS, txfrb => TXFRB, aat => AAT, esyncr => ESYNCR, cplock => CPLOCK, irqs => IRQS, % bits 7-0
        rxfce => RXFCE, rxfcg => RXFCG, rxdfr => RXDFR, rxhe => RXPHE, rxhd => RXPHD, ldeone => LDEDONE, rxsfdd => RXSFDD, rxprd => RXPRD, % bits 15-8
        splt2init => SPL2INIT, gpioirq => GPIOIRQ, rxpto => RXPTO, rxovrr => RXOVRR, res0 => Reserved0, ldeerr => LDEERR, rxrfto => RXRFTO, rxrfls => RXRFLS, % bits 23-16
        icrbp => ICRBP, hsrbp => HSRBP, affrej => AFFREJ, txberr => TXBERR, hdpwarn => HPDWARN, rxsfdto => RXSFDTO, clkpll_ll => CLCKPLL_LL, rfpll_ll => RFPLL_LL, % bits 31-24
        res1 => Reserved1, txpute => TXPUTE, rxprej => RXPREJ, rxrscs => RXRSCS
    };
reg(encode, sys_status, Val) ->
    #{
        txfrs := TXFRS, txphs := TXPHS, txprs := TXPRS, txfrb := TXFRB, aat := AAT, esyncr := ESYNCR, cplock := CPLOCK, irqs := IRQS, % bits 7-0
        rxfce := RXFCE, rxfcg := RXFCG, rxdfr := RXDFR, rxhe := RXPHE, rxhd := RXPHD, ldeone := LDEDONE, rxsfdd := RXSFDD, rxprd := RXPRD, % bits 15-8
        splt2init := SPL2INIT, gpioirq := GPIOIRQ, rxpto := RXPTO, rxovrr := RXOVRR, res0 := Reserved0, ldeerr := LDEERR, rxrfto := RXRFTO, rxrfls := RXRFLS, % bits 23-16
        icrbp := ICRBP, hsrbp := HSRBP, affrej := AFFREJ, txberr := TXBERR, hdpwarn := HPDWARN, rxsfdto := RXSFDTO, clkpll_ll := CLCKPLL_LL, rfpll_ll := RFPLL_LL, % bits 31-24
        res1 := Reserved1, txpute := TXPUTE, rxprej := RXPREJ, rxrscs := RXRSCS
    } = Val,
    <<
        TXFRS:1, TXPHS:1, TXPRS:1, TXFRB:1, AAT:1, ESYNCR:1, CPLOCK:1, IRQS:1, % bits 7-0
        RXFCE:1, RXFCG:1, RXDFR:1, RXPHE:1, RXPHD:1, LDEDONE:1, RXSFDD:1, RXPRD:1, % bits 15-8
        SPL2INIT:1, GPIOIRQ:1, RXPTO:1, RXOVRR:1, Reserved0:1, LDEERR:1, RXRFTO:1, RXRFLS:1, % bits 23-16
        ICRBP:1, HSRBP:1, AFFREJ:1, TXBERR:1, HPDWARN:1, RXSFDTO:1, CLCKPLL_LL:1, RFPLL_LL:1, % bits 31-24
        Reserved1:5, TXPUTE:1, RXPREJ:1, RXRSCS:1 % bits 39-32
    >>;
reg(decode, rx_finfo, Resp) ->
    <<
        RXPACC:12, RXPSR:2, RXPRFR:2, RNG:1, RXBR:2, RXNSPL:2, _:1, RXFLE:3, RXFLEN:7
    >> = reverse(Resp),
    #{
        rxpacc => RXPACC, rxpsr => RXPSR, rxprfr => RXPRFR, rng => RNG, rxbr => RXBR, rxnspl => RXNSPL, rxfle => RXFLE, rxflen => RXFLEN
    };
reg(decode, rx_buffer, Resp) ->
    #{ rx_buffer => reverse(Resp)};
reg(decode, rx_fqual, Resp) -> 
    <<
        CIR_PWR:16, PP_APL3:16, FP_AMPL2:16, STD_NOISE:16
    >> = Resp,
    #{
        cir_pwr => CIR_PWR, pp_apl3 => PP_APL3, fp_ampl2 => FP_AMPL2, std_noise => STD_NOISE
    };
reg(decode, rx_ttcki, Resp) -> 
    #{
        rx_ttcki => reverse(Resp)
    };
reg(decode, rx_ttcko, Resp) ->
    <<
        _:1, RCPHASE:7, RSMPDEL:8, _:5, RXTOFS:19 
    >> = reverse(Resp),
    #{
        rcphase => RCPHASE, rsmpdel => RSMPDEL, rxtofs => RXTOFS
    };
reg(decode, rx_time, Resp) ->
    <<
        RX_RAWST:40, FP_AMPL1:16, FP_INDEX:16, RX_STAMP:40
    >> = reverse(Resp),
    #{ 
        rx_rawst => RX_RAWST, fp_ampl1 => FP_AMPL1, fp_index => FP_INDEX, rx_stamp => RX_STAMP
    };
reg(decode, tx_time, Resp) -> 
    <<
        TX_RAWST:40, TX_STAMP:40
    >> = reverse(Resp),
    #{
        tx_rawst => TX_RAWST, tx_stamp => TX_STAMP
    };
reg(decode, tx_antd, Resp) ->
    #{
        tx_antd => reverse(Resp)
    };
reg(encode, tx_antd, Val) ->
    #{
        tx_antd := TX_ANTD
    } = Val,
    reverse(<<
        TX_ANTD:16
    >>);
reg(decode, sys_state, Resp) ->
    <<
        _:8, PMSC_STATE:8, _:3, RX_STATE:5, _:4, TX_STATE:4
    >> = reverse(Resp),
    #{
        pmsc_state => PMSC_STATE, rx_state => RX_STATE, tx_state => TX_STATE
    };
reg(decode, ack_resp_t, Resp) ->
    <<
        ACK_TIME:8, _:4, W4R_TIME:20
    >> = reverse(Resp),
    #{
        ack_time => ACK_TIME, w4r_time => W4R_TIME
    };
reg(encode, ack_resp_t, Val) ->
    #{
        ack_time := ACK_TIME, w4r_time := W4R_TIME
    } = Val,
    reverse(<<
        ACK_TIME:8, 2#0:4, W4R_TIME:20
    >>);
reg(decode, rx_sniff, Resp) ->
    <<
        Reserved0:16, SNIFF_OFFT:8, Reserved1:4, SNIFF_ONT:4
    >> = reverse(Resp),
    #{
        res0 => Reserved0,
        sniff_offt => SNIFF_OFFT,
        sniff_ont => SNIFF_ONT,
        res1 => Reserved1
    };
reg(encode, rx_sniff, Val) ->
    #{
        res0 := Reserved0,
        sniff_offt := SNIFF_OFFT,
        sniff_ont := SNIFF_ONT,
        res1 := Reserved1
    } = Val,
    reverse(<<
        Reserved0:16, SNIFF_OFFT:8, Reserved1:4, SNIFF_ONT:4
    >>);
% Smart transmit power control (cf. user manual p 104)
reg(decode, tx_power, Resp) ->
    <<
        BOOTSP125:8, BOOSTP250:8, BOOSTP500:8, BOOSTPNORM:8
    >> = reverse(Resp),
    #{
        bootsp125 => BOOTSP125, bootstp250 => BOOSTP250, bootstp500 => BOOSTP500, bootstpnorm => BOOSTPNORM
    };
reg(encode, tx_power, Val) ->
    #{
        bootsp125 := BOOTSP125, bootstp250 := BOOSTP250, bootstp500 := BOOSTP500, bootstpnorm := BOOSTPNORM
    } = Val,
    reverse(<<
        BOOTSP125:8, BOOSTP250:8, BOOSTP500:8, BOOSTPNORM:8
    >>);
reg(decode, chan_ctrl, Resp) ->
    <<
        RX_PCODE:5, TX_PCODE:5, RNSSFD:1, TNSSFD:1, RXPRF:2, DWSFD:1, Reserved0:9, RX_CHAN:4, TX_CHAN:4 
    >> = reverse(Resp),
    #{
        rx_pcode => RX_PCODE, tx_pcode => TX_PCODE, rnssfd => RNSSFD, tnssfd => TNSSFD, rxprf => RXPRF, dwsfd => DWSFD, res0 => Reserved0, rx_chan => RX_CHAN, tx_chan => TX_CHAN
    };
reg(encode, chan_ctrl, Val) ->
    #{
        rx_pcode := RX_PCODE, tx_pcode := TX_PCODE, rnssfd := RNSSFD, tnssfd := TNSSFD, rxprf := RXPRF, dwsfd := DWSFD, res0 := Reserved0, rx_chan := RX_CHAN, tx_chan := TX_CHAN
    } = Val,
    reverse(<<
        RX_PCODE:5, TX_PCODE:5, RNSSFD:1, TNSSFD:1, RXPRF:2, DWSFD:1, Reserved0:9, RX_CHAN:4, TX_CHAN:4 
    >>);
% ! register names differ from user manual where some names are duplicated
% TODO: Decode that register
reg(decode, usr_sfd, Resp) ->
    <<
        _:(8*41)
    >> = Resp,
    #{};
% AGC_CTRL is a complex register with reserved bits that can't be written
reg(decode, agc_ctrl1, Resp) ->
    <<
        Reserved:15, DIS_AM:1
    >> = reverse(Resp),
    #{
        res => Reserved, dis_am => DIS_AM
    };
reg(encode, agc_ctrl1, Val) ->
    #{
        res := Reserved, dis_am := DIS_AM
    } = Val,
    reverse(<<
        Reserved:15, DIS_AM:1
    >>);
reg(decode, agc_tune1, Resp) -> 
    <<
        AGC_TUNE1:16
    >> = reverse(Resp),
    #{
        agc_tune1 => AGC_TUNE1
    };
reg(encode, agc_tune1, Val) -> 
    #{
      agc_tune1 := AGC_TUNE1
     } = Val,
    reverse(<<
        AGC_TUNE1:16
    >>);
reg(decode, agc_tune2, Resp) ->
    <<
        AGC_TUNE2:32
    >> = reverse(Resp),
    #{
        agc_tune2 => AGC_TUNE2
    };
reg(decode, agc_tune3, Resp) ->
    <<
        AGC_TUNE3:16
    >> = reverse(Resp),
    #{
        agc_tune3 => AGC_TUNE3
    };
reg(decode, agc_stat1, Resp) ->
    <<
        Reserved0:4, EDV2:9, EDG1:5, Reserved1:6
    >> = reverse(Resp),
    #{
        res0 => Reserved0, edv2 => EDV2, edg1 => EDG1, res1 => Reserved1
    };
reg(decode, agc_ctrl, Resp) ->
    <<
        _:16, AGC_CTRL1:16/bitstring, AGC_TUNE1:16/bitstring, _:(6*8), AGC_TUNE2:32/bitstring, _:16, AGC_TUNE3:16/bitstring, _:80, AGC_STAT1:24/bitstring
    >> = Resp,
    lists:foldl(fun(Map1, Map2) -> maps:merge(Map1, Map2) end, #{}, 
        [
            #{agc_ctrl1 => reg(decode, agc_ctrl1, AGC_CTRL1)},
            reg(decode, agc_tune1, AGC_TUNE1),
            reg(decode, agc_tune2, AGC_TUNE2),
            reg(decode, agc_tune3, AGC_TUNE3),
            #{agc_stat1 => reg(decode, agc_stat1, AGC_STAT1)}
        ]);
reg(decode, ext_sync, Resp) ->
    <<
        _:26, OFFSET_EXT:6, % EC_GLOP        
        RX_TS_EST:32, % EC_RXTC
        _:20, OSTRM:1, WAIT:8, PLLLDT:1, OSRSM:1, OSRSM:1 % EC_CTRL
    >> = reverse(Resp),
    #{
        ostrm => OSTRM, wait => WAIT, pllldt => PLLLDT, osrsm => OSRSM, rx_ts_est => RX_TS_EST, offset_ext => OFFSET_EXT
    };
% "The host system doesn't need to access the ACC_MEM in normal operation, however it may be of interest [...] for diagnostic purpose" (from DW1000 user manual)
reg(decode, acc_mem, Resp) -> 
    #{
        acc_mem => reverse(Resp)
    };
% TODO: Maybe decode each subregister in a separate function and merge the maps
reg(decode, gpio_ctrl, Resp) -> 
    <<
        _:23, GRAWP8:1, GRAWP7:1, GRAWP6:1, GRAWP5:1, GRAWP4:1, GRAWP3:1, GRAWP2:1, GRAWP1:1, GRAWP0:1, % GPIO_RAW        
        _:23, GIDBE8:1, GIDBE7:1, GIDBE6:1, GIDBE5:1, GIDBE4:1, GIDBE3:1, GIDBE2:1, GIDBE1:1, GIDBE0:1, % GPIO_IDBE
        _:23, GICLR8:1, GICLR7:1, GICLR6:1, GICLR5:1, GICLR4:1, GICLR3:1, GICLR2:1, GICLR1:1, GICLR0:1, % GPIO_ICLR
        _:23, GIBES8:1, GIBES7:1, GIBES6:1, GIBES5:1, GIBES4:1, GIBES3:1, GIBES2:1, GIBES1:1, GIBES0:1, % GPIO_IBES
        _:23, GIMOD8:1, GIMOD7:1, GIMOD6:1, GIMOD5:1, GIMOD4:1, GIMOD3:1, GIMOD2:1, GIMOD1:1, GIMOD0:1, % GPIO_IMOD
        _:23, GISEN8:1, GISEN7:1, GISEN6:1, GISEN5:1, GISEN4:1, GISEN3:1, GISEN2:1, GISEN1:1, GISEN0:1, % GPIO_ISEN
        _:23, GIRQE8:1, GIRQE7:1, GIRQE6:1, GIRQE5:1, GIRQE4:1, GIRQE3:1, GIRQE2:1, GIRQE1:1, GIRQE0:1, % GPIO_IRQE
        _:11, GOM8:1, _:3, GOP8:1, GOM7:1, GOM6:1, GOM5:1, GOM4:1, GOP7:1, GOP6:1, GOP5:1, GOP4:1, GOM3:1, GOM2:1, GOM1:1, GOM0:1, GOP3:1, GOP2:1, GOP1:1, GOP0:1, % GPIO_DOUT
        _:11, GDM8:1, _:3, GDP8:1, GDM7:1, GDM6:1, GDM5:1, GDM4:1, GDP7:1, GDP6:1, GDP5:1, GDP4:1, GDM3:1, GDM2:1, GDM1:1, GDM0:1, GDP3:1, GDP2:1, GDP1:1, GDP0:1, % GPIO_DIR
        _:32, % Reserved
        _:8, MSGP8:2, MSGP7:2, MSGP6:2, MSGP5:2, MSGP4:2, MSGP3:2, MSGP2:2, MSGP1:2, MSGP0:2, _:6 % GPIO_MODE
    >> = reverse(Resp),
    #{
        msgp8 => MSGP8, msgp7 => MSGP7, msgp6 => MSGP6, msgp5 => MSGP5, msgp4 => MSGP4, msgp3 => MSGP3, msgp2 => MSGP2, msgp1 => MSGP1, msgp0 => MSGP0,
        gdm8 => GDM8, gdm7 => GDM7, gdm6 => GDM6, gdm5 => GDM5, gdm4 => GDM4, gdm3 => GDM3, gdm2 => GDM2, gdm1 => GDM1, gdm0 => GDM0,
        gdp8 => GDP8, gdp7 => GDP7, gdp6 => GDP6, gdp5 => GDP5, gdp4 => GDP4, gdp3 => GDP3, gdp2 => GDP2, gdp1 => GDP1, gdp0 => GDP0,
        gom8 => GOM8, gom7 => GOM7, gom6 => GOM6, gom5 => GOM5, gom4 => GOM4, gom3 => GOM3, gom2 => GOM2, gom1 => GOM1, gom0 => GOM0,
        gop8 => GOP8, gop7 => GOP7, gop6 => GOP6, gop5 => GOP5, gop4 => GOP4, gop3 => GOP3, gop2 => GOP2, gop1 => GOP1, gop0 => GOP0,
        girqe8 => GIRQE8, girqe7 => GIRQE7, girqe6 => GIRQE6, girqe5 => GIRQE5, girqe4 => GIRQE4, girqe3 => GIRQE3, girqe2 => GIRQE2, girqe1 => GIRQE1, girqe0 => GIRQE0,
        gisen8 => GISEN8, gisen7 => GISEN7, gisen6 => GISEN6, gisen5 => GISEN5, gisen4 => GISEN4, gisen3 => GISEN3, gisen2 => GISEN2, gisen1 => GISEN1, gisen0 => GISEN0,
        gimod8 => GIMOD8, gimod7 => GIMOD7, gimod6 => GIMOD6, gimod5 => GIMOD5, gimod4 => GIMOD4, gimod3 => GIMOD3, gimod2 => GIMOD2, gimod1 => GIMOD1, gimod0 => GIMOD0,
        gibes8 => GIBES8, gibes7 => GIBES7, gibes6 => GIBES6, gibes5 => GIBES5, gibes4 => GIBES4, gibes3 => GIBES3, gibes2 => GIBES2, gibes1 => GIBES1, gibes0 => GIBES0,
        giclr8 => GICLR8, giclr7 => GICLR7, giclr6 => GICLR6, giclr5 => GICLR5, giclr4 => GICLR4, giclr3 => GICLR3, giclr2 => GICLR2, giclr1 => GICLR1, giclr0 => GICLR0,
        gidbe8 => GIDBE8, gidbe7 => GIDBE7, gidbe6 => GIDBE6, gidbe5 => GIDBE5, gidbe4 => GIDBE4, gidbe3 => GIDBE3, gidbe2 => GIDBE2, gidbe1 => GIDBE1, gidbe0 => GIDBE0,
        grawp8 => GRAWP8, grawp7 => GRAWP7, grawp6 => GRAWP6, grawp5 => GRAWP5, grawp4 => GRAWP4, grawp3 => GRAWP3, grawp2 => GRAWP2, grawp1 => GRAWP1, grawp0 => GRAWP0
    };
reg(decode, drx_conf, Resp) ->
    <<
        %RXPACC_NOSAT:8, % present in the user manual but not in the driver code in C
        _:8, % Placeholder for the remaining 8 bits
        DRX_CAR_INIT:24,
        DRX_TUNE4H:16,
        DRX_PRETOC:16,
        _:16,
        DRX_SFDTOC:16,
        _:160,
        DRX_TUNE2:32,
        DRX_TUNE1b:16,
        DRX_TUNE1a:16,
        DRX_TUNE0b:16,
        _:16
    >> = reverse(Resp),
    #{
        drx_tune0b => DRX_TUNE0b,
        drx_tune1a => DRX_TUNE1a,
        drx_tune1b => DRX_TUNE1b,
        drx_tune2 => DRX_TUNE2,
        drx_tune4h => DRX_TUNE4H,
        drx_car_init => DRX_CAR_INIT,
        drx_sfdtoc => DRX_SFDTOC,
        drx_pretoc => DRX_PRETOC %,
        % rxpacc_nosat => RXPACC_NOSAT
    };
reg(decode, rf_conf, Resp) ->
    <<  
        Placeholder:40, % Placeholder for the remaining 48 bits
        LDOTUNE:40, % LDOTUNE
        _:28, RFPLLLOCK:1, CPLLHIGH:1, CPLLLOW:1, CPLLLOCK:1, % RF_STATUS
        _:128, _:96, % Reserved 2
        Reserved:20, TXMQ:3, TXMTUNE:4, _:5, % RF_TXCTRL
        RF_RXCTRLH:8, % RF_RXCTRLH
        _:56, % Reserved 1
        _:9, TXRXSW:2, LDOFEN:5, PLLFEN:3, TXFEN:5, _:8 % RF_CONF
    >> = reverse(Resp),
    #{
        placeholder => Placeholder,
        ldotune => LDOTUNE,
        rfplllock => RFPLLLOCK, cplllow => CPLLLOW, cpllhigh => CPLLHIGH, cplllock => CPLLLOCK,
        reserved => Reserved, txmq => TXMQ, txmtune => TXMTUNE,
        rf_rxctrlh => RF_RXCTRLH,
        txrxsw => TXRXSW, ldofen => LDOFEN, pllfen => PLLFEN, txfen => TXFEN
    };
reg(decode, tx_cal, Resp) -> 
    <<
        TC_PGTEST:8, % TC_PGTEST
        TC_PGDELAY:8, % TC_PGDELAY
        _:4, DELAY_CNT:12, % TC_PG_STATUS
        _:2, PG_TMEAS:4, _:1, PG_START:1, % TC_PG_CTRL
        SAR_WTEMP:8, SAR_WVBAT:8, % TC_SARW
        _:8, SAR_LTEMP:8, SAR_LVBAT:8, % TC_SARL
        _:15, SAR_CTRL:1 % SAR_CTRL
    >> = reverse(Resp),
    #{
        tc_pgtest => TC_PGTEST,
        tc_pgdelay => TC_PGDELAY,
        delay_cnt => DELAY_CNT,
        pg_tmeas => PG_TMEAS, pg_start => PG_START,
        sar_wtemp => SAR_WTEMP, sar_wvbat => SAR_WVBAT,
        sar_ltemp => SAR_LTEMP, sar_lvbat => SAR_LVBAT,
        sar_ctrl => SAR_CTRL
    };
reg(decode, fs_ctrl, Resp) -> 
    <<
        _:48, % Reserved 3
        Reserved:3, XTALT:5, % FS_XTALT
        _:16, % Reserved 2
        FS_PLLTUNE:8, % FS_PLLTUNE
        FS_PLLCFG:32, % FS_PLLCFG
        _:56 % Reserved 1
    >> = reverse(Resp),
    #{
        reserved_fs_xtalt => Reserved, xtalt => XTALT,
        fs_plltune => FS_PLLTUNE, % ! FIXME: read value isn't correct
        fs_pllcfg => FS_PLLCFG
    };
reg(decode, aon, Resp) ->
    <<
        _:13, LPOSC_C:1, SMXX:1, SLEEP_CE:1, % AON_CFG1
        SLEEP_TIM:16, LPCLKDIVA:11, LPDIV_EN:1, WAKE_CNT:1, WAKE_SPI:1, WAKE_PIN:1, SLEEP_EN:1, % AON_CFG0
        _:8, % Reserved 1
        AON_ADDR:8, % AON_ADDR
        AON_RDAT:8, % AON_RDAT
        DCA_ENAB:1, _:3, DCA_READ:1, UPL_CFG:1, SAVE:1, RESTORE:1, % AON_CTRL
        _:3, ONW_LLD:1, ONW_LLDE:1, _:2, PRES_SLEE:1, OWN_L64:1, OWN_LDC:1, _:2, OWN_LEUI:1, _:1, OWN_RX:1, OWN_RAD:1 % AON_WCFG
    >> = reverse(Resp),
    #{
        lposc_c => LPOSC_C, smxx => SMXX, sleep_ce => SLEEP_CE,
        sleep_tim => SLEEP_TIM, lpclkdiva => LPCLKDIVA, lpdiv_en => LPDIV_EN, wake_cnt => WAKE_CNT, wake_spi => WAKE_SPI, wake_pin => WAKE_PIN, sleep_en => SLEEP_EN,
        aon_addr => AON_ADDR,
        aon_rdat => AON_RDAT,
        dca_enab => DCA_ENAB, dca_read => DCA_READ, upl_cfg => UPL_CFG, save => SAVE, restore => RESTORE,
        onw_lld => ONW_LLD, onw_llde => ONW_LLDE, pres_slee => PRES_SLEE, own_l64 => OWN_L64, own_ldc => OWN_LDC, own_leui => OWN_LEUI, own_rx => OWN_RX, own_rad => OWN_RAD
    };
reg(decode, otp_if, Resp) ->
    <<
        _:2, OPS_SEL:1, _:3, LDO_KICK:1, OPS_KICK:1, % OTP_SF
        OTP_SRDAT:32, % OTP_SRDAT
        OTP_RDAT:32, % OTP_RDAT
        _:14, OTP_VPOK:1, OTPPRGD:1, % OTP_STAT
        LDELOAD:1, _:4, OTPMR:4, OTPPROG:1, _:2, OTPMRWR:1, _:1, OTPREAD:1, OTPRDEN:1, % OTP_CTRL
        _:5, OTP_ADDR:11, % OTP_ADDR
        OTP_WDAT:32 % OTP_WDAT
    >> = reverse(Resp),
    #{
        ops_sel => OPS_SEL, ldo_kick => LDO_KICK, ops_kick => OPS_KICK,
        otp_srdat => OTP_SRDAT,
        otp_rdat => OTP_RDAT,
        otp_vpok => OTP_VPOK, otpprgd => OTPPRGD,
        ldeload => LDELOAD, otpmr => OTPMR, otpprog => OTPPROG, otpmrwr => OTPMRWR, otpread => OTPREAD, otp_rden => OTPRDEN,
        otp_addr => OTP_ADDR,
        otp_wdat => OTP_WDAT
    };
% TODO: decode lde_if (a bit special with lots of offsets) + mnemonic not consistent within the user manual
% reg(decode, lde_if, Resp) -> 
reg(decode, dig_dag, Resp) -> 
    <<
        _:11, TX_PSTM:1, _:4, % DIAG_TMC
        _:64, % Reserved 1
        _:4, EVC_TPW:12, % EVC_TPW
        _:4, EVC_HPW:12, % EVC_HPW
        _:4, EVC_TXFS:12, % EVC_TXFS
        _:4, EVC_FWTO:12, % EVC_FWTO
        _:4, EVC_PTO:12, % EVC_PTO
        _:4, EVC_STO:12, % EVC_STO
        _:4, ECV_OVR:12, % EVC_OVR
        _:4, EVC_FFR:12, % EVC_FFR
        _:4, EVC_FCE:12, % EVC_FCE
        _:4, EVC_FCG:12, % EVC_FCG
        _:4, EVC_RSE:12, % EVC_RSE
        _:4, EVC_PHE:12, % EVC_PHE
        _:30, EVC_CLR:1, EVC_EN:1 % EVC_CTRL
    >> = reverse(Resp),
    #{
        tx_pstm => TX_PSTM,
        evc_tpw => EVC_TPW,
        evc_hpw => EVC_HPW,
        evc_txfs => EVC_TXFS,
        evc_fwto => EVC_FWTO,
        evc_pto => EVC_PTO,
        evc_sto => EVC_STO,
        evc_ovr => ECV_OVR,
        evc_ffr => EVC_FFR,
        evc_fce => EVC_FCE,
        evc_fcg => EVC_FCG,
        evc_rse => EVC_RSE,
        evc_phe => EVC_PHE,
        evc_clr => EVC_CLR, evc_en => EVC_EN
    };
reg(decode, pmsc, Resp) ->
    <<
        _:12, BLNKNOW:4, _:7, BLNKEN:1, BLINK_TIM:8, % PMSC_LEDC
        TXFINESEQ:16, % PMSC_TXFINESEQ
        _:(22*8), % Reserved 2
        SNOZ_TIM:8, % PMSC_SNOZT
        _:32, % Reserved 1
        KHZCLKDIV:6, Reserved:8, LDERUNE:1, _:1, PLLSYN:1, SNOZR:1, SNOZE:1, ARXSLP:1, ATXSLP:1, PKTSEQ:8, _:1, ARX2INIT:1, _:1, % PMSC_CTRL1
        SOFTRESET:4, _:3, PLL2_SEQ_EN:1, KHZCLKEN:1, _:3, GPDRN:1, GPDCE:1, GPRN:1, GPCE:1, AMCE:1, _:4, ADCCE:1, _:3, FACE:1, TXCLKS:2, RXCLKS:2, SYSCLKS:2 % PMSC_CTRL0
    >> = reverse(Resp),
    #{
        blnknow => BLNKNOW, blnken => BLNKEN, blink_tim => BLINK_TIM,
        txfineseq => TXFINESEQ,
        snoz_tim => SNOZ_TIM,
        khzclkdiv => KHZCLKDIV, reserved => Reserved, lderune => LDERUNE, pllsyn => PLLSYN, snozr => SNOZR, snoze => SNOZE, arxslp => ARXSLP, atxslp => ATXSLP, pktseq => PKTSEQ, arx2init => ARX2INIT,
        softreset => SOFTRESET, pll2_seq_en => PLL2_SEQ_EN, khzclken => KHZCLKEN, gpdrn => GPDRN, gpdce => GPDCE, gprn => GPRN, gpce => GPCE, amce => AMCE, adcce => ADCCE, face => FACE, txclks => TXCLKS, rxclks => RXCLKS, sysclks => SYSCLKS
    };
reg(decode, RegFile, Resp) -> error({unknown_regfile_to_decode, RegFile, Resp});
reg(encode, RegFile, Resp) -> error({unknown_regfile_to_encode, RegFile, Resp}).

%--- Debug ---------------------------------------------------------------------

debug_read(Reg, Value) ->
    io:format("[PmodUWB] read [16#~2.16.0B - ~w] --> ~s -> ~s~n",
        [regFile(Reg), Reg, debug_bitstring(Value), debug_bitstring_hex(Value)]
    ).

debug_write(Reg, Value) ->
    io:format("[PmodUWB] write [16#~2.16.0B - ~w] --> ~s -> ~s~n",
        [regFile(Reg), Reg, debug_bitstring(Value), debug_bitstring_hex(Value)]
    ).

debug_bitstring(Bitstring) ->
    lists:flatten([io_lib:format("2#~8.2.0B ", [X]) || <<X>> <= Bitstring]).

debug_bitstring_hex(Bitstring) ->
    lists:flatten([io_lib:format("16#~2.16.0B ", [X]) || <<X>> <= Bitstring]).
