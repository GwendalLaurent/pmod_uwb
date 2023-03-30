-module(pmod_uwb).
-behaviour(gen_server).


%% API
-export([start_link/2]).
-export([init/1, handle_call/3, handle_cast/2]).
-export([read/1]).

% Define the polarity and the phase of the clock
-define(SPI_MODE, #{clock => {low, leading}}).

% Include for the record "device"
-include("grisp.hrl").
-include("pmod_uwb.hrl").

%--- API -----------------------------------------------------------------------

start_link(Connector, _Opts) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Connector, []).

read(RegFileID) -> call({read, RegFileID}).

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
handle_call(Request, _From, _State) -> error({unknown_call, Request}).

handle_cast(Request, _State) -> error({unknown_cast, Request}).

%--- Internal ------------------------------------------------------------------

call(Call) ->
    Dev = grisp_devices:default(?MODULE),
    gen_server:call(Dev#device.pid, Call).

% Reverse the response of the pmod
reverse(Bin) -> reverse(Bin, <<>>).
reverse(<<Bin:8>>, Acc) -> 
    <<Bin, Acc/binary>>;
reverse(<<Bin:8, Rest/bitstring>>, Acc) -> 
    reverse(Rest, <<Bin, Acc/binary>>).

% Decode the response for each register IDs that allows a read operation on them
decode(dev_id, Resp) -> 
    <<Ver:4/unsigned-little, Rev:4/unsigned-little, Model:8/unsigned-integer, RIDTAG2:8, RIDTAG1:8>> = Resp,
    #{ridtag => <<RIDTAG1, RIDTAG2>>, model => Model, ver => Ver, rev => Rev};
decode(eui, Resp) ->
    #{eui => reverse(Resp)};
decode(panadr, Resp) ->
    <<PanId:16, ShortAddr:16>> = reverse(Resp),
    #{panid => PanId, shortaddr => ShortAddr};
decode(sys_cfg, Resp) ->
    <<
        FFA4:1, FFAR:1, FFAM:1, FFAA:1, FFAD:1, FFAB:1, FFBC:1, FFEN:1, % bits 7-0
        FCS_INIT2F:1, DIS_RSDE:1, DIS_PHE:1, DIS_DRXB:1, DIS_FCE:1, SPI_EDGE:1, HIRQ_POL:1, FFA5:1, % bits 15-8
        _:1, RXM110K:1, _:3, DIS_STXP:1, PHR_MODE:2, % bits 23-16
        AACKPEND:1, AUTOACK:1, RXAUTR:1, RXWTOE:1, _:4 % bits 31-24
    >> = Resp,
    #{aackpend => AACKPEND, autoack => AUTOACK, rxautr => RXAUTR, rxwtoe => RXWTOE, 
      rxm110k => RXM110K, dis_stxp => DIS_STXP, phr_mode => PHR_MODE, 
      fcs_init2F => FCS_INIT2F, dis_rsde => DIS_RSDE, dis_phe => DIS_PHE, dis_drxb => DIS_DRXB, dis_fce => DIS_FCE, spi_edge => SPI_EDGE, hirq_pol => HIRQ_POL, ffa5 => FFA5,
      ffa4 => FFA4, ffar => FFAR, ffam => FFAM, ffaa => FFAA, ffad => FFAD, ffab => FFAB, ffbc => FFBC, ffen => FFEN};
decode(sys_time, Resp) -> 
    <<SysTime:40/unsigned-integer>> = reverse(Resp),
    #{sys_time => SysTime}.

% Create the header of the operation
% Op: atom - either read or write
% RegFileID: atom - identifier of the register file id according to the data sheet
% returns the corresponding header in binary format (1 byte long) 
header(Op, RegFileID) ->
    <<(rw(Op)):1, 2#0:1, (reg(RegFileID)):6>>.

read_reg(Bus, RegFileID) ->
    Header = header(read, RegFileID),
    [Resp] = grisp_spi:transfer(Bus, [{?SPI_MODE, Header, 1, regSize(RegFileID)}]),
    debug_read(RegFileID, Resp),
    decode(RegFileID, Resp).

%--- Debug ---------------------------------------------------------------------

debug_read(Reg, Value) ->
    io:format("[PmodUWB] read  16#~2.16.0B --> ~s -> ~s~n",
        [reg(Reg), debug_bitstring(Value), debug_bitstring_hex(Value)]
    ).

debug_bitstring(Bitstring) ->
    lists:flatten([io_lib:format("2#~8.2.0B ", [X]) || <<X>> <= Bitstring]).

debug_bitstring_hex(Bitstring) ->
    lists:flatten([io_lib:format("16#~2.16.0B ", [X]) || <<X>> <= Bitstring]).