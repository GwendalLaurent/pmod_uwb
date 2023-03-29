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

rw(read) -> 0;
rw(write) -> 1.

reg(dev_id) -> 16#00;
reg(eui) -> 16#01;
reg(panadr) -> 16#03;
reg(sys_cfg) -> 16#04;
reg(sys_time) -> 16#06;
reg(tx_ctrl) -> 16#08;
reg(tx_buffer) -> 16#09.

regSize(dev_id) -> 4;
regSize(eui) -> 8;
regSize(panadr) -> 4;
regSize(sys_cfg) -> 4;
regSize(sys_time) -> 5;
regSize(tx_ctrl) -> 5;
regSize(tx_buffer) -> 1024.

reverse(Bin) -> reverse(Bin, <<>>).
reverse(<<Bin:8>>, Acc) -> 
    <<Bin, Acc/binary>>;
reverse(<<Bin:8, Rest/bitstring>>, Acc) -> 
    reverse(Rest, <<Bin, Acc/binary>>).

decode(dev_id, Resp) -> 
    <<Ver:4/unsigned-little, Rev:4/unsigned-little, Model:8/unsigned-integer, RIDTAG2:8, RIDTAG1:8>> = Resp,
    #{ridtag => <<RIDTAG1, RIDTAG2>>, model => Model, ver => Ver, rev => Rev};
decode(eui, Resp) ->
    #{eui => reverse(Resp)}.

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

debug_read(Reg, Value) ->
    io:format("[PmodUWB] read  16#~2.16.0B --> ~s -> ~s~n",
        [reg(Reg), debug_bitstring(Value), debug_bitstring_hex(Value)]
    ).

debug_bitstring(Bitstring) ->
    lists:flatten([io_lib:format("2#~8.2.0B ", [X]) || <<X>> <= Bitstring]).

debug_bitstring_hex(Bitstring) ->
    lists:flatten([io_lib:format("16#~2.16.0B ", [X]) || <<X>> <= Bitstring]).