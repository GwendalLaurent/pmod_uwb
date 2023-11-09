% @doc For now we are using a busy loop for the RX. The goal later on will be to use internal interruption generated by the OS 
-module(mac_rx_sniff_dc).

-behaviour(gen_mac_rx).

%%% gen_mac_rx callbacks
-export([init/1]).
-export([rx_on/2]).
-export([rx_off/1]).
-export([suspend/1]).
-export([resume/1]).
-export([terminate/2]).


%%% gen_mac_rx callbacks
% @doc initialize the sniff duty cycle mode on the pmod
% Params shall contain:
% <li> `sniff_ont': The duration that the pmod has to stay in the RX (on) state (in units of PAC). </li>
% <li> `sniff_offt': The duratin that the pmod has to stay in the Init (off) state (1 unit represents 6.6µs </li>
%
% The amount of time that the pmod will stay in the on state will depend on the PAC size.
% Note: the pmod will automatically adds 1 unit from the SNIFF_ONT. This is taken into account in this module (i.e. this module will internally remove 1 unit to compensate that change)
init({SNIFF_ONT, SNIFF_OFFT, PHY}) ->
    #{sniff_ont => SNIFF_ONT-1, sniff_onf => SNIFF_OFFT, rx => off, phy_layer => PHY}.

-spec rx_on(State::term(), Callback::function()) -> {ok, State::term()} | {error, State::term(), Error::atom()}.
rx_on(#{phy_layer := PhyModule, sniff_ont := SNIFF_ONT, sniff_offt := SNIFF_OFFT} = State, Callback) ->
    PhyModule:write(sys_cfg, #{rxwtoe => 0}),
    PhyModule:write(pmsc, #{pmsc_ctrl1 => #{arx2init => 2#1}}),
    PhyModule:write(rx_sniff, #{sniff_ont => SNIFF_ONT, sniff_offt => SNIFF_OFFT}),
    LoopPid = spawn_link(fun() -> gen_mac_rx:rx_loop(PhyModule, Callback) end), 
    {ok, State#{loop_pid => LoopPid, callback => Callback}}.

-spec rx_off(State::term()) -> {ok, State::term()} | {error, State::term(), Error::atom()}.
rx_off(#{phy_layer := PhyModule} = State) ->
    case State of
        #{loop_pid := LoopPid} -> unlink(LoopPid), exit(LoopPid, shutdown);
        _ -> ok
    end,
    PhyModule:write(pmsc, #{pmsc_ctrl1 => #{arx2init => 2#0}}),
    PhyModule:write(rx_sniff, #{sniff_ont => 2#0, sniff_offt => 2#0}),
    PhyModule:disable_rx(),
    PhyModule:write(sys_cfg, #{rxwtoe => 1}),
    {ok, maps:remove(loop_pid, State)}.

-spec suspend(State::term()) -> {ok, State::term()} | {error, State::term(), Error::atom()}.
suspend(State) ->
    rx_off(State). % For now suspend is the same as stopping rx (later on might be different)

-spec resume(State::term()) -> {ok, State::term()} | {error, State::term(), Error::atom()}.
resume(#{callback := Callback} = State) ->
    rx_on(State, Callback). % Same here, resume is the same as starting the rx (later might change)

-spec terminate(State::term(), Reason::atom()) -> ok.
terminate(State, Reason) ->
    case State of
        #{loop_pid := LoopPid} -> unlink(LoopPid), exit(LoopPid, Reason);
        _ -> ok
    end,
    ok.