-module(pmod_uwb_registers).

-export([default/0]).
-export([update_reg/3]).
-export([get_value/2]).

-spec default() -> map().
default() ->
    #{eui => #{eui => <<16#FFFFFFFFFFFFFFFF:64>>}, % 0x01
      panadr => #{pan_id => <<16#FFFFFFFF:16>>, short_addr => <<16#FFFFFFFF:16>>}, % 0x03
      sys_cfg => #{aackpend => 0, % 0x04
                   autoack => 0,
                   rxautr => 0,
                   rxwtoe => 0,
                   rxm110k => 0,
                   dis_stxp => 0,
                   phr_mode => 0,
                   fcs_init2f => 0,
                   dis_rsde => 0,
                   dis_phe => 0,
                   dis_drxb => 1,
                   dis_fce => 0,
                   spi_edge => 0,
                   hirq_pol => 1,
                   ffa5 => 0,
                   ffa4 => 0,
                   ffar => 0,
                   ffam => 0,
                   ffaa => 0,
                   ffad => 0,
                   ffab => 0,
                   ffbc => 0,
                   ffen => 0},
      rx_fwto => #{rxfwto => 0}, % 0x0C
      rx_sniff => #{sniff_offt => 0, sniff_ont => 0}, % 0x1D
      % DRX_CONF isn't complete yet
      drx_conf => #{drx_pretoc => 0}, % 0x27
      % PMSC isn't complete yet
      pmsc => #{pmsc_ctrl0 => #{},  % 0x36
                pmsc_ctrl1 => #{arx2init => 0}}
     }.

-spec update_reg(Regs::map(), Reg::atom(), NewVal::atom()|map()) -> map().
update_reg(Regs, Reg, NewVal) ->
    OldVal = maps:get(Reg, Regs),
    if is_map(OldVal) -> maps:put(Reg, maps:merge(OldVal, NewVal), Regs);
       true -> maps:put(Reg, NewVal, Regs) end.

-spec get_value(Regs::map(), Reg::atom()) -> atom()|map().
get_value(Regs, Reg) ->
    maps:get(Reg, Regs).
