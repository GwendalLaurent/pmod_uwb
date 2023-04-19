robot
=====

Contains the driver for the Pmod UWB in `pmod_uwb.erl`
`robot.erl` just initialize the driver for us

Build
-----

    $ rebar3 compile

Deploy
------

    $ rebar3 grisp deploy

# Functionalities

## Read a register
The function `pmod_uwb:read/1` let you read any register file described in the DW1000 user manual. It returns a map of all the subregisters present in the register

# Known issues (not complete)

* The DRX_CONF is supposed to have 44 bytes, however, the sum of all the registers reaches 45 bytes. The C code uses 44 bytes but the last sub-register *RXPACC_NOSAT* isn't present and thus the sum of all the registers in the code is 43 bytes. I choosed to stick with 44 bytes and use a place holder for the last byte.

* Reading the register pmsc doesn't return the correct value for fineseq. 

* Reading LDE_CTRL throws an error (can use LDE_IF instead for now)
