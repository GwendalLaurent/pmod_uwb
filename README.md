PMOD UWB
=====

This repository contains the implementation of the pmod uwb's driver as well as an implementation of the MAC layer and a few examples.
The whole implementation was done during my master thesis at UCLouvain.

Structure of the repository
-----

* doc: documentation of the pmod generated with edocs
* examples: contains some examples of applications used in the thesis.
    * `ack_no_jitter`: This application can perform an exchange of a specified number of a specified size between 2 devices. At the end of a run, the *sender* will display the statistics of the exchange
    * `ack_jitter`: This application will perform the same operation as `ack_no_jitter` but will introduce articifial jitter in the network.
    * `ack_fast_tx`: This application also perform an exchange of a specified number of frame. But here, the same hardcoded frame is sent every time. The goal of this application is to test the limit of the driver.
    * `ss_twr`: This application performs a single-sided two-way ranging between two devices. Keep in mind that in the current configuration the results are too unprecise to be used in a RTLS application.
    * `ds_twr`: This application performs a double-sided two-way ranging between two devices.
* exploring: notes made during litterature exploration (no use)
* measurements: contains the python script used to draw some graphs for the two-way ranging measurements + the csv containing the measurements
* src: contains the source code of the driver
* test: contains the unit tests used for the MAC layer
* mcd.sh: the bash script I use to deploy the applications on my SD card. (Works on Manjaro, not tested for other OS)

Deploy the examples
-------------------

Before trying to deploy an example, you have to change the path of the SD card in the file `rebar.config` to match the path of your SD card. By default they are configured to be deployed on a SD card named *GRISP_SD* located at `/run/media/michel/`.
When this is done, you have to run `rebar3 compile` to build the application, and `rebar3 grisp deploy` to deploy the app on your SD card.

Additionally, each example contain an API function for the *sender* and a function for the *receiver*. Please refer to the following table to know which function to use for each example:

# Functionalities

## Read a register
The function `pmod_uwb:read/1` let you read any register file described in the DW1000 user manual. It returns a map of all the subregisters present in the register

# Known issues (not complete)

* The DRX_CONF is supposed to have 44 bytes, however, the sum of all the registers reaches 45 bytes. The C code uses 44 bytes but the last sub-register *RXPACC_NOSAT* isn't present and thus the sum of all the registers in the code is 43 bytes. I choosed to stick with 44 bytes and use a place holder for the last byte.

* Reading the register pmsc doesn't return the correct value for fineseq. 

* Reading LDE_CTRL throws an error (can use LDE_IF instead for now)
    $ rebar3 compile
