#!/bin/bash

mountpoint -q  /run/media/michel/GRISP_SD || udisksctl mount -b /dev/sdc1 # if mountpoint succeeds, udisksctl isn't executed

rebar3 compile

rebar3 grisp deploy