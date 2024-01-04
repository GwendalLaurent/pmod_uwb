#!/bin/bash

mountpoint -q  /run/media/michel/GRISP_SD || udisksctl mount -b /dev/sdg1 # if mountpoint succeeds, udisksctl isn't executed

if [ $? -eq 0 ]; then
    rebar3 compile

    rebar3 grisp deploy -n robot -v 0.1.0
fi
