#!/bin/bash

# Takes two command line arguments
# $1 is the device name
# $2 is the ip address (with subnet)

sudo ./scripts/mktun.sh $1 $2
# To start the code, we need to remove the subnet mask from
# our IP address.
echo ${2%%/*}
vanguard $1 ${2%%/*}
./scripts/deltun.sh $1
