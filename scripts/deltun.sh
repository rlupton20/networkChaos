#!/bin/bash

# $1 is the name of the tunnel interface to delete
ip tuntap del dev $1 mode tun
