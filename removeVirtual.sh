#!\bin\bash

route delete -net 10.0.0.0 netmask 255.255.255.0 dev rvl0

ifconfig rvl0 down
ip link delete rvl0 type dummy
rmmod dummy
