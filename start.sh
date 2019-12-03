#!/bin/bash

ACTION=$1
ID=$2
IP=`ifconfig eth0|grep inet|grep -v 127.0.0.1|grep -v inet6|awk '{print $2}'|tr -d "addr:"`

# 不能换行
NODE_ID=$ID IP=$IP ./_build/default/rel/asiastar/bin/asiastar $ACTION
