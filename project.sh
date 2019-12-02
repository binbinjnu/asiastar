#!/bin/bash

PROJECT_NAME=asiastar
COOKIE=cookie_${PROJECT_NAME}

TYPE=$1
NODE_ID=$2

IP=`ifconfig eth0|grep inet|grep -v 127.0.0.1|grep -v inet6|awk '{print $2}'|tr -d "addr:"`

SNAME=${PROJECT_NAME}_${NODE_ID}
NAME=${SNAME}@${IP}

if [ "$TYPE" == 'name' ] ; then
	echo ${NAME}

elif [ "$TYPE" == 'sname' ] ; then
	echo ${S_NAME}

