#!/usr/bin/env bash

cmd=$@

function elog() {
	message=$1
	printf "%(%Y/%m/%d %H:%M:%S)T | retry.sh $1\n"
}

declare -i iteration=0
declare -i max_iterations=9
for((iteration=1;iteration<=max_iterations;iteration++)); do
	elog "iteration: $iteration/$max_iterations, cmd: $cmd"
	if $cmd; then
		exit 0
	fi
	elog "command failed, retrying ...";
	sleep 1;
done;

elog "failed doing $max_iterations iterations of command: $cmd"
exit 1
