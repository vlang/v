#!/usr/bin/env bash
set -euo pipefail

case "${1:-default}" in
  default) flags=() ;;
  strict) flags=(-W -cstrict) ;;
  *) echo 'Usage: musl_compile_smoke.sh [default|strict]' >&2; exit 2 ;;
esac

test "${VJOBS+x}" != x
export VTEST_FAIL_FAST=1 VTEST_SHOW_CMD=1 VTEST_SHOW_START=1
monitor_pid=
test_pid=
trap 'kill ${monitor_pid:+"$monitor_pid"} ${test_pid:+"$test_pid"} 2>/dev/null || true' EXIT
trap 'exit 130' INT
trap 'exit 143' TERM

for iteration in 1 2 3; do
  echo "Musl compilation smoke: ${1:-default}, iteration $iteration"
  timeout --signal=TERM --kill-after=10s 120s "${VEXE:-./v}" -silent "${flags[@]}" test \
    cmd/tools/check_autobahn_results/main_test.v \
    cmd/tools/testing/cache_session_test.v \
    cmd/tools/testing/folder_creation_test.v \
    vlib/arrays/uniq_test.v &
  test_pid=$!
  (
    trap - EXIT
    sleeper_pid=
    trap 'kill ${sleeper_pid:+"$sleeper_pid"} 2>/dev/null || true; exit 0' INT TERM
    while kill -0 "$test_pid" 2>/dev/null; do
      sleep 15 &
      sleeper_pid=$!
      wait "$sleeper_pid"
      sleeper_pid=
      kill -0 "$test_pid" 2>/dev/null || break
      echo "Musl compilation smoke: process and thread state, iteration $iteration"
      ps -eLo pid,ppid,lwp,stat,etime,time,pcpu,rss,wchan:28,comm
    done
  ) &
  monitor_pid=$!
  status=0
  wait "$test_pid" || status=$?
  test_pid=
  kill "$monitor_pid" 2>/dev/null || true
  wait "$monitor_pid" 2>/dev/null || true
  monitor_pid=
  if [ "$status" -ne 0 ]; then
    exit "$status"
  fi
done
