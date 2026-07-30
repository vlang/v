#!/usr/bin/env bash

set -euo pipefail

declare -a git_cmd=()

parse_git_argv() {
	local git_argv_spec="${GIT:-git}"
	git_cmd=()
	# GIT is a space-delimited argv prefix. Use a wrapper without spaces when
	# one argument itself would otherwise need shell quoting.
	if [[ ! "$git_argv_spec" =~ ^[-A-Za-z0-9_./:=+@%,\ ]+$ ]]; then
		echo 'the Git command contains unsupported characters; use space-separated arguments and a wrapper path without spaces' >&2
		return 2
	fi
	read -r -a git_cmd <<< "$git_argv_spec"
	if [ "${#git_cmd[@]}" -eq 0 ] || [ -z "${git_cmd[0]}" ]; then
		echo 'the Git command must be non-empty' >&2
		return 2
	fi
}

git_executable_is_available() {
	command -v -- "${git_cmd[0]}" >/dev/null 2>&1
}

require_git_executable() {
	if ! git_executable_is_available; then
		echo "the Git executable was not found: ${git_cmd[0]}" >&2
		return 2
	fi
}

run_git() {
	"${git_cmd[@]}" "$@"
}

if [[ "${BASH_SOURCE[0]}" == "$0" ]]; then
	action="${1:-}"
	case "$action" in
		check | run) shift ;;
		*)
			echo "usage: $0 <check|run> [git arguments...]" >&2
			exit 2
			;;
	esac
	parse_git_argv
	require_git_executable
	if [ "$action" = 'run' ]; then
		exec "${git_cmd[@]}" "$@"
	fi
fi
