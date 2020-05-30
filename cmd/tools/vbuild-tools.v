module main

import os
import testing
import v.util

fn main() {
	args := os.args
	args_string := args[1..].join(' ')
	// TODO: fix cmd/tools/gen_vc.v / vweb too
	skips := [
		'cmd/tools/gen_vc.v'
	]
	util.ensure_modules_for_all_tools_are_installed('-v' in args)
	if testing.v_build_failing_skipped(args_string.all_before('build-tools'), 'cmd/tools', skips) {
		exit(1)
	}
}
