module main

import os
import testing

const vroot = @VMODROOT

const efolders = [
	'examples/viewer',
]

fn main() {
	args_string := os.args[1..].join(' ')
	params := args_string.all_before('build-examples')
	skip_prefixes := efolders.map(os.join_path(vroot, it))
	res := testing.v_build_failing_skipped(params, 'examples', skip_prefixes, fn (mut session testing.TestSession) {
		for x in efolders {
			pathsegments := x.split_any('/')
			session.add(os.join_path(vroot, ...pathsegments))
		}
	})
	if res {
		exit(1)
	}
	if testing.v_build_failing_skipped(params + '-live', os.join_path('examples', 'hot_reload'),
		skip_prefixes, fn (mut session testing.TestSession) {})
	{
		exit(1)
	}
}
