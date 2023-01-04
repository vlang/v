module main

import os
import testing

const vroot = @VMODROOT

// build as a project folder
const efolders = [
	'examples/viewer',
	'examples/vweb_orm_jwt',
]

fn main() {
	args_string := os.args[1..].join(' ')
	params := args_string.all_before('build-examples')
	skip_prefixes := efolders.map(os.real_path(os.join_path_single(vroot, it)).replace('\\',
		'/'))
	res := testing.v_build_failing_skipped(params, 'examples', skip_prefixes, fn (mut session testing.TestSession) {
		for x in efolders {
			pathsegments := x.split_any('/')
			session.add(os.real_path(os.join_path(vroot, ...pathsegments)))
		}
	})
	if res {
		exit(1)
	}
	if testing.v_build_failing_skipped(params + '-live', os.join_path_single('examples',
		'hot_reload'), skip_prefixes, fn (mut session testing.TestSession) {})
	{
		exit(1)
	}
}
