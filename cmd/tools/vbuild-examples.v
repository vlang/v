module main

import os
import testing

const vroot = os.dir(os.real_path(os.getenv_opt('VEXE') or { @VEXE }))

// build as a project folder
const efolders = [
	'examples/viewer',
	'examples/vweb_orm_jwt',
	'examples/vweb_fullstack',
	'examples/vanilla_http_server',
]

pub fn normalised_vroot_path(path string) string {
	return os.real_path(os.join_path_single(vroot, path)).replace('\\', '/')
}

fn main() {
	args_string := os.args[1..].join(' ')
	params := args_string.all_before('build-examples')
	mut skip_prefixes := efolders.map(normalised_vroot_path(it))
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
