module main

import os
import testing
import v.util.vflags

const vroot = os.dir(os.real_path(os.getenv_opt('VEXE') or { @VEXE }))

// build as a project folder
const efolders = [
	'examples/viewer',
	'examples/fasthttp',
	'examples/veb_orm_jwt',
	'examples/veb_fullstack',
]

pub fn normalised_vroot_path(path string) string {
	return os.real_path(os.join_path_single(vroot, path)).replace('\\', '/')
}

fn main() {
	args_string := os.args[1..].join(' ')
	params := args_string.all_before('build-examples')
	mut requested_flags := vflags.tokenize_to_args(os.getenv('VFLAGS'))
	requested_flags << vflags.tokenize_to_args(params)
	strict_v3 := '-new-compiler' in requested_flags && '-old-compiler' !in requested_flags
	mut skip_prefixes := efolders.map(normalised_vroot_path(it))
	res := testing.v_build_failing_skipped(params, 'examples', skip_prefixes, fn [strict_v3] (mut session testing.TestSession) {
		if strict_v3 {
			// The V3 compiler currently supports only the C backend. Keep backend-specific
			// examples visible as skips instead of asking V3 to silently fall back to V1.
			session.skip_files << session.files.filter(it.ends_with('.js.v'))
		}
		for x in efolders {
			pathsegments := x.split_any('/')
			fpath := os.real_path(os.join_path(vroot, ...pathsegments))
			session.skip_files = session.skip_files.filter(it != fpath)
			session.add(fpath)
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
