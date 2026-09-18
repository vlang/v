module pref

import v.cmdexec

// pkgconfig_flags_args builds the argument vector for a #pkgconfig flag query.
// Explicit compiler/linker flag filters must not be widened to --cflags --libs.
// With no flag filter, retain both defaults, including for --static shorthand.
pub fn pkgconfig_flags_args(raw string) ![]string {
	query := cmdexec.split_args(raw)!
	if query.len == 0 {
		return []string{}
	}
	for arg in query {
		if arg == '--' {
			break
		}
		if arg in ['--cflags', '--cflags-only-I', '--cflags-only-other', '--libs',
			'--libs-only-L', '--libs-only-l', '--libs-only-other'] {
			return query
		}
	}
	mut args := ['--cflags', '--libs']
	args << query
	return args
}
