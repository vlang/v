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
		if arg in ['--cflags', '--cflags-only-I', '--cflags-only-other', '--libs', '--libs-only-L',
			'--libs-only-l', '--libs-only-other'] {
			return query
		}
	}
	mut args := ['--cflags', '--libs']
	args << query
	return args
}

// pkgconfig_link_flags_args selects static package closures without changing the
// caller's requested output filters or interpreting package names as options.
pub fn pkgconfig_link_flags_args(raw string, static_link bool) ![]string {
	mut args := pkgconfig_flags_args(raw)!
	if static_link && args.len > 0 {
		// Prepending keeps the modifier before a caller's `--` terminator.
		args.prepend('--static')
	}
	return args
}
