module vtest

import os

// ownership_test_directive marks a test file that exercises the ownership or autofree mode in a
// way that its path and its `// vtest vflags:` line do not show, for example by building the
// ownership compiler or by passing `-ownership` to a compiler that it runs.
const ownership_test_directive = '// vtest ownership'

// skip_ownership_autofree_tests reports whether ownership/autofree test coverage is disabled.
// GitHub Actions disables it globally; local and other CI runs can opt in explicitly.
pub fn skip_ownership_autofree_tests() bool {
	return os.getenv('GITHUB_ACTIONS') == 'true' || os.getenv('VTEST_SKIP_OWNERSHIP') == '1'
}

// is_ownership_autofree_test reports whether a test file exercises ownership or autofree.
// That is the case when the file is in an `ownership` folder, its name contains `ownership` or
// `autofree`, its `// vtest vflags:` line enables one of these modes, or it has a
// `// vtest ownership` line. Like the other `// vtest` directives, both lines must start at
// the beginning of a line. Flags elsewhere in the source do not count: many tests pass them
// to a compiler only to check how they are parsed or rejected.
pub fn is_ownership_autofree_test(path string) bool {
	normalized_path := path.replace('\\', '/').to_lower()
	name := os.file_name(normalized_path)
	if 'ownership' in normalized_path.split('/') || name.contains('ownership')
		|| name.contains('autofree') {
		return true
	}
	source := os.read_file(path) or { return false }
	for line in source.split_into_lines() {
		if is_ownership_test_directive(line) {
			return true
		}
		if line.starts_with('// vtest vflags:')
			&& flags_enable_ownership_autofree(line.all_after(':').fields()) {
			return true
		}
	}
	return false
}

fn is_ownership_test_directive(line string) bool {
	if !line.starts_with(ownership_test_directive) {
		return false
	}
	rest := line[ownership_test_directive.len..]
	return rest == '' || rest[0] in [` `, `\t`]
}

// flags_enable_ownership_autofree reports whether compiler arguments select the ownership
// compiler, the same way `ownership_compiler_is_required` in cmd/v/v.v does.
fn flags_enable_ownership_autofree(args []string) bool {
	mut define_follows := false
	for arg in args {
		if define_follows {
			if arg.all_before('=') == 'ownership' {
				return true
			}
			define_follows = false
			continue
		}
		if arg in ['-ownership', '--ownership', '-autofree', '-downership'] {
			return true
		}
		define_follows = arg in ['-d', '-define']
	}
	return false
}

@[params]
pub struct FilterVTestConfig {
pub:
	basepath    string
	fix_slashes bool = true
}

// if VTEST_ONLY env var is set, returns tests that match the query
pub fn filter_vtest_only(paths []string, config FilterVTestConfig) []string {
	mut res := []string{}
	patterns := os.getenv('VTEST_ONLY').split(',')
	for relative_path in paths {
		mut file := relative_path
		if config.basepath != '' {
			file = os.join_path_single(config.basepath, file)
		}
		if config.fix_slashes {
			file = file.replace('\\', '/')
		}
		if patterns.len > 0 && patterns.any(file.contains(it)) {
			res << file
		}
	}
	return res
}
