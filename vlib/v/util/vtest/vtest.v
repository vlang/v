module vtest

import os

// skip_ownership_autofree_tests reports whether ownership/autofree test coverage is disabled.
// GitHub Actions disables it globally; local and other CI runs can opt in explicitly.
pub fn skip_ownership_autofree_tests() bool {
	return os.getenv('GITHUB_ACTIONS') == 'true' || os.getenv('VTEST_SKIP_OWNERSHIP') == '1'
}

// is_ownership_autofree_test reports whether a test file exercises ownership or autofree.
pub fn is_ownership_autofree_test(path string) bool {
	normalized_path := path.replace('\\', '/').to_lower()
	name := os.file_name(normalized_path)
	if 'ownership' in normalized_path.split('/') || name.contains('ownership')
		|| name.contains('autofree') {
		return true
	}
	source := os.read_file(path) or { return false }
	for line in source.split_into_lines() {
		trimmed := line.trim_space()
		if trimmed.starts_with('//') {
			if trimmed.starts_with('// vtest vflags:')
				&& contains_ownership_autofree_flag(trimmed) {
				return true
			}
			continue
		}
		if contains_ownership_autofree_flag(line) {
			return true
		}
	}
	return false
}

fn contains_ownership_autofree_flag(line string) bool {
	return line.contains(' -autofree') || line.contains(' -ownership')
		|| line.contains(' -d ownership') || line.contains(' -d=ownership')
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
