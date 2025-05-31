module vtest

import os

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
