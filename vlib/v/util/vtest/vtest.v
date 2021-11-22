module vtest

import os

pub struct FilterVTestConfig {
	basepath    string
	fix_slashes bool = true
}

// if VTEST_ONLY env var is set, returns tests that match the query
pub fn filter_vtest_only(paths []string, config FilterVTestConfig) []string {
	mut res := []string{}
	patterns := os.getenv('VTEST_ONLY').split(',')
	for relative_path in paths {
		mut file := relative_path
		if config.basepath.len > 0 {
			file = os.join_path_single(config.basepath, file)
		}
		if config.fix_slashes {
			file = file.replace('\\', '/')
		}
		if patterns.len != 0 {
			mut found := 0
			for okpat in patterns {
				if file.contains(okpat) {
					found++
					break
				}
			}
			if found == 0 {
				continue
			}
		}
		res << file
	}
	return res
}
