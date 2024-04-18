module main

import os

fn get_ignore_paths(path string) ![]string {
	ignore_file_path := os.join_path(path, '.vdocignore')
	ignore_content := os.read_file(ignore_file_path) or {
		return error_with_code('ignore file not found.', 1)
	}
	if ignore_content.trim_space() == '' {
		return []string{}
	}
	rules := ignore_content.split_into_lines().map(it.trim_space())
	mut res := []string{}
	for rule in rules {
		if rule.contains('*.') || rule.contains('**') {
			println('vdoc: Wildcards in ignore rules are not allowed for now.')
			continue
		}
		res << rule
	}
	return res.map(os.join_path(path, it.replace('/', os.path_separator)).trim_right(os.path_separator))
}

fn get_modules_list(opath string, ignore_paths2 []string) []string {
	path := opath.trim_right('/\\')
	names := os.ls(path) or { return [] }
	mut ignore_paths := get_ignore_paths(path) or { []string{} }
	ignore_paths << ignore_paths2
	mut dirs := map[string]int{}
	for name in names {
		if name in ['testdata', 'tests'] {
			continue
		}
		fpath := os.join_path(path, name)
		if os.is_dir(fpath) && !ignore_paths.any(fpath.contains(it)) {
			current_ignore_paths := ignore_paths.filter(it.starts_with(fpath))
			for k in get_modules_list(fpath, current_ignore_paths) {
				dirs[k]++
			}
			continue
		}
		if fpath.ends_with('.v') && !fpath.ends_with('_test.v') {
			dirs[path]++
			continue
		}
	}
	mut res := dirs.keys()
	res.sort()
	return res
}
