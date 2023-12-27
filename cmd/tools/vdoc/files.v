module main

import os

fn get_ignore_paths(path string) ![]string {
	ignore_file_path := os.join_path(path, '.vdocignore')
	ignore_content := os.read_file(ignore_file_path) or {
		return error_with_code('ignore file not found.', 1)
	}
	mut res := []string{}
	if ignore_content.trim_space().len > 0 {
		rules := ignore_content.split_into_lines().map(it.trim_space())
		mut final := []string{}
		for rule in rules {
			if rule.contains('*.') || rule.contains('**') {
				println('vdoc: Wildcards in ignore rules are not allowed for now.')
				continue
			}
			final << rule
		}
		res = final.map(os.join_path(path, it.trim_right('/')))
	} else {
		mut dirs := os.ls(path) or { return []string{} }
		res = dirs.map(os.join_path(path, it)).filter(os.is_dir(it))
	}
	return res.map(it.replace('/', os.path_separator))
}

fn is_included(path string, ignore_paths []string) bool {
	if path.len == 0 {
		return true
	}
	for ignore_path in ignore_paths {
		if !path.contains(ignore_path) {
			continue
		}
		return false
	}
	return true
}

fn get_modules_list(opath string, ignore_paths2 []string) []string {
	path := opath.trim_right('/\\')
	names := os.ls(path) or { return [] }
	mut ignore_paths := get_ignore_paths(path) or { []string{} }
	ignore_paths << ignore_paths2
	mut dirs := map[string]int{}
	for name in names {
		if name == 'testdata' {
			continue
		}
		if name == 'tests' {
			continue
		}
		fpath := os.join_path(path, name)
		fmeta := os.inode(fpath)
		if fmeta.typ == .directory && is_included(fpath, ignore_paths) {
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
