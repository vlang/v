module main

import os

struct IgnoreRules {
mut:
	// Ignore patterns use the path with a `.vdocignore` file as a base. E.g.:
	// `{'<path>': ['<pattern1>', '<pattern2>'], '<path/subpath>': ['<pattern3>']}`
	patterns map[string][]string = {
		// Default ignore patterns.
		'': ['testdata', 'tests', '*_test.v']
	}
	paths    map[string]bool
}

fn get_modules(path string) []string {
	mut modules := map[string]bool{}
	for p in get_paths(path, IgnoreRules.get(path)) {
		modules[os.dir(p)] = true
	}
	mut res := modules.keys()
	res.sort()
	return res
}

fn get_paths(path string, ignore_rules IgnoreRules) []string {
	mut res := []string{}
	outer: for p in os.ls(path) or { return [] } {
		fp := os.join_path(path, p)
		if fp in ignore_rules.paths {
			continue
		}
		is_dir := os.is_dir(fp)
		for ignore_path, patterns in ignore_rules.patterns {
			if fp.starts_with(ignore_path) {
				if patterns.any(p == it
					|| (it.contains('*') && p.ends_with(it.all_after('*')))
					|| (is_dir && it.ends_with('/') && fp.ends_with(it.trim_right('/')))
					|| (!it.ends_with('/') && it.contains('/') && fp.contains(it)))
				{
					continue outer
				}
			}
		}
		if is_dir {
			res << get_paths(fp, ignore_rules)
			continue
		}
		if p.ends_with('.v') {
			res << fp
		}
	}
	return res
}

fn IgnoreRules.get(path string) IgnoreRules {
	mut res := IgnoreRules{}
	mut vdocignore_paths := []string{}
	mut vdocignore_paths_ref := &vdocignore_paths
	os.walk(path, fn [vdocignore_paths_ref] (p string) {
		if os.file_name(p) == '.vdocignore' {
			unsafe {
				vdocignore_paths_ref << p
			}
		}
	})
	for ignore_path in vdocignore_paths {
		ignore_content := os.read_file(ignore_path) or { continue }
		if ignore_content.trim_space() == '' {
			continue
		}
		rules := ignore_content.split_into_lines().map(it.trim_space())
		for rule in rules {
			if rule.starts_with('#') {
				continue
			}
			if rule.contains('*.') || rule.contains('**') {
				// Skip wildcards that are defined in an ignore file.
				// For now, only add a basic implementation in `get_paths`
				// that can handle the default `*_test.v` pattern.
				eprintln('vdoc: Wildcards in ignore rules are not yet supported.')
				continue
			}
			p := os.dir(ignore_path)
			if rule.starts_with('/') {
				// Similar to `.gitignore`, a pattern starting with `/` should only ignore
				// the pattern relative to the directory of the `.vdocignore` file.
				// `/a` should ignore `/a` but not `/b/a`. While `a` should ignore `/a` and `/b/a`.
				res.paths[os.join_path(p, rule.trim_left('/'))] = true
			} else {
				res.patterns[p] << rule
			}
		}
	}
	return res
}
