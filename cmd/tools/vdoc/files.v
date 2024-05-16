module main

import os

struct IgnoreRules {
mut:
	// Ignore patterns use the path with a `.vdocignore` file as a base. E.g.:
	// `{'<path>': {'<ignore_pattern>': true}, '<path/subpath>': {'<ignore_pattern>': true}}`
	patterns map[string]map[string]bool = {
		'': {
			// Default ignore patterns.
			'testdata': true
			'tests':    true
			'*_test.v': true
		}
	}
	paths map[string]bool
}

fn get_modules(path string) []string {
	mut ignore_rules := IgnoreRules{}
	mut modules := map[string]bool{}
	for p in get_paths(path, mut ignore_rules) {
		modules[os.dir(p)] = true
	}
	mut res := modules.keys()
	res.sort()
	return res
}

fn get_paths(path string, mut ignore_rules IgnoreRules) []string {
	mut res := []string{}
	outer: for p in os.ls(path) or { return [] } {
		ignore_rules.get(path)
		fp := os.join_path(path, p)
		if fp in ignore_rules.paths {
			continue
		}
		is_dir := os.is_dir(fp)
		for ignore_path, patterns in ignore_rules.patterns {
			if fp.starts_with(ignore_path) {
				if patterns.keys().any(p == it
					|| (it.contains('*') && p.ends_with(it.all_after('*')))
					|| (is_dir && it.ends_with('/') && fp.ends_with(it.trim_right('/')))
					|| (!it.ends_with('/') && it.contains('/') && fp.contains(it)))
				{
					continue outer
				}
			}
		}
		if is_dir {
			res << get_paths(fp, mut ignore_rules)
			continue
		}
		if p.ends_with('.v') {
			res << fp
		}
	}
	return res
}

fn (mut ignore_rules IgnoreRules) get(path string) {
	ignore_content := os.read_file(os.join_path(path, '.vdocignore')) or { return }
	if ignore_content.trim_space() == '' {
		return
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
		if rule.starts_with('/') {
			// Similar to `.gitignore`, a pattern starting with `/` should only ignore
			// the pattern relative to the directory of the `.vdocignore` file.
			// `/a` should ignore `/a` but not `/b/a`. While `a` should ignore `/a` and `/b/a`.
			ignore_rules.paths[os.join_path(path, rule.trim_left('/'))] = true
		} else {
			ignore_rules.patterns[path][rule] = true
		}
	}
}
