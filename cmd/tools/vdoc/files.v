module main

import os

struct IgnoreRules {
mut:
	patterns map[string]bool = {
		'testdata': true
		'tests':    true
		'_test.v':  true
	}
	paths map[string]bool
}

fn get_modules_list(opath string) []string {
	mut ignore_rules := IgnoreRules{}
	res := get_modules(opath, mut ignore_rules)
	return res
}

fn get_modules(opath string, mut ignore_rules IgnoreRules) []string {
	mut res := []string{}
	for p in os.ls(opath) or { return [] } {
		fp := os.join_path(opath, p)
		if fp in ignore_rules.paths
			|| ignore_rules.patterns.keys().any(p.contains(it.trim_right('/'))) {
			continue
		}
		if os.is_dir(os.join_path(opath, p)) {
			ignore_rules.get(opath)
			res << get_modules(os.join_path(opath, p), mut ignore_rules)
			continue
		}
		if p.ends_with('.v') {
			res << os.join_path(opath, p)
		}
	}
	return res
}

// Similar to `.gitignore`, a pattern starting with a / should only ignore
// the pattern relative to the ignore files directory.
// `/a` should ignore `/a` but not `/b/a`. While `a` should ignore `/a` and `/b/a`.
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
			eprintln('vdoc: Wildcards in ignore rules are not yet supported.')
			continue
		}
		if rule.starts_with('/') {
			ignore_rules.paths[os.join_path(path, rule.trim_left('/'))] = true
		} else {
			ignore_rules.patterns[rule] = true
		}
	}
}
