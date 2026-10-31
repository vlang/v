module main

import os
import document as doc
import v.vmod

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
	mut owners := map[string]string{}
	mut declared_owners := map[string]string{}
	mut source_dirs_cache := map[string][]string{}
	mut rules_cache := map[string]IgnoreRules{}
	input_root := os.real_path(path)
	paths := get_paths(path, IgnoreRules.get(path))
	for p in paths {
		dir := os.dir(p)
		if os.file_name(p) == 'v.mod' {
			source_root := module_source_root(dir)
			keep_file := subdir_files_filter(source_root, path, mut rules_cache)
			if keep_file == unsafe { nil } {
				continue
			}
			for source_dir in doc.module_source_dirs(source_root) {
				declared_owners[os.real_path(source_dir)] = source_root
				for filename in os.ls(source_dir) or { continue } {
					file := os.join_path(source_dir, filename)
					if filename.ends_with('.v') && !os.is_dir(file) && keep_file(file) {
						modules[source_root] = true
					}
				}
			}
		}
	}
	for p in paths {
		if os.file_name(p) == 'v.mod' {
			continue
		}
		dir := os.dir(p)
		if dir !in owners {
			owners[dir] = owner_module_dir(dir, input_root, mut source_dirs_cache) or {
				declared_owners[os.real_path(dir)] or { dir }
			}
		}
		modules[owners[dir]] = true
	}
	mut res := modules.keys()
	res.sort()
	return res
}

fn module_source_root(dir string) string {
	manifest := vmod.from_file(os.join_path(dir, 'v.mod')) or { return dir }
	return if manifest.base_url == '' { dir } else { os.join_path(dir, manifest.base_url) }
}

// owner_module_dir returns the closest ancestor of `dir` (up to `input_root`) that
// includes `dir` through the `subdirs` field of its v.mod, so that the files of
// such subdirectories are documented as part of that module, not as a new one.
fn owner_module_dir(dir string, input_root string, mut source_dirs_cache map[string][]string) ?string {
	real_dir := os.real_path(dir)
	mut ancestor := dir
	// A folder with its own v.mod is a module root: it can not be pulled into an outer module.
	for os.real_path(ancestor) != input_root && !os.is_file(os.join_path_single(ancestor, 'v.mod')) {
		parent := os.dir(ancestor)
		if parent == ancestor {
			break
		}
		ancestor = parent
		source_root := module_source_root(ancestor)
		if ancestor !in source_dirs_cache {
			source_dirs_cache[ancestor] = doc.module_source_dirs(source_root).map(os.real_path(it))
		}
		if real_dir in source_dirs_cache[ancestor] {
			return source_root
		}
	}
	return none
}

// subdir_files_filter returns a filter that drops the files of the v.mod `subdirs` of
// the module in `path` that are excluded by `.vdocignore` or by the default ignore rules.
// The rules are only loaded (once, in `rules_cache`) for modules that declare subdirs.
fn subdir_files_filter(path string, rules_root string, mut rules_cache map[string]IgnoreRules) fn (string) bool {
	dir := if os.is_dir(path) { path } else { os.dir(path) }
	if doc.module_source_dirs(dir).len < 2 {
		return unsafe { nil }
	}
	if rules_root !in rules_cache {
		rules_cache[rules_root] = IgnoreRules.get(rules_root)
	}
	ignore_rules := rules_cache[rules_root]
	real_rules_root := os.real_path(rules_root)
	return fn [ignore_rules, rules_root, real_rules_root] (path string) bool {
		// The rules are keyed by paths in the form given on the command line, so the file
		// path is rebuilt in that form. Subdirs outside of that folder are not covered by
		// any `.vdocignore`, but the default rules still apply below their common ancestor.
		mut base := rules_root
		mut real_base := real_rules_root
		if !path.starts_with(real_base + os.path_separator) {
			real_base = common_ancestor(real_base, path)
			base = real_base
		}
		mut fp := base
		for part in path[real_base.len..].trim_left(os.path_separator).split(os.path_separator) {
			fp = os.join_path(fp, part)
			if ignore_rules.is_ignored(fp) {
				return false
			}
		}
		return true
	}
}

fn common_ancestor(a string, b string) string {
	mut ancestor := a
	for !b.starts_with(ancestor + os.path_separator) {
		parent := os.dir(ancestor)
		if parent == ancestor {
			return ancestor
		}
		ancestor = parent
	}
	return ancestor
}

// is_ignored reports whether the file or folder `fp` matches an ignore rule.
fn (ignore_rules IgnoreRules) is_ignored(fp string) bool {
	if fp in ignore_rules.paths {
		return true
	}
	p := os.file_name(fp)
	is_dir := os.is_dir(fp)
	for ignore_path, patterns in ignore_rules.patterns {
		if fp.starts_with(ignore_path) {
			if patterns.any(p == it
				|| (it.contains('*') && p.ends_with(it.all_after('*')))
				|| (is_dir && it.ends_with('/') && fp.ends_with(it.trim_right('/')))
				|| (!it.ends_with('/') && it.contains('/') && fp.contains(it)))
			{
				return true
			}
		}
	}
	return false
}

fn get_paths(path string, ignore_rules IgnoreRules) []string {
	mut res := []string{}
	for p in os.ls(path) or { return [] } {
		fp := os.join_path(path, p)
		if ignore_rules.is_ignored(fp) {
			continue
		}
		if os.is_dir(fp) {
			res << get_paths(fp, ignore_rules)
			continue
		}
		if p.ends_with('.v') || p == 'v.mod' {
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
				if p !in res.patterns {
					res.patterns[p] = []string{}
				}
				res.patterns[p] << rule
			}
		}
	}
	return res
}
