module main

import os
import v.vmod

struct VersionStatus {
	name       string
	current    string
	upgradable string
	resolvable string
	latest     string
}

fn version_outdated_rows(dir string, manifest vmod.Manifest) ![]VersionStatus {
	graph := build_dep_graph()!
	mut selector := new_install_server_selector()
	mut scope := LockScope{ active: true, dir: dir }
	if lf := read_lockfile(dir) {
		scope.entries = lf.modules
	}
	selected := resolve_module_query(manifest.dependencies, mut selector, mut scope, false, map[string]string{})!
	defer {
		for m in selected { rmdir_all(m.tmp_path) or {} }
	}
	mut resolved := map[string]string{}
	for m in selected {
		tag := if m.version == '' { checkout_version_tag(m.tmp_path) } else { m.version }
		resolved[normalized_clone_source(m.url)] = if tag == '' { '-' } else { tag }
	}
	mut rows := []VersionStatus{}
	for path in graph.deps.keys().sorted() {
		origin := checkout_origin_url(path)
		if origin == '' { continue }
		tags := remote_version_tags(origin)!
		retractions := latest_version_retractions(origin, tags)!
		available_tags := tags.filter(!version_is_retracted(it, retractions))
		mut constraints := []string{}
		for key, values in graph.constraints {
			if key.ends_with('\0' + path) { constraints << values }
		}
		mut upgradable := ''
		for tag in available_tags {
			if (constraints.len > 0 || tag_satisfies_range(tag, '*'))
				&& constraints.all(if is_version_range(it) {
					tag_satisfies_range(tag, it)
				} else {
					tag == it
				}) {
				upgradable = tag
				break
			}
		}
		mut current := checkout_version_tag(path)
		for entry in scope.entries.values() {
			if normalized_clone_source(entry.url) == normalized_clone_source(origin)
				&& entry.revision == head_revision(path) {
				current = entry.resolved
				break
			}
		}
		if current == '' {
			current = pseudo_version(head_commit_unix_ts(path), head_revision(path))
		}
		rows << VersionStatus{
			name:       graph.labels[path]
			current:    current
			upgradable: if upgradable == '' { '-' } else { upgradable }
			resolvable: resolved[normalized_clone_source(origin)] or { '-' }
			latest:     select_version_tag(available_tags, '*') or { '-' }
		}
	}
	return rows.sorted(a.name < b.name)
}

fn print_version_outdated() bool {
	dir := project_lockfile_dir()
	if dir == '' { return false }
	manifest := vmod.from_file(os.join_path(dir, 'v.mod')) or { return false }
	if !project_has_ranges(manifest, dir) { return false }
	rows := version_outdated_rows(dir, manifest) or {
		vpm_error(err.msg())
		exit(1)
	}
	println('Package\tCurrent\tUpgradable\tResolvable\tLatest')
	for row in rows {
		println('${row.name}\t${row.current}\t${row.upgradable}\t${row.resolvable}\t${row.latest}')
	}
	return true
}
