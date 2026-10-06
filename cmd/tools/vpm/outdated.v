module main

import os
import semver
import v.vmod

// OutdatedRow is one module and the four versions that answer four different
// questions about it.
pub struct OutdatedRow {
pub:
	name       string
	current    string
	upgradable string
	resolvable string
	latest     string
}

// vpm_outdated prints the four-column table. Each column answers a different
// question, and the difference between them is the point:
//
//   Current    what is installed
//   Upgradable what the project's own constraint allows
//   Resolvable what the resolver would pick given every constraint
//   Latest     what exists upstream
//
// Upgradable alone cannot distinguish "my constraint is too tight" from "someone
// else's is", which is the question a user actually has when something did not
// update.
// pad_right pads `s` with spaces to width `n`, so the columns line up.
fn pad_right(s string, n int) string {
	mut result := s
	for result.len < n {
		result += ' '
	}
	return result
}

fn vpm_outdated() {
	rows := get_outdated_rows()
	if rows.len == 0 {
		println('No modules installed.')
		return
	}
	println('Outdated modules:')
	println('')
	println('  ${pad_right('Module', 20)} ${pad_right('Current', 12)} ${pad_right('Upgradable', 12)} ${pad_right('Resolvable', 12)} ${pad_right('Latest', 12)}')
	for row in rows {
		println('  ${pad_right(row.name, 20)} ${pad_right(row.current, 12)} ${pad_right(row.upgradable, 12)} ${pad_right(row.resolvable, 12)} ${pad_right(row.latest, 12)}')
	}
}

// get_outdated returns the names of installed modules that are not at the latest
// version. It is the shape `vpm.v` expects, and it is a thin wrapper over the
// four-column table.
fn get_outdated() []string {
	mut names := []string{}
	for row in get_outdated_rows() {
		if row.current != row.latest {
			names << row.name
		}
	}
	return names
}

// get_outdated_rows builds a row for every installed module. The constraint on each
// module comes from the project's own v.mod, and the resolvable column from the
// resolver over the whole graph.
fn get_outdated_rows() []OutdatedRow {
	installed := get_installed_modules()
	if installed.len == 0 {
		return []OutdatedRow{}
	}
	constraints := project_constraints()
	mut rows := []OutdatedRow{}
	for name in installed {
		path := get_path_of_existing_module(name) or { continue }
		tags := module_tags(path) or { continue }
		if tags.len == 0 {
			continue
		}
		current := installed_version(path) or { continue }
		latest := select_version_tag(tags, '*') or { continue }
		upgradable := select_version_tag(tags, constraints[name] or { '' }) or { latest }
		resolvable := select_version_tag_with_constraints(tags, constraints_for(constraints, name)) or { latest }
		rows << OutdatedRow{
			name:       name
			current:    current
			upgradable: upgradable
			resolvable: resolvable
			latest:     latest
		}
	}
	return rows
}

// project_constraints reads the project's v.mod and returns, for each dependency
// name, the range the project asks for. A bare name means "any version".
fn project_constraints() map[string]string {
	mut constraints := map[string]string{}
	if os.exists('./v.mod') {
		manifest := vmod.from_file('./v.mod') or { return constraints }
		for dep in manifest.dependencies {
			name := dep.all_before('@').trim_space()
			range_str := dep.all_after('@').trim_space()
			if name != '' {
				constraints[name] = range_str
			}
		}
	}
	return constraints
}

// constraints_for returns every constraint placed on `name`, which for now is the
// project's own. When the resolver walks the graph, this grows to include the
// constraints of every dependent.
fn constraints_for(constraints map[string]string, name string) []Constraint {
	if rng := constraints[name] {
		return [Constraint{
			required_by: 'project'
			range:       rng
		}]
	}
	return []Constraint{}
}

// module_tags lists the semantic-version tags of a module's repository.
fn module_tags(path string) ![]string {
	res := os.exec(['git', 'ls-remote', '--tags', '--refs', '--', path])
	if res.exit_code != 0 {
		return error('failed to list tags: ${res.output.trim_space()}')
	}
	mut tags := []string{}
	for line in res.output.split_into_lines() {
		fields := line.split('\t')
		if fields.len == 2 && fields[1].starts_with('refs/tags/') {
			tags << fields[1].trim_string_left('refs/tags/')
		}
	}
	return tags
}

// installed_version returns the tag the module is checked out at, or the short
// commit when it is not on a tag.
fn installed_version(path string) ?string {
	res := os.exec(['git', '-C', path, 'describe', '--tags', '--exact-match', 'HEAD'])
	if res.exit_code == 0 {
		return res.output.trim_space()
	}
	res2 := os.exec(['git', '-C', path, 'rev-parse', '--short', 'HEAD'])
	if res2.exit_code != 0 {
		return none
	}
	return res2.output.trim_space()
}
