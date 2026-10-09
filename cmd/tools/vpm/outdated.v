module main

import os
import semver
import sync.pool
import v.vmod

pub struct OutdatedResult {
	name string
mut:
	outdated bool
}

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

// pad_right pads `s` with spaces to width `n`, so the columns line up.
fn pad_right(s string, n int) string {
	mut result := s
	for result.len < n {
		result += ' '
	}
	return result
}

// vpm_outdated reports installed versions and project-constrained tag choices.
fn vpm_outdated() {
	if print_version_outdated() { return }
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

// get_outdated retains commit-based repository checks used by upgrade.
fn get_outdated() []string {
	installed := get_installed_modules()
	if installed.len == 0 {
		println('No modules installed.')
		exit(0)
	}
	mut pp := pool.new_pool_processor(
		callback: fn (mut pp pool.PoolProcessor, idx int, wid int) &OutdatedResult {
			mut result := &OutdatedResult{
				name: pp.get_item[string](idx)
			}
			path := get_path_of_existing_module(result.name) or { return result }
			result.outdated = is_outdated(path)
			return result
		}
	)
	pp.work_on_items(installed)
	mut outdated := []string{}
	for res in pp.get_results[OutdatedResult]() {
		if res.outdated {
			outdated << res.name
		}
	}
	return outdated
}

// get_outdated_rows builds a row for every installed module. Resolvable currently
// uses only the root project constraint; transitive candidate discovery is not wired
// into reporting yet.
fn get_outdated_rows() []OutdatedRow {
	constraints := project_constraints()
	mut rows := []OutdatedRow{}
	for name in get_installed_modules() {
		path := get_path_of_existing_module(name) or { continue }
		rows << outdated_row(name, path, constraints)
	}
	return rows
}

fn outdated_row(name string, path string, constraints map[string][]string) OutdatedRow {
	current := installed_version(path) or { 'n/a' }
	tags := module_tags(path) or {
		return OutdatedRow{ name: name, current: current, upgradable: 'n/a', resolvable: 'n/a', latest: 'n/a' }
	}
	latest := select_version_tag(tags, '*') or { 'none' }
	mut required := []Constraint{}
	for request in project_requests_for_module(constraints, name, path) {
		constraint := outdated_constraint(request) or {
			return OutdatedRow{ name: name, current: current, upgradable: 'n/a', resolvable: 'n/a', latest: latest }
		}
		required << Constraint{ required_by: 'project', range: constraint }
	}
	no_match := if required.all(semver.is_valid_range(it.range)) { 'none' } else { 'invalid' }
	upgradable := select_version_tag_with_constraints(tags, required) or { no_match }
	return OutdatedRow{
		name:       name
		current:    current
		upgradable: upgradable
		resolvable: upgradable
		latest:     latest
	}
}

fn project_requests_for_module(constraints map[string][]string, name string, path string) []string {
	mut requests := constraints[name].clone()
	origin := os.exec(['git', '-C', path, 'remote', 'get-url', 'origin'])
	if origin.exit_code == 0 {
		source := origin.output.trim_space()
		normalized_source := normalized_clone_source(source)
		for dependency, requirements in constraints {
			if dependency == name { continue }
			if normalized_clone_source(dependency) == normalized_source {
				requests << requirements
			} else if is_local_repository(source) {
				source_path := source.trim_string_left('file://')
				dependency_path := dependency.trim_string_left('file://')
				if os.exists(dependency_path)
					&& os.real_path(dependency_path) == os.real_path(source_path) {
					requests << requirements
				}
			}
		}
	}
	return requests
}

// outdated_constraint preserves exact semantic-version refs while leaving other Git
// refs out of semantic-version comparisons.
fn outdated_constraint(request string) !string {
	if request == '' || is_version_range(request) {
		return request
	}
	version := version_tag(request) or { return error('not a semantic-version ref') }
	return version.str()
}

// project_constraints reads the project's v.mod and returns, for each dependency
// name, every requirement from dependencies and dev_dependencies. A bare name
// means "any version" and does not replace another requirement on the same module.
fn project_constraints() map[string][]string {
	mut constraints := map[string][]string{}
	if os.exists('./v.mod') {
		manifest := vmod.from_file('./v.mod') or { return constraints }
		for dep in project_dependencies(manifest) {
			name := if dep.starts_with('git@') && dep.count('@') == 1 {
				dep.trim_space()
			} else {
				dep.all_before_last('@').trim_space()
			}
			range_str := requirement_version(dep).trim_space()
			if name != '' {
				constraints[name] << range_str
			}
		}
	}
	return constraints
}

// module_tags lists current upstream tags, including ones absent from the checkout.
fn module_tags(path string) ![]string {
	origin := os.exec(['git', '-C', path, 'remote', 'get-url', 'origin'])
	source := if origin.exit_code == 0 { origin.output.trim_space() } else { path }
	res := os.exec(['git', 'ls-remote', '--tags', '--refs', '--', source])
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

fn is_outdated(path string) bool {
	vcs := vcs_used_in_dir(path) or { return false }
	args := vcs_info[vcs].args
	// A checkout that a locked install left detached has no upstream branch to
	// compare with; compare it with the default branch of the origin instead,
	// which is where `v update` moves such a checkout. A clone made at a tag has
	// no such branch, and stays pinned, the same as before.
	steps := if vcs == .git && head_is_detached(path) {
		['fetch', 'rev-parse HEAD', 'rev-parse origin/HEAD']
	} else {
		args.outdated
	}
	mut outputs := []string{}
	for step in steps {
		cmd := [vcs.str(), args.path, os.quoted_path(path), step].join(' ')
		vpm_log(@FILE_LINE, @FN, 'cmd: ${cmd}')
		res := os.exec([vcs.str(), args.path, path, ...(os.split_args(step) or { panic(err) })])
		vpm_log(@FILE_LINE, @FN, 'output: ${res.output}')
		if res.exit_code != 0 {
			return false
		}
		if vcs == .hg {
			// HG uses only one outdated step. If it has not failed, the module is outdated.
			return true
		}
		outputs << res.output
	}
	// Compare the current and latest origin commit sha.
	return outputs[1] != outputs[2]
}
