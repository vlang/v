module main

import os
import semver

// Constraint is one requirement placed on a module by a dependent. The resolver
// collects every constraint on a module and finds a version that satisfies all of
// them, rather than resolving each dependency against only its own range.
pub struct Constraint {
pub:
	required_by string
	range       string
}

// collect_constraints reads the dependency graph and returns, for each module name,
// every constraint placed on it by its dependents. A module that two dependents ask
// for with different ranges ends up with two entries, which is the case the resolver
// exists for.
fn collect_constraints(modules map[string]Module, graph map[string][]string) map[string][]Constraint {
	mut constraints := map[string][]Constraint{}
	for _, m in modules {
		for dep in m.manifest.dependencies {
			name := dep.all_before('@').trim_space()
			range_str := dep.all_after('@').trim_space()
			if name == '' {
				continue
			}
			constraints[name] << Constraint{
				required_by: m.name
				range:       range_str
			}
		}
	}
	return constraints
}

// select_version_tag_with_constraints returns the highest tag satisfying every
// constraint, or an error naming the constraints that could not be met. This is the
// joint step: `select_version_tag` answers one range, this answers all of them.
fn select_version_tag_with_constraints(tags []string, constraints []Constraint) !string {
	mut sorted := tags.clone()
	sorted.sort()
	mut selected := ''
	mut highest := semver.Version{}
	for tag in sorted {
		version := version_tag(tag) or { continue }
		mut all_satisfy := true
		for c in constraints {
			if !version.satisfies(c.range) {
				all_satisfy = false
				break
			}
		}
		if all_satisfy && (selected == '' || version > highest) {
			selected = tag
			highest = version
		}
	}
	if selected == '' {
		mut msg := 'no version satisfies all constraints:'
		for c in constraints {
			msg += '\n  ${c.required_by} requires ${c.range}'
		}
		return error(msg)
	}
	return selected
}

// resolve_modules resolves every module against the full set of constraints on it,
// returning the tag each one should be installed at. It is the resolver's entry point:
// parse first, collect constraints, then resolve.
fn resolve_modules(modules map[string]Module, constraints map[string][]Constraint) !map[string]string {
	mut resolved := map[string]string{}
	for _, m in modules {
		if m.version_range == '' {
			continue
		}
		tags := module_tags(m) or { continue }
		tag := select_version_tag_with_constraints(tags, constraints[m.name] or { []Constraint{} }) or {
			return error('failed to resolve `${m.name}`: ${err.msg()}')
		}
		resolved[m.name] = tag
	}
	return resolved
}

// module_tags lists the semantic-version tags of a module's repository. It is
// separated so the resolver can be tested without a git remote.
fn module_tags(m Module) ![]string {
	res := os.exec(['git', 'ls-remote', '--tags', '--refs', '--', m.install_path])
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
