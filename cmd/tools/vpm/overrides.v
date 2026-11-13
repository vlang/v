module main

import os
import semver
import v.vmod

// Override selects a root dependency ref or range, optionally only where
// requiring asks for that dependency. Workspace member manifests may supply
// overrides too; they merge after the root's, so the root wins on duplicates.
pub struct Override {
pub:
	name       string
	version    string
	requiring  string
	constraint string
}

// parse_overrides reads [requiring>]name: version entries and rejects malformed
// or duplicate selectors before installation can change the module store.
pub fn parse_overrides(raw []string) ![]Override {
	mut result := []Override{}
	mut seen := map[string]bool{}
	for entry in raw {
		o := parse_override(entry)!
		key := o.requiring + '@' + o.constraint + '>' + o.name
		if key in seen { return error('duplicate dependency override for `${key}`') }
		seen[key] = true
		result << o
	}
	return result
}

fn valid_override_name(name string) bool {
	return name != '' && name !in ['.', '..'] && !name.contains_any('/\\@<>:')
}

fn parse_override(entry string) !Override {
	parts := entry.split(':')
	if parts.len != 2 {
		return error('invalid dependency override `${entry}`; expected [requiring>]name: version')
	}
	selectors := parts[0].trim_space().split('>')
	if selectors.len !in [1, 2] { return error('invalid dependency override selector `${entry}`') }
	name := selectors[selectors.len - 1].trim_space()
	mut requiring := if selectors.len == 2 { selectors[0].trim_space() } else { '' }
	mut constraint := ''
	if requiring.contains('@') {
		requiring, constraint = requiring.split_once('@') or { '', '' }
		if constraint == '' || !semver.is_valid_range(constraint) {
			return error('invalid dependency override constraint `${constraint}` in `${entry}`')
		}
	}
	version := parts[1].trim_space()
	if !valid_override_name(name) || (selectors.len == 2 && !valid_override_name(requiring)) || version == '' {
		return error('invalid dependency override `${entry}`')
	}
	return Override{ name: name, version: version, requiring: requiring, constraint: constraint }
}

// workspace_overrides merges dependency overrides from the root manifest and
// every workspace member manifest. The root is parsed first and wins on
// duplicate selectors (a member entry with the same requiring, constraint and
// name is skipped). A malformed member override is an error, the same as a
// malformed root one; unreadable member manifests are skipped.
pub fn workspace_overrides(root string, root_manifest vmod.Manifest) ![]Override {
	mut merged := parse_overrides(root_manifest.unknown['dependency_overrides'] or { []string{} })!
	if root_manifest.workspaces.len == 0 {
		return merged
	}
	mut seen := map[string]bool{}
	for o in merged {
		seen[o.requiring + '@' + o.constraint + '>' + o.name] = true
	}
	for member_dir in vmod.workspace_member_dirs(root, root_manifest) {
		member_manifest := vmod.from_file(os.join_path(member_dir, 'v.mod')) or { continue }
		for raw in member_manifest.unknown['dependency_overrides'] or { []string{} } {
			o := parse_override(raw)!
			key := o.requiring + '@' + o.constraint + '>' + o.name
			if key in seen {
				continue
			}
			seen[key] = true
			merged << o
		}
	}
	return merged
}

// overridden_request applies global root overrides before a source is selected.
pub fn overridden_request(request string, names []string, overrides []Override) string {
	return overridden_request_for_module(request, names, '', overrides)
}

// overridden_request_for_module applies a selector to the actual requiring edge.
// A matching scoped selector takes precedence over a global override. The result
// becomes the source request and lock entry before its manifest is read.
pub fn overridden_request_for_module(request string, names []string, requiring string, overrides []Override) string {
	for name in names {
		for o in overrides {
			if requiring != '' && o.requiring == requiring && o.name == name && override_constraint_matches(o) {
				return lockfile_module_key(request) + at_version(o.version)
			}
		}
	}
	for name in names {
		for o in overrides {
			if o.requiring == '' && o.name == name && override_constraint_matches(o) {
				return lockfile_module_key(request) + at_version(o.version)
			}
		}
	}
	return request
}

fn override_constraint_matches(o Override) bool {
	if o.constraint == '' || o.version == '-' { return true }
	v := version_tag(o.version) or { return false }
	return v.satisfies(o.constraint)
}

// apply_overrides adjusts parsed graph metadata for callers that display a graph.
// Installation resolves overrides before cloning and does not use this helper to
// change the version recorded for already selected sources.
pub fn apply_overrides(mut modules map[string]Module, overrides []Override, graph map[string][]string) {
	for o in overrides {
		if !override_constraint_matches(o) { continue }
		// A selector override only applies where the requiring module asks for the
		// dependency. Without this, `vsl>c: 1.0.2` would force c everywhere.
		if o.requiring != '' {
			deps := graph[o.requiring] or { continue }
			if o.name !in deps {
				continue
			}
		}
		for key, mut m in modules {
			if m.name == o.name {
				m.version = o.version
				m.version_range = if is_version_range(o.version) { o.version } else { '' }
				modules[key] = m
			}
		}
	}
}
