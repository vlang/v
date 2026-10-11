module main

import semver

// Constraint records a version range imposed on a module by a requiring module.
pub struct Constraint {
pub:
	required_by string
	range       string
}

// VersionedDeps is one candidate version of a module and the dependencies it places.
// The resolver searches over these: for each module it has a list of versions to try,
// highest first, and each version carries the dependencies it needs.
pub struct VersionedDeps {
pub:
	version string
	deps    []string
}

// select_version_tag_with_constraints returns the highest tag satisfying every
// constraint, or an error naming the constraints that could not be met. This is the
// joint step: `select_version_tag` answers one range, this answers all of them.
fn select_version_tag_with_constraints(tags []string, constraints []Constraint) !string {
	if constraints.len == 0 {
		return select_version_tag(tags, '*')!
	}
	for c in constraints {
		if !semver.is_valid_range(c.range) {
			return error('invalid version range `${c.range}` required by `${c.required_by}`')
		}
	}
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
