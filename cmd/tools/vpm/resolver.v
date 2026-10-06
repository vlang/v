module main

import semver

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

// topo_sort orders module names so that a module's dependencies come before it. The
// resolver needs this because a module can only be resolved once the versions of its
// dependencies are known.
fn topo_sort(candidates map[string][]VersionedDeps) []string {
	mut order := []string{}
	mut visited := map[string]bool{}
	mut visiting := map[string]bool{}
	for name in candidates.keys() {
		topo_visit(name, candidates, mut order, mut visited, mut visiting)
	}
	return order
}

fn topo_visit(node string, candidates map[string][]VersionedDeps, mut order []string, mut visited map[string]bool, mut visiting map[string]bool) {
	if visiting[node] {
		return
	}
	if visited[node] {
		return
	}
	visiting[node] = true
	for cand in candidates[node] {
		for dep_str in cand.deps {
			dep := dep_str.all_before('@').trim_space()
			if dep != '' {
				topo_visit(dep, candidates, mut order, mut visited, mut visiting)
			}
		}
	}
	visiting[node] = false
	visited[node] = true
	order << node
}

// version_ok reports whether a candidate version satisfies every constraint on its
// module and is compatible with the versions already resolved.
fn version_ok(cand VersionedDeps, name string, resolved map[string]string, constraints map[string][]Constraint, candidates map[string][]VersionedDeps) bool {
	for c in constraints[name] {
		v := semver.from(cand.version) or { return false }
		if !v.satisfies(c.range) {
			return false
		}
	}
	for dep_str in cand.deps {
		dep_name := dep_str.all_before('@').trim_space()
		dep_range := dep_str.all_after('@').trim_space()
		if dep_name in resolved {
			dep_v := semver.from(resolved[dep_name]) or { return false }
			if !dep_v.satisfies(dep_range) {
				return false
			}
		}
	}
	for resolved_name, resolved_version in resolved {
		if resolved_name == name {
			continue
		}
		for resolved_cand in candidates[resolved_name] {
			if resolved_cand.version != resolved_version {
				continue
			}
			for dep_str in resolved_cand.deps {
				dep_name := dep_str.all_before('@').trim_space()
				dep_range := dep_str.all_after('@').trim_space()
				if dep_name == name {
					v := semver.from(cand.version) or { return false }
					if !v.satisfies(dep_range) {
						return false
					}
				}
			}
		}
	}
	return true
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

// resolve_with_backtracking searches for a consistent assignment of versions. Modules
// are resolved in dependency order, each from its highest version down, and when a
// choice leads to a dead end the search backtracks to the previous module and tries
// its next version.
//
// The position map is what makes it backtracking rather than a single pass: when a
// module fails, the search steps back to the previous module and resumes it from
// where it left off, so a different earlier choice gets a different later outcome.
fn resolve_with_backtracking(candidates map[string][]VersionedDeps, constraints map[string][]Constraint) !map[string]string {
	order := topo_sort(candidates)
	mut resolved := map[string]string{}
	mut pos := map[string]int{}
	for mod_name in order {
		pos[mod_name] = 0
	}

	mut idx := 0
	for idx < order.len {
		mod_name := order[idx]
		mut cands := candidates[mod_name]
		cands.sort(a.version > b.version)

		mut found := false
		for i in 0 .. cands.len {
			if i < pos[mod_name] {
				continue
			}
			cand := cands[i]
			if version_ok(cand, mod_name, resolved, constraints, candidates) {
				resolved[mod_name] = cand.version
				pos[mod_name] = i + 1
				found = true
				break
			}
		}

		if found {
			idx++
		} else {
			resolved.delete(mod_name)
			pos[mod_name] = 0
			if idx == 0 {
				return error('failed to resolve `${mod_name}`: no version satisfies every constraint and dependency')
			}
			idx--
		}
	}
	return resolved
}
