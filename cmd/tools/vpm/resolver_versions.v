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

// topo_sort orders module names so that a module's dependencies come before it. The
// resolver prefers this order to check dependency versions early. Cycles are checked
// in both directions as their candidate versions become known.
fn topo_sort(candidates map[string][]VersionedDeps) []string {
	mut order := []string{}
	mut visited := map[string]bool{}
	mut visiting := map[string]bool{}
	for name in candidates.keys().sorted() {
		topo_visit(name, candidates, mut order, mut visited, mut visiting)
	}
	return order
}

fn topo_visit(node string, candidates map[string][]VersionedDeps, mut order []string, mut visited map[string]bool, mut visiting map[string]bool) {
	if node !in candidates {
		return
	}
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
fn version_ok(cand VersionedDeps, name string, resolved map[string]VersionedDeps, candidates map[string][]VersionedDeps, constraints map[string][]Constraint) bool {
	v := semver.from(cand.version) or { return false }
	for c in constraints[name] {
		if !v.satisfies(c.range) {
			return false
		}
	}
	for dep_str in cand.deps {
		dep_name := dep_str.all_before('@').trim_space()
		dep_range := if dep_str.contains('@') { dep_str.all_after('@').trim_space() } else { '' }
		if dep_name !in candidates {
			return false
		}
		if dep_name == name {
			if !v.satisfies(dep_range) {
				return false
			}
		} else if dep_name in resolved {
			dep_v := semver.from(resolved[dep_name].version) or { return false }
			if !dep_v.satisfies(dep_range) {
				return false
			}
		}
	}
	// A cycle can place a requiring module first. Check its selected dependency
	// against the new candidate too, instead of accepting an unchecked edge.
	for other_name, other in resolved {
		if other_name == name {
			continue
		}
		for dep_str in other.deps {
			if dep_str.all_before('@').trim_space() == name {
				dep_range := if dep_str.contains('@') {
					dep_str.all_after('@').trim_space()
				} else {
					''
				}
				if !v.satisfies(dep_range) {
					return false
				}
			}
		}
	}
	return true
}

fn compare_candidate_versions(a &VersionedDeps, b &VersionedDeps) int {
	av := semver.from(a.version) or { return 0 }
	bv := semver.from(b.version) or { return 0 }
	if av > bv {
		return -1
	}
	if av < bv {
		return 1
	}
	return compare_strings(a.version, b.version)
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

// resolve_with_backtracking searches for a consistent assignment of versions. Modules
// are resolved in dependency order, each from its highest version down, and when a
// choice leads to a dead end the search backtracks to the previous module and tries
// its next version.
//
// The position map is what makes it backtracking rather than a single pass: when a
// module fails, the search steps back to the previous module and resumes it from
// where it left off, so a different earlier choice gets a different later outcome.
fn resolve_with_backtracking(candidates map[string][]VersionedDeps, constraints map[string][]Constraint) !map[string]string {
	for name, requirements in constraints {
		if name !in candidates {
			return error('failed to resolve `${name}`: no candidate versions were supplied')
		}
		for requirement in requirements {
			if !semver.is_valid_range(requirement.range) {
				return error('invalid version range `${requirement.range}` required by `${requirement.required_by}` for `${name}`')
			}
		}
	}
	for name, versions in candidates {
		mut seen_versions := map[string]bool{}
		for candidate in versions {
			_ := semver.from(candidate.version) or {
				return error('invalid candidate version `${candidate.version}` for `${name}`')
			}
			if candidate.version in seen_versions {
				return error('duplicate candidate version `${candidate.version}` for `${name}`')
			}
			seen_versions[candidate.version] = true
			for dep in candidate.deps {
				if dep.all_before('@').trim_space() == '' {
					return error('invalid dependency `${dep}` for `${name}`')
				}
				range := if dep.contains('@') { dep.all_after('@').trim_space() } else { '' }
				if !semver.is_valid_range(range) {
					return error('invalid version range `${range}` required by `${name}`')
				}
			}
		}
	}
	order := topo_sort(candidates)
	mut resolved := map[string]VersionedDeps{}
	mut pos := map[string]int{}
	for mod_name in order {
		pos[mod_name] = 0
	}

	mut idx := 0
	for idx < order.len {
		mod_name := order[idx]
		mut cands := candidates[mod_name].clone()
		cands.sort_with_compare(compare_candidate_versions)

		mut found := false
		for i in 0 .. cands.len {
			if i < pos[mod_name] {
				continue
			}
			cand := cands[i]
			if version_ok(cand, mod_name, resolved, candidates, constraints) {
				resolved[mod_name] = cand
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
	mut versions := map[string]string{}
	for name, candidate in resolved {
		versions[name] = candidate.version
	}
	return versions
}

// Candidate propagation metadata is retained for a future conflict-driven solver.

struct PartialSolution {
mut:
	assignments       map[string]string
	incompatibilities map[string]map[string][]string
}

fn new_partial_solution() PartialSolution {
	return PartialSolution{
		assignments:       map[string]string{}
		incompatibilities: map[string]map[string][]string{}
	}
}

fn (ps &PartialSolution) is_compatible(name string, version string) bool {
	if versions := ps.incompatibilities[name] {
		if reasons := versions[version] {
			if reasons.len > 0 {
				return false
			}
		}
	}
	return true
}

fn (mut ps PartialSolution) add_incompatibility(name string, version string, reason string) {
	if name !in ps.incompatibilities {
		ps.incompatibilities[name] = map[string][]string{}
	}
	if version !in ps.incompatibilities[name] {
		ps.incompatibilities[name][version] = []string{}
	}
	ps.incompatibilities[name][version] << reason
}

fn (mut ps PartialSolution) assign(name string, version string) {
	ps.assignments[name] = version
}

fn (ps &PartialSolution) get(name string) string {
	return ps.assignments[name]
}

fn (ps &PartialSolution) is_assigned(name string) bool {
	return name in ps.assignments
}

// unit_propagation processes constraints and derives new assignments.
// When a module has only one compatible version left, it is assigned.
fn unit_propagation(candidates map[string][]VersionedDeps, constraints map[string][]Constraint, mut ps PartialSolution) !bool {
	mut changed := true
	for changed {
		changed = false
		for name, cands in candidates {
			if ps.is_assigned(name) {
				continue
			}
			mut compatible := []string{}
			for cand in cands {
				if !ps.is_compatible(name, cand.version) {
					continue
				}
				mut ok := true
				for c in constraints[name] {
					v := semver.from(cand.version) or {
						ok = false
						break
					}
					if !v.satisfies(c.range) {
						ok = false
						break
					}
				}
				if ok {
					compatible << cand.version
				}
			}
			if compatible.len == 0 {
				return false
			}
			if compatible.len == 1 {
				ps.assign(name, compatible[0])
				changed = true
			}
		}
	}
	return true
}

// conflict_resolution handles the case where no compatible version exists.
// It identifies the conflicting constraints and reports them.
fn conflict_resolution(name string, candidates []VersionedDeps, constraints []Constraint, ps PartialSolution) !string {
	mut msg := 'failed to resolve `${name}`:'
	for c in constraints {
		msg += '\n  ${c.required_by} requires ${c.range}'
	}
	for cand in candidates {
		if !ps.is_compatible(name, cand.version) {
			msg += '\n  ${cand.version} is incompatible: ${ps.incompatibilities[name][cand.version].join(', ')}'
		}
	}
	return error(msg)
}

// resolve_with_pubgrub preserves the candidate API while checking the complete graph.
// Context-dependent conflict learning is not implemented yet; use the consistent
// search instead of independently accepting incompatible dependency versions.
fn resolve_with_pubgrub(candidates map[string][]VersionedDeps, constraints map[string][]Constraint) !map[string]string {
	return resolve_with_backtracking(candidates, constraints)!
}
