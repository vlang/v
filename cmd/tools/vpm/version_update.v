module main

import os
import v.vmod

fn project_has_ranges(manifest vmod.Manifest, dir string) bool {
	deps := project_deps_for_manifest(dir, manifest)
	if deps.any(is_version_range(requirement_version(it))) {
		return true
	}
	lf := read_lockfile(dir) or { return false }
	for entry in lf.modules.values() {
		if is_version_range(requirement_version(entry.requested)) {
			return true
		}
	}
	return false
}

// project_update_source uses checkout origins only for installed module names.
// Repository requirements retain their own identity even if a same-named
// directory exists in the module store.
fn project_update_source(roots []string, key string) string {
	if key.contains('://') || is_local_repository(key) || key.starts_with('git@') {
		return normalized_clone_source(key)
	}
	_, path := resolve_existing_module(roots, key) or { '', '' }
	origin := if path == '' { '' } else { checkout_origin_url(path) }
	return normalized_clone_source(if origin == '' { key } else { origin })
}

// project_update_changes associates a target with every direct requirement of
// its repository before the resolver selects a version from the first alias.
fn project_update_changes(dependencies []string, targets []string, precise string) map[string]string {
	mut changes := map[string]string{}
	roots := module_roots()
	for target in targets {
		target_key := lockfile_module_key(target)
		target_source := project_update_source(roots, target_key)
		changes[target_key] = precise
		for dependency in dependencies {
			key := lockfile_module_key(dependency)
			if key == target_key || project_update_source(roots, key) == target_source {
				changes[key] = precise
			}
		}
	}
	return changes
}

fn module_matches_update_target(m Module, target string) bool {
	key := lockfile_module_key(target)
	source := normalized_clone_source(key)
	if m.name == key || import_path_of(m.install_path) == key {
		return true
	}
	for alias in m.requested_aliases {
		alias_key := lockfile_module_key(alias)
		if alias_key == key || normalized_clone_source(alias_key) == source {
			return true
		}
	}
	return false
}

// update_versioned_project resolves the whole project before replacing checkouts.
// Partial updates prefer other locked selections but can backtrack when a new
// release changes its transitive requirements.
fn update_versioned_project(query []string) bool {
	dir := project_lockfile_dir()
	if dir == '' {
		if settings.precise != '' || settings.package != '' || settings.is_latest {
			vpm_error('`--precise`, `-p` and `--latest` require a project with a v.mod.')
			exit(1)
		}
		return false
	}
	manifest := vmod.from_file(os.join_path(dir, 'v.mod')) or {
		vpm_error(err.msg())
		exit(1)
	}
	if !project_has_ranges(manifest, dir) && settings.precise == '' && settings.package == ''
		&& !settings.is_latest {
		return false
	}
	if settings.is_locked {
		vpm_error('`v update` changes resolution; use `v install --locked` or `--frozen` to preserve it.')
		exit(1)
	}
	mut targets := query.clone()
	if settings.package != '' {
		if targets.len > 0 {
			vpm_error('use either `-p PACKAGE` or positional packages, not both.')
			exit(1)
		}
		targets = [settings.package]
	}
	if settings.precise != '' && targets.len != 1 {
		vpm_error('`--precise VERSION` requires one package (`-p PACKAGE` or a positional name).')
		exit(1)
	}
	original_dependencies := project_deps_for_manifest(dir, manifest)
	changes := project_update_changes(original_dependencies, targets, settings.precise)
	mut selector := new_install_server_selector()
	mut scope := LockScope{}
	scope.begin()
	scope.complete = true
	// Per-manifest working copies keep, for every aggregated requirement, the
	// manifest and local index it came from. `--latest` widens and rewrites each
	// manifest separately, so member constraints land in the member `v.mod`.
	mut manifests := workspace_manifests(dir, manifest)
	mut orig_lists := manifests.map(project_dependencies(it.manifest))
	mut dep_lists := [][]string{cap: orig_lists.len}
	for lst in orig_lists {
		dep_lists << lst.clone()
	}
	// Manifest indexes whose constraints `--latest` actually widened; only those
	// files are rewritten, so unaffected members stay byte-identical.
	mut dirty := map[int]bool{}
	mut dependencies := original_dependencies.clone()
	if settings.is_latest {
		for li in 0 .. dep_lists.len {
			for di in 0 .. dep_lists[li].len {
				dep_lists[li][di] = widen_for_latest(dep_lists[li][di], targets, changes)
			}
		}
		dependencies = []string{}
		mut seen := map[string]bool{}
		for lst in dep_lists {
			for dep in lst {
				if dep !in seen {
					seen[dep] = true
					dependencies << dep
				}
			}
		}
	}
	modules := resolve_module_query(dependencies, mut selector, mut scope, targets.len > 0, changes) or {
		vpm_error(err.msg())
		exit(1)
	}
	if targets.len > 0 {
		for target in targets {
			if !modules.any(module_matches_update_target(it, target)) {
				for m in modules { rmdir_all(m.tmp_path) or {} }
				vpm_error('`${target}` is not a dependency of this project.')
				exit(1)
			}
		}
	}
	mut selected := modules.clone()
	if settings.is_latest {
		for li in 0 .. dep_lists.len {
			for di in 0 .. dep_lists[li].len {
				dep := dep_lists[li][di]
				if dep == orig_lists[li][di] {
					continue
				}
				for mut m in selected {
					if dep in m.requested_aliases {
						version := version_tag(m.version) or {
							vpm_error('cannot widen `${dep}` without a semantic-version release.')
							exit(1)
						}
						widened := lockfile_module_key(dep) + '@^' + version.str()
						mut mm := manifests[li].manifest
						set_project_dependency(mut mm, di, widened)
						manifests[li].manifest = mm
						dirty[li] = true
						if m.requested == dep {
							m.requested = widened
							m.version_range = requirement_version(m.requested)
						}
					}
				}
			}
		}
	}
	resolve_and_lock(selected, scope) or {
		for m in selected { rmdir_all(m.tmp_path) or {} }
		vpm_error(err.msg())
		exit(1)
	}
	if settings.is_dry_run {
		for m in selected {
			println('${m.name}: would select ${m.version}${if m.version == '' {
				head_revision(m.tmp_path)
			} else {
				''
			}}')
			rmdir_all(m.tmp_path) or {}
		}
		return true
	}
	scope.resolved_keys = selected.map(lockfile_module_key(it.requested))
	install_modules(selected, selector.selected_url, mut scope)
	if settings.is_latest {
		// Only manifests whose constraints actually widened are rewritten, so an
		// unaffected member keeps its `v.mod` byte-identical.
		for li, wm in manifests {
			if !dirty[li] {
				continue
			}
			os.write_file(os.join_path(wm.dir, 'v.mod'), vmod.encode(wm.manifest)) or {
				vpm_error('failed to write widened constraints: ${err.msg()}')
				exit(1)
			}
		}
	}
	scope.finish()
	return true
}
