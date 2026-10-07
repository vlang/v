module main

import os
import v.vmod

fn project_has_ranges(manifest vmod.Manifest, dir string) bool {
	if manifest.dependencies.any(is_version_range(requirement_version(it))) {
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
	mut manifest := vmod.from_file(os.join_path(dir, 'v.mod')) or {
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
	mut changes := map[string]string{}
	for target in targets {
		changes[lockfile_module_key(target)] = settings.precise
	}
	mut selector := new_install_server_selector()
	mut scope := LockScope{}
	scope.begin()
	scope.complete = true
	mut dependencies := manifest.dependencies.clone()
	if settings.is_latest {
		for i, dep in dependencies {
			ident := lockfile_module_key(dep)
			_, installed := resolve_existing_module(module_roots(), dep) or { '', '' }
			name := if installed == '' { ident } else { import_path_of(installed) }
			if targets.len == 0 || ident in changes || name in changes {
				constraint := requirement_version(dep)
				if constraint == '' || is_version_range(constraint) || tag_satisfies_range(constraint, '*') {
					dependencies[i] = ident + '@*'
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
			if !modules.any(lockfile_module_key(it.requested) == lockfile_module_key(target)
				|| it.name == target || import_path_of(it.install_path) == target) {
				for m in modules { rmdir_all(m.tmp_path) or {} }
				vpm_error('`${target}` is not a dependency of this project.')
				exit(1)
			}
		}
	}
	mut selected := modules.clone()
	if settings.is_latest {
		for i, dep in dependencies {
			if dep == manifest.dependencies[i] {
				continue
			}
			for mut m in selected {
				if dep in m.requested_aliases {
					version := version_tag(m.version) or {
						vpm_error('cannot widen `${dep}` without a semantic-version release.')
						exit(1)
					}
					manifest.dependencies[i] = lockfile_module_key(dep) + '@^' + version.str()
					if m.requested == dep {
						m.requested = manifest.dependencies[i]
						m.version_range = requirement_version(m.requested)
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
		os.write_file(os.join_path(dir, 'v.mod'), vmod.encode(manifest)) or {
			vpm_error('failed to write widened constraints: ${err.msg()}')
			exit(1)
		}
	}
	scope.finish()
	return true
}
