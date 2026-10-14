module main

import os
import sync.pool
import v.help
import v.vmod

struct UpdateSession {
	idents []string
}

pub struct UpdateResult {
mut:
	success bool
	// install_path is where the updated checkout lives, so that the lockfile
	// of the project in scope can refresh the entries of the updated modules.
	install_path string
}

fn vpm_update(query []string) {
	if settings.is_help {
		help.print_and_exit('update')
	}
	if settings.is_locked {
		vpm_error('`v update` changes resolution; use `v install --locked` to preserve it.')
		exit(1)
	}
	if update_versioned_project(query) {
		return
	}
	if settings.is_dry_run {
		dry_run_update(query) or {
			vpm_error(err.msg())
			exit(1)
		}
		return
	}
	idents := if query.len == 0 { get_installed_modules() } else { query.clone() }
	mut pp := pool.new_pool_processor(callback: update_module)
	ctx := UpdateSession{idents}
	pp.set_shared_context(&ctx)
	pp.work_on_items(idents)
	results := pp.get_results[UpdateResult]()
	mut errors := 0
	for res in results {
		if !res.success {
			errors++
			continue
		}
	}
	// Refresh the lock entries of the project in scope even when some module
	// failed: the ones that did update sit at new revisions already.
	refresh_lock_entries(results)
	if errors > 0 {
		exit(1)
	}
}

// detached_project_pin finds an exact project ref for the installed checkout.
fn detached_project_pin(name string, path string) string {
	dir := project_lockfile_dir()
	if dir == '' {
		return ''
	}
	origin := normalized_clone_source(checkout_origin_url(path))
	if manifest := vmod.from_file(os.join_path(dir, 'v.mod')) {
		mut dependencies := manifest.dependencies.clone()
		dependencies << manifest.unknown['dev_dependencies']
		for dependency in dependencies {
			pin := requirement_version(dependency)
			if pin == '' || is_version_range(pin) {
				continue
			}
			key := lockfile_module_key(dependency)
			mut source := key
			if is_local_repository(key) {
				local_path := os.expand_tilde_to_home(key.trim_string_left('file://'))
				source = if os.is_abs_path(local_path) {
					local_path
				} else {
					os.join_path(dir, local_path)
				}
			}
			if key == name || (origin != '' && normalized_clone_source(source) == origin) {
				return pin
			}
		}
	}
	lf := read_lockfile(dir) or { return '' }
	if origin == '' {
		return ''
	}
	revision := head_revision(path)
	for entry in lf.modules.values() {
		pin := requirement_version(entry.requested)
		if pin != '' && !is_version_range(pin) && entry.revision == revision
			&& normalized_clone_source(entry.url) == origin {
			return pin
		}
	}
	return ''
}

fn update_module(mut pp pool.PoolProcessor, idx int, _wid int) &UpdateResult {
	ident := pp.get_item[string](idx)
	install_path := get_path_of_existing_module(ident) or {
		fallback_name := get_name_from_url(ident) or { ident }
		vpm_error('failed to find path for `${fallback_name}`.', verbose: true)
		return &UpdateResult{}
	}
	if !install_path_is_in_vmodules(install_path, settings.vmodules_path) {
		vpm_error('refusing to update `${ident}`: `${fmt_mod_path(install_path)}` is outside the modules directory.',
			details: 'Run `v unlink` first to replace it.'
		)
		return &UpdateResult{}
	}
	// Derive the canonical module name from the install path so URL-based
	// updates report the registered name (e.g. `spytheman.vtray` for
	// `<vmodules>/spytheman/vtray`) instead of the bare URL-derived `vtray`.
	name := import_path_of(install_path)
	if !vpm_owns_module_dir(install_path) {
		vpm_error('refusing to update `${name}`: `${fmt_mod_path(install_path)}` was not installed by VPM.',
			details: not_installed_by_vpm_details()
		)
		return &UpdateResult{}
	}
	vcs := vcs_used_in_dir(install_path) or {
		vpm_error('failed to find version control system for `${name}`.', verbose: true)
		return &UpdateResult{}
	}
	vcs.is_executable() or {
		vpm_error(err.msg())
		return &UpdateResult{}
	}
	println('Updating module `${name}` in `${fmt_mod_path(install_path)}`...')
	if vcs == .git {
		reason := local_git_changes_reason(install_path)
		if reason != '' {
			vpm_error('refusing to update module `${name}` in `${install_path}`: ${reason}.')
			return &UpdateResult{}
		}
	}
	args := vcs_info[vcs].args
	mut commands := args.update.clone()
	if vcs == .git && head_is_detached(install_path) {
		// Exact project refs stay pinned, as do their lockfile entries.
		request := detached_project_pin(name, install_path)
		if request != '' {
			println('Skipping module `${name}` pinned at `${request}`.')
			return &UpdateResult{
				success: true
			}
		}
		// A tagged or locked checkout has no branch to pull. FETCH_HEAD records
		// the fetched default branch even when origin/HEAD is absent.
		commands = [['fetch', '--depth', '1', 'origin', 'HEAD'], ['checkout', '--quiet', 'FETCH_HEAD']]
	}
	// `head_revision` is git-only and returns '' elsewhere, so this compares
	// revisions only where revisions mean something.
	old_revision := head_revision(install_path)
	for update in commands {
		vpm_log(@FILE_LINE, @FN, 'update command: ${update}')
		os.exec_opt([vcs.str(), args.path, install_path, ...update]) or {
			vpm_error('failed to update module `${name}` in `${install_path}`.',
				details: err.msg()
			)
			return &UpdateResult{}
		}
	}
	if vcs == .git {
		update_git_submodules(install_path) or {
			vpm_error('failed to update the submodules of module `${name}` in `${install_path}`.',
				details: err.msg()
			)
			return &UpdateResult{}
		}
	}
	if old_revision != '' {
		if head_revision(install_path) == old_revision {
			println('Skipped module `${ident}`. Already up to date.')
		} else {
			println('Updated module `${ident}`.')
		}
	} else {
		println('Updated module `${ident}`.')
	}
	// Don't bail if the download count increment has failed.
	increment_module_download_count(name, '') or { vpm_error(err.msg(), verbose: true) }
	ctx := unsafe { &UpdateSession(pp.get_shared_context()) }
	vpm_log(@FILE_LINE, @FN, 'ident: ${ident}; ctx: ${ctx}')
	resolve_dependencies(get_manifest(install_path), ctx.idents)
	return &UpdateResult{
		success:      true
		install_path: install_path
	}
}

fn dry_run_update(query []string) ! {
	idents := if query.len == 0 { get_installed_modules() } else { query.clone() }
	mut would_update := 0
	for ident in idents {
		path := precise_update_path(ident)!
		vcs := vcs_used_in_dir(path) or { return error('no VCS for `${ident}`') }
		if vcs != .git { return error('--dry-run is supported only for Git repositories') }
		url := checkout_origin_url(path)
		if url == '' { return error('no origin for `${ident}`') }
		remote := os.exec(['git', 'ls-remote', '--', url, 'HEAD'])
		if remote.exit_code != 0 {
			return error('failed to discover origin HEAD for `${ident}`: ${remote.output.trim_space()}')
		}
		mut revision := ''
		for line in remote.output.split_into_lines() {
			fields := line.split('\t')
			if fields.len == 2 && fields[1] == 'HEAD' { revision = fields[0] }
		}
		if revision.len != 40 { return error('origin for `${ident}` has no HEAD revision') }
		local := head_revision(path)
		if local.len != 40 { return error('failed to read HEAD for `${ident}`') }
		if local == revision {
			println('${ident}: up to date')
		} else {
			println('${ident}: would update (${local[..7]} -> ${revision[..7]})')
			would_update++
		}
	}
	if would_update == 0 { println('All modules are up to date.') }
}

fn precise_update_path(module string) !string {
	path := get_path_of_existing_module(module) or { return error('failed to find path for `${module}`') }
	if !install_path_is_in_vmodules(path, settings.vmodules_path) {
		return error('refusing to update `${module}` outside the modules directory')
	}
	if !vpm_owns_module_dir(path) {
		return error('refusing to update `${module}`: it was not installed by VPM')
	}
	return path
}
