module main

import os
import sync.pool
import v.help

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
	if settings.is_precise {
		if settings.pin_module != '' && query.len > 0 {
			vpm_error('--precise takes exactly one module name')
			exit(1)
		}
		module := if settings.pin_module != '' { settings.pin_module } else { query[0] }
		update_precise(module, settings.precise_version)
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

fn update_precise(module string, version string) {
	install_path := get_path_of_existing_module(module) or {
		vpm_error('failed to find path for `${module}`.')
		exit(1)
	}
	if !install_path_is_in_vmodules(install_path, settings.vmodules_path) {
		vpm_error('refusing to update `${module}`: `${fmt_mod_path(install_path)}` is outside the modules directory.')
		exit(1)
	}
	name := import_path_of(install_path)
	if !vpm_owns_module_dir(install_path) {
		vpm_error('refusing to update `${name}`: `${fmt_mod_path(install_path)}` was not installed by VPM.')
		exit(1)
	}
	vcs := vcs_used_in_dir(install_path) or {
		vpm_error('failed to find version control system for `${name}`.')
		exit(1)
	}
	if vcs != .git {
		vpm_error('--precise is only supported for git repositories.')
		exit(1)
	}
	println('Pinning module `${name}` to `${version}` in `${fmt_mod_path(install_path)}`...')
	os.exec_opt(['git', '-C', install_path, 'fetch', 'origin', '--tags']) or {
		vpm_error('failed to fetch tags for module `${name}`.', details: err.msg())
		exit(1)
	}
	os.exec_opt(['git', '-C', install_path, 'checkout', '--quiet', version]) or {
		vpm_error('failed to checkout `${version}` for module `${name}`.', details: err.msg())
		exit(1)
	}
	update_git_submodules(install_path) or {
		vpm_error('failed to update submodules for module `${name}`.', details: err.msg())
		exit(1)
	}
	increment_module_download_count(name, '') or { vpm_error(err.msg(), verbose: true) }
	println('Pinned module `${name}` to `${version}`.')
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
	if vcs == .git && head_is_detached(install_path) {
		// A checkout that is not on a branch cannot be pulled. vpm itself leaves
		// checkouts detached when it installs a locked revision or a tag, so
		// fetch and move HEAD to the default branch of the origin instead.
		os.exec_opt(['git', '-C', install_path, 'fetch', 'origin']) or {
			vpm_error('failed to fetch the origin of module `${name}` in `${install_path}`.',
				details: err.msg()
			)
			return &UpdateResult{}
		}
		old_revision := head_revision(install_path)
		os.exec_opt(['git', '-C', install_path, 'checkout', '--quiet', 'origin/HEAD']) or {
			vpm_error('failed to checkout the default branch of the origin of module `${name}` in `${install_path}`.',
				details: err.msg()
			)
			return &UpdateResult{}
		}
		update_git_submodules(install_path) or {
			vpm_error('failed to update module `${name}` in `${install_path}`.', details: err.msg())
			return &UpdateResult{}
		}
		if head_revision(install_path) == old_revision {
			println('Skipped module `${ident}`. Already up to date.')
		} else {
			println('Updated module `${ident}`.')
		}
	} else {
		args := vcs_info[vcs].args
		cmd := [vcs.str(), args.path, os.quoted_path(install_path), args.update].join(' ')
		vpm_log(@FILE_LINE, @FN, 'cmd: ${cmd}')
		res := os.exec_opt([vcs.str(), args.path, install_path,
			...(os.split_args(args.update) or { panic(err) })]) or {
			vpm_error('failed to update module `${name}` in `${install_path}`.',
				details: err.msg()
			)
			return &UpdateResult{}
		}
		vpm_log(@FILE_LINE, @FN, 'cmd output: ${res.output.trim_space()}')
		if res.output.contains('Already up to date.') {
			println('Skipped module `${ident}`. Already up to date.')
		} else {
			println('Updated module `${ident}`.')
		}
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
