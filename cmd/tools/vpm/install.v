module main

import os
import v.vmod
import v.help

enum InstallResult {
	installed
	failed
	skipped
}

fn vpm_install(query []string) {
	if settings.is_help {
		help.print_and_exit('vpm')
	}
	if settings.is_adopt {
		vpm_adopt(query)
		return
	}
	if settings.is_locked && query.len != 0 && !settings.is_local {
		vpm_error('`--locked` applies to installing the dependencies of a project (a directory with a `v.mod`); a plain `v install <module>` installs globally and has no lockfile to check against.',
			details: 'Run without `--locked`, or run `v install --locked` inside the project directory.'
		)
		exit(1)
	}

	mut selector := new_install_server_selector()
	dep_strings := if query.len == 0 {
		if os.exists('./v.mod') {
			// Case: `v install` was run in a directory of another V-module to install its dependencies
			// - without additional module arguments.
			println('Detected v.mod file inside the project directory. Using it...')
			manifest := vmod.from_file('./v.mod') or { panic(err) }
			if manifest.dependencies.len == 0 {
				println('Nothing to install.')
				exit(0)
			}
			manifest.dependencies
		} else {
			vpm_error('specify at least one module for installation.',
				details: 'example: `v install publisher.package` or `v install https://github.com/owner/repository`'
			)
			exit(2)
		}
	} else {
		query
	}

	// Anchor the run to the project in scope, so that the resolved revisions of
	// its dependencies are recorded in `v.mod.lock` once everything installed.
	mut scope := LockScope{}
	if settings.is_local || query.len == 0 {
		scope.begin()
	}

	mut modules, parse_errors := parse_query(dep_strings, mut selector, mut scope)
	// The dependencies of a project have to resolve completely. The ones that
	// did are still installed, but the run fails, and records no lockfile.
	is_incomplete := parse_errors > 0 && scope.active

	installed_modules := get_installed_modules()

	vpm_log(@FILE_LINE, @FN, 'Queried Modules: ${modules}')
	vpm_log(@FILE_LINE, @FN, 'Installed modules: ${installed_modules}')

	if installed_modules.len > 0 && settings.is_once {
		num_to_install := modules.len
		mut already_installed := []string{}
		if modules.len > 0 {
			mut i_deleted := []int{}
			for i, m in modules {
				if m.name in installed_modules {
					already_installed << m.name
					i_deleted << i
				}
			}
			for i in i_deleted.reverse() {
				modules.delete(i)
			}
		}
		if already_installed.len > 0 {
			verbose_println('Already installed modules: ${already_installed}')
			if already_installed.len == num_to_install {
				println('All modules are already installed.')
				exit(if is_incomplete { 1 } else { 0 })
			}
		}
	}

	install_modules(modules, selector.selected_url, mut scope)
	if is_incomplete {
		vpm_error('failed to install ${parse_errors} module(s) of the project; not recording `${lockfile_name}`.')
		exit(1)
	}
	scope.finish()
}

fn install_modules(modules []Module, selected_server_url string, mut scope LockScope) {
	vpm_log(@FILE_LINE, @FN, 'modules: ${modules}')
	mut errors := 0
	for m in modules {
		vpm_log(@FILE_LINE, @FN, 'module: ${m}')
		match m.install(mut scope) {
			.installed {}
			.failed {
				errors++
				continue
			}
			.skipped {
				continue
			}
		}

		if !m.is_external {
			increment_module_download_count(m.name, selected_server_url) or {
				vpm_error('failed to increment the download count for `${m.name}`',
					details: err.msg()
				)
				errors++
			}
		}
		println('Installed `${m.name}` in ${m.install_path_fmted} .')
		m.warn_on_normalized_name()
	}
	if errors > 0 {
		exit(1)
	}
}

// Module names may contain characters that are not valid in V import paths, e.g. `-`.
// Those are normalized away when the module is placed into `vmodules`, so point out
// the resulting import path instead of leaving the mismatch for the compiler to report.
fn (m Module) warn_on_normalized_name() {
	// Direct HTTP installs intentionally add the repository owner to the install path. That prefix
	// is not a normalization of the manifest name and should not trigger this warning on its own.
	if !m.name_was_normalized() {
		return
	}
	import_path := import_path_of(m.install_path)
	vpm_warn('`${m.name}` is not a valid V import path, it was installed as `${import_path}`.',
		details: m.normalized_name_warning_details(import_path)
	)
}

fn (m Module) normalized_name_warning_details(import_path string) string {
	mut details := 'Use `${import_path}` as the normalized import prefix (for example, `import ${import_path}` when the package root is a module).'
	if m.manifest_name_was_normalized() {
		details += '\nConsider renaming the `name` field in the `v.mod` of the module.'
	}
	return details
}

fn (m Module) name_was_normalized() bool {
	normalized_name := direct_install_mod_path('', m.name).replace(os.path_separator, '.')
	return normalized_name != m.name
}

fn (m Module) manifest_name_was_normalized() bool {
	if m.manifest.name == '' {
		return false
	}
	normalized_name := direct_install_mod_path('', m.manifest.name).replace(os.path_separator, '.')
	return normalized_name != m.manifest.name
}

fn (m Module) install(mut scope LockScope) InstallResult {
	defer {
		os.rmdir_all(m.tmp_path) or {}
	}
	if !install_path_is_in_vmodules(m.install_path, settings.vmodules_path) {
		vpm_error('refusing to install `${m.name}` outside the V modules directory.')
		return .failed
	}
	if install_path_has_symlinked_ancestor(m.install_path, settings.vmodules_path) {
		vpm_error('refusing to install `${m.name}` inside a symlinked module namespace.')
		return .failed
	}
	if ancestor := vcs_backed_install_ancestor(m.install_path, settings.vmodules_path) {
		vpm_error('refusing to install `${m.name}` inside existing module `${fmt_mod_path(ancestor)}`.')
		return .failed
	}
	// Run this check unconditionally — `m.is_installed` is computed via
	// `git ls-remote`, which itself fails when `.git` is corrupted or
	// inaccessible, so relying on it here would skip the guard in exactly
	// the cases we most need to fail closed.
	reason := local_git_changes_reason(m.install_path)
	if reason != '' {
		vpm_error('refusing to install `${m.name}`: `${m.install_path_fmted}` has local git work that would be lost (${reason}). Commit and push your changes, or remove the directory manually before retrying.')
		exit(1)
	}
	if m.is_installed {
		// Case: installed, but not an explicit version. Update instead of continuing the installation,
		// unless the lockfile of the project in scope records the module: installs honor
		// the locked revision, and moving it forward is what `v update` is for.
		if m.version == '' && m.installed_version == '' {
			// The lock only applies while the project still asks for the
			// dependency string and source it was recorded under; a changed one
			// falls through to the update below and is then locked anew.
			if entry := scope.locked_entry(m.requested, m.url) {
				installed_revision := head_revision(m.install_path)
				if installed_revision == entry.revision {
					verbose_println('`${m.name}` is already installed at the locked revision `${entry.revision}`.')
					return .skipped
				}
				// The installed checkout drifted from the locked revision: put the
				// project back on the lock, fetching the revision when the checkout
				// is older than it. The local-changes guard above already refused
				// checkouts holding work that would be lost.
				println('Restoring `${m.name}` to the locked revision `${entry.revision}` ...')
				(m.vcs or { settings.vcs }).checkout(m.install_path, entry.revision) or {
					vpm_error('failed to restore `${m.name}` to the locked revision `${entry.revision}` in `${m.install_path_fmted}`: ${err.msg()}')
					return .failed
				}
				return .skipped
			}
			if m.is_external && m.url.starts_with('http://') {
				vpm_update([
					m.install_path.all_after(settings.vmodules_path).trim_left(os.path_separator).replace(os.path_separator, '.'),
				])
			} else {
				vpm_update([m.name])
			}
			// The module sits at a new revision now, so what the lockfile of the
			// project records for it has to follow.
			scope.record(m)
			return .skipped
		}
		// Case: installed, but conflicting. Confirmation or -[-f]orce flag required.
		if settings.is_force || m.confirm_install() {
			if !vpm_owns_module_dir(m.install_path) {
				vpm_error('refusing to replace `${m.name}`: `${m.install_path_fmted}` was not installed by VPM.',
					details: not_installed_by_vpm_details()
				)
				return .failed
			}
			m.remove() or {
				vpm_error('failed to remove `${m.name}`.', details: err.msg())
				return .failed
			}
		} else {
			return .skipped
		}
	}
	if os.exists(m.install_path) {
		vpm_error('refusing to install `${m.name}`: destination `${m.install_path_fmted}` already exists.')
		return .failed
	}
	println('Installing `${m.name}`...')
	// When the module should be relocated into a subdirectory we need to make sure
	// it exists to not run into permission errors.
	parent_dir := m.install_path.all_before_last(os.path_separator)
	if !os.exists(parent_dir) {
		os.mkdir_all(parent_dir) or {
			vpm_error('failed to create module directory for `${m.name}`.', details: err.msg())
			return .failed
		}
	}
	os.mv(m.tmp_path, m.install_path) or {
		vpm_error('failed to install `${m.name}`.', details: err.msg())
		return .failed
	}
	if settings.is_local {
		// The local root is shared with the project's own modules, so this record is
		// the only thing that will later tell VPM the directory is one it may touch.
		// An install it cannot own could neither be updated nor removed afterwards,
		// which is worse than no install at all, so undo it.
		record_local_install(m.install_path) or {
			vpm_error('failed to record the local installation of `${m.name}`.',
				details: err.msg()
			)
			rmdir_all(m.install_path) or {
				vpm_error('failed to undo the unrecorded installation at `${m.install_path_fmted}`.',
					details: err.msg()
				)
			}
			return .failed
		}
	}
	scope.record(m)
	return .installed
}

fn install_path_is_in_vmodules(install_path string, vmodules_path string) bool {
	vmodules_root := real_path_with_missing_suffix(vmodules_path)
	resolved_install_path := real_path_with_missing_suffix(install_path)
	return path_is_below(resolved_install_path, vmodules_root)
}

fn path_is_below(path string, root string) bool {
	if path == root {
		return false
	}
	boundary := if root.ends_with(os.path_separator) { root } else { root + os.path_separator }
	return path.starts_with(boundary)
}

fn install_path_has_symlinked_ancestor(install_path string, vmodules_path string) bool {
	vmodules_root := os.abs_path(vmodules_path)
	mut parent := os.dir(os.abs_path(install_path))
	for path_is_below(parent, vmodules_root) {
		if os.is_link(parent) {
			return true
		}
		next := os.dir(parent)
		if next == parent {
			break
		}
		parent = next
	}
	return false
}

fn vcs_backed_install_ancestor(install_path string, vmodules_path string) ?string {
	vmodules_root := real_path_with_missing_suffix(vmodules_path)
	mut parent := real_path_with_missing_suffix(os.dir(install_path))
	for path_is_below(parent, vmodules_root) {
		if vcs_used_in_dir(parent) != none {
			return parent
		}
		next := os.dir(parent)
		if next == parent {
			break
		}
		parent = next
	}
	return none
}

fn real_path_with_missing_suffix(path string) string {
	mut existing := path
	mut missing := []string{}
	for !os.exists(existing) {
		parent := os.dir(existing)
		if parent == existing {
			break
		}
		missing << os.file_name(existing)
		existing = parent
	}
	mut resolved := os.real_path(existing)
	for i := missing.len - 1; i >= 0; i-- {
		resolved = os.join_path(resolved, missing[i])
	}
	return resolved
}

fn (m Module) confirm_install() bool {
	if m.installed_version == m.version {
		println('Module `${m.name}${at_version(m.installed_version)}` is already installed, use --force to overwrite.')
		return false
	} else {
		println('Module `${m.name}${at_version(m.installed_version)}` is already installed at `${m.install_path_fmted}`.')
		if settings.fail_on_prompt {
			vpm_error('VPM should not have entered a confirmation prompt.')
			exit(1)
		}
		install_version := at_version(if m.version == '' { 'latest' } else { m.version })
		input := os.input('Replace it with `${m.name}${install_version}`? [Y/n]: ')
		match input.trim_space().to_lower() {
			'', 'y' {
				return true
			}
			else {
				verbose_println('Skipping `${m.name}`.')
				return false
			}
		}
	}
}

// local_git_changes_reason returns a non-empty reason string if `path` is a
// git repository whose contents should not be silently overwritten — either
// because it has uncommitted/unpushed work, or because git could not be
// queried at all (in which case we fail closed rather than risk data loss).
// Returns '' when the path is safe to overwrite (not a git repo, or a clean
// repo fully in sync with its remote).
fn local_git_changes_reason(path string) string {
	if !os.exists(os.join_path(path, '.git')) {
		return ''
	}
	status := os.exec_opt(['git', '-C', path, 'status', '--porcelain']) or {
		return 'failed to run `git status`: ${err.msg()}'
	}
	if status.output.trim_space() != '' {
		return 'uncommitted changes detected'
	}
	// Include `HEAD` so commits made on a detached HEAD (e.g. after
	// `git clone -b <tag>`, the layout vpm uses for versioned installs) are
	// also detected. `--branches` alone only walks local branch refs.
	// Negate `--tags` as well: vpm's versioned installs clone with `-b <tag>`,
	// which leaves HEAD detached at a tag without creating a remote tracking
	// branch, so HEAD would otherwise appear as unpushed even on a pristine
	// clone.
	unpushed := os.exec_opt(['git', '-C', path, 'rev-list', 'HEAD', '--branches', '--not', '--remotes',
		'--tags']) or {
		return 'failed to run `git rev-list`: ${err.msg()}'
	}
	if unpushed.output.trim_space() != '' {
		return 'unpushed local commits detected'
	}
	return ''
}

fn (m Module) remove() ! {
	verbose_println('Removing `${m.name}` from `${m.install_path_fmted}`...')
	remove_installed_dir(m.install_path)!
	verbose_println('Removed `${m.name}`.')
}
