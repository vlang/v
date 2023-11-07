module main

import os
import v.vmod
import v.help

struct Module {
mut:
	// Can be populated via via VPM API.
	name string
	url  string
	vcs  string
	// Based on input / environment.
	version      string
	install_path string
	external     bool
}

fn parse_query(query []string) []Module {
	mut modules := []Module{}
	mut errors := 0
	for mod in query {
		ident, version := mod.rsplit_once('@') or { mod, '' }
		if ident.starts_with('https://') {
			name := get_name_from_url(ident) or {
				vpm_error(err.msg())
				errors++
				continue
			}
			name_normalized := name.replace('-', '_').to_lower()
			modules << Module{
				name: name
				url: ident
				version: version
				install_path: os.real_path(os.join_path(settings.vmodules_path, name_normalized))
				external: true
			}
		} else {
			info := get_mod_vpm_info(ident) or {
				vpm_error(err.msg())
				errors++
				continue
			}
			name_normalized := info.name.replace('-', '_').to_lower()
			modules << Module{
				name: info.name
				url: info.url
				vcs: info.vcs
				version: version
				install_path: os.real_path(os.join_path(settings.vmodules_path, name_normalized.replace('.',
					os.path_separator)))
			}
		}
	}
	if errors > 0 && errors == query.len {
		exit(1)
	}
	return modules
}

fn vpm_install(requested_modules []string) {
	vpm_log(@FILE_LINE, @FN, 'requested_modules: ${requested_modules}')

	if settings.is_help {
		help.print_and_exit('vpm')
	}

	mut vpm_modules, mut external_modules := parse_query(if requested_modules.len == 0 {
		if os.exists('./v.mod') {
			// Case: `v install` was run in a directory of another V-module to install its dependencies
			// - without additional module arguments.
			println('Detected v.mod file inside the project directory. Using it...')
			manifest := vmod.from_file('./v.mod') or { panic(err) }
			if manifest.dependencies.len == 0 {
				println('Nothing to install.')
				exit(0)
			}
			manifest.dependencies.clone()
		} else {
			vpm_error('specify at least one module for installation.',
				details: 'example: `v install publisher.package` or `v install https://github.com/owner/repository`'
			)
			exit(2)
		}
	} else {
		requested_modules.clone()
	})

	mut vpm_modules := modules.filter(!it.external)
	mut external_modules := modules.filter(it.external)
	installed_modules := get_installed_modules()

	vpm_log(@FILE_LINE, @FN, 'VPM modules: ${vpm_modules}')
	vpm_log(@FILE_LINE, @FN, 'External modules: ${external_modules}')
	vpm_log(@FILE_LINE, @FN, 'Installed modules: ${installed_modules}')

	if installed_modules.len > 0 && settings.is_once {
		mut already_installed := []string{}
		if external_modules.len > 0 {
			mut i_deleted := []int{}
			for i, m in external_modules {
				if m.name in installed_modules {
					already_installed << m.name
					i_deleted << i
				}
			}
			for i in i_deleted.reverse() {
				external_modules.delete(i)
			}
		}
		if vpm_modules.len > 0 {
			mut i_deleted := []int{}
			for i, m in vpm_modules {
				if m.name in installed_modules {
					already_installed << m.name
					i_deleted << i
				}
			}
			for i in i_deleted.reverse() {
				vpm_modules.delete(i)
			}
		}
		if already_installed.len > 0 {
			verbose_println('Already installed modules: ${already_installed}')
			if already_installed.len == modules.len {
				println('All modules are already installed.')
				exit(0)
			}
		}
	}

	if vpm_modules.len > 0 {
		vpm_install_from_vpm(vpm_modules)
	}
	if external_modules.len > 0 {
		vpm_install_from_vcs(external_modules, supported_vcs[settings.vcs])
	}
}

fn vpm_install_from_vpm(modules []Module) {
	vpm_log(@FILE_LINE, @FN, 'modules: ${modules}')
	mut errors := 0
	idents := modules.map(it.name)
	for mod in modules {
		vpm_log(@FILE_LINE, @FN, 'mod: ${mod}')
		vcs := if mod.vcs != '' {
			supported_vcs[mod.vcs] or {
				errors++
				vpm_error('skipping `${mod.name}`, since it uses an unsupported version control system `${mod.vcs}`.')
				continue
			}
		} else {
			supported_vcs['git']
		}
		vcs.is_executable() or {
			errors++
			vpm_error(err.msg())
			continue
		}
		if os.exists(mod.install_path) {
			vpm_log(@FILE_LINE, @FN, 'exists: ${mod.install_path}')
			vpm_update([mod.name])
			continue
		}
		mod.install(vcs) or {
			errors++
			vpm_error(err.msg())
			continue
		}
		increment_module_download_count(mod.name) or {
			errors++
			vpm_error('failed to increment the download count for `${mod.name}`', details: err.msg())
		}
		manifest := vmod.from_file(os.join_path(mod.install_path, 'v.mod')) or {
			vpm_error(err.msg())
			return
		}
		deps := manifest.dependencies.filter(it !in idents)
		vpm_log(@FILE_LINE, @FN, 'manifest: ${manifest}; deps: ${deps}')
		install_dependencies(mod.name, deps)
	}
	if errors > 0 {
		exit(1)
	}
}

// TODO: if requested version differs and the current install is a version install,
// overwrite with -force or prompt + print(updating `module` to version `version`).
fn vpm_install_from_vcs(modules []Module, vcs &VCS) {
	vpm_log(@FILE_LINE, @FN, 'modules: ${modules}')
	mut errors := 0
	urls := modules.map(it.url)
	for mod in modules {
		vcs.is_executable() or {
			errors++
			vpm_error(err.msg())
			continue
		}
		if os.exists(mod.install_path) {
			vpm_update([mod.name])
			continue
		}
		mod.install(vcs) or {
			errors++
			vpm_error(err.msg())
			continue
		}
		manifest := vmod.from_file(os.join_path(mod.install_path, 'v.mod')) or {
			vpm_error(err.msg())
			return
		}
		final_path := os.real_path(os.join_path(settings.vmodules_path, manifest.name))
		if mod.install_path != final_path {
			println('Relocating module from `${mod.name}` to `${manifest.name}` (`${final_path}`) ...')
			if os.exists(final_path) {
				// TODO: detect updatability
				// TODO: either require force flag or prompt for replace if a different module with the same name is already installed.
				// TODO: check for local changes, don't overwrite by default
				// vpm_update([manifest.name])
				println('Warning module `${final_path}` already exists!')
				println('Removing module `${final_path}` ...')
				mut err_msg := ''
				$if windows {
					os.execute_opt('rd /s /q ${final_path}') or { err_msg = err.msg() }
				} $else {
					os.rmdir_all(final_path) or { err_msg = err.msg() }
				}
				if err_msg != '' {
					errors++
					vpm_error('failed to remove `${final_path}`.', details: err_msg)
					continue
				}
			}
			// When the module should be relocated into a subdirectory we need to make sure
			// it exists to not into permission errors.
			if mod.install_path.count(os.path_separator) < final_path.count(os.path_separator)
				&& !os.exists(final_path) {
				os.mkdir_all(final_path) or {
					errors++
					vpm_error('failed to create directory for `${manifest.name}` — ${err}')
					continue
				}
			}
			vpm_log(@FILE_LINE, @FN, 'mv `${mod.install_path}` to `${final_path}`')
			os.mv(mod.install_path, final_path) or {
				errors++
				vpm_error('failed to relocate module `${mod.name}`.', details: err.msg())
				os.rmdir_all(mod.install_path) or {
					errors++
					vpm_error('failed to remove `${mod.install_path}`.', details: err.msg())
					continue
				}
				continue
			}
			verbose_println('Relocated `${mod.name}` to `${manifest.name}`.')
		}
		// Filter out modules that are both contained in the input query and listed as
		// dependencies in the mod file of the module that is supposed to be installed.
		deps := manifest.dependencies.filter(it !in urls)
		vpm_log(@FILE_LINE, @FN, 'manifest: ${manifest}; deps: ${deps}')
		install_dependencies(manifest.name, deps)
	}
	if errors > 0 {
		exit(1)
	}
}

fn (m Module) install(vcs &VCS) ! {
	install_arg := if m.version != '' {
		'${vcs.args.install} --single-branch -b ${m.version}'
	} else {
		vcs.args.install
	}
	cmd := '${vcs.cmd} ${install_arg} "${m.url}" "${m.install_path}"'
	vpm_log(@FILE_LINE, @FN, 'command: ${cmd}')
	println('Installing module `${m.name}` from `${m.url}` to `${m.install_path}` ...')
	res := os.execute_opt(cmd) or {
		vpm_log(@FILE_LINE, @FN, 'cmd output: ${err}')
		return error('failed to install module `${m.name}`.')
	}
	vpm_log(@FILE_LINE, @FN, 'cmd output: ${res.output}')
}

// TODO: check for merging possiblity with resolve_dependencies after progressing with `update` functions
fn install_dependencies(name string, deps []string) {
	if deps.len > 0 {
		println('Resolving ${deps.len} dependencies for module `${name}` ...')
		verbose_println('Found dependencies: ${deps}')
		vpm_install(deps)
	}
}
