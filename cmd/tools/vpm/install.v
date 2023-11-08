module main

import os
import v.vmod
import v.help

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

	mut vpm_modules := modules.filter(!it.is_external)
	mut external_modules := modules.filter(it.is_external)
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
		vpm_install_from_vcs(external_modules)
	}
}

fn vpm_install_from_vpm(modules []Module) {
	vpm_log(@FILE_LINE, @FN, 'modules: ${modules}')
	mut errors := 0
	idents := modules.map(it.name)
	for m in modules {
		vpm_log(@FILE_LINE, @FN, 'module: ${m}')
		vcs := if m.vcs != '' {
			supported_vcs[m.vcs] or {
				vpm_error('skipping `${m.name}`, since it uses an unsupported version control system `${m.vcs}`.')
				errors++
				continue
			}
		} else {
			supported_vcs['git']
		}
		vcs.is_executable() or {
			vpm_error(err.msg())
			errors++
			continue
		}
		if !m.confirm_install() or {
			vpm_error('failed to replace `${m.name}`.', details: err.msg())
			errors++
			continue
		} {
			continue
		}
		m.install(vcs) or {
			vpm_error(err.msg())
			errors++
			continue
		}
		increment_module_download_count(m.name) or {
			vpm_error('failed to increment the download count for `${m.name}`', details: err.msg())
			errors++
		}
		manifest := vmod.from_file(os.join_path(m.install_path, 'v.mod')) or {
			vpm_error(err.msg())
			errors++
			continue
		}
		resolve_dependencies(manifest.name, manifest.dependencies, idents)
	}
	if errors > 0 {
		exit(1)
	}
}

fn vpm_install_from_vcs(modules []Module) {
	vpm_log(@FILE_LINE, @FN, 'modules: ${modules}')
	vcs := supported_vcs[settings.vcs]
	vcs.is_executable() or {
		vpm_error(err.msg())
		exit(1)
	}
	urls := modules.map(it.url)
	mut errors := 0
	for m in modules {
		vpm_log(@FILE_LINE, @FN, 'module: ${m}')
		if !m.confirm_install() or {
			vpm_error('failed to replace `${m.name}`.', details: err.msg())
			errors++
			continue
		} {
			continue
		}
		m.install(vcs) or {
			vpm_error(err.msg())
			errors++
			continue
		}
		manifest := vmod.from_file(os.join_path(m.install_path, 'v.mod')) or {
			vpm_error(err.msg())
			errors++
			continue
		}
		vpm_log(@FILE_LINE, @FN, 'manifest: ${manifest}')
		final_path := os.real_path(os.join_path(settings.vmodules_path, manifest.name))
		if m.install_path != final_path {
			verbose_println('Relocating `${m.name} (${m.install_path})` to `${manifest.name} (${final_path})`...')
			if os.exists(final_path) {
				println('Target directory for `${m.name} (${final_path})` already exists.')
				input := os.input('Replace it with the module directory? [Y/n]: ')
				match input.to_lower() {
					'', 'y' {
						mut err_msg := ''
						$if windows {
							os.execute_opt('rd /s /q ${final_path}') or { err_msg = err.msg() }
						} $else {
							os.rmdir_all(final_path) or { err_msg = err.msg() }
						}
						if err_msg != '' {
							vpm_error('failed to remove `${final_path}`.', details: err_msg)
							errors++
							continue
						}
					}
					else {
						verbose_println('Skipping `${m.name}`.')
						continue
					}
				}
			}
			// When the module should be relocated into a subdirectory we need to make sure
			// it exists to not run into permission errors.
			if m.install_path.count(os.path_separator) < final_path.count(os.path_separator)
				&& !os.exists(final_path) {
				os.mkdir_all(final_path) or {
					vpm_error('failed to create directory for `${manifest.name}`.',
						details: err.msg()
					)
					errors++
					continue
				}
			}
			os.mv(m.install_path, final_path) or {
				errors++
				vpm_error('failed to relocate module `${m.name}`.', details: err.msg())
				os.rmdir_all(m.install_path) or {
					vpm_error('failed to remove `${m.install_path}`.', details: err.msg())
					errors++
					continue
				}
				continue
			}
			verbose_println('Relocated `${m.name}` to `${manifest.name}`.')
		}
		resolve_dependencies(manifest.name, manifest.dependencies, urls)
	}
	if errors > 0 {
		exit(1)
	}
}

fn (m Module) confirm_install() !bool {
	// Case: define version, proceed if not installed.
	v := m.installed_version or { return true }
	// Case: installed, but not an explicit version.
	if m.version == '' && v == '' {
		vpm_update([m.name])
		return false
	}
	// Case: installed, conflicting.
	if settings.is_force {
		m.remove()!
		return true
	} else if v == m.version {
		println('Module `${m.name}${at_version(v)}` is already installed.')
		return false
	} else {
		install_version := if m.version == '' { 'latest' } else { m.version }
		println('Module `${m.name}${at_version(v)}` is already installed at `${m.install_path}`.')
		input := os.input('Replace it with `${m.name}${at_version(install_version)}`? [Y/n]: ')
		match input.to_lower() {
			'', 'y' {
				m.remove()!
				return true
			}
			else {
				verbose_println('Skipping `${m.name}`.')
				return false
			}
		}
	}
}

fn (m Module) remove() ! {
	verbose_println('Removing `${m.name}` from `${m.install_path}`...')
	$if windows {
		os.execute_opt('rd /s /q ${m.install_path}')!
	} $else {
		os.rmdir_all(m.install_path)!
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
	println('Installing `${m.name}`...')
	verbose_println('  cloning from `${m.url}` to `${m.install_path}`')
	res := os.execute_opt(cmd) or {
		vpm_log(@FILE_LINE, @FN, 'cmd output: ${err}')
		return error('failed to install module `${m.name}`.')
	}
	vpm_log(@FILE_LINE, @FN, 'cmd output: ${res.output}')
}

fn at_version(version string) string {
	return if version != '' { '@${version}' } else { '' }
}
