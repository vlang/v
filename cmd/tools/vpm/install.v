module main

import os
import v.vmod
import v.help

fn install(queried_modules []string, opts []string) {
	mut modules := queried_modules.clone()
	if settings.is_help {
		help.print_and_exit('install')
		exit(0)
	}
	if modules.len == 0 {
		if os.exists('./v.mod') {
			println('Detected v.mod file inside the project directory. Using it...')
			manifest := vmod.from_file('./v.mod') or { panic(err) }
			modules = manifest.dependencies.clone()
		} else {
			eprintln('´v install´ requires *at least one* module name.')
			exit(2)
		}
	}

	if '--once' in opts {
		// TODO: --once should also work for external modules.
		installed_modules := get_installed_modules()
		modules = modules.filter(it !in installed_modules)
		if modules.len == 0 {
			return
		}
	}

	external_module_names := modules.filter(it.starts_with('https://'))
	vpm_module_names := modules.filter(it !in external_module_names)

	if vpm_module_names.len > 0 {
		vpm_install_from_vpm(vpm_module_names)
	}

	if external_module_names.len > 0 {
		vcs := if '--hg' in opts { supported_vcs['hd'] } else { supported_vcs['git'] }
		vpm_install_from_vcs(external_module_names, vcs)
	}
}

fn install_module(vcs &VCS, name string, url string, final_module_path string) ! {
	cmd := '${vcs.cmd} ${vcs.install_arg} "${url}" "${final_module_path}"'
	verbose_println('      command: ${cmd}')
	println('Installing module "${name}" from "${url}" to "${final_module_path}" ...')
	res := os.execute(cmd)
	if res.exit_code != 0 {
		print_failed_cmd(cmd, res)
		return error('Failed installing module "${name}" to "${final_module_path}" .')
	}
}

fn vpm_install_from_vpm(modules []string) {
	mut errors := 0
	for n in modules {
		name := n.trim_space().replace('_', '-')
		mod := get_module_meta_info(name) or {
			errors++
			eprintln('Errors while retrieving meta data for module ${name}:')
			eprintln(err)
			continue
		}
		vcs := if mod.vcs != '' {
			supported_vcs[mod.vcs] or {
				errors++
				eprintln('Skipping module "${name}", since it uses an unsupported VCS {${mod.vcs}} .')
				continue
			}
		} else {
			supported_vcs['git']
		}
		if !ensure_vcs_is_installed(vcs) {
			errors++
			continue
		}
		minfo := mod_name_info(mod.name)
		if os.exists(minfo.final_module_path) {
			vpm_update([name])
			continue
		}
		install_module(vcs, name, mod.url, minfo.final_module_path) or {
			errors++
			eprintln(err)
			continue
		}
		increment_module_download_count(name) or {
			errors++
			eprintln('Errors while incrementing the download count for ${name}:')
		}
		resolve_dependencies(name, minfo.final_module_path, modules)
	}
	if errors > 0 {
		exit(1)
	}
}

fn vpm_install_from_vcs(modules []string, vcs &VCS) {
	mut errors := 0
	for n in modules {
		url := n.trim_space()

		first_cut_pos := url.last_index('/') or {
			errors++
			eprintln('Errors while retrieving name for module "${url}" :')
			eprintln(err)
			continue
		}

		mod_name := url.substr(first_cut_pos + 1, url.len)

		second_cut_pos := url.substr(0, first_cut_pos).last_index('/') or {
			errors++
			eprintln('Errors while retrieving name for module "${url}" :')
			eprintln(err)
			continue
		}

		repo_name := url.substr(second_cut_pos + 1, first_cut_pos)
		mut name := os.join_path(repo_name, mod_name)
		mod_name_as_path := name.replace('-', '_').to_lower()
		mut final_module_path := os.real_path(os.join_path(settings.vmodules_path, mod_name_as_path))
		if os.exists(final_module_path) {
			vpm_update([name.replace('-', '_')])
			continue
		}
		if !ensure_vcs_is_installed(vcs) {
			errors++
			continue
		}
		install_module(vcs, name, url, final_module_path) or {
			errors++
			eprintln(err)
			continue
		}
		vmod_path := os.join_path(final_module_path, 'v.mod')
		if os.exists(vmod_path) {
			manifest := vmod.from_file(vmod_path) or {
				eprintln(err)
				return
			}
			minfo := mod_name_info(manifest.name)
			if final_module_path != minfo.final_module_path {
				println('Relocating module from "${name}" to "${manifest.name}" ( "${minfo.final_module_path}" ) ...')
				if os.exists(minfo.final_module_path) {
					eprintln('Warning module "${minfo.final_module_path}" already exists!')
					eprintln('Removing module "${minfo.final_module_path}" ...')
					os.rmdir_all(minfo.final_module_path) or {
						errors++
						eprintln('Errors while removing "${minfo.final_module_path}" :')
						eprintln(err)
						continue
					}
				}
				os.mv(final_module_path, minfo.final_module_path) or {
					errors++
					eprintln('Errors while relocating module "${name}" :')
					eprintln(err)
					os.rmdir_all(final_module_path) or {
						errors++
						eprintln('Errors while removing "${final_module_path}" :')
						eprintln(err)
						continue
					}
					continue
				}
				println('Module "${name}" relocated to "${manifest.name}" successfully.')
				publisher_dir := final_module_path.all_before_last(os.path_separator)
				if os.is_dir_empty(publisher_dir) {
					os.rmdir(publisher_dir) or {
						errors++
						eprintln('Errors while removing "${publisher_dir}" :')
						eprintln(err)
					}
				}
				final_module_path = minfo.final_module_path
			}
			name = manifest.name
		}
		resolve_dependencies(name, final_module_path, modules)
	}
	if errors > 0 {
		exit(1)
	}
}
