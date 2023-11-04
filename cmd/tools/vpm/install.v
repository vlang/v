module main

import os
import v.vmod
import v.help
import net.urllib

fn vpm_install(requested_modules []string, opts []string) {
	if settings.is_help {
		help.print_and_exit('vpm')
	}

	modules := if requested_modules.len == 0 {
		// Run `v install` in a directory of another V module without passing modules as arguments
		// to install its dependencies.
		if os.exists('./v.mod') {
			println('Detected v.mod file inside the project directory. Using it...')
			manifest := vmod.from_file('./v.mod') or { panic(err) }
			if manifest.dependencies.len == 0 {
				println('Nothing to install.')
				exit(0)
			}
			manifest.dependencies.clone()
		} else {
			eprintln('Specify a module for installation.')
			help.print_and_exit('vpm')
			return
		}
	} else {
		requested_modules.clone()
	}

	mut external_modules := modules.filter(it.starts_with('https://'))
	mut vpm_modules := modules.filter(it !in external_modules)
	installed_modules := get_installed_modules()

	if installed_modules.len > 0 && '--once' in opts {
		mut already_installed := []string{}
		if external_modules.len > 0 {
			mut i_deleted := []int{}
			for i, raw_url in external_modules {
				url := urllib.parse(raw_url) or {
					eprintln('Errors while parsing module url "${raw_url}" : ${err}')
					continue
				}
				mod_name := url.path.all_after_last('/').replace('.git', '')
				if mod_name in installed_modules {
					already_installed << mod_name
					i_deleted << i
				}
			}
			for i in i_deleted.reverse() {
				external_modules.delete(i)
			}
		}
		if vpm_modules.len > 0 {
			mut i_deleted := []int{}
			for i, mod_name in vpm_modules {
				if mod_name in installed_modules {
					already_installed << mod_name
					i_deleted << i
					continue
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
		vcs := if '--hg' in opts { supported_vcs['hg'] } else { supported_vcs['git'] }
		vpm_install_from_vcs(external_modules, vcs)
	}
}

fn install_module(vcs &VCS, name string, url string, final_module_path string) ! {
	cmd := '${vcs.cmd} ${vcs.args.install} "${url}" "${final_module_path}"'
	verbose_println('      command: ${cmd}')
	println('Installing module "${name}" from "${url}" to "${final_module_path}" ...')
	os.execute_opt(cmd) or {
		verbose_println('      command output: ${err}')
		return error('Failed installing module "${name}" to "${final_module_path}" .')
	}
}

fn vpm_install_from_vpm(module_names []string) {
	mut errors := 0
	for n in module_names {
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
		ensure_vcs_is_installed(vcs) or {
			errors++
			eprintln(err)
			continue
		}
		minfo := get_mod_name_info(mod.name)
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
		resolve_dependencies(name, minfo.final_module_path, module_names)
	}
	if errors > 0 {
		exit(1)
	}
}

fn vpm_install_from_vcs(modules []string, vcs &VCS) {
	mut errors := 0
	for raw_url in modules {
		url := urllib.parse(raw_url) or {
			errors++
			eprintln('Errors while parsing module url "${raw_url}" : ${err}')
			continue
		}
		// Module identifier based on URL.
		// E.g.: `https://github.com/owner/awesome-v-project` -> `owner/awesome_v_project`
		mut ident := url.path#[1..].replace('-', '_').replace('.git', '')
		owner, repo_name := ident.split_once('/') or {
			errors++
			eprintln('Errors while retrieving module name for: "${url}"')
			continue
		}
		mut final_module_path := os.real_path(os.join_path(settings.vmodules_path, owner.to_lower(),
			repo_name.to_lower()))
		if os.exists(final_module_path) {
			vpm_update([ident])
			continue
		}
		ensure_vcs_is_installed(vcs) or {
			errors++
			eprintln(err)
			continue
		}
		install_module(vcs, repo_name, url.str(), final_module_path) or {
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
			minfo := get_mod_name_info(manifest.name)
			if final_module_path != minfo.final_module_path {
				println('Relocating module from "${ident}" to "${manifest.name}" ("${minfo.final_module_path}") ...')
				if os.exists(minfo.final_module_path) {
					eprintln('Warning module "${minfo.final_module_path}" already exists!')
					eprintln('Removing module "${minfo.final_module_path}" ...')
					mut err_msg := ''
					$if windows {
						os.execute_opt('rd /s /q ${minfo.final_module_path}') or {
							err_msg = err.msg()
						}
					} $else {
						os.rmdir_all(minfo.final_module_path) or { err_msg = err.msg() }
					}
					if err_msg != '' {
						errors++
						eprintln('Errors while removing "${minfo.final_module_path}" :')
						eprintln(err_msg)
						continue
					}
				}
				os.mv(final_module_path, minfo.final_module_path) or {
					errors++
					eprintln('Errors while relocating module "${repo_name}" :')
					eprintln(err)
					os.rmdir_all(final_module_path) or {
						errors++
						eprintln('Errors while removing "${final_module_path}" :')
						eprintln(err)
						continue
					}
					continue
				}
				println('Module "${repo_name}" relocated to "${manifest.name}" successfully.')
				publisher_dir := os.dir(final_module_path)
				if os.is_dir_empty(publisher_dir) {
					os.rmdir(publisher_dir) or {
						errors++
						eprintln('Errors while removing "${publisher_dir}" :')
						eprintln(err)
					}
				}
				final_module_path = minfo.final_module_path
			}
			ident = manifest.name
		}
		resolve_dependencies(ident, final_module_path, modules)
	}
	if errors > 0 {
		exit(1)
	}
}
