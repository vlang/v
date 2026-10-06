module main

import os
import v.help
import v.vmod

const vendor_dir = 'vendor'

fn vpm_vendor() {
	if settings.is_help {
		help.print_and_exit('vendor')
	}
	project := vmod.get_cache().get_by_folder(os.getwd())
	if project.vmod_file == '' {
		vpm_error('no v.mod found at or above `${os.getwd()}`')
		exit(1)
	}
	root := vmod.from_file(project.vmod_file) or { panic(err) }
	deps := root.dependencies
	if deps.len == 0 {
		println('No dependencies to vendor.')
		return
	}
	vendor_path := os.join_path(os.getwd(), vendor_dir)
	os.mkdir_all(vendor_path) or {
		vpm_error('failed to create `${vendor_dir}/`: ${err.msg()}')
		exit(1)
	}
	mut vendored := 0
	for dep in deps {
		name := dep.all_before('@').trim_space()
		install_path := get_path_of_existing_module(name) or {
			vpm_error('`${name}` is not installed. Run `v install` first.')
			exit(1)
		}
		dest := os.join_path(vendor_path, name.replace('.', os.path_separator))
		if os.exists(dest) {
			os.rmdir_all(dest) or {
				vpm_error('failed to remove existing `${dest}`: ${err.msg()}')
				exit(1)
			}
		}
		os.mkdir_all(os.dir(dest)) or {
			vpm_error('failed to create directory for `${name}`: ${err.msg()}')
			exit(1)
		}
		os.cp_all(install_path, dest, false) or {
			vpm_error('failed to vendor `${name}`: ${err.msg()}')
			exit(1)
		}
		vendored++
		println('Vendored `${name}`')
	}
	println('Vendored ${vendored} module(s) to `${vendor_dir}/`.')
}
