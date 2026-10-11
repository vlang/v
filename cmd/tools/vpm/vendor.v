module main

import os
import rand
import v.help
import v.vmod

const vendor_dir = 'vendor'

fn vpm_vendor() {
	if settings.is_help { help.print_and_exit('vendor') }
	vendor_project() or {
		vpm_error(err.msg())
		exit(1)
	}
}

fn vendor_project() ! {
	project := vmod.get_cache().get_by_folder(os.getwd())
	if project.vmod_file == '' { return error('no v.mod found at or above `${os.getwd()}`') }
	root := vmod.from_file(project.vmod_file)!
	if project_dependencies(root).len == 0 {
		println('No dependencies to vendor.')
		return
	}
	graph := build_dep_graph()!
	vendor_path := os.join_path(project.vmod_folder, vendor_dir)
	if os.exists(vendor_path) || os.is_link(vendor_path) {
		return error('refusing to replace existing `${vendor_path}`; remove it explicitly before vendoring again')
	}
	mut names := map[string]string{}
	for id, label in graph.labels {
		if graph.absent[id] { return error('`${label}` is not installed. Run `v install` first.') }
		if !valid_override_name(label) {
			return error('cannot vendor invalid import path `${label}`')
		}
		if previous := names[label] {
			if previous != id {
				return error('multiple sources share vendor import path `${label}`')
			}
		}
		names[label] = id
	}
	stage := get_tmp_path(project.vmod_folder, '.vpm-vendor-' + rand.ulid())!
	os.mkdir_all(stage)!
	defer { os.rmdir_all(stage) or {} }
	for name in names.keys().sorted() {
		dest := os.join_path(stage, name.replace('.', os.path_separator))
		if !path_is_below(os.norm_path(dest), os.norm_path(stage)) {
			return error('vendor destination escapes its staging directory')
		}
		os.mkdir_all(os.dir(dest))!
		os.cp_all(names[name], dest, false)!
	}
	// Publish only a complete graph; every error before this point leaves vendor absent.
	if os.exists(vendor_path) || os.is_link(vendor_path) {
		return error('vendor destination appeared during staging')
	}
	os.rename(stage, vendor_path)!
	println('Vendored ${names.len} module(s) to `${vendor_dir}/`.')
}
