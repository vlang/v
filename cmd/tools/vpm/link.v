module main

import os
import v.help
import v.vmod

struct LinkedProject {
	name        string
	project_dir string
	link_path   string
}

fn vpm_link(query []string) {
	if settings.is_help {
		help.print_and_exit('link')
	}
	ensure_no_query_for_project_command('link', query)
	ensure_vmodules_dir_exist()

	project := get_project_for_linking() or {
		vpm_error(err.msg())
		exit(1)
	}
	if project.project_dir == os.real_path(project.link_path) {
		println('Module `${project.name}` is already available in `${fmt_mod_path(project.link_path)}`.')
		return
	}
	if os.exists(project.link_path) || os.is_link(project.link_path) {
		if os.is_link(project.link_path) {
			if os.real_path(project.link_path) == project.project_dir {
				println('Module `${project.name}` is already linked in `${fmt_mod_path(project.link_path)}`.')
				return
			}
			vpm_error('`${project.name}` is already linked at `${fmt_mod_path(project.link_path)}`.',
				details: 'Run `v unlink` first to replace it.'
			)
			exit(1)
		}
		vpm_error('`${project.name}` already exists at `${fmt_mod_path(project.link_path)}`.')
		exit(1)
	}
	parent_dir := os.dir(project.link_path)
	os.mkdir_all(parent_dir) or {
		vpm_error('failed to create `${fmt_mod_path(parent_dir)}`.', details: err.msg())
		exit(1)
	}
	os.symlink(project.project_dir, project.link_path) or {
		vpm_error('failed to link `${project.name}`.', details: err.msg())
		exit(1)
	}
	println('Linked `${project.name}` to `${fmt_mod_path(project.link_path)}`.')
}

fn vpm_unlink(query []string) {
	if settings.is_help {
		help.print_and_exit('unlink')
	}
	ensure_no_query_for_project_command('unlink', query)

	project := get_project_for_linking() or {
		vpm_error(err.msg())
		exit(1)
	}
	if !os.exists(project.link_path) && !os.is_link(project.link_path) {
		println('Module `${project.name}` is not linked in `${fmt_mod_path(project.link_path)}`.')
		return
	}
	if !os.is_link(project.link_path) {
		vpm_error('`${project.name}` at `${fmt_mod_path(project.link_path)}` is not a symlink.')
		exit(1)
	}
	remove_symlink(project.link_path) or {
		vpm_error('failed to unlink `${project.name}`.', details: err.msg())
		exit(1)
	}
	cleanup_empty_link_parent_dirs(project.link_path)
	println('Unlinked `${project.name}` from `${fmt_mod_path(project.link_path)}`.')
}

fn ensure_no_query_for_project_command(command string, query []string) {
	if query.len == 0 {
		return
	}
	vpm_error('`${command}` does not accept package names.',
		details: 'Run `v ${command}` from inside the project directory.'
	)
	exit(2)
}

fn get_project_for_linking() !LinkedProject {
	wrkdir := os.getwd()
	mut mcache := vmod.get_cache()
	vmod_location := mcache.get_by_folder(wrkdir)
	if vmod_location.vmod_file == '' {
		return error('no `v.mod` file found in `${wrkdir}` or its parent directories.')
	}
	manifest := vmod.from_file(vmod_location.vmod_file)!
	if manifest.name.trim_space() == '' {
		return error('`${vmod_location.vmod_file}` is missing the `name` field.')
	}
	mod_path := normalize_mod_path(manifest.name.replace('.', os.path_separator))
	vmodules_path := if os.is_dir(settings.vmodules_path) {
		os.real_path(settings.vmodules_path)
	} else {
		settings.vmodules_path
	}
	return LinkedProject{
		name:        manifest.name
		project_dir: os.real_path(vmod_location.vmod_folder)
		link_path:   os.join_path(vmodules_path, mod_path)
	}
}

fn remove_symlink(path string) ! {
	os.rm(path) or {
		$if windows {
			os.rmdir(path)!
		} $else {
			return err
		}
	}
}

fn cleanup_empty_link_parent_dirs(link_path string) {
	vmodules_path := if os.is_dir(settings.vmodules_path) {
		os.real_path(settings.vmodules_path)
	} else {
		settings.vmodules_path
	}
	mut parent := os.dir(link_path)
	for parent != vmodules_path && parent != os.dir(parent) {
		if !os.is_dir(parent) || !os.is_dir_empty(parent) {
			break
		}
		os.rmdir(parent) or { break }
		parent = os.dir(parent)
	}
}
