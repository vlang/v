module document

import os
import v.ast
import v.parser
import v.pref
import v.vmod

fn module_path_from_vmod_root(vmod_root string, mod string) !string {
	vmod_path := os.join_path(vmod_root, 'v.mod')
	if !os.is_file(vmod_path) {
		return error('module not found')
	}
	manifest := vmod.from_file(vmod_path)!
	tail_path := mod_tail_after_vmod_name(mod, manifest.name)!
	mut try_path := manifest.source_root(vmod_root)
	if tail_path.len > 0 {
		try_path = os.join_path(try_path, tail_path)
	}
	if os.is_dir(try_path) && !os.is_dir_empty(try_path) {
		return try_path
	}
	return error('module not found')
}

fn module_path_from_search_root(search_path string, mod string) !string {
	mod_path := mod.replace('.', os.path_separator)
	try_path := os.join_path_single(search_path, mod_path)
	if os.is_dir(try_path) && !os.is_dir_empty(try_path) {
		return try_path
	}
	return module_path_from_vmod_root(search_path, mod)
}

fn mod_tail_after_vmod_name(mod string, vmod_name string) !string {
	if vmod_name == '' {
		return error('module not found')
	}
	mod_parts := mod.split('.')
	vmod_parts := vmod_name.split('.')
	for i := 0; i + vmod_parts.len <= mod_parts.len; i++ {
		if i > 1 {
			break
		}
		if mod_parts[i..i + vmod_parts.len].join('.') == vmod_name {
			return mod_parts[i + vmod_parts.len..].join(os.path_separator)
		}
	}
	return error('module not found')
}

// get_parent_mod returns the parent mod name, in dot format.
// It works by climbing up the folder hierarchy, until a folder,
// that either contains main .v files, or a v.mod file is reached.
// For example, given something like /languages/v/vlib/x/websocket/tests/autobahn
// it returns `x.websocket.tests`, because /languages/v/ has v.mod file in it.
// Note: calling this is expensive, so keep the result, instead of recomputing it.
// TODO: turn this to a Doc method, so that the new_vdoc_preferences call here can
// be removed.
fn get_parent_mod(input_dir string) !string {
	if input_dir == '' {
		return error('no input folder')
	}
	// windows root path is C: or D:
	if input_dir.len == 2 && input_dir[1] == `:` {
		return error('root folder reached')
	}
	// unix systems have / at the top:
	if input_dir == '/' {
		return error('root folder reached')
	}
	base_dir := os.dir(input_dir)
	input_dir_name := os.file_name(base_dir)
	prefs := new_vdoc_preferences()
	fentries := os.ls(base_dir) or { []string{} }
	files := fentries.filter(!os.is_dir(os.join_path(base_dir, it)))
	if 'v.mod' in files {
		// the top level is reached, no point in climbing up further
		return ''
	}
	v_files := prefs.should_compile_filtered_files(base_dir, files)
	if v_files.len == 0 {
		parent_mod := get_parent_mod(base_dir) or { return input_dir_name }
		if parent_mod.len > 0 {
			return parent_mod + '.' + input_dir_name
		}
		return error('No V files found.')
	}
	mut tbl := ast.new_table()
	file_ast := parser.parse_file(v_files[0], mut tbl, .skip_comments, prefs)
	if file_ast.mod.short_name == 'main' {
		return ''
	}
	parent_mod := get_parent_mod(base_dir) or { return input_dir_name }
	if parent_mod.len > 0 {
		return '${parent_mod}.${file_ast.mod.short_name}'
	}
	return file_ast.mod.short_name
}

// lookup_module_with_path looks up the path of a given module name.
// Throws an error if the module was not found.
pub fn lookup_module_with_path(mod string, base_path string) !string {
	vexe := pref.vexe_path()
	vroot := os.dir(vexe)
	mut compile_dir := os.real_path(base_path)
	if !os.is_dir(compile_dir) {
		compile_dir = os.dir(compile_dir)
	}
	if path := module_path_from_search_root(compile_dir, mod) {
		return path
	}
	modules_dir := os.join_path(compile_dir, 'modules')
	if path := module_path_from_search_root(modules_dir, mod) {
		return path
	}
	mut current_dir := compile_dir
	for {
		parent_dir := os.dir(current_dir)
		if parent_dir == current_dir {
			break
		}
		current_dir = parent_dir
		if path := module_path_from_search_root(current_dir, mod) {
			return path
		}
	}
	mut search_roots := [os.join_path(vroot, 'vlib')]
	search_roots << os.vmodules_paths()
	for search_root in search_roots {
		if path := module_path_from_search_root(search_root, mod) {
			return path
		}
	}
	return error('module "${mod}" not found.')
}

// lookup_module returns the result of the `lookup_module_with_path`
// but with the current directory as the provided base lookup path.
pub fn lookup_module(mod string) !string {
	return lookup_module_with_path(mod, os.dir('.'))
}

// generate_from_mod generates a documentation from a specific module.
pub fn generate_from_mod(module_name string, pub_only bool, with_comments bool) !Doc {
	mod_path := lookup_module(module_name)!
	return generate(mod_path, pub_only, with_comments, .auto)
}
