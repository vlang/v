module main

import os
import v.doc

fn lookup_module(d doc.Doc, mod string) ?string {
	mod_path := mod.replace('.', '/')
	vexe_path := os.base_dir(os.base_dir(os.base_dir(os.executable())))

	compile_dir := os.real_path(os.base_dir('.'))
	modules_dir := os.join_path(compile_dir, 'modules', mod_path)
	vlib_path := os.join_path(vexe_path, 'vlib', mod_path)
	vmodules_path := os.join_path(os.home_dir(), '.vmodules', mod_path)
	paths := [modules_dir, vlib_path, vmodules_path]

	for path in paths {
		if os.is_dir_empty(path) { continue }
		return path
	}

	return error('vdoc: Module "${mod}" not found.')
}

fn main() {
	args := os.args[2..]
	
	if args.len == 0 || args[0] == 'help' {
		os.system('v help doc')
		exit(0)
	}

	mut src_path := args[0]
	mut opath := if args.len >= 2 { args[1] } else { '' }
	mut pub_only := true

	if args[0] == '-pub_only' {
		if args[1] in ['true', 'false'] {
			pub_only = args[1] == 'true'
			src_path = args[2]
			opath = args[3]
		} else {
			src_path = args[1]
			opath = args[2]
		}
	}

	mut doc := doc.new_doc(src_path)
	doc.pub_only = pub_only
	is_path := src_path.ends_with('.v') || src_path.split('/').len > 1 || src_path == '.'

	if is_path {
		doc.generate_from_file(opath)
	} else {
		mod_path := lookup_module(doc, src_path) or {
			eprintln(err)
			exit(1)
		}

		doc.input_path = mod_path
		doc.generate(opath)
	}
}