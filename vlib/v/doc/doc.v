module doc

import (
	strings
	// v.builder
	v.pref
	v.table
	v.parser
	v.ast
	os
	filepath
)

struct Doc {
	out   strings.Builder
	table &table.Table
	mod   string
mut:
	stmts []ast.Stmt // all module statements from all files
}

type FilterFn fn(node ast.FnDecl)bool

pub fn doc(mod string, table &table.Table) string {
	mut d := Doc{
		out: strings.new_builder(1000)
		table: table
		mod: mod
	}
	vlib_path := filepath.dir(pref.vexe_path()) + '/vlib'
	mod_path := mod.replace('.', filepath.separator)
	path := filepath.join(vlib_path, mod_path)
	if !os.exists(path) {
		println('module "$mod" not found')
		println(path)
		return ''
	}
	// vfiles := os.walk_ext(path, '.v')
	files := os.ls(path) or {
		panic(err)
	}
	for file in files {
		if !file.ends_with('.v') {
			continue
		}
		if file.ends_with('_test.v') || file.ends_with('_windows.v') || file.ends_with('_macos.v') {
			continue
		}
		file_ast := parser.parse_file(filepath.join(path,file), table, .skip_comments)
		d.stmts << file_ast.stmts
	}
	d.print_fns()
	d.out.writeln('')
	d.print_methods()
	/*
		for stmt in file_ast.stmts {
			d.stmt(stmt)
		}
	println(path)
	*/

	return d.out.str().trim_space()
}

fn (d &Doc) get_fn_node(f ast.FnDecl) string {
	return f.str(d.table).replace(d.mod + '.', '')
}

fn (d mut Doc) print_fns() {
	fn_signatures := d.get_fn_signatures(is_pub_function)
	d.write_fn_signatures(fn_signatures)
}

fn (d mut Doc) print_methods() {
	fn_signatures := d.get_fn_signatures(is_pub_method)
	d.write_fn_signatures(fn_signatures)
}

[inline]
fn (d mut Doc) write_fn_signatures(fn_signatures []string) {
	for s in fn_signatures {
		d.out.writeln(s)
	}
}

fn (d Doc) get_fn_signatures(filter_fn FilterFn) []string {
	mut fn_signatures := []string
	for stmt in d.stmts {
		match stmt {
			ast.FnDecl {
				if filter_fn(it) {
					fn_signatures << d.get_fn_node(it)
				}
			}
			else {}
	}
	}
	fn_signatures.sort()
	return fn_signatures
}

fn is_pub_method(node ast.FnDecl) bool {
	return node.is_pub && node.is_method && !node.is_deprecated
}

fn is_pub_function(node ast.FnDecl) bool {
	return node.is_pub && !node.is_method && !node.is_deprecated
}
