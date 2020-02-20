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

pub fn doc(mod string, table &table.Table) string {
	mut d := Doc{
		out: strings.new_builder(1000)
		table: table
		mod: mod
	}
	mods_path := filepath.dir(pref.vexe_path()) + '/vlib'
	path := filepath.join(mods_path,mod).replace('.', filepath.separator)
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
		if file.ends_with('_test.v') {
			continue
		}
		file_ast := parser.parse_file(filepath.join(path,file), table)
		d.stmts << file_ast.stmts
	}
	d.print_fns()
	d.writeln('')
	d.print_methods()
	/*
		for stmt in file_ast.stmts {
			d.stmt(stmt)
		}
	println(path)
	*/

	return d.out.str()
}

fn (d mut Doc) writeln(s string) {
	d.out.writeln(s)
}

fn (d mut Doc) write_fn_node(f ast.FnDecl) {
	d.writeln(f.str(d.table).replace(d.mod + '.', ''))
}

fn (d mut Doc) print_fns() {
	for stmt in d.stmts {
		match stmt {
			ast.FnDecl {
				if it.is_pub && !it.is_method {
					d.write_fn_node(it)
				}
			}
			else {}
	}
	}
}

fn (d mut Doc) print_methods() {
	for stmt in d.stmts {
		match stmt {
			ast.FnDecl {
				if it.is_pub && it.is_method {
					d.write_fn_node(it)
				}
			}
			else {}
	}
	}
}
