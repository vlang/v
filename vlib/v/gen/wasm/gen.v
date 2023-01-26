module wasm

import v.ast
import v.pref
import v.util

[heap; minify]
pub struct Gen {
	out_name string
	pref     &pref.Preferences = unsafe { nil } // Preferences shared from V struct
	files    []&ast.File
mut:
	table    &ast.Table = unsafe { nil }
}

fn (mut g Gen) stmt(node ast.Stmt) {
	println(node)
}

pub fn (mut g Gen) stmts(stmts []ast.Stmt) {
	for stmt in stmts {
		g.stmt(stmt)
	}
}

pub fn gen(files []&ast.File, table &ast.Table, out_name string, pref &pref.Preferences) (int, int) {
	mut g := &Gen{
		table: table
		out_name: out_name
		pref: pref
		files: files
	}
	
	for file in g.files {
		if file.errors.len > 0 {
			util.verror('wasm error', file.errors[0].str())
		}
		g.stmts(file.stmts)
	}

	return 0, 0
}