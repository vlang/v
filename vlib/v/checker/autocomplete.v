// Copyright (c) 2019-2024 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license that can be found in the LICENSE file.
module checker

import strings
import v.ast
import os

struct ACFieldMethod {
	name string
	typ  string
}

fn abs(a int) int {
	if a < 0 {
		return -a
	}
	return a
}

pub fn (mut c Checker) run_ac(ast_file &ast.File) {
}

pub fn (mut c Checker) autocomplete_for_fn_call_expr() {
	// println(c.pref.linfo.expr)
	fn_name := c.pref.linfo.expr.replace('()', '').trim_space()
	f := c.table.find_fn(fn_name) or {
		println('failed to find fn "${fn_name}"')
		return
	}
	res := c.build_fn_summary(f)
	println(res)
}

fn (mut c Checker) ident_autocomplete(node ast.Ident) {
	// Mini LS hack (v -line-info "a.v:16")
	if c.pref.is_verbose {
		println(
			'checker.ident_autocomplete() info.line_nr=${c.pref.linfo.line_nr} node.line_nr=${node.pos.line_nr} ' +
			' node.col=${node.pos.col} pwd="${os.getwd()}" file="${c.file.path}", ' +
			//' pref.linfo.path="${c.pref.linfo.path}" node.name="${node.name}" expr="${c.pref.linfo.expr}"')
		 ' pref.linfo.path="${c.pref.linfo.path}" node.name="${node.name}" node.mod="${node.mod}" col="${c.pref.linfo.col}"')
	}
	// Make sure this ident is on the same line as requeste, in the same file, and has the same name
	same_line := node.pos.line_nr in [c.pref.linfo.line_nr - 1, c.pref.linfo.line_nr + 1, c.pref.linfo.line_nr]
	if !same_line {
		return
	}
	same_col := abs(c.pref.linfo.col - node.pos.col) < 3
	if !same_col {
		return
	}
	abs_path := os.join_path(os.getwd(), c.file.path)
	if c.pref.linfo.path !in [c.file.path, abs_path] {
		return
	}
	// Module autocomplete
	// `os. ...`
	// println(node)
	if node.name == '' && node.mod != 'builtin' {
		c.module_autocomplete(node)
		return
	} else if node.name == '' && node.mod == 'builtin' {
		return
	}
	mut sb := strings.new_builder(10)
	if node.kind == .unresolved {
		// println(node)
		eprintln('unresolved type, maybe "${node.name}" was not defined. otherwise this is a bug, should never happen; please report')
		exit(1)
	}
	sym := c.table.sym(c.unwrap_generic(node.obj.typ))
	// sb.writeln('VAR ${node.name}:${sym.name} ${node.pos.line_nr}')
	nt := '${node.name}:${sym.name}'
	sb.writeln('{')
	if !c.pref.linfo.vars_printed[nt] { // avoid dups
		// sb.writeln('===')
		// sb.writeln('VAR ${nt}') //${node.name}:${sym.name}')
		sb.writeln('\t"name":"${node.name}",')
		sb.writeln('\t"type":"${sym.name}",')
		sb.writeln('\t"fields":[')

		// print_backtrace()
		/*
		if sym.kind == .alias {
			parent_sym := c.table.sym(sym.parent_type)
		}
		*/

		mut fields := []ACFieldMethod{cap: 10}
		mut methods := []ACFieldMethod{cap: 10}
		if sym.kind == .struct {
			// Add fields, but only if it's a struct.
			struct_info := sym.info as ast.Struct
			// match struct_info {
			// ast.Struct
			//}
			for field in struct_info.fields {
				field_sym := c.table.sym(field.typ)
				fields << ACFieldMethod{field.name, field_sym.name}
			}
		} else if sym.kind == .array {
			// t := typeof(sym.info).name
			if sym.info is ast.Aggregate {
			} else if sym.info is ast.Array {
				fields << ACFieldMethod{'len', 'int'}
				fields << ACFieldMethod{'cap', 'int'}
			}
			// array_info := sym.info as ast.Array
		} else if sym.kind == .string {
			fields << ACFieldMethod{'len', 'int'}
		}
		// Aliases and other types can have methods, add them
		for method in sym.methods {
			method_ret_type := c.table.sym(method.return_type)
			methods << ACFieldMethod{build_method_summary(method), method_ret_type.name}
		}
		fields.sort(a.name < b.name)
		for i, field in fields {
			// sb.writeln('${field.name}:${field.typ}')
			sb.write_string('\t\t"${field.name}:${field.typ}"')
			if i < fields.len - 1 {
				sb.writeln(', ')
			}
		}
		sb.writeln('\n\t], "methods":[')

		for i, method in methods {
			sb.write_string('\t\t"${method.name}:${method.typ}"')
			if i < methods.len - 1 {
				sb.writeln(', ')
			}
		}
		sb.writeln('\n\t]\n}')
		res := sb.str().trim_space()
		if res != '' {
			println(res)
			c.pref.linfo.vars_printed[nt] = true
		}
	}
}

fn (mut c Checker) module_autocomplete(node ast.Ident) {
	mut sb := strings.new_builder(10)
	// println(c.table.fns)
	sb.writeln('{"methods":[')
	prefix := node.mod + '.'
	mut empty := true
	for _, f in c.table.fns {
		mut name := f.name
		if name.starts_with(prefix) {
			empty = false
			if name.contains('__static__') {
				name = name.replace('__static__', '.')
			}
			name = name.after('.') // The user already typed `mod.`, so suggest the name without module
			sb.writeln('"${name}:int" ,')
		}
	}
	if !empty {
		sb.go_back(2) // remove final ,
	}
	sb.writeln(']}')
	println(sb.str().trim_space())
}

fn build_method_summary(method ast.Fn) string {
	mut s := method.name + '('
	for i, param in method.params {
		if i == 0 {
			continue
		}
		s += param.name
		if i < method.params.len - 1 {
			s += ', '
		}
	}
	return s + ')'
}

fn (c &Checker) build_fn_summary(method ast.Fn) string {
	mut s := method.name + '('
	for i, param in method.params {
		s += param.name + ' ' + c.table.type_to_str(param.typ)
		if i < method.params.len - 1 {
			s += ', '
		}
	}
	return s + ')'
}
