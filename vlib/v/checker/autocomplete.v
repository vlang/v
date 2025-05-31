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

fn (mut c Checker) ident_autocomplete(node ast.Ident) {
	// Mini LS hack (v -line-info "a.v:16")
	println(
		'checker.ident() info.line_nr=${c.pref.linfo.line_nr} node.line_nr=${node.pos.line_nr} ' +
		' pwd="${os.getwd()}" file="${c.file.path}", ' +
		' pref.linfo.path="${c.pref.linfo.path}" node.name="${node.name}" expr="${c.pref.linfo.expr}"')
	// Make sure this ident is on the same line as requeste, in the same file, and has the same name
	same_line := node.pos.line_nr in [c.pref.linfo.line_nr - 1, c.pref.linfo.line_nr + 1, c.pref.linfo.line_nr]
	if !same_line {
		return
	}
	same_name := c.pref.linfo.expr == node.name
	if !same_name {
		return
	}
	abs_path := os.join_path(os.getwd(), c.file.path)
	if c.pref.linfo.path !in [c.file.path, abs_path] {
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
	if !c.pref.linfo.vars_printed[nt] { // avoid dups
		sb.writeln('===')
		sb.writeln('VAR ${nt}') //${node.name}:${sym.name}')
		// print_backtrace()
		/*
		if sym.kind == .alias {
			parent_sym := c.table.sym(sym.parent_type)
		}
		*/

		mut fields := []ACFieldMethod{cap: 10}
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
		}
		// Aliases and other types can have methods, add them
		for method in sym.methods {
			method_ret_type := c.table.sym(method.return_type)
			fields << ACFieldMethod{build_method_summary(method), method_ret_type.name}
		}
		fields.sort(a.name < b.name)
		for field in fields {
			sb.writeln('${field.name}:${field.typ}')
		}
		res := sb.str().trim_space()
		if res != '' {
			println(res)
			c.pref.linfo.vars_printed[nt] = true
		}
	}
}

fn build_method_summary(method ast.Fn) string {
	mut s := method.name + '('
	for i, param in method.params {
		s += param.name
		if i < method.params.len - 1 {
			s += ','
		}
	}
	return s + ')'
}
