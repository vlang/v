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

pub fn (mut c Checker) run_ac(ast_file &ast.File) {
}

// Autocomplete for function parameters `os.write_bytes(**path string, bytes []u8***)` etc
pub fn (mut c Checker) autocomplete_for_fn_call_expr() {
	// println(c.pref.linfo.expr)
	mut fn_name := if c.pref.linfo.expr.ends_with('(') {
		c.pref.linfo.expr[..c.pref.linfo.expr.len - 1].trim_space()
	} else {
		c.pref.linfo.expr.replace('()', '').trim_space()
	}
	mod_name := fn_name.all_before_last('.')
	resolved_mod_name := c.try_resolve_to_import_mod_name(mod_name)
	if resolved_mod_name.len > 0 {
		fn_name = resolved_mod_name + '.' + fn_name.all_after_last('.')
	}
	f := c.table.find_fn(fn_name) or {
		println('failed to find fn "${fn_name}"')
		return
	}
	res := c.build_fn_summary(f)
	println(res)
}

fn (mut c Checker) ident_gotodef() {
	mut ident_name := c.pref.linfo.expr.after('gd^').trim_space()
	mod_name := ident_name.all_before_last('.')
	resolved_mod_name := c.try_resolve_to_import_mod_name(mod_name)
	if resolved_mod_name.len > 0 {
		ident_name = resolved_mod_name + '.' + ident_name.all_after_last('.')
	}
	if f := c.table.find_fn(ident_name) {
		println('${f.file}:${f.pos.line_nr}:${f.pos.col}')
		return
	}
	// TODO: As EnumDecl has no `file` field, we can't goto enum right now
	// for name,val in c.table.enum_decls {
	//	if val.is_pub && name == ident_name {
	//		println('${val.file}:${val.pos.line_nr}:${val.pos.col}')
	//		return
	//	}
	//}

	// TODO: we need other TypeDecl information here
	// for mut sym in c.table.type_symbols {
	//	if sym.is_pub && sym.name == ident_name {
	//		match mut sym.info {
	//			ast.Alias {
	//			}
	//			ast.Interface {
	//			}
	//			ast.Struct {
	//			}
	//			else {
	//			}
	//		}
	//	}
	//}
}

// Autocomplete for `myvar. ...`, `os. ...`
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
	same_line := c.pref.linfo.line_nr == node.pos.line_nr
	if !same_line {
		return
	}
	same_col := c.pref.linfo.col == node.pos.col + node.pos.len
	if !same_col {
		return
	}
	abs_path := os.join_path(os.getwd(), c.file.path)
	if c.pref.linfo.path !in [c.file.path, abs_path] {
		return
	}
	// Module autocomplete
	// `os. ...`
	mod_name := c.try_resolve_to_import_mod_name(node.name)
	if mod_name.len > 0 {
		if node.mod == c.file.mod.name {
			c.module_autocomplete(mod_name)
		}
		exit(0)
	}
	mut sb := strings.new_builder(10)
	if node.kind == .unresolved {
		// println(node)
		eprintln('unresolved type, maybe "${node.name}" was not defined. otherwise this is a bug, should never happen; please report')
		exit(1)
	}
	if node.obj.typ == ast.no_type {
		exit(0)
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

fn (mut c Checker) module_autocomplete(mod string) {
	println('{')
	c.write_mod_funcs(mod)
	c.write_mod_type_alias(mod)
	c.write_mod_interfaces(mod)
	c.write_mod_enums(mod)
	c.write_mod_consts(mod)
	c.write_mod_structs(mod)
	println('}')
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

fn (c &Checker) try_resolve_to_import_mod_name(name string) string {
	if name in c.file.used_imports {
		// resolve alias to mod name
		mod_name := c.file.imports.filter(it.alias == name)[0].mod
		return mod_name
	}
	return ''
}

fn (c &Checker) write_mod_funcs(mod string) {
	mut sb := strings.new_builder(128)
	sb.writeln('"functions":[')
	mut empty := true
	for _, f in c.table.fns {
		mut name := f.name
		if f.is_pub && name.all_before_last('.') == mod {
			empty = false
			if name.contains('__static__') {
				name = name.replace('__static__', '.')
			}
			name = name.all_after_last('.') // The user already typed `mod.`, so suggest the name without module
			type_string := if f.return_type != ast.no_type {
				c.table.type_to_str(f.return_type)
			} else {
				'int'
			}
			sb.writeln('"${name}:${type_string}" ,')
		}
	}
	if !empty {
		sb.go_back(2) // remove final ,
	}
	sb.writeln('],')
	println(sb.str().trim_space())
}

fn (c &Checker) write_mod_type_alias(mod string) {
	mut sb := strings.new_builder(128)
	sb.writeln('"type_alias":[')
	mut empty := true
	for sym in c.table.type_symbols {
		if sym.is_pub && sym.info is ast.Alias {
			if sym.name.all_before_last('.') == mod {
				empty = false
				sb.writeln('"${sym.name.all_after_last('.')}" ,')
			}
		}
	}
	if !empty {
		sb.go_back(2) // remove final ,
	}
	sb.writeln('],')
	println(sb.str().trim_space())
}

fn (c &Checker) write_mod_interfaces(mod string) {
	mut sb := strings.new_builder(128)
	sb.writeln('"interfaces":[')
	mut empty := true
	for sym in c.table.type_symbols {
		if sym.is_pub && sym.info is ast.Interface {
			if sym.name.all_before_last('.') == mod {
				empty = false
				sb.writeln('"${sym.name.all_after_last('.')}" ,')
			}
		}
	}
	if !empty {
		sb.go_back(2) // remove final ,
	}
	sb.writeln('],')
	println(sb.str().trim_space())
}

fn (c &Checker) write_mod_enums(mod string) {
	mut sb := strings.new_builder(128)
	sb.writeln('"enums":[')
	mut empty := true
	for name, val in c.table.enum_decls {
		if val.is_pub && name.all_before_last('.') == mod {
			empty = false
			sb.writeln('"${name.all_after_last('.')}" ,')
		}
	}
	if !empty {
		sb.go_back(2) // remove final ,
	}
	sb.writeln('],')
	println(sb.str().trim_space())
}

fn (c &Checker) write_mod_consts(mod string) {
	mut sb := strings.new_builder(128)
	sb.writeln('"constants":[')
	mut empty := true
	for _, obj in c.table.global_scope.objects {
		if obj is ast.ConstField && obj.is_pub {
			if obj.name.all_before_last('.') == mod {
				empty = false
				if obj.typ != ast.no_type {
					sb.writeln('"${obj.name.all_after_last('.')}:${c.table.type_to_str(obj.typ)}" ,')
				} else {
					sb.writeln('"${obj.name.all_after_last('.')}:int" ,')
				}
			}
		}
	}
	if !empty {
		sb.go_back(2) // remove final ,
	}
	sb.writeln('],')
	println(sb.str().trim_space())
}

fn (c &Checker) write_mod_structs(mod string) {
	mut sb := strings.new_builder(128)
	sb.writeln('"structs":[')
	mut empty := true
	for sym in c.table.type_symbols {
		if sym.is_pub && sym.info is ast.Struct {
			if sym.name.all_before_last('.') == mod {
				empty = false
				sb.writeln('"${sym.name.all_after_last('.')}" ,')
			}
		}
	}
	if !empty {
		sb.go_back(2) // remove final ,
	}
	sb.writeln(']')
	println(sb.str().trim_space())
}
