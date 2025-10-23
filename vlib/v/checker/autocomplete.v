// Copyright (c) 2019-2024 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license that can be found in the LICENSE file.
module checker

import strings
import v.ast
import os

enum DetailKind {
	text           = 1
	method         = 2
	function       = 3
	constructor    = 4
	field          = 5
	variable       = 6
	class          = 7
	interface      = 8
	module         = 9
	property       = 10
	unit           = 11
	value          = 12
	enum           = 13
	keyword        = 14
	snippet        = 15
	color          = 16
	file           = 17
	reference      = 18
	folder         = 19
	enum_member    = 20
	const          = 21
	struct         = 22
	event          = 23
	operator       = 24
	type_parameter = 25
}

struct Detail {
	kind               DetailKind // The type of item (e.g., Method, Function, Field)
	label              string     // The name of the completion item
	detail             string     // Additional info like the function signature or return type
	documentation      string     // The documentation for the item
	insert_text        ?string
	insert_text_format ?int // 1 for PlainText, 2 for Snippet
}

// Autocomplete for function parameters `os.write_bytes(**path string, bytes []u8***)` etc
pub fn (mut c Checker) autocomplete_for_fn_call_expr(node ast.CallExpr) {
	// Make sure this ident is on the same line and same file as request
	same_line := c.pref.linfo.line_nr + 1 == node.pos.line_nr
	if !same_line {
		return
	}
	if node.pos.file_idx < 0 {
		return
	}
	if c.pref.linfo.path != c.table.filelist[node.pos.file_idx] {
		return
	}
	col := c.pref.linfo.expr.all_after_last('^').int()
	if node.name_pos.col + node.name_pos.len + 1 != col {
		return
	}
	fn_name := node.name
	if c.pref.linfo.vars_printed[fn_name] {
		return
	}
	c.pref.linfo.vars_printed[fn_name] = true
	f := if node.is_method {
		left_sym := c.table.sym(c.unwrap_generic(node.left_type))
		c.table.find_method(left_sym, fn_name) or {
			println('failed to find method "${fn_name}"')
			return
		}
	} else {
		c.table.find_fn(fn_name) or {
			println('failed to find fn "${fn_name}"')
			return
		}
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
	if node.mod == 'builtin' {
		// User can't type in `builtin.func(` at all
		return
	}
	// Make sure this ident is on the same line and same file as request
	same_line := c.pref.linfo.line_nr == node.pos.line_nr
	if !same_line {
		return
	}
	if node.pos.file_idx < 0 {
		return
	}
	if c.pref.linfo.path != c.table.filelist[node.pos.file_idx] {
		return
	}
	check_name := if c.pref.linfo.col == node.pos.col {
		if node.name == '' {
			// for `os` in middle of text, followed by something else in next line:
			// `os.
			//  something`
			node.mod
		} else {
			''
		}
	} else if c.pref.linfo.col == node.pos.col + node.pos.len {
		// for `os` at end of scope :
		// `os. }`
		node.name
	} else {
		''
	}
	if check_name.len == 0 {
		return
	}
	// Module autocomplete
	// `os. ...`
	mod_name := c.try_resolve_to_import_mod_name(check_name)
	if mod_name.len > 0 {
		c.module_autocomplete(mod_name)
		exit(0)
	}

	if node.kind == .unresolved {
		eprintln('unresolved type, maybe "${node.name}" was not defined. otherwise this is a bug, should never happen; please report')
		exit(1)
	}
	if node.obj.typ == ast.no_type {
		exit(0)
	}
	sym := c.table.sym(c.unwrap_generic(node.obj.typ))
	mut details := []Detail{cap: 10}
	c.vls_gen_type_details(mut details, sym)
	c.vls_write_details(details)
}

fn (mut c Checker) module_autocomplete(mod string) {
	mut details := []Detail{cap: 128}
	c.vls_gen_mod_funcs_details(mut details, mod)
	c.vls_gen_mod_type_details(mut details, mod, .alias, .interface, .enum, .sum_type,
		.struct)
	c.vls_gen_mod_consts_details(mut details, mod)
	c.vls_write_details(details)
}

fn (c &Checker) build_fn_summary(func ast.Fn) string {
	mut sb := strings.new_builder(128)
	fn_name := func.name.all_after_last('.')
	sb.writeln('{\n"signatures":[{')
	sb.write_string('\t"label":"${fn_name}(')
	mut params := []string{cap: func.params.len}
	for i, param in func.params {
		if func.is_method && i == 0 {
			// skip receiver
			continue
		}
		params << '${param.name} ${c.table.type_to_str(param.typ)}'
	}
	sb.write_string(params.join(', '))
	sb.write_string(')')
	if func.return_type != ast.void_type {
		sb.write_string(' ')
		sb.write_string(c.table.type_to_str(func.return_type))
	}
	sb.writeln('",\n\t"parameters":[{')
	for i, p in params {
		sb.write_string('\t\t"label":"${p}"')
		if i < params.len - 1 {
			sb.write_string(',')
		}
		sb.writeln('')
	}
	sb.writeln('\t}]')
	sb.writeln('}],')
	sb.writeln('"activeSignature":0,')
	sb.writeln('"activeParameter":0,')
	sb.writeln('"_type":"SignatureHelp"')
	sb.writeln('}')
	return sb.str()
}

fn (c &Checker) try_resolve_to_import_mod_name(name string) string {
	for imp in c.file.imports {
		if (imp.alias == name || imp.mod == name) && imp.alias in c.file.used_imports {
			return imp.mod
		}
	}
	return ''
}

fn (c &Checker) vls_gen_mod_funcs_details(mut details []Detail, mod string) {
	for _, f in c.table.fns {
		mut name := f.name
		if f.is_pub && !f.is_method && !f.is_static_type_method && name.all_before_last('.') == mod {
			name = name.all_after_last('.') // The user already typed `mod.`, so suggest the name without module
			type_string := if f.return_type != ast.no_type {
				c.table.type_to_str(f.return_type)
			} else {
				''
			}
			mut doc := ''
			if info := c.table.vls_info['fn_${mod}[]${name}'] {
				doc = info.doc
			}
			details << Detail{
				kind:          .function
				label:         name
				detail:        type_string
				documentation: doc
			}
		}
	}
}

fn (c &Checker) vls_gen_mod_type_details(mut details []Detail, mod string, kinds ...ast.Kind) {
	for sym in c.table.type_symbols {
		if sym.is_pub && sym.kind in kinds {
			if sym.name.all_before_last('.') == mod {
				mut doc := ''
				key := match sym.kind {
					.alias {
						'aliastype'
					}
					.interface {
						'interface'
					}
					.enum {
						'enum'
					}
					.sum_type {
						'sumtype'
					}
					.struct {
						'struct'
					}
					else {
						'${sym.kind}'
					}
				}
				if info := c.table.vls_info['${key}_${sym.name}'] {
					doc = info.doc
				}
				details << Detail{
					kind:          c.vls_map_v_kind_to_lsp_kind(sym.kind)
					label:         sym.name.all_after_last('.')
					documentation: doc
				}
			}
		}
	}
}

fn (c &Checker) vls_gen_mod_consts_details(mut details []Detail, mod string) {
	for _, obj in c.table.global_scope.objects {
		if obj is ast.ConstField && obj.is_pub {
			if obj.name.all_before_last('.') == mod {
				mut doc := ''
				if info := c.table.vls_info['const_${obj.name}'] {
					doc = info.doc
				}
				details << Detail{
					kind:          .const
					label:         obj.name.all_after_last('.')
					documentation: doc
				}
			}
		}
	}
}

fn (c &Checker) vls_gen_type_details(mut details []Detail, sym ast.TypeSymbol) {
	match sym.kind {
		.struct {
			struct_info := sym.info as ast.Struct
			for field in struct_info.fields {
				field_sym := c.table.sym(field.typ)
				details << Detail{
					kind:   .field
					label:  field.name
					detail: field_sym.name
				}
			}
		}
		.array {
			if sym.info is ast.Aggregate {
			} else if sym.info is ast.Array {
				details << Detail{
					kind:   .property
					label:  'len'
					detail: 'int'
				}
				details << Detail{
					kind:   .property // use class icon
					label:  'cap'
					detail: 'int'
				}
			}
		}
		.string {
			details << Detail{
				kind:   .property // use class icon
				label:  'len'
				detail: 'int'
			}
		}
		else {}
	}
	// Aliases and other types can have methods, add them
	for method in sym.methods {
		method_ret_type := c.table.sym(method.return_type)
		details << Detail{
			kind:   .method
			label:  method.name
			detail: method_ret_type.name
		}
	}
}

fn (c &Checker) vls_write_details(details []Detail) {
	mut sb := strings.new_builder(details.len * 32)
	sb.writeln('{"details" : [')
	for detail in details {
		sb.write_string('{"kind":${int(detail.kind)},')
		sb.write_string('"label":"${detail.label}",')
		sb.write_string('"detail":"${detail.detail}",')
		sb.write_string('"documentation":"${detail.documentation}",')
		if insert_text := detail.insert_text {
			sb.write_string('"insert_text":"${insert_text}",')
		}
		if insert_text_format := detail.insert_text_format {
			sb.write_string('"insert_text_format":${insert_text_format},')
		}
		sb.go_back(1)
		sb.writeln('},')
	}
	if details.len > 0 {
		sb.go_back(2)
	}
	sb.write_string('\n]}')
	print(sb.str())
}

fn (c &Checker) vls_map_v_kind_to_lsp_kind(kind ast.Kind) DetailKind {
	match kind {
		.alias, .sum_type {
			return .class
		}
		.function {
			return .function
		}
		.interface {
			return .interface
		}
		.enum {
			return .enum
		}
		.struct {
			return .struct
		}
		else {
			return .text
		}
	}
	return .text
}
