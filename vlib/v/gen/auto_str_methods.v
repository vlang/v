// Copyright (c) 2019-2020 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license that can be found in the LICENSE file.
module gen

import v.table
import v.pref
import v.util

// already generated styp, reuse it
fn (mut g Gen) gen_str_for_type_with_styp(typ table.Type, styp string) string {
	mut sym := g.table.get_type_symbol(g.unwrap_generic(typ))
	mut str_fn_name := styp_to_str_fn_name(styp)
	if mut sym.info is table.Alias {
		if sym.info.is_import {
			sym = g.table.get_type_symbol(sym.info.parent_type)
			str_fn_name = styp_to_str_fn_name(sym.name.replace('.', '__'))
		}
	}
	sym_has_str_method, str_method_expects_ptr, str_nr_args := sym.str_method_info()
	// generate for type
	if sym_has_str_method && str_method_expects_ptr && str_nr_args == 1 {
		// TODO: optimize out this.
		// It is needed, so that println() can be called with &T and T has `fn (t &T).str() string`
		/*
		eprintln('>> gsftws: typ: $typ | typ_is_ptr $typ_is_ptr | styp: $styp ' +
			'| $str_fn_name | sym.name: $sym.name has_str: $sym_has_str_method ' +
			'| expects_ptr: $str_method_expects_ptr')
		*/
		str_fn_name_no_ptr := '${str_fn_name}_no_ptr'
		already_generated_key_no_ptr := '$styp:$str_fn_name_no_ptr'
		if already_generated_key_no_ptr !in g.str_types {
			g.str_types << already_generated_key_no_ptr
			g.type_definitions.writeln('string ${str_fn_name_no_ptr}($styp it); // auto no_ptr version')
			g.auto_str_funcs.writeln('string ${str_fn_name_no_ptr}($styp it){ return ${str_fn_name}(&it); }')
		}
		/*
		typ_is_ptr := typ.is_ptr()
		ret_type := if typ_is_ptr { str_fn_name } else { str_fn_name_no_ptr }
		eprintln('    ret_type: $ret_type')
		return ret_type
		*/
		return str_fn_name_no_ptr
	}
	already_generated_key := '$styp:$str_fn_name'
	if !sym_has_str_method && already_generated_key !in g.str_types {
		$if debugautostr ? {
			eprintln('> gen_str_for_type_with_styp: |typ: ${typ:5}, ${sym.name:20}|has_str: ${sym_has_str_method:5}|expects_ptr: ${str_method_expects_ptr:5}|nr_args: ${str_nr_args:1}|fn_name: ${str_fn_name:20}')
		}
		g.str_types << already_generated_key
		match mut sym.info {
			table.Alias {
				if sym.info.is_import {
					g.gen_str_default(sym, styp, str_fn_name)
				} else {
					g.gen_str_for_alias(sym.info, styp, str_fn_name)
				}
			}
			table.Array {
				g.gen_str_for_array(sym.info, styp, str_fn_name)
			}
			table.ArrayFixed {
				g.gen_str_for_array_fixed(sym.info, styp, str_fn_name)
			}
			table.Enum {
				g.gen_str_for_enum(sym.info, styp, str_fn_name)
			}
			table.Struct {
				g.gen_str_for_struct(sym.info, styp, str_fn_name)
			}
			table.Map {
				g.gen_str_for_map(sym.info, styp, str_fn_name)
			}
			table.MultiReturn {
				g.gen_str_for_multi_return(sym.info, styp, str_fn_name)
			}
			table.SumType {
				g.gen_str_for_union_sum_type(sym.info, styp, str_fn_name)
			}
			else {
				verror("could not generate string method $str_fn_name for type '$styp'")
			}
		}
	}
	// if varg, generate str for varg
	if typ.has_flag(.variadic) {
		varg_already_generated_key := 'varg_$already_generated_key'
		if varg_already_generated_key !in g.str_types {
			g.gen_str_for_varg(styp, str_fn_name, sym_has_str_method)
			g.str_types << varg_already_generated_key
		}
		return 'varg_$str_fn_name'
	}
	return str_fn_name
}

fn (mut g Gen) gen_str_for_alias(info table.Alias, styp string, str_fn_name string) {
	sym := g.table.get_type_symbol(info.parent_type)
	sym_has_str_method, _, _ := sym.str_method_info()
	mut parent_str_fn_name := styp_to_str_fn_name(sym.name.replace('.', '__'))
	if !sym_has_str_method {
		parent_styp := g.typ(info.parent_type)
		parent_str_fn_name = g.gen_str_for_type_with_styp(info.parent_type, parent_styp)
	}
	mut clean_type_v_type_name := util.strip_main_name(styp.replace('__', '.'))
	g.type_definitions.writeln('string ${str_fn_name}($styp it); // auto')
	g.auto_str_funcs.writeln('string ${str_fn_name}($styp it) { return indent_${str_fn_name}(it, 0); }')
	g.type_definitions.writeln('string indent_${str_fn_name}($styp it, int indent_count); // auto')
	g.auto_str_funcs.writeln('string indent_${str_fn_name}($styp it, int indent_count) {')
	g.auto_str_funcs.writeln('\tstring indents = tos_lit("");')
	g.auto_str_funcs.writeln('\tfor (int i = 0; i < indent_count; ++i) {')
	g.auto_str_funcs.writeln('\t\tindents = string_add(indents, tos_lit("    "));')
	g.auto_str_funcs.writeln('\t}')
	g.auto_str_funcs.writeln('\treturn _STR("%.*s\\000${clean_type_v_type_name}(%.*s\\000)", 3, indents, ${parent_str_fn_name}(it));')
	g.auto_str_funcs.writeln('}')
}

fn (mut g Gen) gen_str_for_array(info table.Array, styp string, str_fn_name string) {
	mut typ := info.elem_type
	mut sym := g.table.get_type_symbol(info.elem_type)
	if mut sym.info is table.Alias {
		typ = sym.info.parent_type
		sym = g.table.get_type_symbol(typ)
	}
	field_styp := g.typ(typ)
	is_elem_ptr := typ.is_ptr()
	sym_has_str_method, str_method_expects_ptr, _ := sym.str_method_info()
	mut elem_str_fn_name := ''
	if sym_has_str_method {
		elem_str_fn_name = if is_elem_ptr { field_styp.replace('*', '') + '_str' } else { field_styp +
				'_str' }
		if sym.kind == .byte {
			elem_str_fn_name = elem_str_fn_name + '_escaped'
		}
	} else {
		elem_str_fn_name = styp_to_str_fn_name(field_styp)
	}
	if !sym_has_str_method {
		g.gen_str_for_type_with_styp(typ, field_styp)
	}
	g.type_definitions.writeln('string ${str_fn_name}($styp a); // auto')
	g.auto_str_funcs.writeln('string ${str_fn_name}($styp a) { return indent_${str_fn_name}(a, 0);}')
	g.type_definitions.writeln('string indent_${str_fn_name}($styp a, int indent_count); // auto')
	g.auto_str_funcs.writeln('string indent_${str_fn_name}($styp a, int indent_count) {')
	g.auto_str_funcs.writeln('\tstrings__Builder sb = strings__new_builder(a.len * 10);')
	g.auto_str_funcs.writeln('\tstrings__Builder_write(&sb, tos_lit("["));')
	g.auto_str_funcs.writeln('\tfor (int i = 0; i < a.len; ++i) {')
	g.auto_str_funcs.writeln('\t\t$field_styp it = (*($field_styp*)array_get(a, i));')
	if sym.kind == .struct_ && !sym_has_str_method {
		if is_elem_ptr {
			g.auto_str_funcs.writeln('\t\tstring x = indent_${elem_str_fn_name}(*it, indent_count);')
		} else {
			g.auto_str_funcs.writeln('\t\tstring x = indent_${elem_str_fn_name}(it, indent_count);')
		}
	} else if sym.kind in [.f32, .f64] {
		g.auto_str_funcs.writeln('\t\tstring x = _STR("%g", 1, it);')
	} else {
		// There is a custom .str() method, so use it.
		// NB: we need to take account of whether the user has defined
		// `fn (x T) str() {` or `fn (x &T) str() {`, and convert accordingly
		if (str_method_expects_ptr && is_elem_ptr) || (!str_method_expects_ptr && !is_elem_ptr) {
			g.auto_str_funcs.writeln('\t\tstring x = ${elem_str_fn_name}(it);')
		} else if str_method_expects_ptr && !is_elem_ptr {
			g.auto_str_funcs.writeln('\t\tstring x = ${elem_str_fn_name}(&it);')
		} else if !str_method_expects_ptr && is_elem_ptr {
			g.auto_str_funcs.writeln('\t\tstring x = ${elem_str_fn_name}(*it);')
		}
	}
	g.auto_str_funcs.writeln('\t\tstrings__Builder_write(&sb, x);')
	if g.pref.autofree && typ != table.bool_type {
		// no need to free "true"/"false" literals
		g.auto_str_funcs.writeln('\t\tstring_free(&x);')
	}
	g.auto_str_funcs.writeln('\t\tif (i < a.len-1) {')
	g.auto_str_funcs.writeln('\t\t\tstrings__Builder_write(&sb, tos_lit(", "));')
	g.auto_str_funcs.writeln('\t\t}')
	g.auto_str_funcs.writeln('\t}')
	g.auto_str_funcs.writeln('\tstrings__Builder_write(&sb, tos_lit("]"));')
	g.auto_str_funcs.writeln('\tstring res = strings__Builder_str(&sb);')
	g.auto_str_funcs.writeln('\tstrings__Builder_free(&sb);')
	// g.auto_str_funcs.writeln('\treturn strings__Builder_str(&sb);')
	g.auto_str_funcs.writeln('\treturn res;')
	g.auto_str_funcs.writeln('}')
}

fn (mut g Gen) gen_str_for_array_fixed(info table.ArrayFixed, styp string, str_fn_name string) {
	mut typ := info.elem_type
	mut sym := g.table.get_type_symbol(info.elem_type)
	if mut sym.info is table.Alias {
		typ = sym.info.parent_type
		sym = g.table.get_type_symbol(typ)
	}
	field_styp := g.typ(typ)
	is_elem_ptr := typ.is_ptr()
	sym_has_str_method, str_method_expects_ptr, _ := sym.str_method_info()
	mut elem_str_fn_name := ''
	if sym_has_str_method {
		elem_str_fn_name = if is_elem_ptr { field_styp.replace('*', '') + '_str' } else { field_styp +
				'_str' }
	} else {
		elem_str_fn_name = styp_to_str_fn_name(field_styp)
	}
	if !sym.has_method('str') {
		elem_str_fn_name = g.gen_str_for_type_with_styp(typ, field_styp)
	}
	g.type_definitions.writeln('string ${str_fn_name}($styp a); // auto')
	g.auto_str_funcs.writeln('string ${str_fn_name}($styp a) { return indent_${str_fn_name}(a, 0);}')
	g.type_definitions.writeln('string indent_${str_fn_name}($styp a, int indent_count); // auto')
	g.auto_str_funcs.writeln('string indent_${str_fn_name}($styp a, int indent_count) {')
	g.auto_str_funcs.writeln('\tstrings__Builder sb = strings__new_builder($info.size * 10);')
	g.auto_str_funcs.writeln('\tstrings__Builder_write(&sb, tos_lit("["));')
	g.auto_str_funcs.writeln('\tfor (int i = 0; i < $info.size; ++i) {')
	if sym.kind == .struct_ && !sym_has_str_method {
		g.auto_str_funcs.writeln('\t\tstrings__Builder_write(&sb, ${elem_str_fn_name}(a[i], indent_count));')
	} else if sym.kind in [.f32, .f64] {
		g.auto_str_funcs.writeln('\t\tstrings__Builder_write(&sb, _STR("%g", 1, a[i]));')
	} else if sym.kind == .string {
		g.auto_str_funcs.writeln('\t\tstrings__Builder_write(&sb, _STR("\'%.*s\\000\'", 2, a[i]));')
	} else {
		if (str_method_expects_ptr && is_elem_ptr) || (!str_method_expects_ptr && !is_elem_ptr) {
			g.auto_str_funcs.writeln('\t\tstrings__Builder_write(&sb, ${elem_str_fn_name}(a[i]));')
		} else if str_method_expects_ptr && !is_elem_ptr {
			g.auto_str_funcs.writeln('\t\tstrings__Builder_write(&sb, ${elem_str_fn_name}(&a[i]));')
		} else if !str_method_expects_ptr && is_elem_ptr {
			g.auto_str_funcs.writeln('\t\tstrings__Builder_write(&sb, ${elem_str_fn_name}(*a[i]));')
		}
	}
	g.auto_str_funcs.writeln('\t\tif (i < ${info.size - 1}) {')
	g.auto_str_funcs.writeln('\t\t\tstrings__Builder_write(&sb, tos_lit(", "));')
	g.auto_str_funcs.writeln('\t\t}')
	g.auto_str_funcs.writeln('\t}')
	g.auto_str_funcs.writeln('\tstrings__Builder_write(&sb, tos_lit("]"));')
	g.auto_str_funcs.writeln('\treturn strings__Builder_str(&sb);')
	g.auto_str_funcs.writeln('}')
}

fn (mut g Gen) gen_str_for_map(info table.Map, styp string, str_fn_name string) {
	key_sym := g.table.get_type_symbol(info.key_type)
	key_styp := g.typ(info.key_type)
	if !key_sym.has_method('str') {
		g.gen_str_for_type_with_styp(info.key_type, key_styp)
	}
	val_sym := g.table.get_type_symbol(info.value_type)
	val_styp := g.typ(info.value_type)
	elem_str_fn_name := val_styp.replace('*', '') + '_str'
	if !val_sym.has_method('str') {
		g.gen_str_for_type_with_styp(info.value_type, val_styp)
	}
	zero := g.type_default(info.value_type)
	g.type_definitions.writeln('string ${str_fn_name}($styp m); // auto')
	g.auto_str_funcs.writeln('string ${str_fn_name}($styp m) { return indent_${str_fn_name}(m, 0);}')
	g.type_definitions.writeln('string indent_${str_fn_name}($styp m, int indent_count); // auto')
	g.auto_str_funcs.writeln('string indent_${str_fn_name}($styp m, int indent_count) { /* gen_str_for_map */')
	g.auto_str_funcs.writeln('\tstrings__Builder sb = strings__new_builder(m.key_values.len*10);')
	g.auto_str_funcs.writeln('\tstrings__Builder_write(&sb, tos_lit("{"));')
	g.auto_str_funcs.writeln('\tfor (unsigned int i = 0; i < m.key_values.len; ++i) {')
	g.auto_str_funcs.writeln('\t\tif (m.key_values.keys[i].str == 0) { continue; }')
	g.auto_str_funcs.writeln('\t\tstring key = (*(string*)DenseArray_get(m.key_values, i));')
	g.auto_str_funcs.writeln('\t\tstrings__Builder_write(&sb, _STR("\'%.*s\\000\'", 2, key));')
	g.auto_str_funcs.writeln('\t\tstrings__Builder_write(&sb, tos_lit(": "));')
	g.auto_str_funcs.write('\t$val_styp it = (*($val_styp*)map_get(')
	g.auto_str_funcs.write('m, (*(string*)DenseArray_get(m.key_values, i))')
	g.auto_str_funcs.write(', ')
	g.auto_str_funcs.writeln(' &($val_styp[]) { $zero }));')
	if val_sym.kind == .string {
		g.auto_str_funcs.writeln('\t\tstrings__Builder_write(&sb, _STR("\'%.*s\\000\'", 2, it));')
	} else if val_sym.kind == .struct_ && !val_sym.has_method('str') {
		g.auto_str_funcs.writeln('\t\tstrings__Builder_write(&sb, indent_${elem_str_fn_name}(it, indent_count));')
	} else if val_sym.kind in [.f32, .f64] {
		g.auto_str_funcs.writeln('\t\tstrings__Builder_write(&sb, _STR("%g", 1, it));')
	} else {
		g.auto_str_funcs.writeln('\t\tstrings__Builder_write(&sb, ${elem_str_fn_name}(it));')
	}
	g.auto_str_funcs.writeln('\t\tif (i != m.key_values.len-1) {')
	g.auto_str_funcs.writeln('\t\t\tstrings__Builder_write(&sb, tos_lit(", "));')
	g.auto_str_funcs.writeln('\t\t}')
	g.auto_str_funcs.writeln('\t}')
	g.auto_str_funcs.writeln('\tstrings__Builder_write(&sb, tos_lit("}"));')
	g.auto_str_funcs.writeln('\treturn strings__Builder_str(&sb);')
	g.auto_str_funcs.writeln('}')
}

fn (mut g Gen) gen_str_for_varg(styp string, str_fn_name string, has_str_method bool) {
	g.definitions.writeln('string varg_${str_fn_name}(varg_$styp it); // auto')
	g.auto_str_funcs.writeln('string varg_${str_fn_name}(varg_$styp it) {')
	g.auto_str_funcs.writeln('\tstrings__Builder sb = strings__new_builder(it.len);')
	g.auto_str_funcs.writeln('\tstrings__Builder_write(&sb, tos_lit("["));')
	g.auto_str_funcs.writeln('\tfor(int i=0; i<it.len; ++i) {')
	g.auto_str_funcs.writeln('\t\tstrings__Builder_write(&sb, ${str_fn_name}(it.args[i]));')
	g.auto_str_funcs.writeln('\t\tif (i < it.len-1) {')
	g.auto_str_funcs.writeln('\t\t\tstrings__Builder_write(&sb, tos_lit(", "));')
	g.auto_str_funcs.writeln('\t\t}')
	g.auto_str_funcs.writeln('\t}')
	g.auto_str_funcs.writeln('\tstrings__Builder_write(&sb, tos_lit("]"));')
	g.auto_str_funcs.writeln('\treturn strings__Builder_str(&sb);')
	g.auto_str_funcs.writeln('}')
}

fn (mut g Gen) gen_str_for_multi_return(info table.MultiReturn, styp string, str_fn_name string) {
	for typ in info.types {
		sym := g.table.get_type_symbol(typ)
		if !sym.has_method('str') {
			field_styp := g.typ(typ)
			g.gen_str_for_type_with_styp(typ, field_styp)
		}
	}
	g.type_definitions.writeln('string ${str_fn_name}($styp a); // auto')
	g.auto_str_funcs.writeln('string ${str_fn_name}($styp a) {')
	g.auto_str_funcs.writeln('\tstrings__Builder sb = strings__new_builder($info.types.len * 10);')
	g.auto_str_funcs.writeln('\tstrings__Builder_write(&sb, tos_lit("("));')
	for i, typ in info.types {
		sym := g.table.get_type_symbol(typ)
		field_styp := g.typ(typ)
		is_arg_ptr := typ.is_ptr()
		sym_has_str_method, str_method_expects_ptr, _ := sym.str_method_info()
		mut arg_str_fn_name := ''
		if sym_has_str_method {
			arg_str_fn_name = if is_arg_ptr { field_styp.replace('*', '') + '_str' } else { field_styp +
					'_str' }
		} else {
			arg_str_fn_name = styp_to_str_fn_name(field_styp)
		}
		if sym.kind == .struct_ && !sym_has_str_method {
			g.auto_str_funcs.writeln('\tstrings__Builder_write(&sb, ${str_fn_name}(a.arg$i));')
		} else if sym.kind in [.f32, .f64] {
			g.auto_str_funcs.writeln('\tstrings__Builder_write(&sb, _STR("%g", 1, a.arg$i));')
		} else if sym.kind == .string {
			g.auto_str_funcs.writeln('\tstrings__Builder_write(&sb, _STR("\'%.*s\\000\'", 2, a.arg$i));')
		} else {
			if (str_method_expects_ptr && is_arg_ptr) || (!str_method_expects_ptr && !is_arg_ptr) {
				g.auto_str_funcs.writeln('\tstrings__Builder_write(&sb, ${arg_str_fn_name}(a.arg$i));')
			} else if str_method_expects_ptr && !is_arg_ptr {
				g.auto_str_funcs.writeln('\tstrings__Builder_write(&sb, ${arg_str_fn_name}(&a.arg$i));')
			} else if !str_method_expects_ptr && is_arg_ptr {
				g.auto_str_funcs.writeln('\tstrings__Builder_write(&sb, ${arg_str_fn_name}(*a.arg$i));')
			}
		}
		if i != info.types.len - 1 {
			g.auto_str_funcs.writeln('\tstrings__Builder_write(&sb, tos_lit(", "));')
		}
	}
	g.auto_str_funcs.writeln('\tstrings__Builder_write(&sb, tos_lit(")"));')
	g.auto_str_funcs.writeln('\treturn strings__Builder_str(&sb);')
	g.auto_str_funcs.writeln('}')
}

fn (mut g Gen) gen_str_for_struct(info table.Struct, styp string, str_fn_name string) {
	// TODO: short it if possible
	// generates all definitions of substructs
	mut fnames2strfunc := {
		'': ''
	} // map[string]string // TODO vfmt bug
	for field in info.fields {
		sym := g.table.get_type_symbol(field.typ)
		if !sym.has_method('str') {
			mut typ := field.typ
			if typ.is_ptr() {
				typ = typ.deref()
			}
			field_styp := g.typ(typ)
			field_fn_name := g.gen_str_for_type_with_styp(field.typ, field_styp)
			fnames2strfunc[field_styp] = field_fn_name
		}
	}
	// _str() functions should have a single argument, the indenting ones take 2:
	g.type_definitions.writeln('string ${str_fn_name}($styp x); // auto')
	g.auto_str_funcs.writeln('string ${str_fn_name}($styp x) { return indent_${str_fn_name}(x, 0);}')
	g.type_definitions.writeln('string indent_${str_fn_name}($styp x, int indent_count); // auto')
	g.auto_str_funcs.writeln('string indent_${str_fn_name}($styp x, int indent_count) {')
	mut clean_struct_v_type_name := styp.replace('__', '.')
	if clean_struct_v_type_name.contains('_T_') {
		// TODO: this is a bit hacky. styp shouldn't be even parsed with _T_
		// use something different than g.typ for styp
		clean_struct_v_type_name = clean_struct_v_type_name.replace('_T_', '<').replace('_', ', ') +
			'>'
	}
	if styp.ends_with('*') {
		deref_typ := styp.replace('*', '')
		g.auto_str_funcs.writeln('\t$deref_typ *it = x;')
		clean_struct_v_type_name = '&' + clean_struct_v_type_name.replace('*', '')
	} else {
		deref_typ := styp
		g.auto_str_funcs.writeln('\t$deref_typ *it = &x;')
	}
	clean_struct_v_type_name = util.strip_main_name(clean_struct_v_type_name)
	// generate ident / indent length = 4 spaces
	g.auto_str_funcs.writeln('\tstring indents = tos_lit("");')
	g.auto_str_funcs.writeln('\tfor (int i = 0; i < indent_count; ++i) {')
	g.auto_str_funcs.writeln('\t\tindents = string_add(indents, tos_lit("    "));')
	g.auto_str_funcs.writeln('\t}')
	if info.fields.len == 0 {
		g.auto_str_funcs.write('\treturn tos_lit("$clean_struct_v_type_name{}");')
	} else {
		g.auto_str_funcs.write('\treturn _STR("$clean_struct_v_type_name{\\n"')
		for field in info.fields {
			mut fmt := g.type_to_fmt(field.typ)
			if field.typ.is_ptr() {
				fmt = '&$fmt'
			}
			g.auto_str_funcs.writeln('\t\t"%.*s\\000    $field.name: $fmt\\n"')
		}
		g.auto_str_funcs.write('\t\t"%.*s\\000}", ${2 * (info.fields.len + 1)}')
		if info.fields.len > 0 {
			g.auto_str_funcs.write(',\n\t\t')
			for i, field in info.fields {
				sym := g.table.get_type_symbol(field.typ)
				has_custom_str := sym.has_method('str')
				mut field_styp := g.typ(field.typ)
				if field_styp.ends_with('*') {
					field_styp = field_styp.replace('*', '')
				}
				field_styp_fn_name := if has_custom_str { '${field_styp}_str' } else { fnames2strfunc[field_styp] }
				g.auto_str_funcs.write('indents, ')
				func := struct_auto_str_func(sym, field.typ, field_styp_fn_name, field.name)
				if field.typ.is_ptr() {
					g.auto_str_funcs.write('isnil(it->${c_name(field.name)})')
					g.auto_str_funcs.write(' ? tos_lit("nil") : ')
					// struct, floats and ints have a special case through the _str function
					if sym.kind != .struct_ && !field.typ.is_int() && !field.typ.is_float() {
						g.auto_str_funcs.write('*')
					}
				}
				g.auto_str_funcs.write(func)
				if i < info.fields.len - 1 {
					g.auto_str_funcs.write(',\n\t\t')
				}
			}
		}
		g.auto_str_funcs.writeln(',')
		g.auto_str_funcs.writeln('\t\tindents);')
	}
	g.auto_str_funcs.writeln('}')
}

fn struct_auto_str_func(sym table.TypeSymbol, field_type table.Type, fn_name string, field_name string) string {
	has_custom_str := sym.has_method('str')
	if sym.kind == .enum_ {
		return '${fn_name}(it->${c_name(field_name)})'
	} else if sym.kind == .struct_ {
		mut obj := 'it->${c_name(field_name)}'
		if field_type.is_ptr() {
			obj = '*$obj'
		}
		if has_custom_str {
			return '${fn_name}($obj)'
		}
		return 'indent_${fn_name}($obj, indent_count + 1)'
	} else if sym.kind in [.array, .array_fixed, .map, .sum_type] {
		if has_custom_str {
			return '${fn_name}(it->${c_name(field_name)})'
		}
		return 'indent_${fn_name}(it->${c_name(field_name)}, indent_count + 1)'
	} else {
		mut method_str := 'it->${c_name(field_name)}'
		if sym.kind == .bool {
			method_str += ' ? _SLIT("true") : _SLIT("false")'
		} else if (field_type.is_int() || field_type.is_float()) && field_type.is_ptr() {
			// ptr int can be "nil", so this needs to be castet to a string
			fmt := if sym.kind in [.f32, .f64] {
				'%g\\000'
			} else if sym.kind == .u64 {
				'%lld\\000'
			} else {
				'%d\\000'
			}
			method_str = '_STR("$fmt", 2, *$method_str)'
		}
		return method_str
	}
}

fn (mut g Gen) gen_str_for_enum(info table.Enum, styp string, str_fn_name string) {
	s := util.no_dots(styp)
	g.type_definitions.writeln('string ${str_fn_name}($styp it); // auto')
	g.auto_str_funcs.writeln('string ${str_fn_name}($styp it) { /* gen_str_for_enum */')
	g.auto_str_funcs.writeln('\tswitch(it) {')
	// Only use the first multi value on the lookup
	mut seen := []string{len: info.vals.len}
	for val in info.vals {
		if info.is_multi_allowed && val in seen {
			continue
		} else if info.is_multi_allowed {
			seen << val
		}
		g.auto_str_funcs.writeln('\t\tcase ${s}_$val: return tos_lit("$val");')
	}
	g.auto_str_funcs.writeln('\t\tdefault: return tos_lit("unknown enum value");')
	g.auto_str_funcs.writeln('\t}')
	g.auto_str_funcs.writeln('}')
}

fn (mut g Gen) gen_str_for_union_sum_type(info table.SumType, styp string, str_fn_name string) {
	mut gen_fn_names := map[string]string{}
	for typ in info.variants {
		sym := g.table.get_type_symbol(typ)
		if !sym.has_method('str') {
			field_styp := g.typ(typ)
			field_fn_name := g.gen_str_for_type_with_styp(typ, field_styp)
			gen_fn_names[field_styp] = field_fn_name
		}
	}
	// _str() functions should have a single argument, the indenting ones take 2:
	g.type_definitions.writeln('string ${str_fn_name}($styp x); // auto')
	g.auto_str_funcs.writeln('string ${str_fn_name}($styp x) { return indent_${str_fn_name}(x, 0); }')
	g.type_definitions.writeln('string indent_${str_fn_name}($styp x, int indent_count); // auto')
	g.auto_str_funcs.writeln('string indent_${str_fn_name}($styp x, int indent_count) {')
	mut clean_sum_type_v_type_name := styp.replace('__', '.')
	if styp.ends_with('*') {
		clean_sum_type_v_type_name = '&' + clean_sum_type_v_type_name.replace('*', '')
	}
	clean_sum_type_v_type_name = util.strip_main_name(clean_sum_type_v_type_name)
	g.auto_str_funcs.writeln('\tswitch(x.typ) {')
	for typ in info.variants {
		mut value_fmt := '%.*s\\000'
		if typ == table.string_type {
			value_fmt = "'$value_fmt'"
		}
		typ_str := g.typ(typ)
		mut func_name := if typ_str in gen_fn_names { gen_fn_names[typ_str] } else { g.gen_str_for_type_with_styp(typ,
				typ_str) }
		sym := g.table.get_type_symbol(typ)
		if sym.kind == .struct_ {
			func_name = 'indent_$func_name'
		}
		g.auto_str_funcs.write('\t\tcase $typ: return _STR("${clean_sum_type_v_type_name}($value_fmt)", 2, ${func_name}(*($typ_str*)x._$typ.idx()')
		if sym.kind == .struct_ {
			g.auto_str_funcs.write(', indent_count')
		}
		g.auto_str_funcs.writeln('));')
	}
	g.auto_str_funcs.writeln('\t\tdefault: return tos_lit("unknown sum type value");')
	g.auto_str_funcs.writeln('\t}')
	g.auto_str_funcs.writeln('}')
}
