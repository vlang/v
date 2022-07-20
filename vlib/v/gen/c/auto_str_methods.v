// Copyright (c) 2019-2022 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license that can be found in the LICENSE file.
module c

import v.ast
import v.util
import strings

const (
	// BUG: this const is not released from the memory! use a const for now
	// si_s_code = "0x" + int(StrIntpType.si_s).hex() // code for a simple string
	si_s_code = '0xfe10'
)

fn (mut g Gen) gen_str_default(sym ast.TypeSymbol, styp string, str_fn_name string) {
	$if trace_autostr ? {
		eprintln('> gen_str_default: $sym.name | $styp | str_fn_name')
	}
	mut convertor := ''
	mut typename_ := ''
	if sym.parent_idx in ast.integer_type_idxs {
		convertor = 'int'
		typename_ = 'int'
	} else if sym.parent_idx == ast.f32_type_idx {
		convertor = 'float'
		typename_ = 'f32'
	} else if sym.parent_idx == ast.f64_type_idx {
		convertor = 'double'
		typename_ = 'f64'
	} else if sym.parent_idx == ast.bool_type_idx {
		convertor = 'bool'
		typename_ = 'bool'
	} else {
		verror('could not generate string method for type `$styp`')
	}
	g.definitions.writeln('string ${str_fn_name}($styp it); // auto')
	g.auto_str_funcs.writeln('string ${str_fn_name}($styp it) {')
	if convertor == 'bool' {
		g.auto_str_funcs.writeln('\tstring tmp1 = string__plus(_SLIT("${styp}("), ($convertor)it ? _SLIT("true") : _SLIT("false"));')
	} else {
		g.auto_str_funcs.writeln('\tstring tmp1 = string__plus(_SLIT("${styp}("), tos3(${typename_}_str(($convertor)it).str));')
	}
	g.auto_str_funcs.writeln('\tstring tmp2 = string__plus(tmp1, _SLIT(")"));')
	g.auto_str_funcs.writeln('\tstring_free(&tmp1);')
	g.auto_str_funcs.writeln('\treturn tmp2;')
	g.auto_str_funcs.writeln('}')
}

struct StrType {
	styp string
mut:
	typ ast.Type
}

fn (mut g Gen) get_str_fn(typ ast.Type) string {
	$if trace_autostr ? {
		eprintln('> get_str_fn: $typ.debug()')
	}
	mut unwrapped := g.unwrap_generic(typ).set_nr_muls(0).clear_flag(.variadic)
	if g.pref.nofloat {
		if typ == ast.f32_type {
			unwrapped = ast.u32_type
		} else if typ == ast.f64_type {
			unwrapped = ast.u64_type
		}
	}
	if typ.has_flag(.optional) {
		unwrapped.set_flag(.optional)
	}
	styp := g.typ(unwrapped)
	mut sym := g.table.sym(unwrapped)
	mut str_fn_name := styp_to_str_fn_name(styp)
	if mut sym.info is ast.Alias && !sym.has_method('str') {
		if sym.info.is_import {
			sym = g.table.sym(sym.info.parent_type)
			str_fn_name = styp_to_str_fn_name(sym.name)
		}
	}
	if sym.has_method_with_generic_parent('str') && mut sym.info is ast.Struct {
		str_fn_name = g.generic_fn_name(sym.info.concrete_types, str_fn_name, false)
	}
	g.str_types << StrType{
		typ: unwrapped
		styp: styp
	}
	return str_fn_name
}

fn (mut g Gen) final_gen_str(typ StrType) {
	if typ in g.generated_str_fns {
		return
	}
	$if trace_autostr ? {
		eprintln('> final_gen_str: $typ')
	}
	g.generated_str_fns << typ
	sym := g.table.sym(typ.typ)
	if sym.has_method_with_generic_parent('str') && !(typ.typ.has_flag(.optional)
		|| typ.typ.has_flag(.result)) {
		return
	}
	styp := typ.styp
	str_fn_name := styp_to_str_fn_name(styp)
	if typ.typ.has_flag(.optional) {
		g.gen_str_for_option(typ.typ, styp, str_fn_name)
		return
	}
	if typ.typ.has_flag(.result) {
		g.gen_str_for_result(typ.typ, styp, str_fn_name)
		return
	}
	match sym.info {
		ast.Alias {
			if sym.info.is_import {
				g.gen_str_default(sym, styp, str_fn_name)
			} else {
				g.gen_str_for_alias(sym.info, styp, str_fn_name)
			}
		}
		ast.Array {
			g.gen_str_for_array(sym.info, styp, str_fn_name)
		}
		ast.ArrayFixed {
			g.gen_str_for_array_fixed(sym.info, styp, str_fn_name)
		}
		ast.Enum {
			g.gen_str_for_enum(sym.info, styp, str_fn_name)
		}
		ast.FnType {
			g.gen_str_for_fn_type(sym.info, styp, str_fn_name)
		}
		ast.Struct {
			g.gen_str_for_struct(sym.info, styp, str_fn_name)
		}
		ast.Map {
			g.gen_str_for_map(sym.info, styp, str_fn_name)
		}
		ast.MultiReturn {
			g.gen_str_for_multi_return(sym.info, styp, str_fn_name)
		}
		ast.SumType {
			g.gen_str_for_union_sum_type(sym.info, styp, str_fn_name)
		}
		ast.Interface {
			g.gen_str_for_interface(sym.info, styp, str_fn_name)
		}
		ast.Chan {
			g.gen_str_for_chan(sym.info, styp, str_fn_name)
		}
		ast.Thread {
			g.gen_str_for_thread(sym.info, styp, str_fn_name)
		}
		else {
			verror('could not generate string method `$str_fn_name` for type `$styp`')
		}
	}
}

fn (mut g Gen) gen_str_for_option(typ ast.Type, styp string, str_fn_name string) {
	$if trace_autostr ? {
		eprintln('> gen_str_for_option: $typ.debug() | $styp | $str_fn_name')
	}
	parent_type := typ.clear_flag(.optional)
	sym := g.table.sym(parent_type)
	sym_has_str_method, _, _ := sym.str_method_info()
	parent_str_fn_name := g.get_str_fn(parent_type)

	g.definitions.writeln('string ${str_fn_name}($styp it); // auto')
	g.auto_str_funcs.writeln('string ${str_fn_name}($styp it) { return indent_${str_fn_name}(it, 0); }')
	g.definitions.writeln('string indent_${str_fn_name}($styp it, int indent_count); // auto')
	g.auto_str_funcs.writeln('string indent_${str_fn_name}($styp it, int indent_count) {')
	g.auto_str_funcs.writeln('\tstring res;')
	g.auto_str_funcs.writeln('\tif (it.state == 0) {')
	if sym.kind == .string {
		tmp_res := '${parent_str_fn_name}(*($sym.cname*)it.data)'
		g.auto_str_funcs.writeln('\t\tres = ${str_intp_sq(tmp_res)};')
	} else if should_use_indent_func(sym.kind) && !sym_has_str_method {
		g.auto_str_funcs.writeln('\t\tres = indent_${parent_str_fn_name}(*($sym.cname*)it.data, indent_count);')
	} else {
		g.auto_str_funcs.writeln('\t\tres = ${parent_str_fn_name}(*($sym.cname*)it.data);')
	}
	g.auto_str_funcs.writeln('\t} else {')

	tmp_str := str_intp_sub('error: %%', 'IError_str(it.err)')
	g.auto_str_funcs.writeln('\t\tres = $tmp_str;')
	g.auto_str_funcs.writeln('\t}')

	g.auto_str_funcs.writeln('\treturn ${str_intp_sub('Option(%%)', 'res')};')
	g.auto_str_funcs.writeln('}')
}

fn (mut g Gen) gen_str_for_result(typ ast.Type, styp string, str_fn_name string) {
	$if trace_autostr ? {
		eprintln('> gen_str_for_result: $typ.debug() | $styp | $str_fn_name')
	}
	parent_type := typ.clear_flag(.result)
	sym := g.table.sym(parent_type)
	sym_has_str_method, _, _ := sym.str_method_info()
	parent_str_fn_name := g.get_str_fn(parent_type)

	g.definitions.writeln('string ${str_fn_name}($styp it); // auto')
	g.auto_str_funcs.writeln('string ${str_fn_name}($styp it) { return indent_${str_fn_name}(it, 0); }')
	g.definitions.writeln('string indent_${str_fn_name}($styp it, int indent_count); // auto')
	g.auto_str_funcs.writeln('string indent_${str_fn_name}($styp it, int indent_count) {')
	g.auto_str_funcs.writeln('\tstring res;')
	g.auto_str_funcs.writeln('\tif (!it.is_error) {')
	if sym.kind == .string {
		tmp_res := '${parent_str_fn_name}(*($sym.cname*)it.data)'
		g.auto_str_funcs.writeln('\t\tres = ${str_intp_sq(tmp_res)};')
	} else if should_use_indent_func(sym.kind) && !sym_has_str_method {
		g.auto_str_funcs.writeln('\t\tres = indent_${parent_str_fn_name}(*($sym.cname*)it.data, indent_count);')
	} else {
		g.auto_str_funcs.writeln('\t\tres = ${parent_str_fn_name}(*($sym.cname*)it.data);')
	}
	g.auto_str_funcs.writeln('\t} else {')

	tmp_str := str_intp_sub('error: %%', 'IError_str(it.err)')
	g.auto_str_funcs.writeln('\t\tres = $tmp_str;')
	g.auto_str_funcs.writeln('\t}')

	g.auto_str_funcs.writeln('\treturn ${str_intp_sub('result(%%)', 'res')};')
	g.auto_str_funcs.writeln('}')
}

fn (mut g Gen) gen_str_for_alias(info ast.Alias, styp string, str_fn_name string) {
	parent_str_fn_name := g.get_str_fn(info.parent_type)
	$if trace_autostr ? {
		eprintln('> gen_str_for_alias: $parent_str_fn_name | $styp | $str_fn_name')
	}
	mut clean_type_v_type_name := util.strip_main_name(styp.replace('__', '.'))
	g.definitions.writeln('static string ${str_fn_name}($styp it); // auto')
	g.auto_str_funcs.writeln('static string ${str_fn_name}($styp it) { return indent_${str_fn_name}(it, 0); }')
	g.definitions.writeln('static string indent_${str_fn_name}($styp it, int indent_count); // auto')
	g.auto_str_funcs.writeln('static string indent_${str_fn_name}($styp it, int indent_count) {')
	g.auto_str_funcs.writeln('\tstring indents = string_repeat(_SLIT("    "), indent_count);')
	g.auto_str_funcs.writeln('\tstring tmp_ds = ${parent_str_fn_name}(it);')
	g.auto_str_funcs.writeln('\tstring res = str_intp(3, _MOV((StrIntpData[]){
		{_SLIT0, $c.si_s_code, {.d_s = indents }},
		{_SLIT("${clean_type_v_type_name}("), $c.si_s_code, {.d_s = tmp_ds }},
		{_SLIT(")"), 0, {.d_c = 0 }}
	}));')
	g.auto_str_funcs.writeln('\tstring_free(&indents);')
	g.auto_str_funcs.writeln('\tstring_free(&tmp_ds);')
	g.auto_str_funcs.writeln('\treturn res;')
	g.auto_str_funcs.writeln('}')
}

fn (mut g Gen) gen_str_for_multi_return(info ast.MultiReturn, styp string, str_fn_name string) {
	$if trace_autostr ? {
		eprintln('> gen_str_for_multi_return: $info.types | $styp | $str_fn_name')
	}
	g.definitions.writeln('static string ${str_fn_name}($styp a); // auto')
	mut fn_builder := strings.new_builder(512)
	fn_builder.writeln('static string ${str_fn_name}($styp a) {')
	fn_builder.writeln('\tstrings__Builder sb = strings__new_builder($info.types.len * 10);')
	fn_builder.writeln('\tstrings__Builder_write_string(&sb, _SLIT("("));')
	for i, typ in info.types {
		sym := g.table.sym(typ)
		is_arg_ptr := typ.is_ptr()
		sym_has_str_method, str_method_expects_ptr, _ := sym.str_method_info()
		arg_str_fn_name := g.get_str_fn(typ)

		if should_use_indent_func(sym.kind) && !sym_has_str_method {
			fn_builder.writeln('\tstrings__Builder_write_string(&sb, ${arg_str_fn_name}(a.arg$i));')
		} else if sym.kind in [.f32, .f64] {
			if sym.kind == .f32 {
				tmp_val := str_intp_g32('a.arg$i')
				fn_builder.writeln('\tstrings__Builder_write_string(&sb, $tmp_val);')
			} else {
				tmp_val := str_intp_g64('a.arg$i')
				fn_builder.writeln('\tstrings__Builder_write_string(&sb, $tmp_val);')
			}
		} else if sym.kind == .string {
			tmp_str := str_intp_sq('a.arg$i')
			fn_builder.writeln('\tstrings__Builder_write_string(&sb, $tmp_str);')
		} else if sym.kind == .function {
			fn_builder.writeln('\tstrings__Builder_write_string(&sb, ${arg_str_fn_name}());')
		} else {
			deref, deref_label := deref_kind(str_method_expects_ptr, is_arg_ptr, typ)
			fn_builder.writeln('\t\tstrings__Builder_write_string(&sb, _SLIT("$deref_label"));')
			fn_builder.writeln('\tstrings__Builder_write_string(&sb, ${arg_str_fn_name}( $deref a.arg$i));')
		}
		if i != info.types.len - 1 {
			fn_builder.writeln('\tstrings__Builder_write_string(&sb, _SLIT(", "));')
		}
	}
	fn_builder.writeln('\tstrings__Builder_write_string(&sb, _SLIT(")"));')
	fn_builder.writeln('\tstring res = strings__Builder_str(&sb);')
	fn_builder.writeln('\tstrings__Builder_free(&sb);')
	fn_builder.writeln('\treturn res;')
	fn_builder.writeln('}')
	g.auto_fn_definitions << fn_builder.str()
}

fn (mut g Gen) gen_str_for_enum(info ast.Enum, styp string, str_fn_name string) {
	$if trace_autostr ? {
		eprintln('> gen_str_for_enum: $info | $styp | $str_fn_name')
	}
	s := util.no_dots(styp)
	g.definitions.writeln('static string ${str_fn_name}($styp it); // auto')
	g.auto_str_funcs.writeln('static string ${str_fn_name}($styp it) { /* gen_str_for_enum */')
	// Enums tagged with `[flag]` are special in that they can be a combination of enum values
	if info.is_flag {
		clean_name := util.strip_main_name(styp.replace('__', '.'))
		g.auto_str_funcs.writeln('\tstring ret = _SLIT("$clean_name{");')
		g.auto_str_funcs.writeln('\tint first = 1;')
		for i, val in info.vals {
			g.auto_str_funcs.writeln('\tif (it & (1 << $i)) {if (!first) {ret = string__plus(ret, _SLIT(" | "));} ret = string__plus(ret, _SLIT(".$val")); first = 0;}')
		}
		g.auto_str_funcs.writeln('\tret = string__plus(ret, _SLIT("}"));')
		g.auto_str_funcs.writeln('\treturn ret;')
	} else {
		g.auto_str_funcs.writeln('\tswitch(it) {')
		// Only use the first multi value on the lookup
		mut seen := []string{len: info.vals.len}
		for val in info.vals {
			if info.is_multi_allowed && val in seen {
				continue
			} else if info.is_multi_allowed {
				seen << val
			}
			g.auto_str_funcs.writeln('\t\tcase ${s}__$val: return _SLIT("$val");')
		}
		g.auto_str_funcs.writeln('\t\tdefault: return _SLIT("unknown enum value");')
		g.auto_str_funcs.writeln('\t}')
	}
	g.auto_str_funcs.writeln('}')
}

fn (mut g Gen) gen_str_for_interface(info ast.Interface, styp string, str_fn_name string) {
	$if trace_autostr ? {
		eprintln('> gen_str_for_interface: $info.types | $styp | $str_fn_name')
	}
	// _str() functions should have a single argument, the indenting ones take 2:
	g.definitions.writeln('static string ${str_fn_name}($styp x); // auto')
	g.auto_str_funcs.writeln('static string ${str_fn_name}($styp x) { return indent_${str_fn_name}(x, 0); }')
	g.definitions.writeln('static string indent_${str_fn_name}($styp x, int indent_count); // auto')
	mut fn_builder := strings.new_builder(512)
	mut clean_interface_v_type_name := styp.replace('__', '.')
	if styp.ends_with('*') {
		clean_interface_v_type_name = '&' + clean_interface_v_type_name.replace('*', '')
	}
	if clean_interface_v_type_name.contains('_T_') {
		clean_interface_v_type_name =
			clean_interface_v_type_name.replace('Array_', '[]').replace('_T_', '<').replace('_', ', ') +
			'>'
	}
	clean_interface_v_type_name = util.strip_main_name(clean_interface_v_type_name)
	fn_builder.writeln('static string indent_${str_fn_name}($styp x, int indent_count) { /* gen_str_for_interface */')
	for typ in info.types {
		sub_sym := g.table.sym(ast.mktyp(typ))
		mut func_name := g.get_str_fn(typ)
		sym_has_str_method, str_method_expects_ptr, _ := sub_sym.str_method_info()
		if should_use_indent_func(sub_sym.kind) && !sym_has_str_method {
			func_name = 'indent_$func_name'
		}

		// str_intp
		deref := if sym_has_str_method && str_method_expects_ptr { ' ' } else { '*' }
		if typ == ast.string_type {
			mut val := '${func_name}(${deref}($sub_sym.cname*)x._$sub_sym.cname'
			if should_use_indent_func(sub_sym.kind) && !sym_has_str_method {
				val += ', indent_count'
			}
			val += ')'
			res := 'str_intp(2, _MOV((StrIntpData[]){
				{_SLIT("${clean_interface_v_type_name}(\'"), $c.si_s_code, {.d_s = $val}},
				{_SLIT("\')"), 0, {.d_c = 0 }}
			}))'
			fn_builder.write_string('\tif (x._typ == _${styp}_${sub_sym.cname}_index)')
			fn_builder.write_string(' return $res;')
		} else {
			mut val := '${func_name}(${deref}($sub_sym.cname*)x._$sub_sym.cname'
			if should_use_indent_func(sub_sym.kind) && !sym_has_str_method {
				val += ', indent_count'
			}
			val += ')'
			res := 'str_intp(2, _MOV((StrIntpData[]){
				{_SLIT("${clean_interface_v_type_name}("), $c.si_s_code, {.d_s = $val}},
				{_SLIT(")"), 0, {.d_c = 0 }}
			}))'
			fn_builder.write_string('\tif (x._typ == _${styp}_${sub_sym.cname}_index)')
			fn_builder.write_string(' return $res;\n')
		}
	}
	fn_builder.writeln('\treturn _SLIT("unknown interface value");')
	fn_builder.writeln('}')
	g.auto_fn_definitions << fn_builder.str()
}

fn (mut g Gen) gen_str_for_union_sum_type(info ast.SumType, styp string, str_fn_name string) {
	$if trace_autostr ? {
		eprintln('> gen_str_for_union_sum_type: $info.variants | $styp | $str_fn_name')
	}
	// _str() functions should have a single argument, the indenting ones take 2:
	g.definitions.writeln('static string ${str_fn_name}($styp x); // auto')
	g.auto_str_funcs.writeln('static string ${str_fn_name}($styp x) { return indent_${str_fn_name}(x, 0); }')
	g.definitions.writeln('static string indent_${str_fn_name}($styp x, int indent_count); // auto')
	mut fn_builder := strings.new_builder(512)
	fn_builder.writeln('static string indent_${str_fn_name}($styp x, int indent_count) {')
	mut clean_sum_type_v_type_name := ''
	if info.is_anon {
		variant_names := info.variants.map(util.strip_main_name(g.table.sym(it).name))
		clean_sum_type_v_type_name = '${variant_names.join('|')}'
	} else {
		clean_sum_type_v_type_name = styp.replace('__', '.')
		if styp.ends_with('*') {
			clean_sum_type_v_type_name = '&' + clean_sum_type_v_type_name.replace('*', '')
		}
		if clean_sum_type_v_type_name.contains('_T_') {
			clean_sum_type_v_type_name =
				clean_sum_type_v_type_name.replace('Array_', '[]').replace('_T_', '<').replace('_', ', ') +
				'>'
		}
		clean_sum_type_v_type_name = util.strip_main_name(clean_sum_type_v_type_name)
	}
	fn_builder.writeln('\tswitch(x._typ) {')
	for typ in info.variants {
		typ_str := g.typ(typ)
		mut func_name := g.get_str_fn(typ)
		sym := g.table.sym(typ)
		sym_has_str_method, str_method_expects_ptr, _ := sym.str_method_info()
		deref := if sym_has_str_method && str_method_expects_ptr { ' ' } else { '*' }
		if should_use_indent_func(sym.kind) && !sym_has_str_method {
			func_name = 'indent_$func_name'
		}

		// str_intp
		if typ == ast.string_type {
			mut val := '${func_name}(${deref}($typ_str*)x._$sym.cname'
			if should_use_indent_func(sym.kind) && !sym_has_str_method {
				val += ', indent_count'
			}
			val += ')'
			res := 'str_intp(2, _MOV((StrIntpData[]){
				{_SLIT("${clean_sum_type_v_type_name}(\'"), $c.si_s_code, {.d_s = $val}},
				{_SLIT("\')"), 0, {.d_c = 0 }}
			}))'
			fn_builder.write_string('\t\tcase $typ.idx(): return $res;\n')
		} else {
			mut val := '${func_name}(${deref}($typ_str*)x._$sym.cname'
			if should_use_indent_func(sym.kind) && !sym_has_str_method {
				val += ', indent_count'
			}
			val += ')'
			res := 'str_intp(2, _MOV((StrIntpData[]){
				{_SLIT("${clean_sum_type_v_type_name}("), $c.si_s_code, {.d_s = $val}},
				{_SLIT(")"), 0, {.d_c = 0 }}
			}))'
			fn_builder.write_string('\t\tcase $typ.idx(): return $res;\n')
		}
	}
	fn_builder.writeln('\t\tdefault: return _SLIT("unknown sum type value");')
	fn_builder.writeln('\t}')
	fn_builder.writeln('}')
	g.auto_fn_definitions << fn_builder.str()
}

fn (mut g Gen) fn_decl_str(info ast.FnType) string {
	mut fn_str := 'fn ('
	for i, arg in info.func.params {
		if arg.is_mut {
			fn_str += 'mut '
		}
		if i > 0 {
			fn_str += ', '
		}
		fn_str += util.strip_main_name(g.table.get_type_name(g.unwrap_generic(arg.typ)))
	}
	fn_str += ')'
	if info.func.return_type == ast.ovoid_type {
		fn_str += ' ?'
	} else if info.func.return_type == ast.rvoid_type {
		fn_str += ' !'
	} else if info.func.return_type != ast.void_type {
		x := util.strip_main_name(g.table.get_type_name(g.unwrap_generic(info.func.return_type)))
		if info.func.return_type.has_flag(.optional) {
			fn_str += ' ?$x'
		} else {
			fn_str += ' $x'
		}
	}
	return fn_str
}

fn (mut g Gen) gen_str_for_fn_type(info ast.FnType, styp string, str_fn_name string) {
	$if trace_autostr ? {
		eprintln('> gen_str_for_fn_type: $info.func.name | $styp | $str_fn_name')
	}
	g.definitions.writeln('static string ${str_fn_name}(); // auto')
	g.auto_str_funcs.writeln('static string ${str_fn_name}() { return _SLIT("${g.fn_decl_str(info)}");}')
}

fn (mut g Gen) gen_str_for_chan(info ast.Chan, styp string, str_fn_name string) {
	$if trace_autostr ? {
		eprintln('> gen_str_for_chan: $info.elem_type.debug() | $styp | $str_fn_name')
	}
	elem_type_name := util.strip_main_name(g.table.get_type_name(g.unwrap_generic(info.elem_type)))
	g.definitions.writeln('static string ${str_fn_name}($styp x); // auto')
	g.auto_str_funcs.writeln('static string ${str_fn_name}($styp x) { return sync__Channel_auto_str(x, _SLIT("$elem_type_name")); }')
}

fn (mut g Gen) gen_str_for_thread(info ast.Thread, styp string, str_fn_name string) {
	$if trace_autostr ? {
		eprintln('> gen_str_for_thread: $info.return_type.debug() | $styp | $str_fn_name')
	}
	ret_type_name := util.strip_main_name(g.table.get_type_name(info.return_type))
	g.definitions.writeln('static string ${str_fn_name}($styp _); // auto}')
	g.auto_str_funcs.writeln('static string ${str_fn_name}($styp _) { return _SLIT("thread($ret_type_name)");}')
}

[inline]
fn styp_to_str_fn_name(styp string) string {
	return styp.replace_each(['*', '', '.', '__', ' ', '__']) + '_str'
}

// deref_kind returns deref, deref_label
fn deref_kind(str_method_expects_ptr bool, is_elem_ptr bool, typ ast.Type) (string, string) {
	if str_method_expects_ptr != is_elem_ptr {
		if is_elem_ptr {
			return '*'.repeat(typ.nr_muls()), '&'.repeat(typ.nr_muls())
		} else {
			return '&', ''
		}
	}
	return '', ''
}

fn (mut g Gen) gen_str_for_array(info ast.Array, styp string, str_fn_name string) {
	$if trace_autostr ? {
		eprintln('> gen_str_for_array: $info.elem_type.debug() | $styp | $str_fn_name')
	}
	mut typ := info.elem_type
	mut sym := g.table.sym(info.elem_type)
	if mut sym.info is ast.Alias {
		typ = sym.info.parent_type
		sym = g.table.sym(typ)
	}
	field_styp := g.typ(typ)
	is_elem_ptr := typ.is_ptr()
	sym_has_str_method, str_method_expects_ptr, _ := sym.str_method_info()
	mut elem_str_fn_name := g.get_str_fn(typ)
	if sym.kind == .u8 {
		elem_str_fn_name = elem_str_fn_name + '_escaped'
	}

	g.definitions.writeln('static string ${str_fn_name}($styp a); // auto')
	g.auto_str_funcs.writeln('static string ${str_fn_name}($styp a) { return indent_${str_fn_name}(a, 0);}')
	g.definitions.writeln('static string indent_${str_fn_name}($styp a, int indent_count); // auto')
	g.auto_str_funcs.writeln('static string indent_${str_fn_name}($styp a, int indent_count) {')
	g.auto_str_funcs.writeln('\tstrings__Builder sb = strings__new_builder(a.len * 10);')
	g.auto_str_funcs.writeln('\tstrings__Builder_write_string(&sb, _SLIT("["));')
	g.auto_str_funcs.writeln('\tfor (int i = 0; i < a.len; ++i) {')
	if sym.kind == .function {
		g.auto_str_funcs.writeln('\t\tstring x = ${elem_str_fn_name}();')
	} else {
		if sym.kind == .array_fixed {
			g.auto_str_funcs.writeln('\t\t$field_styp it;')
			g.auto_str_funcs.writeln('\t\tmemcpy(*($field_styp*)it, (byte*)array_get(a, i), sizeof($field_styp));')
		} else {
			g.auto_str_funcs.writeln('\t\t$field_styp it = *($field_styp*)array_get(a, i);')
		}
		if should_use_indent_func(sym.kind) && !sym_has_str_method {
			if is_elem_ptr {
				g.auto_str_funcs.writeln('\t\tstring x = indent_${elem_str_fn_name}(*it, indent_count);')
			} else {
				g.auto_str_funcs.writeln('\t\tstring x = indent_${elem_str_fn_name}(it, indent_count);')
			}
		} else if sym.kind in [.f32, .f64] {
			if sym.kind == .f32 {
				g.auto_str_funcs.writeln('\t\tstring x = ${str_intp_g32('it')};')
			} else {
				g.auto_str_funcs.writeln('\t\tstring x = ${str_intp_g64('it')};')
			}
		} else if sym.kind == .rune {
			// Rune are managed at this level as strings
			g.auto_str_funcs.writeln('\t\tstring x = str_intp(2, _MOV((StrIntpData[]){{_SLIT("\`"), $c.si_s_code, {.d_s = ${elem_str_fn_name}(it) }}, {_SLIT("\`"), 0, {.d_c = 0 }}}));\n')
		} else if sym.kind == .string {
			if is_elem_ptr {
				g.auto_str_funcs.writeln('\t\tstring x = str_intp(2, _MOV((StrIntpData[]){{_SLIT("&\'"), $c.si_s_code, {.d_s = *it }}, {_SLIT("\'"), 0, {.d_c = 0 }}}));\n')
			} else {
				g.auto_str_funcs.writeln('\t\tstring x = str_intp(2, _MOV((StrIntpData[]){{_SLIT("\'"), $c.si_s_code, {.d_s = it }}, {_SLIT("\'"), 0, {.d_c = 0 }}}));\n')
			}
		} else {
			// There is a custom .str() method, so use it.
			// Note: we need to take account of whether the user has defined
			// `fn (x T) str() {` or `fn (x &T) str() {`, and convert accordingly
			deref, deref_label := deref_kind(str_method_expects_ptr, is_elem_ptr, typ)
			if is_elem_ptr {
				g.auto_str_funcs.writeln('\t\tstring x = _SLIT("nil");')
				g.auto_str_funcs.writeln('\t\tif (it != 0) {')
				g.auto_str_funcs.writeln('\t\t\tstrings__Builder_write_string(&sb, _SLIT("$deref_label"));')
				g.auto_str_funcs.writeln('\t\t\tx = ${elem_str_fn_name}(${deref}it);')
				g.auto_str_funcs.writeln('\t\t}')
			} else {
				g.auto_str_funcs.writeln('\t\tstrings__Builder_write_string(&sb, _SLIT("$deref_label"));')
				g.auto_str_funcs.writeln('\t\tstring x = ${elem_str_fn_name}(${deref}it);')
			}
		}
	}
	g.auto_str_funcs.writeln('\t\tstrings__Builder_write_string(&sb, x);')
	if g.is_autofree && typ != ast.bool_type {
		// no need to free "true"/"false" literals
		g.auto_str_funcs.writeln('\t\tstring_free(&x);')
	}
	g.auto_str_funcs.writeln('\t\tif (i < a.len-1) {')
	g.auto_str_funcs.writeln('\t\t\tstrings__Builder_write_string(&sb, _SLIT(", "));')
	g.auto_str_funcs.writeln('\t\t}')
	g.auto_str_funcs.writeln('\t}')
	g.auto_str_funcs.writeln('\tstrings__Builder_write_string(&sb, _SLIT("]"));')
	g.auto_str_funcs.writeln('\tstring res = strings__Builder_str(&sb);')
	g.auto_str_funcs.writeln('\tstrings__Builder_free(&sb);')
	g.auto_str_funcs.writeln('\treturn res;')
	g.auto_str_funcs.writeln('}')
}

fn (mut g Gen) gen_str_for_array_fixed(info ast.ArrayFixed, styp string, str_fn_name string) {
	$if trace_autostr ? {
		eprintln('> gen_str_for_array_fixed: $info.elem_type.debug() | $styp | $str_fn_name')
	}
	mut typ := info.elem_type
	mut sym := g.table.sym(info.elem_type)
	if mut sym.info is ast.Alias {
		typ = sym.info.parent_type
		sym = g.table.sym(typ)
	}
	is_elem_ptr := typ.is_ptr()
	sym_has_str_method, str_method_expects_ptr, _ := sym.str_method_info()
	elem_str_fn_name := g.get_str_fn(typ)

	g.definitions.writeln('static string ${str_fn_name}($styp a); // auto')
	g.auto_str_funcs.writeln('static string ${str_fn_name}($styp a) { return indent_${str_fn_name}(a, 0);}')
	g.definitions.writeln('static string indent_${str_fn_name}($styp a, int indent_count); // auto')
	g.auto_str_funcs.writeln('static string indent_${str_fn_name}($styp a, int indent_count) {')
	g.auto_str_funcs.writeln('\tstrings__Builder sb = strings__new_builder($info.size * 10);')
	g.auto_str_funcs.writeln('\tstrings__Builder_write_string(&sb, _SLIT("["));')
	g.auto_str_funcs.writeln('\tfor (int i = 0; i < $info.size; ++i) {')
	if sym.kind == .function {
		g.auto_str_funcs.writeln('\t\tstring x = ${elem_str_fn_name}();')
		g.auto_str_funcs.writeln('\t\tstrings__Builder_write_string(&sb, x);')
	} else {
		deref, deref_label := deref_kind(str_method_expects_ptr, is_elem_ptr, typ)
		if should_use_indent_func(sym.kind) && !sym_has_str_method {
			if is_elem_ptr {
				g.auto_str_funcs.writeln('\t\tstrings__Builder_write_string(&sb, _SLIT("$deref_label"));')
				g.auto_str_funcs.writeln('\t\tif ( 0 == a[i] ) {')
				g.auto_str_funcs.writeln('\t\t\tstrings__Builder_write_string(&sb, _SLIT("0"));')
				g.auto_str_funcs.writeln('\t\t}else{')
				g.auto_str_funcs.writeln('\t\t\tstrings__Builder_write_string(&sb, ${elem_str_fn_name}( $deref a[i]) );')
				g.auto_str_funcs.writeln('\t\t}')
			} else {
				g.auto_str_funcs.writeln('\t\tstrings__Builder_write_string(&sb, ${elem_str_fn_name}( $deref a[i]) );')
			}
		} else if sym.kind in [.f32, .f64] {
			if sym.kind == .f32 {
				g.auto_str_funcs.writeln('\t\tstrings__Builder_write_string(&sb, ${str_intp_g32('a[i]')} );')
			} else {
				g.auto_str_funcs.writeln('\t\tstrings__Builder_write_string(&sb, ${str_intp_g64('a[i]')} );')
			}
		} else if sym.kind == .string {
			g.auto_str_funcs.writeln('\t\tstrings__Builder_write_string(&sb, ${str_intp_sq('a[i]')});')
		} else if sym.kind == .rune {
			tmp_str := str_intp_rune('${elem_str_fn_name}(  $deref a[i])')
			g.auto_str_funcs.writeln('\t\tstrings__Builder_write_string(&sb, $tmp_str);')
		} else {
			g.auto_str_funcs.writeln('\t\tstrings__Builder_write_string(&sb, ${elem_str_fn_name}( $deref a[i]));')
		}
	}
	g.auto_str_funcs.writeln('\t\tif (i < ${info.size - 1}) {')
	g.auto_str_funcs.writeln('\t\t\tstrings__Builder_write_string(&sb, _SLIT(", "));')
	g.auto_str_funcs.writeln('\t\t}')
	g.auto_str_funcs.writeln('\t}')
	g.auto_str_funcs.writeln('\tstrings__Builder_write_string(&sb, _SLIT("]"));')
	g.auto_str_funcs.writeln('\tstring res = strings__Builder_str(&sb);')
	g.auto_str_funcs.writeln('\tstrings__Builder_free(&sb);')
	g.auto_str_funcs.writeln('\treturn res;')
	g.auto_str_funcs.writeln('}')
}

fn (mut g Gen) gen_str_for_map(info ast.Map, styp string, str_fn_name string) {
	$if trace_autostr ? {
		eprintln('> gen_str_for_map: $info.key_type.debug() -> $info.value_type.debug() | $styp | $str_fn_name')
	}
	mut key_typ := info.key_type
	mut key_sym := g.table.sym(key_typ)
	if mut key_sym.info is ast.Alias {
		key_typ = key_sym.info.parent_type
		key_sym = g.table.sym(key_typ)
	}
	key_styp := g.typ(key_typ)
	key_str_fn_name := key_styp.replace('*', '') + '_str'
	if !key_sym.has_method('str') {
		g.get_str_fn(key_typ)
	}

	mut val_typ := info.value_type
	mut val_sym := g.table.sym(val_typ)
	if mut val_sym.info is ast.Alias {
		val_typ = val_sym.info.parent_type
		val_sym = g.table.sym(val_typ)
	}
	val_styp := g.typ(val_typ)
	elem_str_fn_name := val_styp.replace('*', '') + '_str'
	if !val_sym.has_method('str') {
		g.get_str_fn(val_typ)
	}

	g.definitions.writeln('static string ${str_fn_name}($styp m); // auto')
	g.auto_str_funcs.writeln('static string ${str_fn_name}($styp m) { return indent_${str_fn_name}(m, 0);}')
	g.definitions.writeln('static string indent_${str_fn_name}($styp m, int indent_count); // auto')
	g.auto_str_funcs.writeln('static string indent_${str_fn_name}($styp m, int indent_count) { /* gen_str_for_map */')
	g.auto_str_funcs.writeln('\tstrings__Builder sb = strings__new_builder(m.key_values.len*10);')
	g.auto_str_funcs.writeln('\tstrings__Builder_write_string(&sb, _SLIT("{"));')
	g.auto_str_funcs.writeln('\tfor (int i = 0; i < m.key_values.len; ++i) {')
	g.auto_str_funcs.writeln('\t\tif (!DenseArray_has_index(&m.key_values, i)) { continue; }')

	if key_sym.kind == .string {
		g.auto_str_funcs.writeln('\t\tstring key = *(string*)DenseArray_key(&m.key_values, i);')
	} else {
		g.auto_str_funcs.writeln('\t\t$key_styp key = *($key_styp*)DenseArray_key(&m.key_values, i);')
	}
	if key_sym.kind == .string {
		g.auto_str_funcs.writeln('\t\tstrings__Builder_write_string(&sb, ${str_intp_sq('key')});')
	} else if key_sym.kind == .rune {
		tmp_str := str_intp_rune('${key_str_fn_name}(key)')
		g.auto_str_funcs.writeln('\t\tstrings__Builder_write_string(&sb, $tmp_str);')
	} else {
		g.auto_str_funcs.writeln('\t\tstrings__Builder_write_string(&sb, ${key_str_fn_name}(key));')
	}
	g.auto_str_funcs.writeln('\t\tstrings__Builder_write_string(&sb, _SLIT(": "));')
	if val_sym.kind == .function {
		g.auto_str_funcs.writeln('\t\tstrings__Builder_write_string(&sb, ${elem_str_fn_name}());')
	} else if val_sym.kind == .string {
		tmp_str := str_intp_sq('*($val_styp*)DenseArray_value(&m.key_values, i)')
		g.auto_str_funcs.writeln('\t\tstrings__Builder_write_string(&sb, $tmp_str);')
	} else if should_use_indent_func(val_sym.kind) && !val_sym.has_method('str') {
		ptr_str := '*'.repeat(val_typ.nr_muls())
		g.auto_str_funcs.writeln('\t\tstrings__Builder_write_string(&sb, indent_${elem_str_fn_name}(*${ptr_str}($val_styp*)DenseArray_value(&m.key_values, i), indent_count));')
	} else if val_sym.kind in [.f32, .f64] {
		tmp_val := '*($val_styp*)DenseArray_value(&m.key_values, i)'
		if val_sym.kind == .f32 {
			g.auto_str_funcs.writeln('\t\tstrings__Builder_write_string(&sb, ${str_intp_g32(tmp_val)});')
		} else {
			g.auto_str_funcs.writeln('\t\tstrings__Builder_write_string(&sb, ${str_intp_g64(tmp_val)});')
		}
	} else if val_sym.kind == .rune {
		tmp_str := str_intp_rune('${elem_str_fn_name}(*($val_styp*)DenseArray_value(&m.key_values, i))')
		g.auto_str_funcs.writeln('\t\tstrings__Builder_write_string(&sb, $tmp_str);')
	} else {
		g.auto_str_funcs.writeln('\t\tstrings__Builder_write_string(&sb, ${elem_str_fn_name}(*($val_styp*)DenseArray_value(&m.key_values, i)));')
	}
	g.auto_str_funcs.writeln('\t\tif (i != m.key_values.len-1) {')
	g.auto_str_funcs.writeln('\t\t\tstrings__Builder_write_string(&sb, _SLIT(", "));')
	g.auto_str_funcs.writeln('\t\t}')
	g.auto_str_funcs.writeln('\t}')
	g.auto_str_funcs.writeln('\tstrings__Builder_write_string(&sb, _SLIT("}"));')
	g.auto_str_funcs.writeln('\tstring res = strings__Builder_str(&sb);')
	g.auto_str_funcs.writeln('\tstrings__Builder_free(&sb);')
	g.auto_str_funcs.writeln('\treturn res;')
	g.auto_str_funcs.writeln('}')
}

fn (g &Gen) type_to_fmt(typ ast.Type) StrIntpType {
	if typ == ast.u8_type_idx {
		return .si_u8
	}
	if typ == ast.char_type_idx {
		return .si_c
	}
	if typ in ast.voidptr_types || typ in ast.byteptr_types {
		return .si_p
	}
	if typ in ast.charptr_types {
		// return '%C\\000' // a C string
		return .si_s
	}
	sym := g.table.sym(typ)
	if typ.is_ptr() && (typ.is_int_valptr() || typ.is_float_valptr()) {
		return .si_s
	} else if sym.kind in [.struct_, .array, .array_fixed, .map, .bool, .enum_, .interface_,
		.sum_type, .function, .alias, .chan] {
		return .si_s
	} else if sym.kind == .string {
		return .si_s
		// return "'%.*s\\000'"
	} else if sym.kind in [.f32, .f64] {
		if sym.kind == .f32 {
			return .si_g32
		}
		return .si_g64
	} else if sym.kind == .int {
		return .si_i32
	} else if sym.kind == .u32 {
		return .si_u32
	} else if sym.kind == .u64 {
		return .si_u64
	} else if sym.kind == .i64 {
		return .si_i64
	}
	return .si_i32
}

fn (mut g Gen) gen_str_for_struct(info ast.Struct, styp string, str_fn_name string) {
	$if trace_autostr ? {
		eprintln('> gen_str_for_struct: $info.parent_type.debug() | $styp | $str_fn_name')
	}
	// _str() functions should have a single argument, the indenting ones take 2:
	g.definitions.writeln('static string ${str_fn_name}($styp it); // auto')
	g.auto_str_funcs.writeln('static string ${str_fn_name}($styp it) { return indent_${str_fn_name}(it, 0);}')
	g.definitions.writeln('static string indent_${str_fn_name}($styp it, int indent_count); // auto')
	mut fn_builder := strings.new_builder(512)
	defer {
		g.auto_fn_definitions << fn_builder.str()
	}
	fn_builder.writeln('static string indent_${str_fn_name}($styp it, int indent_count) {')
	mut clean_struct_v_type_name := styp.replace('__', '.')
	if clean_struct_v_type_name.contains('_T_') {
		// TODO: this is a bit hacky. styp shouldn't be even parsed with _T_
		// use something different than g.typ for styp
		clean_struct_v_type_name =
			clean_struct_v_type_name.replace('Array_', '[]').replace('_T_', '<').replace('_', ', ') +
			'>'
	}
	clean_struct_v_type_name = util.strip_main_name(clean_struct_v_type_name)
	// generate ident / indent length = 4 spaces
	if info.fields.len == 0 {
		fn_builder.writeln('\treturn _SLIT("$clean_struct_v_type_name{}");')
		fn_builder.writeln('}')
		return
	}

	fn_builder.writeln('\tstring indents = string_repeat(_SLIT("    "), indent_count);')

	mut fn_body_surrounder := util.new_surrounder(info.fields.len)
	mut fn_body := strings.new_builder(info.fields.len * 256)
	defer {
		fn_body_surrounder.builder_write_befores(mut fn_builder)
		fn_builder << fn_body
		fn_body_surrounder.builder_write_afters(mut fn_builder)
		fn_builder.writeln('\tstring_free(&indents);')
		fn_builder.writeln('\treturn res;')
		fn_builder.writeln('}')
	}
	fn_body.writeln('\tstring res = str_intp( ${info.fields.len * 4 + 3}, _MOV((StrIntpData[]){')
	fn_body.writeln('\t\t{_SLIT("$clean_struct_v_type_name{\\n"), 0, {.d_c=0}},')
	for i, field in info.fields {
		ftyp_noshared := if field.typ.has_flag(.shared_f) {
			field.typ.deref().clear_flag(.shared_f)
		} else {
			field.typ
		}
		mut ptr_amp := if ftyp_noshared.is_ptr() {
			'&'
		} else {
			''
		}
		base_fmt := g.type_to_fmt(g.unwrap_generic(field.typ))

		// manage prefix and quote symbol for the filed
		mut quote_str := ''
		mut prefix := ''
		sym := g.table.sym(g.unwrap_generic(field.typ))
		if sym.kind == .string {
			quote_str = "'"
		} else if field.typ in ast.charptr_types {
			quote_str = '\\"'
			prefix = 'C'
		}

		// first fields doesn't need \n
		if i == 0 {
			fn_body.write_string('\t\t{_SLIT0, $c.si_s_code, {.d_s=indents}}, {_SLIT("    $field.name: $ptr_amp$prefix"), 0, {.d_c=0}}, ')
		} else {
			fn_body.write_string('\t\t{_SLIT("\\n"), $c.si_s_code, {.d_s=indents}}, {_SLIT("    $field.name: $ptr_amp$prefix"), 0, {.d_c=0}}, ')
		}

		// custom methods management
		sym_has_str_method, str_method_expects_ptr, _ := sym.str_method_info()
		sftyp := g.typ(ftyp_noshared)
		mut field_styp := sftyp.replace('*', '')
		field_styp_fn_name := if sym_has_str_method {
			mut field_fn_name := '${field_styp}_str'
			if sym.info is ast.Struct {
				field_fn_name = g.generic_fn_name(sym.info.concrete_types, field_fn_name,
					false)
			}
			field_fn_name
		} else {
			g.get_str_fn(ftyp_noshared)
		}

		// manage the fact hat with float we use always the g representation
		if sym.kind !in [.f32, .f64] {
			fn_body.write_string('{_SLIT("$quote_str"), ${int(base_fmt)}, {.${data_str(base_fmt)}=')
		} else {
			g_fmt := '0x' + (u32(base_fmt) | u32(0x7F) << 9).hex()
			fn_body.write_string('{_SLIT("$quote_str"), $g_fmt, {.${data_str(base_fmt)}=')
		}

		mut funcprefix := ''
		mut func, mut caller_should_free := struct_auto_str_func(sym, field.typ, field_styp_fn_name,
			field.name, sym_has_str_method, str_method_expects_ptr)
		if field.typ in ast.cptr_types {
			func = '(voidptr) it.$field.name'
			caller_should_free = false
		} else if ftyp_noshared.is_ptr() {
			// reference types can be "nil"
			funcprefix += 'isnil(it.${c_name(field.name)})'
			funcprefix += ' ? _SLIT("nil") : '
			// struct, floats and ints have a special case through the _str function
			if sym.kind !in [.struct_, .alias] && !field.typ.is_int_valptr()
				&& !field.typ.is_float_valptr() {
				funcprefix += '*'
			}
		}
		// handle circular ref type of struct to the struct itself
		if styp == field_styp {
			fn_body.write_string('${funcprefix}_SLIT("<circular>")')
		} else {
			// manage C charptr
			if field.typ in ast.charptr_types {
				fn_body.write_string('tos2((byteptr)$func)')
			} else {
				if field.typ.is_ptr() && sym.kind == .struct_ {
					funcprefix += '(indent_count > 25)? _SLIT("<probably circular>") : '
				}
				// eprintln('>>> caller_should_free: ${caller_should_free:6s} | funcprefix: $funcprefix | func: $func')
				if caller_should_free {
					tmpvar := g.new_tmp_var()
					fn_body_surrounder.add('\tstring $tmpvar = $funcprefix$func;', '\tstring_free(&$tmpvar);')
					fn_body.write_string(tmpvar)
				} else {
					fn_body.write_string(funcprefix)
					fn_body.write_string(func)
				}
			}
		}

		fn_body.writeln('}}, {_SLIT("$quote_str"), 0, {.d_c=0}},')
	}
	fn_body.writeln('\t\t{_SLIT("\\n"), $c.si_s_code, {.d_s=indents}}, {_SLIT("}"), 0, {.d_c=0}},')
	fn_body.writeln('\t}));')
}

fn struct_auto_str_func(sym &ast.TypeSymbol, _field_type ast.Type, fn_name string, field_name string, has_custom_str bool, expects_ptr bool) (string, bool) {
	$if trace_autostr ? {
		eprintln('> struct_auto_str_func: $sym.name | field_type.debug() | $fn_name | $field_name | $has_custom_str | $expects_ptr')
	}
	field_type := if _field_type.has_flag(.shared_f) { _field_type.deref() } else { _field_type }
	sufix := if field_type.has_flag(.shared_f) { '->val' } else { '' }
	deref, _ := deref_kind(expects_ptr, field_type.is_ptr(), field_type)
	if sym.kind == .enum_ {
		return '${fn_name}(${deref}it.${c_name(field_name)})', true
	} else if should_use_indent_func(sym.kind) {
		obj := '${deref}it.${c_name(field_name)}$sufix'
		if has_custom_str {
			return '${fn_name}($obj)', true
		}
		return 'indent_${fn_name}($obj, indent_count + 1)', true
	} else if sym.kind in [.array, .array_fixed, .map, .sum_type] {
		obj := '${deref}it.${c_name(field_name)}$sufix'
		if has_custom_str {
			return '${fn_name}($obj)', true
		}
		return 'indent_${fn_name}($obj, indent_count + 1)', true
	} else if sym.kind == .function {
		return '${fn_name}()', true
	} else if sym.kind == .chan {
		return '${fn_name}(${deref}it.${c_name(field_name)}$sufix)', true
	} else {
		mut method_str := 'it.${c_name(field_name)}'
		if sym.kind == .bool {
			return '$method_str ? _SLIT("true") : _SLIT("false")', false
		} else if (field_type.is_int_valptr() || field_type.is_float_valptr())
			&& field_type.is_ptr() && !expects_ptr {
			// ptr int can be "nil", so this needs to be casted to a string
			if sym.kind == .f32 {
				return 'str_intp(1, _MOV((StrIntpData[]){
					{_SLIT0, $si_g32_code, {.d_f32 = *$method_str }}
				}))', true
			} else if sym.kind == .f64 {
				return 'str_intp(1, _MOV((StrIntpData[]){
					{_SLIT0, $si_g64_code, {.d_f64 = *$method_str }}
				}))', true
			} else if sym.kind == .u64 {
				fmt_type := StrIntpType.si_u64
				return 'str_intp(1, _MOV((StrIntpData[]){{_SLIT0, ${u32(fmt_type) | 0xfe00}, {.d_u64 = *$method_str }}}))', true
			}
			fmt_type := StrIntpType.si_i32
			return 'str_intp(1, _MOV((StrIntpData[]){{_SLIT0, ${u32(fmt_type) | 0xfe00}, {.d_i32 = *$method_str }}}))', true
		}
		return method_str, false
	}
}

fn data_str(x StrIntpType) string {
	return match x {
		.si_no_str { 'no_str' }
		.si_c { 'd_c' }
		.si_u8 { 'd_u8' }
		.si_i8 { 'd_i8' }
		.si_u16 { 'd_u16' }
		.si_i16 { 'd_i16' }
		.si_u32 { 'd_u32' }
		.si_i32 { 'd_i32' }
		.si_u64 { 'd_u64' }
		.si_i64 { 'd_i64' }
		.si_f32 { 'd_f32' }
		.si_f64 { 'd_f64' }
		.si_g32 { 'd_f32' } // g32 format use f32 data
		.si_g64 { 'd_f64' } // g64 format use f64 data
		.si_e32 { 'd_f32' } // e32 format use f32 data
		.si_e64 { 'd_f64' } // e64 format use f64 data
		.si_s { 'd_s' }
		.si_p { 'd_p' }
		.si_vp { 'd_vp' }
	}
}

fn should_use_indent_func(kind ast.Kind) bool {
	return kind in [.struct_, .alias, .array, .array_fixed, .map, .sum_type, .interface_]
}
