// Copyright (c) 2019-2024 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license that can be found in the LICENSE file.
module c

import v.ast
import v.util
import strings

// BUG: this const is not released from the memory! use a const for now
// si_s_code = "0x" + int(StrIntpType.si_s).hex() // code for a simple string
const si_s_code = '0xfe10'

fn (mut g Gen) gen_str_default(sym ast.TypeSymbol, styp string, str_fn_name string) {
	$if trace_autostr ? {
		eprintln('> gen_str_default: ${sym.name} | ${styp} | str_fn_name')
	}
	mut convertor := ''
	mut typename_ := ''
	mut got_int_str := false
	if sym.parent_idx in ast.integer_type_idxs {
		convertor = 'int'
		typename_ = 'int'
		$if new_int ? {
			if str_fn_name == 'i64_str' {
				if got_int_str {
					return
				} else {
					got_int_str = true
				}
			}

			// if sym.parent_idx == ast.int_type_idx {
			// return
			//}
		}
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
		verror('could not generate string method for type `${styp}`')
	}
	g.definitions.writeln('string ${str_fn_name}(${styp} it);')
	g.auto_str_funcs.writeln('string ${str_fn_name}(${styp} it) {')
	if convertor == 'bool' {
		g.auto_str_funcs.writeln('\tstring tmp1 = string__plus(_S("${styp}("), (${convertor})it ? _S("true") : _S("false"));')
	} else {
		g.auto_str_funcs.writeln('\tstring tmp1 = string__plus(_S("${styp}("), tos3(${typename_}_str((${convertor})it).str));')
	}
	g.auto_str_funcs.writeln('\tstring tmp2 = string__plus(tmp1, _S(")"));')
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
		eprintln('> get_str_fn: ${typ.debug()}')
	}
	mut unwrapped := if typ.has_flag(.option) {
		g.unwrap_generic(typ).clear_flag(.variadic)
	} else {
		g.unwrap_generic(typ).set_nr_muls(0).clear_flag(.variadic)
	}
	if g.pref.nofloat {
		if typ == ast.f32_type {
			unwrapped = ast.u32_type
		} else if typ == ast.f64_type {
			unwrapped = ast.u64_type
		}
	}
	styp := g.styp(unwrapped)
	mut sym := g.table.sym(unwrapped)
	mut str_fn_name := styp_to_str_fn_name(styp)
	if mut sym.info is ast.Alias && !sym.has_method('str') {
		if sym.info.is_import {
			sym = g.table.sym(sym.info.parent_type)
			str_fn_name = styp_to_str_fn_name(sym.name)
		}
	}
	if sym.has_method_with_generic_parent('str') {
		match mut sym.info {
			ast.Struct, ast.SumType, ast.Interface {
				str_fn_name = g.generic_fn_name(sym.info.concrete_types, str_fn_name)
			}
			else {}
		}
	}
	if sym.language == .c && !typ.has_flag(.option) && sym.has_method('str') {
		str_fn_name = util.no_dots(g.cc_type(unwrapped, false)) + '_str'
	}
	g.str_types << StrType{
		typ:  unwrapped
		styp: styp
	}
	return str_fn_name
}

fn (mut g Gen) final_gen_str(typ StrType) {
	if typ in g.generated_str_fns {
		return
	}
	$if trace_autostr ? {
		eprintln('> final_gen_str: ${typ}')
	}
	g.generated_str_fns << typ
	sym := g.table.sym(typ.typ)
	if sym.has_method_with_generic_parent('str') && !(typ.typ.has_flag(.option)
		|| typ.typ.has_flag(.result)) {
		return
	}
	styp := typ.styp
	str_fn_name := styp_to_str_fn_name(styp)
	if str_fn_name in g.str_fn_names {
		return
	}
	lock g.str_fn_names {
		g.str_fn_names << str_fn_name
	}
	if typ.typ.has_flag(.option) {
		opt_typ := if typ.typ.has_flag(.option_mut_param_t) { styp.replace('*', '') } else { styp }
		g.gen_str_for_option(typ.typ, opt_typ, str_fn_name)
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
			g.gen_str_for_struct(sym.info, sym.language, styp, g.table.type_to_str(typ.typ),
				str_fn_name)
		}
		ast.Map {
			g.gen_str_for_map(sym.info, styp, str_fn_name)
		}
		ast.MultiReturn {
			g.gen_str_for_multi_return(sym.info, styp, str_fn_name)
		}
		ast.SumType {
			g.gen_str_for_union_sum_type(sym.info, styp, g.table.type_to_str(typ.typ),
				str_fn_name)
		}
		ast.Interface {
			g.gen_str_for_interface(sym.info, styp, g.table.type_to_str(typ.typ), str_fn_name)
		}
		ast.Chan {
			g.gen_str_for_chan(sym.info, styp, str_fn_name)
		}
		ast.Thread {
			g.gen_str_for_thread(sym.info, styp, str_fn_name)
		}
		else {
			if sym.name != 'nil' {
				verror('could not generate string method `${str_fn_name}` for type `${styp}`')
			}
		}
	}
}

fn (mut g Gen) gen_str_for_option(typ ast.Type, styp string, str_fn_name string) {
	$if trace_autostr ? {
		eprintln('> gen_str_for_option: ${typ.debug()} | ${styp} | ${str_fn_name}')
	}
	parent_type := typ.clear_flag(.option)
	sym := g.table.sym(parent_type)
	sym_has_str_method, expects_ptr, _ := sym.str_method_info()
	parent_str_fn_name := g.get_str_fn(parent_type)

	g.definitions.writeln('string ${str_fn_name}(${styp} it);')
	g.auto_str_funcs.writeln('string ${str_fn_name}(${styp} it) { return indent_${str_fn_name}(it, 0); }')
	g.definitions.writeln('string indent_${str_fn_name}(${styp} it, int indent_count);')
	g.auto_str_funcs.writeln('string indent_${str_fn_name}(${styp} it, int indent_count) {')
	g.auto_str_funcs.writeln('\tstring res;')
	g.auto_str_funcs.writeln('\tif (it.state == 0) {')
	deref := if typ.is_ptr() && !typ.has_flag(.option_mut_param_t) {
		dot := if expects_ptr {
			'*'.repeat(typ.nr_muls())
		} else {
			'*'.repeat(typ.nr_muls() + 1)
		}
		'${dot}(${sym.cname}**)&'
	} else if typ.has_flag(.option_mut_param_t) {
		'*(${sym.cname}*)'
	} else if expects_ptr {
		'(${sym.cname}*)'
	} else {
		'*(${sym.cname}*)'
	}
	if sym.kind == .string {
		if typ.nr_muls() > 1 {
			g.auto_str_funcs.writeln('\t\tres = ptr_str(*(${sym.cname}**)&it.data);')
		} else {
			tmp_res := '${parent_str_fn_name}(${deref}it.data)'
			g.auto_str_funcs.writeln('\t\tres = ${str_intp_sq(tmp_res)};')
		}
	} else if should_use_indent_func(sym.kind) && !sym_has_str_method {
		g.auto_str_funcs.writeln('\t\tres = indent_${parent_str_fn_name}(${deref}it.data, indent_count);')
	} else if sym.kind == .function {
		g.auto_str_funcs.writeln('\t\tres = ${parent_str_fn_name}();')
	} else {
		if typ.nr_muls() > 1 {
			g.auto_str_funcs.writeln('\t\tres = ptr_str(*(${sym.cname}**)&it.data);')
		} else {
			g.auto_str_funcs.writeln('\t\tres = ${parent_str_fn_name}(${deref}it.data);')
		}
	}
	g.auto_str_funcs.writeln('\t\treturn ${str_intp_sub('Option(%%)', 'res')};')
	g.auto_str_funcs.writeln('\t}')
	g.auto_str_funcs.writeln('\treturn _S("Option(none)");')
	g.auto_str_funcs.writeln('}')
}

fn (mut g Gen) gen_str_for_result(typ ast.Type, styp string, str_fn_name string) {
	$if trace_autostr ? {
		eprintln('> gen_str_for_result: ${typ.debug()} | ${styp} | ${str_fn_name}')
	}
	parent_type := typ.clear_flag(.result)
	sym := g.table.sym(parent_type)
	sym_has_str_method, _, _ := sym.str_method_info()
	parent_str_fn_name := g.get_str_fn(parent_type)

	g.definitions.writeln('string ${str_fn_name}(${styp} it);')
	g.auto_str_funcs.writeln('string ${str_fn_name}(${styp} it) { return indent_${str_fn_name}(it, 0); }')
	g.definitions.writeln('string indent_${str_fn_name}(${styp} it, int indent_count);')
	g.auto_str_funcs.writeln('string indent_${str_fn_name}(${styp} it, int indent_count) {')
	g.auto_str_funcs.writeln('\tstring res;')
	g.auto_str_funcs.writeln('\tif (!it.is_error) {')
	if sym.kind == .string {
		tmp_res := '${parent_str_fn_name}(*(${sym.cname}*)it.data)'
		g.auto_str_funcs.writeln('\t\tres = ${str_intp_sq(tmp_res)};')
	} else if should_use_indent_func(sym.kind) && !sym_has_str_method {
		g.auto_str_funcs.writeln('\t\tres = indent_${parent_str_fn_name}(*(${sym.cname}*)it.data, indent_count);')
	} else {
		g.auto_str_funcs.writeln('\t\tres = ${parent_str_fn_name}(*(${sym.cname}*)it.data);')
	}
	g.auto_str_funcs.writeln('\t} else {')

	tmp_str := str_intp_sub('error: %%', 'IError_str(it.err)')
	g.auto_str_funcs.writeln('\t\tres = ${tmp_str};')
	g.auto_str_funcs.writeln('\t}')

	g.auto_str_funcs.writeln('\treturn ${str_intp_sub('Result(%%)', 'res')};')
	g.auto_str_funcs.writeln('}')
}

fn (mut g Gen) gen_str_for_alias(info ast.Alias, styp string, str_fn_name string) {
	parent_str_fn_name := g.get_str_fn(info.parent_type)
	parent_sym := g.table.sym(info.parent_type)
	_, str_method_expects_ptr, _ := parent_sym.str_method_info()

	$if trace_autostr ? {
		eprintln('> gen_str_for_alias: ${parent_str_fn_name} | ${styp} | ${str_fn_name}')
	}
	mut clean_type_v_type_name := util.strip_main_name(styp.replace('__', '.'))

	is_c_struct := parent_sym.is_c_struct() && str_method_expects_ptr
	arg_def := if is_c_struct { '${styp}* it' } else { '${styp} it' }

	g.definitions.writeln('${g.static_non_parallel}string ${str_fn_name}(${arg_def});')
	g.auto_str_funcs.writeln('${g.static_non_parallel}string ${str_fn_name}(${arg_def}) { return indent_${str_fn_name}(it, 0); }')
	g.definitions.writeln('${g.static_non_parallel}string indent_${str_fn_name}(${arg_def}, int indent_count);')
	g.auto_str_funcs.writeln('${g.static_non_parallel}string indent_${str_fn_name}(${arg_def}, int indent_count) {')
	old := g.reset_tmp_count()
	defer { g.tmp_count = old }
	g.auto_str_funcs.writeln('\tstring indents = string_repeat(_S("    "), indent_count);')
	if str_method_expects_ptr {
		it_arg := if is_c_struct { 'it' } else { '&it' }
		g.auto_str_funcs.writeln('\tstring tmp_ds = ${parent_str_fn_name}(${it_arg});')
	} else {
		deref, _ := deref_kind(str_method_expects_ptr, info.parent_type.is_ptr(), info.parent_type)
		g.auto_str_funcs.writeln('\tstring tmp_ds = ${parent_str_fn_name}(${deref}it);')
	}
	g.auto_str_funcs.writeln('\tstring res = str_intp(3, _MOV((StrIntpData[]){
		{_SLIT0, ${si_s_code}, {.d_s = indents }},
		{_S("${clean_type_v_type_name}("), ${si_s_code}, {.d_s = tmp_ds }},
		{_S(")"), 0, {.d_c = 0 }}
	}));')
	g.auto_str_funcs.writeln('\tstring_free(&indents);')
	g.auto_str_funcs.writeln('\tstring_free(&tmp_ds);')
	g.auto_str_funcs.writeln('\treturn res;')
	g.auto_str_funcs.writeln('}')
}

fn (mut g Gen) gen_str_for_multi_return(info ast.MultiReturn, styp string, str_fn_name string) {
	$if trace_autostr ? {
		eprintln('> gen_str_for_multi_return: ${info.types} | ${styp} | ${str_fn_name}')
	}
	g.definitions.writeln('${g.static_non_parallel}string ${str_fn_name}(${styp} a);')
	mut fn_builder := strings.new_builder(512)
	fn_builder.writeln('${g.static_non_parallel}string ${str_fn_name}(${styp} a) {')
	fn_builder.writeln('\tstrings__Builder sb = strings__new_builder(2 + ${info.types.len} * 10);')
	fn_builder.writeln('\tstrings__Builder_write_string(&sb, _S("("));')
	for i, typ in info.types {
		sym := g.table.sym(typ)
		is_arg_ptr := typ.is_ptr()
		sym_has_str_method, str_method_expects_ptr, _ := sym.str_method_info()
		arg_str_fn_name := g.get_str_fn(typ)

		if should_use_indent_func(sym.kind) && !sym_has_str_method {
			fn_builder.writeln('\tstrings__Builder_write_string(&sb, ${arg_str_fn_name}(a.arg${i}));')
		} else if sym.kind in [.f32, .f64] {
			if sym.kind == .f32 {
				tmp_val := str_intp_g32('a.arg${i}')
				fn_builder.writeln('\tstrings__Builder_write_string(&sb, ${tmp_val});')
			} else {
				tmp_val := str_intp_g64('a.arg${i}')
				fn_builder.writeln('\tstrings__Builder_write_string(&sb, ${tmp_val});')
			}
		} else if sym.kind == .string {
			tmp_str := str_intp_sq('a.arg${i}')
			fn_builder.writeln('\tstrings__Builder_write_string(&sb, ${tmp_str});')
		} else if sym.kind == .function {
			fn_builder.writeln('\tstrings__Builder_write_string(&sb, ${arg_str_fn_name}());')
		} else {
			deref, deref_label := deref_kind(str_method_expects_ptr, is_arg_ptr, typ)
			fn_builder.writeln('\t\tstrings__Builder_write_string(&sb, _S("${deref_label}"));')
			fn_builder.writeln('\tstrings__Builder_write_string(&sb, ${arg_str_fn_name}( ${deref} a.arg${i}));')
		}
		if i != info.types.len - 1 {
			fn_builder.writeln('\tstrings__Builder_write_string(&sb, _S(", "));')
		}
	}
	fn_builder.writeln('\tstrings__Builder_write_string(&sb, _S(")"));')
	fn_builder.writeln('\tstring res = strings__Builder_str(&sb);')
	fn_builder.writeln('\tstrings__Builder_free(&sb);')
	fn_builder.writeln('\treturn res;')
	fn_builder.writeln('}')
	g.auto_fn_definitions << fn_builder.str()
}

fn (mut g Gen) gen_str_for_enum(info ast.Enum, styp string, str_fn_name string) {
	$if trace_autostr ? {
		eprintln('> gen_str_for_enum: ${info} | ${styp} | ${str_fn_name}')
	}
	s := util.no_dots(styp)
	g.definitions.writeln('${g.static_non_parallel}string ${str_fn_name}(${styp} it);')
	g.auto_str_funcs.writeln('${g.static_non_parallel}string ${str_fn_name}(${styp} it) { /* gen_str_for_enum */')
	// Enums tagged with `@[flag]` are special in that they can be a combination of enum values
	if info.is_flag {
		clean_name := util.strip_main_name(styp.replace('__', '.'))
		g.auto_str_funcs.writeln('\tstring ret = _S("${clean_name}{");')
		g.auto_str_funcs.writeln('\tint first = 1;')
		g.auto_str_funcs.writeln('\tu64 zit = (u64)it;')
		for i, val in info.vals {
			mask := u64(1) << i
			g.auto_str_funcs.writeln('\tif (zit & 0x${mask:016x}U) {if (!first) {ret = string__plus(ret, _S(" | "));} ret = string__plus(ret, _S(".${val}")); first = 0;}')
		}
		g.auto_str_funcs.writeln('\tret = string__plus(ret, _S("}"));')
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
			g.auto_str_funcs.writeln('\t\tcase ${s}__${val}: return _S("${val}");')
		}
		g.auto_str_funcs.writeln('\t\tdefault: return _S("unknown enum value");')
		g.auto_str_funcs.writeln('\t}')
	}
	g.auto_str_funcs.writeln('}')
}

fn (mut g Gen) gen_str_for_interface(info ast.Interface, styp string, typ_str string, str_fn_name string) {
	$if trace_autostr ? {
		eprintln('> gen_str_for_interface: ${info.types} | ${styp} | ${str_fn_name}')
	}
	// _str() functions should have a single argument, the indenting ones take 2:
	g.definitions.writeln('${g.static_non_parallel}string ${str_fn_name}(${styp} x);')
	g.auto_str_funcs.writeln('${g.static_non_parallel}string ${str_fn_name}(${styp} x) { return indent_${str_fn_name}(x, 0); }')
	g.definitions.writeln('${g.static_non_parallel}string indent_${str_fn_name}(${styp} x, int indent_count);')
	mut fn_builder := strings.new_builder(512)
	clean_interface_v_type_name := util.strip_main_name(typ_str)
	fn_builder.writeln('${g.static_non_parallel}string indent_${str_fn_name}(${styp} x, int indent_count) { /* gen_str_for_interface */')
	for typ in info.types {
		sub_sym := g.table.sym(ast.mktyp(typ))
		if g.pref.skip_unused && sub_sym.idx !in g.table.used_features.used_syms {
			continue
		}
		mut func_name := g.get_str_fn(typ)
		sym_has_str_method, str_method_expects_ptr, _ := sub_sym.str_method_info()
		if should_use_indent_func(sub_sym.kind) && !sym_has_str_method {
			func_name = 'indent_${func_name}'
		}

		// str_intp
		deref := if sym_has_str_method && str_method_expects_ptr { ' ' } else { '*' }
		if typ == ast.string_type {
			mut val := '${func_name}(${deref}(${sub_sym.cname}*)x._${sub_sym.cname}'
			if should_use_indent_func(sub_sym.kind) && !sym_has_str_method {
				val += ', indent_count'
			}
			val += ')'
			res := 'str_intp(2, _MOV((StrIntpData[]){
				{_S("${clean_interface_v_type_name}(\'"), ${si_s_code}, {.d_s = ${val}}},
				{_S("\')"), 0, {.d_c = 0 }}
			}))'
			fn_builder.write_string2('\tif (x._typ == _${styp}_${sub_sym.cname}_index)',
				' return ${res};')
		} else {
			if !(sub_sym.kind == .array && g.table.sym(g.table.value_type(typ)).cname == styp) {
				mut val := '${func_name}(${deref}(${sub_sym.cname}*)x._${sub_sym.cname}'
				if should_use_indent_func(sub_sym.kind) && !sym_has_str_method {
					val += ', indent_count'
				}
				val += ')'
				res := 'str_intp(2, _MOV((StrIntpData[]){
					{_S("${clean_interface_v_type_name}("), ${si_s_code}, {.d_s = ${val}}},
					{_S(")"), 0, {.d_c = 0 }}
				}))'
				fn_builder.write_string2('\tif (x._typ == _${styp}_${sub_sym.cname}_index)',
					' return ${res};\n')
			} else {
				fn_builder.write_string2('\tif (x._typ == _${styp}_${sub_sym.cname}_index)',
					' return _S("<circular>");\n')
			}
		}
	}
	fn_builder.writeln('\treturn _S("unknown interface value");')
	fn_builder.writeln('}')
	g.auto_fn_definitions << fn_builder.str()
}

fn (mut g Gen) gen_str_for_union_sum_type(info ast.SumType, styp string, typ_str string, str_fn_name string) {
	$if trace_autostr ? {
		eprintln('> gen_str_for_union_sum_type: ${info.variants} | ${styp} | ${str_fn_name}')
	}
	// _str() functions should have a single argument, the indenting ones take 2:
	g.definitions.writeln('${g.static_non_parallel}string ${str_fn_name}(${styp} x);')
	g.auto_str_funcs.writeln('${g.static_non_parallel}string ${str_fn_name}(${styp} x) { return indent_${str_fn_name}(x, 0); }')
	g.definitions.writeln('${g.static_non_parallel}string indent_${str_fn_name}(${styp} x, int indent_count);')
	mut fn_builder := strings.new_builder(512)
	fn_builder.writeln('${g.static_non_parallel}string indent_${str_fn_name}(${styp} x, int indent_count) {')
	mut clean_sum_type_v_type_name := ''
	if info.is_anon {
		variant_names := info.variants.map(util.strip_main_name(g.table.sym(it).name))
		clean_sum_type_v_type_name = '${variant_names.join('|')}'
	} else {
		clean_sum_type_v_type_name = util.strip_main_name(typ_str)
	}
	fn_builder.writeln('\tswitch(x._typ) {')
	mut idxs := []int{}
	for typ in info.variants {
		if typ in idxs {
			continue
		}
		idxs << typ
		typ_name := g.styp(typ)
		mut func_name := g.get_str_fn(typ)
		sym := g.table.sym(typ)
		is_c_struct := sym.is_c_struct()
		sym_has_str_method, str_method_expects_ptr, _ := sym.str_method_info()
		deref := if is_c_struct || (sym_has_str_method && str_method_expects_ptr) {
			' '
		} else {
			'*'
		}
		if should_use_indent_func(sym.kind) && !sym_has_str_method {
			func_name = 'indent_${func_name}'
		}

		// str_intp
		if typ == ast.string_type {
			mut val := '${func_name}(${deref}(${typ_name}*)x._${sym.cname}'
			if should_use_indent_func(sym.kind) && !sym_has_str_method {
				val += ', indent_count'
			}
			val += ')'
			res := 'str_intp(2, _MOV((StrIntpData[]){
				{_S("${clean_sum_type_v_type_name}(\'"), ${si_s_code}, {.d_s = ${val}}},
				{_S("\')"), 0, {.d_c = 0 }}
			}))'
			fn_builder.write_string('\t\tcase ${int(typ)}: return ${res};\n')
		} else {
			mut val := '${func_name}(${deref}(${typ_name}*)x._${g.get_sumtype_variant_name(typ,
				sym)}'
			if should_use_indent_func(sym.kind) && !sym_has_str_method {
				val += ', indent_count'
			}
			val += ')'
			res := 'str_intp(2, _MOV((StrIntpData[]){
				{_S("${clean_sum_type_v_type_name}("), ${si_s_code}, {.d_s = ${val}}},
				{_S(")"), 0, {.d_c = 0 }}
			}))'
			fn_builder.write_string('\t\tcase ${int(typ)}: return ${res};\n')
		}
	}
	fn_builder.writeln('\t\tdefault: return _S("unknown sum type value");')
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
		if arg.typ.has_flag(.option) {
			fn_str += '?'
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
		if info.func.return_type.has_flag(.option) {
			fn_str += ' ?${x}'
		} else if info.func.return_type.has_flag(.result) {
			fn_str += ' !${x}'
		} else {
			fn_str += ' ${x}'
		}
	}
	return fn_str
}

fn (mut g Gen) gen_str_for_fn_type(info ast.FnType, styp string, str_fn_name string) {
	$if trace_autostr ? {
		eprintln('> gen_str_for_fn_type: ${info.func.name} | ${styp} | ${str_fn_name}')
	}
	g.definitions.writeln('${g.static_non_parallel}string ${str_fn_name}();')
	g.auto_str_funcs.writeln('${g.static_non_parallel}string ${str_fn_name}() { return _S("${g.fn_decl_str(info)}");}')
}

fn (mut g Gen) gen_str_for_chan(info ast.Chan, styp string, str_fn_name string) {
	$if trace_autostr ? {
		eprintln('> gen_str_for_chan: ${info.elem_type.debug()} | ${styp} | ${str_fn_name}')
	}
	elem_type_name := util.strip_main_name(g.table.get_type_name(g.unwrap_generic(info.elem_type)))
	g.definitions.writeln('${g.static_non_parallel}string ${str_fn_name}(${styp} x);')
	g.auto_str_funcs.writeln('${g.static_non_parallel}string ${str_fn_name}(${styp} x) { return sync__Channel_auto_str(x, _S("${elem_type_name}")); }')
}

fn (mut g Gen) gen_str_for_thread(info ast.Thread, styp string, str_fn_name string) {
	$if trace_autostr ? {
		eprintln('> gen_str_for_thread: ${info.return_type.debug()} | ${styp} | ${str_fn_name}')
	}
	ret_type_name := util.strip_main_name(g.table.get_type_name(info.return_type))
	g.definitions.writeln('${g.static_non_parallel}string ${str_fn_name}(${styp} _); // auto}')
	g.auto_str_funcs.writeln('${g.static_non_parallel}string ${str_fn_name}(${styp} _) { return _S("thread(${ret_type_name})");}')
}

@[inline]
fn styp_to_str_fn_name(styp string) string {
	return styp.replace_each(['*', '', '.', '__', ' ', '__']) + '_str'
}

// deref_kind returns deref, deref_label
fn deref_kind(str_method_expects_ptr bool, is_elem_ptr bool, typ ast.Type) (string, string) {
	if typ.has_flag(.option) {
		return '', ''
	}
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
		eprintln('> gen_str_for_array: ${info.elem_type.debug()} | ${styp} | ${str_fn_name}')
	}
	mut typ := info.elem_type
	mut sym := g.table.sym(info.elem_type)
	is_option := typ.has_flag(.option)
	if !is_option && mut sym.info is ast.Alias {
		typ = sym.info.parent_type
		sym = g.table.sym(typ)
	}
	field_styp := g.styp(typ)
	is_elem_ptr := typ.is_ptr()
	sym_has_str_method, str_method_expects_ptr, _ := sym.str_method_info()
	elem_str_fn_name := g.get_str_fn(typ)

	g.definitions.writeln('${g.static_non_parallel}string ${str_fn_name}(${styp} a);')
	g.auto_str_funcs.writeln('${g.static_non_parallel}string ${str_fn_name}(${styp} a) { return indent_${str_fn_name}(a, 0);}')
	g.definitions.writeln('${g.static_non_parallel}string indent_${str_fn_name}(${styp} a, int indent_count);')
	g.auto_str_funcs.writeln('${g.static_non_parallel}string indent_${str_fn_name}(${styp} a, int indent_count) {')
	g.auto_str_funcs.writeln('\tstrings__Builder sb = strings__new_builder(2 + a.len * 10);')
	g.auto_str_funcs.writeln('\tstrings__Builder_write_string(&sb, _S("["));')
	g.auto_str_funcs.writeln('\tfor (int i = 0; i < a.len; ++i) {')
	if sym.kind == .function {
		g.auto_str_funcs.writeln('\t\tstring x = ${elem_str_fn_name}();')
	} else {
		if !typ.has_flag(.option) && sym.kind == .array_fixed {
			g.auto_str_funcs.writeln('\t\t${field_styp} it;')
			g.auto_str_funcs.writeln('\t\tmemcpy(*(${field_styp}*)it, (byte*)array_get(a, i), sizeof(${field_styp}));')
		} else {
			g.auto_str_funcs.writeln('\t\t${field_styp} it = *(${field_styp}*)array_get(a, i);')
		}
		if should_use_indent_func(sym.kind) && !sym_has_str_method {
			if is_elem_ptr {
				deref, deref_label := deref_kind(str_method_expects_ptr, is_elem_ptr,
					typ)
				g.auto_str_funcs.writeln('\t\tstring x = _S("nil");')
				if !typ.has_flag(.option) {
					g.auto_str_funcs.writeln('\t\tif (it != 0) {')
				}
				g.auto_str_funcs.writeln('\t\t\tstrings__Builder_write_string(&sb, _S("${deref_label}"));')
				g.auto_str_funcs.writeln('\t\t\tx = ${elem_str_fn_name}(${deref}it);')
				if !typ.has_flag(.option) {
					g.auto_str_funcs.writeln('\t\t}')
				}
			} else {
				prefix := if !is_option && sym.is_c_struct() && str_method_expects_ptr {
					'&'
				} else {
					''
				}
				g.auto_str_funcs.writeln('\t\tstring x = indent_${elem_str_fn_name}(${prefix}it, indent_count);')
			}
		} else if sym.kind in [.f32, .f64] {
			if sym.kind == .f32 {
				g.auto_str_funcs.writeln('\t\tstring x = ${str_intp_g32('it')};')
			} else {
				g.auto_str_funcs.writeln('\t\tstring x = ${str_intp_g64('it')};')
			}
		} else if sym.kind == .rune {
			// Rune are managed at this level as strings
			g.auto_str_funcs.writeln('\t\tstring x = str_intp(2, _MOV((StrIntpData[]){{_S("\`"), ${si_s_code}, {.d_s = ${elem_str_fn_name}(it) }}, {_S("\`"), 0, {.d_c = 0 }}}));\n')
		} else if sym.kind == .string {
			if typ.has_flag(.option) {
				func := g.get_str_fn(typ)
				g.auto_str_funcs.writeln('\t\tstring x = ${func}(it);\n')
			} else if is_elem_ptr {
				g.auto_str_funcs.writeln('\t\tstring x = str_intp(2, _MOV((StrIntpData[]){{_S("&\'"), ${si_s_code}, {.d_s = *it }}, {_S("\'"), 0, {.d_c = 0 }}}));\n')
			} else {
				g.auto_str_funcs.writeln('\t\tstring x = str_intp(2, _MOV((StrIntpData[]){{_S("\'"), ${si_s_code}, {.d_s = it }}, {_S("\'"), 0, {.d_c = 0 }}}));\n')
			}
		} else {
			// There is a custom .str() method, so use it.
			// Note: we need to take account of whether the user has defined
			// `fn (x T) str() {` or `fn (x &T) str() {`, and convert accordingly
			deref, deref_label := deref_kind(str_method_expects_ptr, is_elem_ptr, typ)
			if is_elem_ptr {
				g.auto_str_funcs.writeln('\t\tstring x = _S("nil");')
				if !typ.has_flag(.option) {
					g.auto_str_funcs.writeln('\t\tif (it != 0) {')
				}
				g.auto_str_funcs.writeln('\t\t\tstrings__Builder_write_string(&sb, _S("${deref_label}"));')
				g.auto_str_funcs.writeln('\t\t\tx = ${elem_str_fn_name}(${deref}it);')
				if !typ.has_flag(.option) {
					g.auto_str_funcs.writeln('\t\t}')
				}
			} else {
				g.auto_str_funcs.writeln('\t\tstrings__Builder_write_string(&sb, _S("${deref_label}"));')
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
	g.auto_str_funcs.writeln('\t\t\tstrings__Builder_write_string(&sb, _S(", "));')
	g.auto_str_funcs.writeln('\t\t}')
	g.auto_str_funcs.writeln('\t}')
	g.auto_str_funcs.writeln('\tstrings__Builder_write_string(&sb, _S("]"));')
	g.auto_str_funcs.writeln('\tstring res = strings__Builder_str(&sb);')
	g.auto_str_funcs.writeln('\tstrings__Builder_free(&sb);')
	g.auto_str_funcs.writeln('\treturn res;')
	g.auto_str_funcs.writeln('}')
}

fn (mut g Gen) gen_str_for_array_fixed(info ast.ArrayFixed, styp string, str_fn_name string) {
	$if trace_autostr ? {
		eprintln('> gen_str_for_array_fixed: ${info.elem_type.debug()} | ${styp} | ${str_fn_name}')
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
	def_arg := if info.is_fn_ret { '${g.styp(typ)} a[${info.size}]' } else { '${styp} a' }

	g.definitions.writeln('${g.static_non_parallel}string ${str_fn_name}(${def_arg});')
	g.auto_str_funcs.writeln('${g.static_non_parallel}string ${str_fn_name}(${def_arg}) { return indent_${str_fn_name}(a, 0);}')
	g.definitions.writeln('${g.static_non_parallel}string indent_${str_fn_name}(${def_arg}, int indent_count);')
	g.auto_str_funcs.writeln('${g.static_non_parallel}string indent_${str_fn_name}(${def_arg}, int indent_count) {')
	g.auto_str_funcs.writeln('\tstrings__Builder sb = strings__new_builder(2 + ${info.size} * 10);')
	g.auto_str_funcs.writeln('\tstrings__Builder_write_string(&sb, _S("["));')
	g.auto_str_funcs.writeln('\tfor (int i = 0; i < ${info.size}; ++i) {')
	if sym.kind == .function {
		g.auto_str_funcs.writeln('\t\tstring x = ${elem_str_fn_name}();')
		g.auto_str_funcs.writeln('\t\tstrings__Builder_write_string(&sb, x);')
	} else {
		deref, deref_label := deref_kind(str_method_expects_ptr, is_elem_ptr, typ)
		if should_use_indent_func(sym.kind) && !sym_has_str_method {
			if is_elem_ptr {
				g.auto_str_funcs.writeln('\t\tstrings__Builder_write_string(&sb, _S("${deref_label}"));')
				g.auto_str_funcs.writeln('\t\tif ( 0 == a[i] ) {')
				g.auto_str_funcs.writeln('\t\t\tstrings__Builder_write_string(&sb, _S("0"));')
				g.auto_str_funcs.writeln('\t\t}else{')
				g.auto_str_funcs.writeln('\t\t\tstrings__Builder_write_string(&sb, ${elem_str_fn_name}(${deref}a[i]));')
				g.auto_str_funcs.writeln('\t\t}')
			} else {
				g.auto_str_funcs.writeln('\t\tstrings__Builder_write_string(&sb, ${elem_str_fn_name}(${deref}a[i]));')
			}
		} else if sym.kind in [.f32, .f64] && !typ.has_flag(.option) {
			if sym.kind == .f32 {
				field_str := str_intp_g32('a[i]')
				g.auto_str_funcs.writeln('\t\tstrings__Builder_write_string(&sb, ${field_str});')
			} else {
				field_str := str_intp_g64('a[i]')
				g.auto_str_funcs.writeln('\t\tstrings__Builder_write_string(&sb, ${field_str});')
			}
		} else if sym.kind == .string && !typ.has_flag(.option) {
			field_str := str_intp_sq('a[i]')
			g.auto_str_funcs.writeln('\t\tstrings__Builder_write_string(&sb, ${field_str});')
		} else if sym.kind == .rune && !typ.has_flag(.option) {
			tmp_str := str_intp_rune('${elem_str_fn_name}(${deref}a[i])')
			g.auto_str_funcs.writeln('\t\tstrings__Builder_write_string(&sb, ${tmp_str});')
		} else {
			g.auto_str_funcs.writeln('\t\tstrings__Builder_write_string(&sb, ${elem_str_fn_name}(${deref}a[i]));')
		}
	}
	g.auto_str_funcs.writeln('\t\tif (i < ${info.size - 1}) {')
	g.auto_str_funcs.writeln('\t\t\tstrings__Builder_write_string(&sb, _S(", "));')
	g.auto_str_funcs.writeln('\t\t}')
	g.auto_str_funcs.writeln('\t}')
	g.auto_str_funcs.writeln('\tstrings__Builder_write_string(&sb, _S("]"));')
	g.auto_str_funcs.writeln('\tstring res = strings__Builder_str(&sb);')
	g.auto_str_funcs.writeln('\tstrings__Builder_free(&sb);')
	g.auto_str_funcs.writeln('\treturn res;')
	g.auto_str_funcs.writeln('}')
}

fn (mut g Gen) gen_str_for_map(info ast.Map, styp string, str_fn_name string) {
	$if trace_autostr ? {
		eprintln('> gen_str_for_map: ${info.key_type.debug()} -> ${info.value_type.debug()} | ${styp} | ${str_fn_name}')
	}
	mut key_typ := info.key_type
	mut key_sym := g.table.sym(key_typ)
	if mut key_sym.info is ast.Alias {
		key_typ = key_sym.info.parent_type
		key_sym = g.table.sym(key_typ)
	}
	key_styp := g.styp(key_typ)
	key_str_fn_name := styp_to_str_fn_name(key_styp)
	if !key_sym.has_method('str') {
		g.get_str_fn(key_typ)
	}

	mut val_typ := info.value_type
	mut val_sym := g.table.sym(val_typ)
	is_option := val_typ.has_flag(.option)
	if !is_option && mut val_sym.info is ast.Alias {
		val_typ = val_sym.info.parent_type
		val_sym = g.table.sym(val_typ)
	}
	val_styp := g.styp(val_typ)
	mut elem_str_fn_name := styp_to_str_fn_name(val_styp)

	mut receiver_is_ptr := false
	fn_str := val_sym.find_method_with_generic_parent('str') or { ast.Fn{} }

	if fn_str.name == 'str' {
		receiver_is_ptr = fn_str.receiver_type.is_ptr()
		match mut val_sym.info {
			ast.Struct, ast.Interface, ast.SumType {
				if val_sym.info.generic_types.len > 0 {
					elem_str_fn_name = g.generic_fn_name(val_sym.info.concrete_types,
						elem_str_fn_name)
				}
			}
			else {}
		}
	} else {
		g.get_str_fn(val_typ)
	}

	g.definitions.writeln('${g.static_non_parallel}string ${str_fn_name}(${styp} m);')
	g.auto_str_funcs.writeln('${g.static_non_parallel}string ${str_fn_name}(${styp} m) { return indent_${str_fn_name}(m, 0);}')
	g.definitions.writeln('${g.static_non_parallel}string indent_${str_fn_name}(${styp} m, int indent_count);')
	g.auto_str_funcs.writeln('${g.static_non_parallel}string indent_${str_fn_name}(${styp} m, int indent_count) { /* gen_str_for_map */')
	g.auto_str_funcs.writeln('\tstrings__Builder sb = strings__new_builder(2 + m.key_values.len * 10);')
	g.auto_str_funcs.writeln('\tstrings__Builder_write_string(&sb, _S("{"));')
	g.auto_str_funcs.writeln('\tbool is_first = true;')
	g.auto_str_funcs.writeln('\tfor (int i = 0; i < m.key_values.len; ++i) {')
	g.auto_str_funcs.writeln('\t\tif (!DenseArray_has_index(&m.key_values, i)) { continue; }')
	g.auto_str_funcs.writeln('\t\telse if (!is_first) { strings__Builder_write_string(&sb, _S(", ")); }')

	if key_sym.kind == .string {
		g.auto_str_funcs.writeln('\t\tstring key = *(string*)DenseArray_key(&m.key_values, i);')
	} else {
		g.auto_str_funcs.writeln('\t\t${key_styp} key = *(${key_styp}*)DenseArray_key(&m.key_values, i);')
	}
	if key_sym.kind == .string {
		g.auto_str_funcs.writeln('\t\tstrings__Builder_write_string(&sb, ${str_intp_sq('key')});')
	} else if key_sym.kind == .rune {
		tmp_str := str_intp_rune('${key_str_fn_name}(key)')
		g.auto_str_funcs.writeln('\t\tstrings__Builder_write_string(&sb, ${tmp_str});')
	} else {
		g.auto_str_funcs.writeln('\t\tstrings__Builder_write_string(&sb, ${key_str_fn_name}(key));')
	}
	g.auto_str_funcs.writeln('\t\tstrings__Builder_write_string(&sb, _S(": "));')
	_, str_method_expects_ptr, _ := val_sym.str_method_info()
	_, deref_label := deref_kind(str_method_expects_ptr, val_typ.is_ptr(), val_typ)
	if deref_label != '' {
		g.auto_str_funcs.writeln('\t\t\tstrings__Builder_write_string(&sb, _S("${deref_label}"));')
	}
	if val_sym.kind == .function {
		g.auto_str_funcs.writeln('\t\tstrings__Builder_write_string(&sb, ${elem_str_fn_name}());')
	} else if val_sym.kind == .string {
		if val_typ.has_flag(.option) {
			func := g.get_str_fn(val_typ)
			g.auto_str_funcs.writeln('\t\tstrings__Builder_write_string(&sb, ${func}(*(${val_styp}*)DenseArray_value(&m.key_values, i)));')
		} else {
			tmp_str := str_intp_sq('*(${val_styp}*)DenseArray_value(&m.key_values, i)')
			g.auto_str_funcs.writeln('\t\tstrings__Builder_write_string(&sb, ${tmp_str});')
		}
	} else if should_use_indent_func(val_sym.kind) && fn_str.name != 'str' {
		ptr_str := if !is_option && val_sym.is_c_struct() && str_method_expects_ptr {
			''
		} else {
			'*'.repeat(val_typ.nr_muls() + 1)
		}
		g.auto_str_funcs.writeln('\t\tstrings__Builder_write_string(&sb, indent_${elem_str_fn_name}(${ptr_str}(${val_styp}*)DenseArray_value(&m.key_values, i), indent_count));')
	} else if val_sym.kind in [.f32, .f64] {
		tmp_val := '*(${val_styp}*)DenseArray_value(&m.key_values, i)'
		if val_typ.has_flag(.option) {
			g.auto_str_funcs.writeln('\t\tstrings__Builder_write_string(&sb, ${g.get_str_fn(val_typ)}(*(${val_styp}*)DenseArray_value(&m.key_values, i)));')
		} else {
			if val_sym.kind == .f32 {
				g.auto_str_funcs.writeln('\t\tstrings__Builder_write_string(&sb, ${str_intp_g32(tmp_val)});')
			} else {
				g.auto_str_funcs.writeln('\t\tstrings__Builder_write_string(&sb, ${str_intp_g64(tmp_val)});')
			}
		}
	} else if val_sym.kind == .rune {
		tmp_str := str_intp_rune('${elem_str_fn_name}(*(${val_styp}*)DenseArray_value(&m.key_values, i))')
		g.auto_str_funcs.writeln('\t\tstrings__Builder_write_string(&sb, ${tmp_str});')
	} else {
		ptr_str := '*'.repeat(val_typ.nr_muls())
		if val_typ.has_flag(.option) {
			g.auto_str_funcs.writeln('\t\tstrings__Builder_write_string(&sb, ${g.get_str_fn(val_typ)}(*${ptr_str}(${val_styp}*)DenseArray_value(&m.key_values, i)));')
		} else if receiver_is_ptr {
			g.auto_str_funcs.writeln('\t\tstrings__Builder_write_string(&sb, ${elem_str_fn_name}(${ptr_str}(${val_styp}*)DenseArray_value(&m.key_values, i)));')
		} else {
			g.auto_str_funcs.writeln('\t\tstrings__Builder_write_string(&sb, ${elem_str_fn_name}(*${ptr_str}(${val_styp}*)DenseArray_value(&m.key_values, i)));')
		}
	}
	g.auto_str_funcs.writeln('\t\tis_first = false;')
	g.auto_str_funcs.writeln('\t}')
	g.auto_str_funcs.writeln('\tstrings__Builder_write_string(&sb, _S("}"));')
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
	if typ in ast.voidptr_types || typ == ast.nil_type || typ in ast.byteptr_types {
		return .si_p
	}
	if typ in ast.charptr_types {
		// return '%C\\000' // a C string
		return .si_s
	}
	typ_nr_muls := typ.nr_muls()
	if typ_nr_muls > 1 {
		return .si_p
	}
	sym := g.table.sym(typ)
	if typ.is_int_valptr() || typ.is_float_valptr() {
		return .si_s
	} else if sym.kind in [.struct, .array, .array_fixed, .map, .bool, .enum, .interface, .sum_type,
		.function, .alias, .chan, .thread] {
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
	} else if sym.kind == .usize {
		return .si_u64
	} else if sym.kind == .isize {
		return .si_i64
	}
	return .si_i32
}

fn (mut g Gen) gen_str_for_struct(info ast.Struct, lang ast.Language, styp string, typ_str string, str_fn_name string) {
	$if trace_autostr ? {
		eprintln('> gen_str_for_struct: ${info.parent_type.debug()} | ${styp} | ${str_fn_name}')
	}
	// _str() functions should have a single argument, the indenting ones take 2:
	is_c_struct := lang == .c
	arg_def := if is_c_struct { '${styp}* it' } else { '${styp} it' }
	g.definitions.writeln('${g.static_non_parallel}string ${str_fn_name}(${arg_def});')
	g.auto_str_funcs.writeln('${g.static_non_parallel}string ${str_fn_name}(${arg_def}) { return indent_${str_fn_name}(it, 0);}')
	g.definitions.writeln('${g.static_non_parallel}string indent_${str_fn_name}(${arg_def}, int indent_count);')
	mut fn_builder := strings.new_builder(512)
	defer {
		g.auto_fn_definitions << fn_builder.str()
	}
	fn_builder.writeln('string indent_${str_fn_name}(${arg_def}, int indent_count) {')
	old := g.reset_tmp_count()
	defer { g.tmp_count = old }
	clean_struct_v_type_name := if info.is_anon { 'struct ' } else { util.strip_main_name(typ_str) }
	// generate ident / indent length = 4 spaces
	if info.fields.len == 0 {
		fn_builder.writeln('\treturn _S("${clean_struct_v_type_name}{}");')
		fn_builder.writeln('}')
		return
	}

	fn_builder.writeln('\tstring indents = string_repeat(_S("    "), indent_count);')

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
	// find `[str: skip]` fields
	mut field_skips := []int{}
	for i, field in info.fields {
		if attr := field.attrs.find_first('str') {
			if attr.arg == 'skip' {
				field_skips << i
			}
		}
	}
	// -hide-auto-str hides potential sensitive struct data from resulting binary files
	if g.pref.hide_auto_str {
		fn_body.writeln('\tstring res = { .str ="str() used with -hide-auto-str", .len=30 }; return res;')
		return
	}
	fn_body.writeln('\tstring res = str_intp( ${(info.fields.len - field_skips.len) * 4 + 3}, _MOV((StrIntpData[]){')
	fn_body.writeln('\t\t{_S("${clean_struct_v_type_name}{\\n"), 0, {.d_c=0}},')

	allow_circular := info.attrs.any(it.name == 'autostr' && it.arg == 'allowrecurse')
	mut is_first := true
	for i, field in info.fields {
		// Skip `str:skip` fields
		if i in field_skips {
			continue
		}
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
		mut base_typ := g.unwrap_generic(field.typ)
		if base_typ.has_flag(.shared_f) {
			base_typ = base_typ.clear_flag(.shared_f).deref()
		}
		base_fmt := g.type_to_fmt(base_typ)
		is_opt_field := field.typ.has_flag(.option)

		// manage prefix and quote symbol for the filed
		mut quote_str := ''
		mut prefix := ''
		sym := g.table.sym(g.unwrap_generic(field.typ))
		if !is_opt_field {
			if sym.kind == .string {
				quote_str = "'"
			} else if field.typ in ast.charptr_types {
				quote_str = '\\"'
				prefix = 'C'
			}
		}

		if is_first {
			// first field doesn't need \n
			fn_body.write_string('\t\t{_SLIT0, ${si_s_code}, {.d_s=indents}}, {_S("    ${field.name}: ${ptr_amp}${prefix}"), 0, {.d_c=0}}, ')
			is_first = false
		} else {
			fn_body.write_string('\t\t{_S("\\n"), ${si_s_code}, {.d_s=indents}}, {_S("    ${field.name}: ${ptr_amp}${prefix}"), 0, {.d_c=0}}, ')
		}

		// custom methods management
		sym_has_str_method, str_method_expects_ptr, _ := sym.str_method_info()
		sftyp := g.styp(ftyp_noshared)
		mut field_styp := sftyp.replace('*', '')
		field_styp_fn_name := if sym_has_str_method {
			mut field_fn_name := if ftyp_noshared.has_flag(.option) {
				g.get_str_fn(ftyp_noshared)
			} else {
				left_cc_type := g.cc_type(ftyp_noshared, false)
				left_fn_name := util.no_dots(left_cc_type)
				'${left_fn_name}_str'
			}
			if sym.info is ast.Struct {
				field_fn_name = g.generic_fn_name(sym.info.concrete_types, field_fn_name)
			}
			field_fn_name
		} else {
			g.get_str_fn(ftyp_noshared)
		}
		// with floats we use always the g representation:
		if is_opt_field {
			fn_body.write_string('{_S("${quote_str}"), ${si_s_code}, {.d_s=')
		} else if sym.kind !in [.f32, .f64] {
			fn_body.write_string('{_S("${quote_str}"), ${int(base_fmt)}, {.${data_str(base_fmt)}=')
		} else {
			g_fmt := '0x' + (u32(base_fmt) | u32(0x7F) << 9).hex()
			fn_body.write_string('{_S("${quote_str}"), ${g_fmt}, {.${data_str(base_fmt)}=')
		}

		mut funcprefix := ''
		mut func, mut caller_should_free := struct_auto_str_func(sym, lang, field.typ,
			field_styp_fn_name, field.name, sym_has_str_method, str_method_expects_ptr)
		ftyp_nr_muls := field.typ.nr_muls()
		field_name := if lang == .c { field.name } else { c_name(field.name) }
		op := if is_c_struct { '->' } else { '.' }
		it_field_name := 'it${op}${field_name}'
		if ftyp_nr_muls > 1 || field.typ in ast.cptr_types {
			if is_opt_field {
			} else {
				func = '(voidptr) ${it_field_name}'
				caller_should_free = false
			}
		} else if ftyp_noshared.is_ptr() {
			// reference types can be "nil"
			if ftyp_noshared.has_flag(.option) {
				funcprefix += 'isnil(&${it_field_name}) || isnil(&${it_field_name}.data)'
			} else {
				funcprefix += 'isnil(${it_field_name})'
			}
			funcprefix += ' ? _S("nil") : '
			// struct, floats and ints have a special case through the _str function
			if !ftyp_noshared.has_flag(.option)
				&& sym.kind !in [.struct, .alias, .enum, .sum_type, .map, .interface]
				&& !field.typ.is_int_valptr() && !field.typ.is_float_valptr() {
				funcprefix += '*'
			}
		}
		mut is_field_array := false
		if sym.info is ast.Array {
			field_styp = g.styp(sym.info.elem_type).trim('*')
			is_field_array = true
		} else if sym.info is ast.ArrayFixed {
			field_styp = g.styp(sym.info.elem_type).trim('*')
			is_field_array = true
		}
		// handle circular ref type of struct to the struct itself
		if styp == field_styp && !allow_circular {
			if is_field_array {
				if is_opt_field {
					arr_styp := g.base_type(field.typ)
					fn_body.write_string('${it_field_name}.state != 2 && (*(${arr_styp}*)${it_field_name}.data).len > 0 ? ${funcprefix}_S("[<circular>]") : ${funcprefix}_S("[]")')
				} else {
					fn_body.write_string('${it_field_name}.len > 0 ? ${funcprefix}_S("[<circular>]") : ${funcprefix}_S("[]")')
				}
			} else {
				fn_body.write_string('${funcprefix}_S("<circular>")')
			}
		} else {
			// manage C charptr
			if field.typ in ast.charptr_types {
				fn_body.write_string('tos4((byteptr)${func})')
			} else {
				if field.typ.is_ptr() && sym.kind in [.struct, .interface] {
					funcprefix += '(indent_count > 25)? _S("<probably circular>") : '
				}
				// eprintln('>>> caller_should_free: ${caller_should_free:6s} | funcprefix: $funcprefix | func: $func')
				if caller_should_free {
					tmpvar := g.new_tmp_var()
					fn_body_surrounder.add('\tstring ${tmpvar} = ${funcprefix}${func};',
						'\tstring_free(&${tmpvar});')
					fn_body.write_string(tmpvar)
				} else {
					fn_body.write_string2(funcprefix, func)
				}
			}
		}

		fn_body.writeln('}}, {_S("${quote_str}"), 0, {.d_c=0}},')
	}
	fn_body.writeln('\t\t{_S("\\n"), ${si_s_code}, {.d_s=indents}}, {_S("}"), 0, {.d_c=0}},')
	fn_body.writeln('\t}));')
}

// c_struct_ptr handles the C struct argument for .str() method
fn c_struct_ptr(sym &ast.TypeSymbol, typ ast.Type, expects_ptr bool) string {
	if sym.is_c_struct() {
		if typ.has_flag(.option) {
			return ''
		}
		if typ.nr_muls() >= 1 {
			if expects_ptr {
				return '*'.repeat(typ.nr_muls() - 1)
			} else {
				return '*'.repeat(typ.nr_muls())
			}
		}
		return if expects_ptr { '&' } else { '' }
	}
	return ''
}

fn struct_auto_str_func(sym &ast.TypeSymbol, lang ast.Language, _field_type ast.Type, fn_name string, field_name string,
	has_custom_str bool, expects_ptr bool) (string, bool) {
	$if trace_autostr ? {
		eprintln('> struct_auto_str_func: ${sym.name} | field_type.debug() | ${fn_name} | ${field_name} | ${has_custom_str} | ${expects_ptr}')
	}
	field_type := if _field_type.has_flag(.shared_f) { _field_type.deref() } else { _field_type }
	sufix := if field_type.has_flag(.shared_f) { '->val' } else { '' }
	deref, _ := deref_kind(expects_ptr, field_type.is_ptr(), field_type)
	final_field_name := if lang == .c { field_name } else { c_name(field_name) }
	op := if lang == .c { '->' } else { '.' }
	prefix := if sym.is_c_struct() { c_struct_ptr(sym, _field_type, expects_ptr) } else { deref }
	if sym.kind == .enum {
		return '${fn_name}(${deref}(it${op}${final_field_name}))', true
	} else if _field_type.has_flag(.option) || should_use_indent_func(sym.kind) {
		obj := '${prefix}it${op}${final_field_name}${sufix}'
		if has_custom_str {
			if sym.kind == .interface && (sym.info as ast.Interface).defines_method('str') {
				iface_obj := '${prefix}it${op}${final_field_name}${sufix}'
				dot := if field_type.is_ptr() { '->' } else { '.' }
				return '${fn_name.trim_string_right('_str')}_name_table[${iface_obj}${dot}_typ]._method_str(${iface_obj}${dot}_object)', true
			}
			return '${fn_name}(${obj})', true
		}
		return 'indent_${fn_name}(${obj}, indent_count + 1)', true
	} else if sym.kind in [.array, .array_fixed, .map, .sum_type] {
		obj := '${prefix}it${op}${final_field_name}${sufix}'
		if has_custom_str {
			return '${fn_name}(${obj})', true
		}
		return 'indent_${fn_name}(${obj}, indent_count + 1)', true
	} else if sym.kind == .function {
		return '${fn_name}()', true
	} else if sym.kind == .chan {
		return '${fn_name}(${deref}it${op}${final_field_name}${sufix})', true
	} else if sym.kind == .thread {
		return '${fn_name}(${deref}it${op}${final_field_name}${sufix})', false
	} else {
		mut method_str := ''
		if !field_type.is_ptr() && field_type.has_option_or_result() {
			method_str = '(*(${sym.name}*)it${op}${final_field_name}.data)'
		} else {
			method_str = 'it${op}${final_field_name}${sufix}'
		}
		if sym.kind == .bool {
			return '${method_str} ? _S("true") : _S("false")', false
		} else if (field_type.is_int_valptr() || field_type.is_float_valptr()) && !expects_ptr {
			// ptr int can be "nil", so this needs to be casted to a string
			if sym.kind == .f32 {
				return 'str_intp(1, _MOV((StrIntpData[]){
					{_SLIT0, ${si_g32_code}, {.d_f32 = *${method_str} }}
				}))', true
			} else if sym.kind == .f64 {
				return 'str_intp(1, _MOV((StrIntpData[]){
					{_SLIT0, ${si_g64_code}, {.d_f64 = *${method_str} }}
				}))', true
			} else if sym.kind in [.u64, .usize] {
				fmt_type := StrIntpType.si_u64
				return 'str_intp(1, _MOV((StrIntpData[]){{_SLIT0, ${u32(fmt_type) | 0xfe00}, {.d_u64 = *${method_str} }}}))', true
			} else if sym.kind in [.i64, .isize] {
				fmt_type := StrIntpType.si_i64
				return 'str_intp(1, _MOV((StrIntpData[]){{_SLIT0, ${u32(fmt_type) | 0xfe00}, {.d_i64 = *${method_str} }}}))', true
			}
			fmt_type := StrIntpType.si_i32
			return 'str_intp(1, _MOV((StrIntpData[]){{_SLIT0, ${u32(fmt_type) | 0xfe00}, {.d_i32 = *${method_str} }}}))', true
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
		.si_r { 'd_r' } // repeat string
		.si_p { 'd_p' }
		.si_vp { 'd_vp' }
	}
}

fn should_use_indent_func(kind ast.Kind) bool {
	return kind in [.struct, .alias, .array, .array_fixed, .map, .sum_type, .interface]
}

fn (mut g Gen) get_enum_type_idx_from_fn_name(fn_name string) (string, int) {
	enum_name := fn_name.all_before('__static__')
	mut mod_enum_name := if !enum_name.contains('.') {
		g.cur_mod.name + '.' + enum_name
	} else {
		enum_name
	}
	mut idx := g.table.type_idxs[mod_enum_name]
	if idx == 0 && (enum_name.contains('.') || enum_name[0].is_capital()) {
		// no cur mod, find from another mods.
		for import_sym in g.file.imports {
			mod_enum_name = '${import_sym.mod}.${enum_name}'
			idx = g.table.type_idxs[mod_enum_name]
			if idx > 0 {
				break
			}
		}
	}
	return mod_enum_name, idx
}

fn (mut g Gen) gen_enum_static_from_string(fn_name string, mod_enum_name string, idx int) {
	enum_typ := ast.idx_to_type(idx)
	enum_styp := g.styp(enum_typ)
	option_enum_typ := enum_typ.set_flag(.option)
	option_enum_styp := g.styp(option_enum_typ)
	enum_field_names := g.table.get_enum_field_names(mod_enum_name)
	enum_field_vals := g.table.get_enum_field_vals(mod_enum_name)

	mut fn_builder := strings.new_builder(512)
	g.definitions.writeln('${g.static_non_parallel}${option_enum_styp} ${fn_name}(string name);')

	fn_builder.writeln('${g.static_non_parallel}${option_enum_styp} ${fn_name}(string name) {')
	fn_builder.writeln('\t${option_enum_styp} t1;')
	fn_builder.writeln('\tbool exists = false;')
	fn_builder.writeln('\tint inx = 0;')
	fn_builder.writeln('\tarray field_names = __new_array_with_default(0, 0, sizeof(string), 0);')
	for field_name in enum_field_names {
		fn_builder.writeln('\tarray_push((array*)&field_names, _MOV((string[]){ _S("${field_name}") }));')
	}
	fn_builder.writeln('\tarray field_vals = __new_array_with_default(0, 0, sizeof(i64), 0);')
	for field_val in enum_field_vals {
		fn_builder.writeln('\tarray_push((array*)&field_vals, _MOV((i64[]){ ${field_val} }));')
	}
	fn_builder.writeln('\tfor (int i = 0; i < ${enum_field_names.len}; ++i) {')
	fn_builder.writeln('\t\tif (fast_string_eq(name, (*(string*)array_get(field_names, i)))) {')
	fn_builder.writeln('\t\t\texists = true;')
	fn_builder.writeln('\t\t\tinx = i;')
	fn_builder.writeln('\t\t\tbreak;')
	fn_builder.writeln('\t\t}')
	fn_builder.writeln('\t}')
	fn_builder.writeln('\tif (exists) {')
	fn_builder.writeln('\t\t_option_ok(&(${enum_styp}[]){ (*(i64*)array_get(field_vals, inx)) }, (_option*)&t1, sizeof(${enum_styp}));')
	fn_builder.writeln('\t\treturn t1;')
	fn_builder.writeln('\t} else {')
	fn_builder.writeln('\t\treturn (${option_enum_styp}){ .state=2, .err=_const_none__, .data={E_STRUCT} };')
	fn_builder.writeln('\t}')
	fn_builder.writeln('}')
	g.auto_fn_definitions << fn_builder.str()
}
