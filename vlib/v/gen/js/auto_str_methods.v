// Copyright (c) 2019-2022 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license that can be found in the LICENSE file.
module js

import v.ast
import v.util
import strings

struct StrType {
	styp string
mut:
	typ ast.Type
}

fn (mut g JsGen) get_str_fn(typ ast.Type) string {
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
	if mut sym.info is ast.Alias {
		if sym.info.is_import {
			sym = g.table.sym(sym.info.parent_type)
			str_fn_name = styp_to_str_fn_name(sym.name)
		}
	}
	g.str_types << StrType{
		typ: unwrapped
		styp: styp
	}
	return str_fn_name
}

fn (mut g JsGen) final_gen_str(typ StrType) {
	if typ in g.generated_str_fns {
		return
	}
	g.generated_str_fns << typ
	sym := g.table.sym(typ.typ)
	if sym.has_method('str') && !typ.typ.has_flag(.optional) {
		return
	}
	styp := typ.styp
	if styp == 'any' {
		return
	}
	str_fn_name := styp_to_str_fn_name(styp)
	if typ.typ.has_flag(.optional) {
		g.gen_str_for_option(typ.typ, styp, str_fn_name)
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
			verror("could not generate string method $str_fn_name for type '$styp'")
		}
	}
}

pub enum StrIntpType {
	si_no_str = 0 // no parameter to print only fix string
	si_c
	si_u8
	si_i8
	si_u16
	si_i16
	si_u32
	si_i32
	si_u64
	si_i64
	si_e32
	si_e64
	si_f32
	si_f64
	si_g32
	si_g64
	si_s
	si_p
	si_vp
}

pub fn type_to_str(x StrIntpType) string {
	match x {
		.si_no_str { return 'no_str' }
		.si_c { return 'c' }
		.si_u8 { return 'u8' }
		.si_i8 { return 'i8' }
		.si_u16 { return 'u16' }
		.si_i16 { return 'i16' }
		.si_u32 { return 'u32' }
		.si_i32 { return 'i32' }
		.si_u64 { return 'u64' }
		.si_i64 { return 'i64' }
		.si_f32 { return 'f32' }
		.si_f64 { return 'f64' }
		.si_g32 { return 'f32' } // g32 format use f32 data
		.si_g64 { return 'f64' } // g64 format use f64 data
		.si_e32 { return 'f32' } // e32 format use f32 data
		.si_e64 { return 'f64' } // e64 format use f64 data
		.si_s { return 's' }
		.si_p { return 'p' }
		.si_vp { return 'vp' }
	}
}

pub fn data_str(x StrIntpType) string {
	match x {
		.si_no_str { return 'no_str' }
		.si_c { return 'd_c' }
		.si_u8 { return 'd_u8' }
		.si_i8 { return 'd_i8' }
		.si_u16 { return 'd_u16' }
		.si_i16 { return 'd_i16' }
		.si_u32 { return 'd_u32' }
		.si_i32 { return 'd_i32' }
		.si_u64 { return 'd_u64' }
		.si_i64 { return 'd_i64' }
		.si_f32 { return 'd_f32' }
		.si_f64 { return 'd_f64' }
		.si_g32 { return 'd_f32' } // g32 format use f32 data
		.si_g64 { return 'd_f64' } // g64 format use f64 data
		.si_e32 { return 'd_f32' } // e32 format use f32 data
		.si_e64 { return 'd_f64' } // e64 format use f64 data
		.si_s { return 'd_s' }
		.si_p { return 'd_p' }
		.si_vp { return 'd_vp' }
	}
}

const (
	// BUG: this const is not released from the memory! use a const for now
	// si_s_code = "0x" + int(StrIntpType.si_s).hex() // code for a simple string
	si_s_code = '0xfe10'
)

fn should_use_indent_func(kind ast.Kind) bool {
	return kind in [.struct_, .alias, .array, .array_fixed, .map, .sum_type, .interface_]
}

fn (mut g JsGen) gen_str_default(sym ast.TypeSymbol, styp string, str_fn_name string) {
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
		panic("could not generate string method for type '$styp'")
	}

	g.definitions.writeln('function ${str_fn_name}(it) {')
	if convertor == 'bool' {
		g.definitions.writeln('\tlet tmp1 = string__plus(new string("${styp}("), it.valueOf() ? new string("true") : new string("false"));')
	} else {
		g.definitions.writeln('\tlet tmp1 = string__plus(new string("${styp}("), new string(${typename_}_str(($convertor)it).str));')
	}
	g.definitions.writeln('\tstring tmp2 = string__plus(tmp1, new string(")"));')
	g.definitions.writeln('\treturn tmp2;')
	g.definitions.writeln('}')
}

fn (mut g JsGen) gen_str_for_option(typ ast.Type, styp string, str_fn_name string) {
	parent_type := typ.clear_flag(.optional)
	sym := g.table.sym(parent_type)
	sym_has_str_method, _, _ := sym.str_method_info()
	parent_str_fn_name := g.get_str_fn(parent_type)

	g.definitions.writeln('function ${str_fn_name}(it) { return indent_${str_fn_name}(it, 0); }')
	g.definitions.writeln('function indent_${str_fn_name}(it, indent_count) {')
	g.definitions.writeln('\tlet res;')
	g.definitions.writeln('\tif (it.state.val == 0) {')
	if sym.kind == .string {
		tmp_res := '${parent_str_fn_name}(it.data)'
		g.definitions.writeln('\t\tres = ${str_intp_sq(tmp_res)};')
	} else if should_use_indent_func(sym.kind) && !sym_has_str_method {
		g.definitions.writeln('\t\tres = indent_${parent_str_fn_name}(it.data, indent_count);')
	} else {
		g.definitions.writeln('\t\tres = ${parent_str_fn_name}(it.data);')
	}
	g.definitions.writeln('\t} else {')

	tmp_str := str_intp_sub('error: %%', 'IError_str(it.err)')
	g.definitions.writeln('\t\tres = $tmp_str;')
	g.definitions.writeln('\t}')

	g.definitions.writeln('\treturn ${str_intp_sub('Option(%%)', 'res')};')
	g.definitions.writeln('}')
}

fn (mut g JsGen) gen_str_for_alias(info ast.Alias, styp string, str_fn_name string) {
	parent_str_fn_name := g.get_str_fn(info.parent_type)
	g.definitions.writeln('function ${str_fn_name}(it) { return indent_${str_fn_name}(it, 0); }')

	g.definitions.writeln('function indent_${str_fn_name}(it, indent_count) {')
	g.definitions.writeln('\tlet indents = string_repeat(new string("    "), indent_count);')
	g.definitions.writeln('\tlet tmp_ds = ${parent_str_fn_name}(it);')
	g.definitions.writeln('\tlet res = new string("TODO");')
	g.definitions.writeln('\treturn res;')
	g.definitions.writeln('}')
}

fn (mut g JsGen) gen_str_for_multi_return(info ast.MultiReturn, styp string, str_fn_name string) {
	mut fn_builder := strings.new_builder(512)
	fn_builder.writeln('function ${str_fn_name}(a) {')
	fn_builder.writeln('\tlet sb = strings__new_builder($info.types.len * 10);')
	fn_builder.writeln('\tstrings__Builder_write_string(sb, new string("("));')
	for i, typ in info.types {
		sym := g.table.sym(typ)
		is_arg_ptr := typ.is_ptr()
		sym_has_str_method, str_method_expects_ptr, _ := sym.str_method_info()
		arg_str_fn_name := g.get_str_fn(typ)

		if should_use_indent_func(sym.kind) && !sym_has_str_method {
			fn_builder.writeln('\tstrings__Builder_write_string(sb, ${arg_str_fn_name}(a[$i]));')
		} else if sym.kind in [.f32, .f64] {
			if sym.kind == .f32 {
				tmp_val := str_intp_g32('a[$i]')
				fn_builder.writeln('\tstrings__Builder_write_string(sb, $tmp_val);')
			} else {
				tmp_val := str_intp_g64('a[$i]')
				fn_builder.writeln('\tstrings__Builder_write_string(sb, $tmp_val);')
			}
		} else if sym.kind == .string {
			tmp_str := str_intp_sq('a[$i]')
			fn_builder.writeln('\tstrings__Builder_write_string(sb, $tmp_str);')
		} else if sym.kind == .function {
			fn_builder.writeln('\tstrings__Builder_write_string(sb, ${arg_str_fn_name}());')
		} else {
			deref, deref_label := deref_kind(str_method_expects_ptr, is_arg_ptr, typ)
			fn_builder.writeln('\t\tstrings__Builder_write_string(sb, new string("$deref_label"));')
			fn_builder.writeln('\tstrings__Builder_write_string(sb, ${arg_str_fn_name}( a[$i] $deref ));')
		}
		if i != info.types.len - 1 {
			fn_builder.writeln('\tstrings__Builder_write_string(sb, new string(", "));')
		}
	}
	fn_builder.writeln('\tstrings__Builder_write_string(sb, new string(")"));')
	fn_builder.writeln('\tlet res = strings__Builder_str(sb);')
	fn_builder.writeln('\treturn res;')
	fn_builder.writeln('}')
	g.definitions.writeln(fn_builder.str())
}

fn (mut g JsGen) gen_str_for_enum(info ast.Enum, styp string, str_fn_name string) {
	s := util.no_dots(styp)

	g.definitions.writeln('function ${str_fn_name}(it) { /* gen_str_for_enum */')
	// Enums tagged with `[flag]` are special in that they can be a combination of enum values
	if info.is_flag {
		clean_name := util.strip_main_name(styp.replace('__', '.'))
		g.definitions.writeln('\tlet ret = new string("$clean_name{");')
		g.definitions.writeln('\tlet first = 1;')
		for i, val in info.vals {
			g.definitions.writeln('\tif (it & (1 << $i)) {if (!first) {ret = string__plus(ret, new string(" | "));} ret = string__plus(ret, new string(".$val")); first = 0;}')
		}
		g.definitions.writeln('\tret = string__plus(ret, new string("}"));')
		g.definitions.writeln('\treturn ret;')
	} else {
		g.definitions.writeln('\tswitch(it) {')
		// Only use the first multi value on the lookup
		mut seen := []string{len: info.vals.len}
		for val in info.vals {
			if info.is_multi_allowed && val in seen {
				continue
			} else if info.is_multi_allowed {
				seen << val
			}
			g.definitions.writeln('\t\tcase ${s}.$val: return new string("$val");')
		}
		g.definitions.writeln('\t\tdefault: return new string("unknown enum value");')
		g.definitions.writeln('\t}')
	}
	g.definitions.writeln('}')
}

fn (mut g JsGen) gen_str_for_interface(info ast.Interface, styp string, str_fn_name string) {
	// _str() functions should have a single argument, the indenting ones take 2:

	g.definitions.writeln('function ${str_fn_name}(x) { return indent_${str_fn_name}(x, 0); }')

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
	fn_builder.writeln('function indent_${str_fn_name}(x,indent_count) { /* gen_str_for_interface */')
	for typ in info.types {
		subtype := g.table.sym(typ)
		mut func_name := g.get_str_fn(typ)
		sym_has_str_method, str_method_expects_ptr, _ := subtype.str_method_info()
		if should_use_indent_func(subtype.kind) && !sym_has_str_method {
			func_name = 'indent_$func_name'
		}
		deref := if sym_has_str_method && str_method_expects_ptr { ' ' } else { '.valueOf()' }
		// str_intp

		if typ == ast.string_type {
			fn_builder.write_string('\tif (x.val instanceof string)')
			fn_builder.write_string(' return "new string(${clean_interface_v_type_name}(" + x.val.str + ")");')
		} else {
			mut val := '${func_name}(x $deref'
			if should_use_indent_func(subtype.kind) && !sym_has_str_method {
				val += ', indent_count'
			}
			val += ')'

			fn_builder.write_string('\tif (x.val instanceof $subtype.cname)')
			fn_builder.write_string(' return new string("${clean_interface_v_type_name}(" + ${val}.str + ")");\n')
		}
	}
	fn_builder.writeln('\treturn new string("unknown interface value");')
	fn_builder.writeln('}')
	g.definitions.writeln(fn_builder.str())
}

fn (mut g JsGen) gen_str_for_union_sum_type(info ast.SumType, styp string, str_fn_name string) {
	g.definitions.writeln('function ${str_fn_name}(x) { return indent_${str_fn_name}(x, 0); }')
	mut fn_builder := strings.new_builder(512)
	fn_builder.writeln('function indent_${str_fn_name}(x, indent_count) {')
	for typ in info.variants {
		typ_str := g.typ(typ)
		mut func_name := g.get_str_fn(typ)
		sym := g.table.sym(typ)
		sym_has_str_method, str_method_expects_ptr, _ := sym.str_method_info()
		deref := if sym_has_str_method && str_method_expects_ptr {
			' '
		} else {
			if typ.is_ptr() { '.valueOf()' } else { ' ' }
		}
		if should_use_indent_func(sym.kind) && !sym_has_str_method {
			func_name = 'indent_$func_name'
		}
		fn_builder.writeln('if (x instanceof $typ_str) { return ${func_name}(x$deref); }')
	}
	fn_builder.writeln('builtin__panic(new string("unknown sum type value"));\n}')
	g.definitions.writeln(fn_builder.str())
}

fn (mut g JsGen) fn_decl_str(info ast.FnType) string {
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

fn (mut g JsGen) gen_str_for_fn_type(info ast.FnType, styp string, str_fn_name string) {
	g.definitions.writeln('function ${str_fn_name}() { return new string("${g.fn_decl_str(info)}");}')
}

fn (mut g JsGen) gen_str_for_chan(info ast.Chan, styp string, str_fn_name string) {
	elem_type_name := util.strip_main_name(g.table.get_type_name(g.unwrap_generic(info.elem_type)))

	g.definitions.writeln('function ${str_fn_name}(x) { return sync__Channel_auto_str(x, new string("$elem_type_name")); }')
}

fn (mut g JsGen) gen_str_for_thread(info ast.Thread, styp string, str_fn_name string) {
	ret_type_name := util.strip_main_name(g.table.get_type_name(info.return_type))

	g.definitions.writeln('function ${str_fn_name}(_) { return new string("thread($ret_type_name)");}')
}

[inline]
fn styp_to_str_fn_name(styp string) string {
	return styp.replace_each(['*', '', '.', '__', ' ', '__']) + '_str'
}

fn deref_kind(str_method_expects_ptr bool, is_elem_ptr bool, typ ast.Type) (string, string) {
	if str_method_expects_ptr != is_elem_ptr {
		if is_elem_ptr {
			return '.val'.repeat(typ.nr_muls()), 'new \$ref('.repeat(typ.nr_muls())
		} else {
			return 'new \$ref', ''
		}
	}
	return '', ''
}

fn (mut g JsGen) gen_str_for_array(info ast.Array, styp string, str_fn_name string) {
	mut typ := info.elem_type
	mut sym := g.table.sym(info.elem_type)
	if mut sym.info is ast.Alias {
		typ = sym.info.parent_type
		sym = g.table.sym(typ)
	}
	is_elem_ptr := typ.is_ptr()
	sym_has_str_method, str_method_expects_ptr, _ := sym.str_method_info()
	mut elem_str_fn_name := g.get_str_fn(typ)
	if sym.kind == .byte {
		elem_str_fn_name = elem_str_fn_name + '_escaped'
	}

	g.definitions.writeln('function ${str_fn_name}(a) { return indent_${str_fn_name}(a, 0);}')
	g.definitions.writeln('function indent_${str_fn_name}(a, indent_count) {')
	g.definitions.writeln('\tlet sb = strings__new_builder(a.len * 10);')
	g.definitions.writeln('\tstrings__Builder_write_string(sb, new string("["));')
	g.definitions.writeln('\tfor (let i = 0; i < a.len; ++i) {')
	if sym.kind == .function {
		g.definitions.writeln('\t\tlet it = ${elem_str_fn_name}();')
	} else {
		g.definitions.writeln('\t\tlet it = a.arr.get(new int(i));')

		if should_use_indent_func(sym.kind) && !sym_has_str_method {
			if is_elem_ptr {
				g.definitions.writeln('\t\tlet x = indent_${elem_str_fn_name}(it.val, indent_count);')
			} else {
				g.definitions.writeln('\t\tlet x = indent_${elem_str_fn_name}(it, indent_count);')
			}
		} else if sym.kind in [.f32, .f64] {
			g.definitions.writeln('\t\tlet x = new string( it.val + "");')
		} else if sym.kind == .rune {
			g.definitions.writeln('\t\tlet x = new string("\`" + String.fromCharCode(it.val) + "\`");')
			// Rune are managed at this level as strings
			// g.definitions.writeln('\t\tstring x = str_intp(2, _MOV((StrIntpData[]){{new string("\`"), $c.si_s_code, {.d_s = ${elem_str_fn_name}(it) }}, {new string("\`"), 0, {.d_c = 0 }}}));\n')
		} else if sym.kind == .string {
			g.definitions.writeln('\t\tlet x = new string(it);')
			// g.definitions.writeln('\t\tstring x = str_intp(2, _MOV((StrIntpData[]){{new string("\'"), $c.si_s_code, {.d_s = it }}, {new string("\'"), 0, {.d_c = 0 }}}));\n')
		} else {
			// There is a custom .str() method, so use it.
			// Note: we need to take account of whether the user has defined
			// `fn (x T) str() {` or `fn (x &T) str() {`, and convert accordingly
			deref, deref_label := deref_kind(str_method_expects_ptr, is_elem_ptr, typ)
			g.definitions.writeln('\t\tstrings__Builder_write_string(sb, new string("$deref_label"));')
			g.definitions.writeln('\t\tlet x = ${elem_str_fn_name}( $deref it);')
		}
	}
	g.definitions.writeln('\t\tstrings__Builder_write_string(sb, x);')

	g.definitions.writeln('\t\tif (i < a.len-1) {')
	g.definitions.writeln('\t\t\tstrings__Builder_write_string(sb, new string(", "));')
	g.definitions.writeln('\t\t}')
	g.definitions.writeln('\t}')
	g.definitions.writeln('\tstrings__Builder_write_string(sb, new string("]"));')
	g.definitions.writeln('\tlet res = strings__Builder_str(sb);')
	g.definitions.writeln('\treturn res;')
	g.definitions.writeln('}')
}

fn (mut g JsGen) gen_str_for_array_fixed(info ast.ArrayFixed, styp string, str_fn_name string) {
	mut typ := info.elem_type
	mut sym := g.table.sym(info.elem_type)
	if mut sym.info is ast.Alias {
		typ = sym.info.parent_type
		sym = g.table.sym(typ)
	}
	is_elem_ptr := typ.is_ptr()
	sym_has_str_method, str_method_expects_ptr, _ := sym.str_method_info()
	elem_str_fn_name := g.get_str_fn(typ)

	g.definitions.writeln('function ${str_fn_name}(a) { return indent_${str_fn_name}(a, 0);}')

	g.definitions.writeln('function indent_${str_fn_name}(a, indent_count) {')
	g.definitions.writeln('\tlet sb = strings__new_builder($info.size * 10);')
	g.definitions.writeln('\tstrings__Builder_write_string(sb, new string("["));')
	g.definitions.writeln('\tfor (let i = 0; i < $info.size; ++i) {')
	if sym.kind == .function {
		g.definitions.writeln('\t\tstring x = ${elem_str_fn_name}();')
		g.definitions.writeln('\t\tstrings__Builder_write_string(sb, x);')
	} else {
		deref, deref_label := deref_kind(str_method_expects_ptr, is_elem_ptr, typ)
		if should_use_indent_func(sym.kind) && !sym_has_str_method {
			if is_elem_ptr {
				g.definitions.writeln('\t\tstrings__Builder_write_string(sb, new string("$deref_label"));')
				g.definitions.writeln('\t\tif ( 0 == a.arr.get(new int(i)) ) {')
				g.definitions.writeln('\t\t\tstrings__Builder_write_string(sb, new string("0"));')
				g.definitions.writeln('\t\t}else{')
				g.definitions.writeln('\t\t\tstrings__Builder_write_string(sb, ${elem_str_fn_name}(a.arr.get(new int(i)) $deref) );')
				g.definitions.writeln('\t\t}')
			} else {
				g.definitions.writeln('\t\tstrings__Builder_write_string(sb, ${elem_str_fn_name}(a.arr.get(new int(i))) );')
			}
		} else if sym.kind in [.f32, .f64] {
			g.definitions.writeln('\t\tstrings__Builder_write_string(sb, new string(a.arr.get(new int(i)).val.toString()) );')
		} else if sym.kind == .string {
			g.definitions.writeln('\t\tstrings__Builder_write_string(sb, a.arr.get(new int(i)));')
		} else if sym.kind == .rune {
			g.definitions.writeln('\t\tlet x = new string("\`" + String.fromCharCode(a.arr.get(new int(i)).val) + "\`");')
			g.definitions.writeln('\t\tstrings__Builder_write_string(sb,x);')
		} else {
			g.definitions.writeln('\t\tstrings__Builder_write_string(sb, ${elem_str_fn_name}(a.arr.get(new int(i)) $deref));')
		}
	}
	g.definitions.writeln('\t\tif (i < ${info.size - 1}) {')
	g.definitions.writeln('\t\t\tstrings__Builder_write_string(sb, new string(", "));')
	g.definitions.writeln('\t\t}')
	g.definitions.writeln('\t}')
	g.definitions.writeln('\tstrings__Builder_write_string(sb, new string("]"));')
	g.definitions.writeln('\tlet res = strings__Builder_str(sb);')
	g.definitions.writeln('\treturn res;')
	g.definitions.writeln('}')
}

fn (mut g JsGen) gen_str_for_map(info ast.Map, styp string, str_fn_name string) {
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

	g.definitions.writeln('function ${str_fn_name}(m) { return indent_${str_fn_name}(m, 0);}')

	g.definitions.writeln('function indent_${str_fn_name}(m, indent_count) { /* gen_str_for_map */')
	g.definitions.writeln('\tlet sb = strings__new_builder(m.map.length * 10);')
	g.definitions.writeln('\tstrings__Builder_write_string(sb, new string("{"));')
	g.definitions.writeln('\tlet i = 0;')
	g.definitions.writeln('\tlet keys = Object.keys(m.map);')
	g.definitions.writeln('\tfor (let j = 0; j < keys.length;j++) {')
	g.definitions.writeln('\t\tlet key = keys[j];')
	g.definitions.writeln('\t\tlet value = m.map[key];')
	g.definitions.writeln('\t\tkey = new ${key_styp}(key);')
	if key_sym.kind == .string {
		g.definitions.writeln('\t\tstrings__Builder_write_string(sb, new string("\'" + key.str + "\'"));')
	} else if key_sym.kind == .rune {
		g.definitions.writeln('\t\tlet x = new string("\`" + String.fromCharCode(key.val) + "\`");')
		g.definitions.writeln('\t\tstrings__Builder_write_string(sb,x);')
		// g.definitions.writeln('\t\tstrings__Builder_write_string(sb, $tmp_str);')
	} else {
		g.definitions.writeln('\t\tstrings__Builder_write_string(sb, ${key_str_fn_name}(key));')
	}
	g.definitions.writeln('\t\tstrings__Builder_write_string(sb, new string(": "));')
	if val_sym.kind == .function {
		g.definitions.writeln('\t\tstrings__Builder_write_string(sb, ${elem_str_fn_name}());')
	} else if val_sym.kind == .string {
		// tmp_str := str_intp_sq('*($val_styp*)DenseArray_value(&m.key_values, i)')
		g.definitions.writeln('\t\tstrings__Builder_write_string(sb,new string("\'" + value.str + "\'"));')
	} else if should_use_indent_func(val_sym.kind) && !val_sym.has_method('str') {
		g.definitions.writeln('\t\tstrings__Builder_write_string(sb, indent_${elem_str_fn_name}(value, indent_count));')
	} else if val_sym.kind in [.f32, .f64] {
		g.definitions.writeln('\t\tstrings__Builder_write_string(sb, value.val + "");')
	} else if val_sym.kind == .rune {
		g.definitions.writeln('\t\tlet x = new string("\`" + String.fromCharCode(value.val) + "\`");')
		g.definitions.writeln('\t\tstrings__Builder_write_string(sb,x);')
	} else {
		g.definitions.writeln('\t\tstrings__Builder_write_string(sb, ${elem_str_fn_name}(value));')
	}
	g.definitions.writeln('\t\tif (i != keys.length-1) {')
	g.definitions.writeln('\t\t\tstrings__Builder_write_string(sb, new string(", "));')
	g.definitions.writeln('\t\t}')
	g.definitions.writeln('\t\ti++;')
	g.definitions.writeln('\t}')
	g.definitions.writeln('\tstrings__Builder_write_string(sb, new string("}"));')
	g.definitions.writeln('\tlet res = strings__Builder_str(sb);')
	g.definitions.writeln('\treturn res;')
	g.definitions.writeln('}')
}

fn (g &JsGen) type_to_fmt(typ ast.Type) StrIntpType {
	if typ == ast.byte_type_idx {
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

fn (mut g JsGen) gen_str_for_struct(info ast.Struct, styp string, str_fn_name string) {
	// _str() functions should have a single argument, the indenting ones take 2:

	g.definitions.writeln('function ${str_fn_name}(it) { return indent_${str_fn_name}(it, 0);}')

	mut fn_builder := strings.new_builder(512)
	defer {
		g.definitions.writeln(fn_builder.str())
	}
	fn_builder.writeln('function indent_${str_fn_name}(it, indent_count) {')
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
		fn_builder.writeln('\treturn new string("$clean_struct_v_type_name{}");')
		fn_builder.writeln('}')
		return
	}

	fn_builder.writeln('\tlet res = /*struct name*/new string("$clean_struct_v_type_name{\\n")')

	for i, field in info.fields {
		mut ptr_amp := if field.typ.is_ptr() { '&' } else { '' }
		mut prefix := ''
		// manage prefix and quote symbol for the filed
		/*
		mut quote_str := ''


		if sym.kind == .string {
			quote_str = "'"
		} else if field.typ in ast.charptr_types {
			quote_str = '\\"'
			prefix = 'C'
		}
		quote_str = quote_str
		*/
		sym := g.table.sym(g.unwrap_generic(field.typ))
		// first fields doesn't need \n
		if i == 0 {
			fn_builder.write_string('res.str += "    $field.name: $ptr_amp$prefix" + ')
		} else {
			fn_builder.write_string('res.str += "\\n    $field.name: $ptr_amp$prefix" + ')
		}

		// custom methods management
		has_custom_str := sym.has_method('str')
		mut field_styp := g.typ(field.typ).replace('*', '')
		field_styp_fn_name := if has_custom_str {
			'${field_styp}_str'
		} else {
			g.get_str_fn(field.typ)
		}

		mut func := struct_auto_str_func(mut g, sym, field.typ, field_styp_fn_name, field.name)
		if field.typ in ast.cptr_types {
			func = '(voidptr) it.$field.name'
		} else if field.typ.is_ptr() {
			// reference types can be "nil"
			fn_builder.write_string('isnil(it.${g.js_name(field.name)})')
			fn_builder.write_string(' ? new string("nil") : ')
			// struct, floats and ints have a special case through the _str function
			if sym.kind != .struct_ && !field.typ.is_int_valptr() && !field.typ.is_float_valptr() {
				fn_builder.write_string('*')
			}
		}
		// handle circular ref type of struct to the struct itself
		if styp == field_styp {
			fn_builder.write_string('res.str += new string("<circular>")')
		} else {
			// manage C charptr
			if field.typ in ast.charptr_types {
				fn_builder.write_string('tos2((byteptr)$func)')
			} else {
				if field.typ.is_ptr() && sym.kind == .struct_ {
					fn_builder.write_string('(indent_count > 25) ? new string("<probably circular>") : ')
				}
				fn_builder.write_string(func)
			}
		}

		fn_builder.writeln('')
	}
	fn_builder.writeln('res.str += "\\n}"')
	//	fn_builder.writeln('\t\t{new string("\\n"), $c.si_s_code, {.d_s=indents}}, {new string("}"), 0, {.d_c=0}},')
	fn_builder.writeln('\treturn res;')
	fn_builder.writeln('}')
}

fn struct_auto_str_func(mut g JsGen, sym &ast.TypeSymbol, field_type ast.Type, fn_name string, field_name string) string {
	has_custom_str, expects_ptr, _ := sym.str_method_info()
	if sym.kind == .enum_ {
		return '${fn_name}(it.${g.js_name(field_name)})'
	} else if should_use_indent_func(sym.kind) {
		mut obj := 'it.${g.js_name(field_name)}'
		if field_type.is_ptr() && !expects_ptr {
			obj = '*$obj'
		}
		if has_custom_str {
			return '${fn_name}($obj)'
		}
		return 'indent_${fn_name}($obj, indent_count + 1)'
	} else if sym.kind in [.array, .array_fixed, .map, .sum_type] {
		if has_custom_str {
			return '${fn_name}(it.${g.js_name(field_name)})'
		}
		return 'indent_${fn_name}(it.${g.js_name(field_name)}, indent_count + 1)'
	} else if sym.kind == .function {
		return '${fn_name}()'
	} else {
		if sym.kind == .chan {
			return '${fn_name}(it.${g.js_name(field_name)})'
		}
		mut method_str := 'it.${g.js_name(field_name)}'
		if sym.kind == .bool {
			method_str += ' ? new string("true") : new string("false")'
		} else if (field_type.is_int_valptr() || field_type.is_float_valptr())
			&& field_type.is_ptr() && !expects_ptr {
			// ptr int can be "nil", so this needs to be casted to a string
			if sym.kind == .f32 {
				return 'str_intp(1, _MOV((StrIntpData[]){
					{_SLIT0, $si_g32_code, {.d_f32 = *$method_str }}
				}))'
			} else if sym.kind == .f64 {
				return 'str_intp(1, _MOV((StrIntpData[]){
					{_SLIT0, $si_g64_code, {.d_f64 = *$method_str }}
				}))'
			} else if sym.kind == .u64 {
				fmt_type := StrIntpType.si_u64
				return 'str_intp(1, _MOV((StrIntpData[]){{_SLIT0, ${u32(fmt_type) | 0xfe00}, {.d_u64 = *$method_str }}}))'
			}
			fmt_type := StrIntpType.si_i32
			return 'str_intp(1, _MOV((StrIntpData[]){{_SLIT0, ${u32(fmt_type) | 0xfe00}, {.d_i32 = *$method_str }}}))'
		}
		return method_str
	}
}
