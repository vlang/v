module js

import v.ast
import strings
import v.util

[inline]
fn styp_to_copy_fn_name(styp string) string {
	return styp.replace_each(['*', '', '.', '__', ' ', '__']) + '_\$copy'
}

fn (mut g JsGen) get_copy_fn(typ ast.Type) string {
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
	mut copy_fn_name := styp_to_copy_fn_name(styp)
	if mut sym.info is ast.Alias {
		if sym.info.is_import {
			sym = g.table.sym(sym.info.parent_type)
			copy_fn_name = styp_to_copy_fn_name(sym.name)
		}
	}
	g.copy_types << StrType{
		typ: unwrapped
		styp: styp
	}
	return copy_fn_name
}

fn (mut g JsGen) gen_copy_for_option(typ ast.Type, styp string, copy_fn_name string) {
	g.definitions.writeln('function ${copy_fn_name}(it) { return it; }')
}

fn (mut g JsGen) gen_copy_for_alias(info ast.Alias, styp string, copy_fn_name string) {
	parent_copy_fn_name := g.get_str_fn(info.parent_type)

	g.definitions.writeln('function ${copy_fn_name}(it) {')
	g.definitions.writeln('\tlet res = ${parent_copy_fn_name}(it);')
	g.definitions.writeln('\treturn res;')
	g.definitions.writeln('}')
}

fn (mut g JsGen) gen_copy_for_multi_return(info ast.MultiReturn, styp string, copy_fn_name string) {
	mut fn_builder := strings.new_builder(512)
	fn_builder.writeln('function ${copy_fn_name}(a) {')
	fn_builder.writeln('\tlet arr = []')
	for i, typ in info.types {
		sym := g.table.sym(typ)
		arg_copy_fn_name := g.get_copy_fn(typ)

		if sym.kind in [.f32, .f64] {
			if sym.kind == .f32 {
				fn_builder.writeln('\tarr.push(new f32(a[${i}].val));')
			} else {
				fn_builder.writeln('\tarr.push(new f64(a[${i}].val));')
			}
		} else if sym.kind == .string {
			fn_builder.writeln('\tarr.push(new string(a[${i}].str +""));')
		} else if sym.kind == .function {
			fn_builder.writeln('\tarr.push(a[${i}]);')
		} else {
			fn_builder.writeln('\tarr.push(${arg_copy_fn_name}(a[${i}]));')
		}
	}
	fn_builder.writeln('\treturn arr;')
	fn_builder.writeln('}')
	g.definitions.writeln(fn_builder.str())
}

fn (mut g JsGen) gen_copy_for_enum(info ast.Enum, styp string, copy_fn_name string) {
	g.definitions.writeln('function ${copy_fn_name}(it) { return it; }')
}

fn (mut g JsGen) gen_copy_for_union_sum_type(info ast.SumType, styp string, copy_fn_name string) {
	mut fn_builder := strings.new_builder(512)
	fn_builder.writeln('function ${copy_fn_name}(x) {')
	for typ in info.variants {
		typ_str := g.typ(typ)
		mut func_name := g.get_copy_fn(typ)
		fn_builder.writeln('if (x instanceof ${typ_str}) { return ${func_name}(x); }')
	}
	fn_builder.writeln('builtin__panic(new string("unknown sum type value"));\n}')
	g.definitions.writeln(fn_builder.str())
}

fn (mut g JsGen) gen_copy_for_interface(info ast.Interface, styp string, copy_fn_name string) {
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
	fn_builder.writeln('function ${copy_fn_name}(x) { return x; }')
	/*
	for typ in info.types {
		subtype := g.table.sym(typ)
		mut func_name := g.get_copy_fn(typ)
		if typ == ast.string_type {

			fn_builder.write_string('\tif (x instanceof string)')
			fn_builder.write_string(' return new string(x.str + "");')
		} else {
			/*
			mut val := '${func_name}(${deref}($subtype.cname*)x._$subtype.cname'
			if should_use_indent_func(subtype.kind) && !sym_has_str_method {
				val += ', indent_count'
			}
			val += ')'
			val = val
			*/
			res := '"TODO'
			fn_builder.write_string('\tif (x instanceof ${g.typ(typ)})')
			fn_builder.write_string(' return ${func_name}(x);\n')
		}
	}
	fn_builder.writeln('\tbuiltin__panic("unknown interface value");')
	fn_builder.writeln('}')*/
	g.definitions.writeln(fn_builder.str())
}

fn (mut g JsGen) gen_copy_for_fn_type(info ast.FnType, styp string, copy_fn_name string) {
	g.definitions.writeln('function ${copy_fn_name} (x) { return x; }')
}

fn (mut g JsGen) gen_copy_for_array(info ast.Array, styp string, copy_fn_name string) {
	g.definitions.writeln('function ${copy_fn_name} (x) { return x; }')
}

fn (mut g JsGen) gen_copy_for_array_fixed(info ast.ArrayFixed, styp string, copy_fn_name string) {
	g.definitions.writeln('function ${copy_fn_name} (x) { return x; }')
}

fn (mut g JsGen) gen_copy_for_map(info ast.Map, styp string, copy_fn_name string) {
	g.definitions.writeln('function ${copy_fn_name} (x) { return x; }')
}

fn (mut g JsGen) gen_copy_for_struct(info ast.Struct, styp string, copy_fn_name string) {
	mut fn_builder := strings.new_builder(512)
	defer {
		g.definitions.writeln(fn_builder.str())
	}

	fn_builder.writeln('function ${copy_fn_name}(it) { return it }')

	/*
	tmp := g.new_tmp_var()
	fn_builder.writeln('\tlet $tmp = new ${styp}({});')
	for field in info.fields {
		println(field)
		if field.name.len == 0 {

		} else {
			mut shall_copy := true
			for attr in field.attrs {
				if attr.name == 'noinit' {
					shall_copy = false
					break
				}
			}
			if shall_copy {
				func_name := g.get_copy_fn(field.typ)
				fn_builder.writeln('\t${tmp}.$field.name = ${func_name}(it.$field.name);')
			} else {
				fn_builder.writeln('\t${tmp}.$field.name = it.$field.name')
			}
		}
	}
	fn_builder.writeln('\treturn $tmp;\n}')*/
}

fn (mut g JsGen) final_gen_copy(typ StrType) {
	if typ in g.generated_copy_fns {
		return
	}
	g.generated_copy_fns << typ
	sym := g.table.sym(typ.typ)
	if sym.has_method('\$copy') && !typ.typ.has_flag(.optional) {
		return
	}
	styp := typ.styp
	copy_fn_name := styp_to_copy_fn_name(styp)
	if typ.typ.has_flag(.optional) {
		g.gen_copy_for_option(typ.typ, styp, copy_fn_name)
		return
	}
	match styp {
		'byte', 'u8', 'u16', 'u32', 'u64', 'i16', 'int', 'i64', 'isize', 'usize', 'bool',
		'int_literal', 'float_literal', 'f32', 'f64', 'voidptr' {
			g.definitions.writeln('function ${sym.cname}_\$copy(it) { return new ${sym.cname}(it.val); }')
			return
		}
		else {}
	}
	match sym.info {
		ast.Alias {
			g.gen_copy_for_alias(sym.info, styp, copy_fn_name)
		}
		ast.Array {
			g.gen_copy_for_array(sym.info, styp, copy_fn_name)
		}
		ast.ArrayFixed {
			g.gen_copy_for_array_fixed(sym.info, styp, copy_fn_name)
		}
		ast.Enum {
			g.gen_copy_for_enum(sym.info, styp, copy_fn_name)
		}
		ast.FnType {
			g.gen_copy_for_fn_type(sym.info, styp, copy_fn_name)
		}
		ast.Struct {
			g.gen_copy_for_struct(sym.info, styp, copy_fn_name)
		}
		ast.Map {
			g.gen_copy_for_map(sym.info, styp, copy_fn_name)
		}
		ast.MultiReturn {
			g.gen_copy_for_multi_return(sym.info, styp, copy_fn_name)
		}
		ast.SumType {
			g.gen_copy_for_union_sum_type(sym.info, styp, copy_fn_name)
		}
		ast.Interface {
			g.gen_copy_for_interface(sym.info, styp, copy_fn_name)
		}
		else {
			verror("could not generate string method ${copy_fn_name} for type '${styp}'")
		}
	}
}
