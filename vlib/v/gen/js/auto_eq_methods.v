module js

import v.ast
import strings

fn (mut g JsGen) gen_sumtype_equality_fn(left_type ast.Type) string {
	left := g.unwrap(left_type)
	ptr_styp := g.typ(left.typ.set_nr_muls(0))
	if ptr_styp in g.sumtype_fn_definitions {
		return ptr_styp
	}
	g.sumtype_fn_definitions << ptr_styp
	info := left.sym.sumtype_info()
	mut fn_builder := strings.new_builder(512)
	fn_builder.writeln('function ${ptr_styp}_sumtype_eq(a,b) {')
	fn_builder.writeln('\tlet aProto = Object.getPrototypeOf(a);')
	fn_builder.writeln('\tlet bProto = Object.getPrototypeOf(b);')
	fn_builder.writeln('\tif (aProto !== bProto) { return new booL(false); }')
	for typ in info.variants {
		variant := g.unwrap(typ)
		fn_builder.writeln('\tif (aProto == ${g.js_name(variant.sym.name)}) {')
		if variant.sym.kind == .string {
			fn_builder.writeln('\t\treturn new bool(a.str == b.str);')
		} else if variant.sym.kind == .sum_type && !typ.is_ptr() {
			eq_fn := g.gen_sumtype_equality_fn(typ)
			fn_builder.writeln('\t\treturn ${eq_fn}_sumtype_eq(a,b);')
		} else if variant.sym.kind == .function {
			fn_builder.writeln('\t\treturn a == b;')
		}
		// TODO(playX): struct,array,map eq fns
		fn_builder.writeln('\t}')
	}
	fn_builder.writeln('\treturn new bool(false);')
	fn_builder.writeln('}')
	g.definitions.writeln(fn_builder.str())
	return ptr_styp
}

fn (mut g JsGen) gen_struct_equality_fn(left_type ast.Type) string {
	return ''
}

fn (mut g JsGen) gen_alias_equality_fn(left_type ast.Type) string {
	return ''
}

fn (mut g JsGen) gen_array_equality_fn(left_type ast.Type) string {
	return ''
}

fn (mut g JsGen) gen_fixed_array_equality_fn(left_type ast.Type) string {
	return ''
}

fn (mut g JsGen) gen_map_equality_fn(left_type ast.Type) string {
	return ''
}
