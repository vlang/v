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
	defer {
		g.definitions.writeln(fn_builder.str())
	}
	fn_builder.writeln('function ${ptr_styp}_sumtype_eq(a,b) {')
	fn_builder.writeln('\tlet aProto = Object.getPrototypeOf(a);')
	fn_builder.writeln('\tlet bProto = Object.getPrototypeOf(b);')
	fn_builder.writeln('\tif (aProto !== bProto) { return new bool(false); }')
	for typ in info.variants {
		variant := g.unwrap(typ)
		fn_builder.writeln('\tif (aProto == ${g.js_name(variant.sym.name)}) {')
		if variant.sym.kind == .string {
			fn_builder.writeln('\t\treturn new bool(a.str == b.str);')
		} else if variant.sym.kind == .sum_type && !typ.is_ptr() {
			eq_fn := g.gen_sumtype_equality_fn(typ)
			fn_builder.writeln('\t\treturn ${eq_fn}_sumtype_eq(a,b);')
		} else if variant.sym.kind == .struct_ && !typ.is_ptr() {
			eq_fn := g.gen_struct_equality_fn(typ)
			fn_builder.writeln('\t\treturn ${eq_fn}_struct_eq(a,b);')
		} else if variant.sym.kind == .array && !typ.is_ptr() {
			eq_fn := g.gen_array_equality_fn(typ)
			fn_builder.writeln('\t\treturn ${eq_fn}_arr_eq(a,b);')
		} else if variant.sym.kind == .array_fixed && !typ.is_ptr() {
			eq_fn := g.gen_fixed_array_equality_fn(typ)
			fn_builder.writeln('\t\treturn ${eq_fn}_arr_eq(a,b);')
		} else if variant.sym.kind == .map && !typ.is_ptr() {
			eq_fn := g.gen_map_equality_fn(typ)
			fn_builder.writeln('\t\treturn ${eq_fn}_map_eq(a,b);')
		} else if variant.sym.kind == .alias && !typ.is_ptr() {
			eq_fn := g.gen_alias_equality_fn(typ)
			fn_builder.writeln('\t\treturn ${eq_fn}_alias_eq(a,b);')
		} else if variant.sym.kind == .function {
			fn_builder.writeln('\t\treturn new bool(a == b);')
		} else {
			fn_builder.writeln('\t\treturn new bool(vEq(a,b));')
		}
		fn_builder.writeln('\t}')
	}
	fn_builder.writeln('\treturn new bool(false);')
	fn_builder.writeln('}')

	return ptr_styp
}

fn (mut g JsGen) gen_struct_equality_fn(left_type ast.Type) string {
	left := g.unwrap(left_type)
	ptr_styp := g.typ(left.typ.set_nr_muls(0))
	fn_name := ptr_styp.replace('struct ', '')
	if fn_name in g.struct_fn_definitions {
		return fn_name
	}
	g.struct_fn_definitions << fn_name
	info := left.sym.struct_info()
	mut fn_builder := strings.new_builder(512)
	defer {
		g.definitions.writeln(fn_builder.str())
	}
	fn_builder.writeln('function ${fn_name}_struct_eq(a,b) {')

	// overloaded
	if left.sym.has_method('==') {
		fn_builder.writeln('\treturn ${fn_name}__eq(a, b);')
		fn_builder.writeln('}')
		return fn_name
	}

	fn_builder.write_string('\treturn new bool(')
	if info.fields.len > 0 {
		for i, field in info.fields {
			if i > 0 {
				fn_builder.write_string('\n\t\t&& ')
			}
			field_type := g.unwrap(field.typ)
			field_name := g.js_name(field.name)
			if field_type.sym.kind == .string {
				fn_builder.write_string('a.${field_name}.str == b.${field_name}.str')
			} else if field_type.sym.kind == .sum_type && !field.typ.is_ptr() {
				eq_fn := g.gen_sumtype_equality_fn(field.typ)
				fn_builder.write_string('${eq_fn}_sumtype_eq(a.${field_name}, b.${field_name})')
			} else if field_type.sym.kind == .struct_ && !field.typ.is_ptr() {
				eq_fn := g.gen_struct_equality_fn(field.typ)
				fn_builder.write_string('${eq_fn}_struct_eq(a.${field_name}, b.${field_name})')
			} else if field_type.sym.kind == .array && !field.typ.is_ptr() {
				eq_fn := g.gen_array_equality_fn(field.typ)
				fn_builder.write_string('${eq_fn}_arr_eq(a.${field_name}, b.${field_name})')
			} else if field_type.sym.kind == .array_fixed && !field.typ.is_ptr() {
				eq_fn := g.gen_fixed_array_equality_fn(field.typ)
				fn_builder.write_string('${eq_fn}_arr_eq(a.${field_name}, b.${field_name})')
			} else if field_type.sym.kind == .map && !field.typ.is_ptr() {
				eq_fn := g.gen_map_equality_fn(field.typ)
				fn_builder.write_string('${eq_fn}_map_eq(a.${field_name}, b.${field_name})')
			} else if field_type.sym.kind == .alias && !field.typ.is_ptr() {
				eq_fn := g.gen_alias_equality_fn(field.typ)
				fn_builder.write_string('${eq_fn}_alias_eq(a.${field_name}, b.${field_name})')
			} else if field_type.sym.kind == .function {
				fn_builder.write_string('a.${field_name} == b.${field_name}')
			} else {
				// fallback to vEq for JS types or primitives.
				fn_builder.write_string('vEq(a.${field_name},b.${field_name})')
			}
		}
	} else {
		fn_builder.write_string('true')
	}
	fn_builder.writeln(');')
	fn_builder.writeln('}')
	return fn_name
}

fn (mut g JsGen) gen_alias_equality_fn(left_type ast.Type) string {
	left := g.unwrap(left_type)
	ptr_styp := g.typ(left.typ.set_nr_muls(0))
	if ptr_styp in g.alias_fn_definitions {
		return ptr_styp
	}
	g.alias_fn_definitions << ptr_styp
	info := left.sym.info as ast.Alias

	mut fn_builder := strings.new_builder(512)
	defer {
		g.definitions.writeln(fn_builder.str())
	}
	fn_builder.writeln('function ${ptr_styp}_alias_eq(a,b) {')
	sym := g.table.sym(info.parent_type)
	if sym.kind == .string {
		fn_builder.writeln('\treturn new bool(a.str == b.str);')
	} else if sym.kind == .sum_type && !left.typ.is_ptr() {
		eq_fn := g.gen_sumtype_equality_fn(info.parent_type)
		fn_builder.writeln('\treturn ${eq_fn}_sumtype_eq(a, b);')
	} else if sym.kind == .struct_ && !left.typ.is_ptr() {
		eq_fn := g.gen_struct_equality_fn(info.parent_type)
		fn_builder.writeln('\treturn ${eq_fn}_struct_eq(a, b);')
	} else if sym.kind == .array && !left.typ.is_ptr() {
		eq_fn := g.gen_array_equality_fn(info.parent_type)
		fn_builder.writeln('\treturn ${eq_fn}_arr_eq(a, b);')
	} else if sym.kind == .array_fixed && !left.typ.is_ptr() {
		eq_fn := g.gen_fixed_array_equality_fn(info.parent_type)
		fn_builder.writeln('\treturn ${eq_fn}_arr_eq(a, b);')
	} else if sym.kind == .map && !left.typ.is_ptr() {
		eq_fn := g.gen_map_equality_fn(info.parent_type)
		fn_builder.writeln('\treturn ${eq_fn}_map_eq(a, b);')
	} else if sym.kind == .function {
		fn_builder.writeln('\treturn new bool(a == b);')
	} else {
		fn_builder.writeln('\treturn new bool(vEq(a,b));')
	}
	fn_builder.writeln('}')

	return ptr_styp
}

fn (mut g JsGen) gen_array_equality_fn(left_type ast.Type) string {
	left := g.unwrap(left_type)
	ptr_styp := g.typ(left.typ.set_nr_muls(0))
	if ptr_styp in g.array_fn_definitions {
		return ptr_styp
	}
	g.array_fn_definitions << ptr_styp
	elem := g.unwrap(left.sym.array_info().elem_type)

	mut fn_builder := strings.new_builder(512)
	defer {
		g.definitions.writeln(fn_builder.str())
	}
	fn_builder.writeln('function ${ptr_styp}_arr_eq(a,b) {')
	fn_builder.writeln('\tif (a.arr.length != b.arr.length) {')
	fn_builder.writeln('\t\treturn new bool(false);')
	fn_builder.writeln('\t}')
	fn_builder.writeln('\tfor (let i = 0; i < a.len; ++i) {')
	// compare every pair of elements of the two arrays
	if elem.sym.kind == .string {
		fn_builder.writeln('\t\tif (a.arr.get(new int(i)).str != b.arr.get(new int(i)).str) {')
	} else if elem.sym.kind == .sum_type && !elem.typ.is_ptr() {
		eq_fn := g.gen_sumtype_equality_fn(elem.typ)
		fn_builder.writeln('\t\tif (!${eq_fn}_sumtype_eq(a.arr.get(new int(i)),b.arr.get(new int(i))).val) {')
	} else if elem.sym.kind == .struct_ && !elem.typ.is_ptr() {
		eq_fn := g.gen_struct_equality_fn(elem.typ)
		fn_builder.writeln('\t\tif (!${eq_fn}_struct_eq(a.arr.get(new int(i)),b.arr.get(new int(i))).val) {')
	} else if elem.sym.kind == .array && !elem.typ.is_ptr() {
		eq_fn := g.gen_array_equality_fn(elem.typ)
		fn_builder.writeln('\t\tif (!${eq_fn}_arr_eq(a.arr.get(new int(i)),b.arr.get(new int(i))).val) {')
	} else if elem.sym.kind == .array_fixed && !elem.typ.is_ptr() {
		eq_fn := g.gen_fixed_array_equality_fn(elem.typ)
		fn_builder.writeln('\t\tif (!${eq_fn}_arr_eq(a.arr.get(new int(i)),b.arr.get(new int(i))).val) {')
	} else if elem.sym.kind == .map && !elem.typ.is_ptr() {
		eq_fn := g.gen_map_equality_fn(elem.typ)
		fn_builder.writeln('\t\tif (!${eq_fn}_map_eq(a.arr.get(new int(i)),b.arr.get(new int(i))).val) {')
	} else if elem.sym.kind == .alias && !elem.typ.is_ptr() {
		eq_fn := g.gen_alias_equality_fn(elem.typ)
		fn_builder.writeln('\t\tif (!${eq_fn}_alias_eq(a.arr.get(new int(i)),b.arr.get(new int(i))).val) {')
	} else if elem.sym.kind == .function {
		fn_builder.writeln('\t\tif (a.arr.get(new int(i)) != b.arr.get(new int(i))) {')
	} else {
		fn_builder.writeln('\t\tif (!vEq(a.arr.get(new int(i)),b.arr.get(new int(i)))) {')
	}
	fn_builder.writeln('\t\t\treturn new bool(false);')
	fn_builder.writeln('\t\t}')
	fn_builder.writeln('\t}')
	fn_builder.writeln('\treturn new bool(true);')
	fn_builder.writeln('}')

	return ptr_styp
}

fn (mut g JsGen) gen_fixed_array_equality_fn(left_type ast.Type) string {
	left := g.unwrap(left_type)
	ptr_styp := g.typ(left.typ.set_nr_muls(0))
	if ptr_styp in g.array_fn_definitions {
		return ptr_styp
	}
	g.array_fn_definitions << ptr_styp
	elem_info := left.sym.array_fixed_info()
	elem := g.unwrap(elem_info.elem_type)
	size := elem_info.size

	mut fn_builder := strings.new_builder(512)
	defer {
		g.definitions.writeln(fn_builder.str())
	}
	fn_builder.writeln('function ${ptr_styp}_arr_eq(a,b) {')
	fn_builder.writeln('\tfor (let i = 0; i < ${size}; ++i) {')
	// compare every pair of elements of the two fixed arrays
	if elem.sym.kind == .string {
		fn_builder.writeln('\t\tif (a.arr.get(new int(i)).str != b.arr.get(new int(i)).str) {')
	} else if elem.sym.kind == .sum_type && !elem.typ.is_ptr() {
		eq_fn := g.gen_sumtype_equality_fn(elem.typ)
		fn_builder.writeln('\t\tif (!${eq_fn}_sumtype_eq(a.arr.get(new int(i)), b.arr.get(new int(i))).val) {')
	} else if elem.sym.kind == .struct_ && !elem.typ.is_ptr() {
		eq_fn := g.gen_struct_equality_fn(elem.typ)
		fn_builder.writeln('\t\tif (!${eq_fn}_struct_eq(a.arr.get(new int(i)), b.arr.get(new int(i))).val) {')
	} else if elem.sym.kind == .array && !elem.typ.is_ptr() {
		eq_fn := g.gen_array_equality_fn(elem.typ)
		fn_builder.writeln('\t\tif (!${eq_fn}_arr_eq(a.arr.get(new int(i)), b.arr.get(new int(i))).val) {')
	} else if elem.sym.kind == .array_fixed && !elem.typ.is_ptr() {
		eq_fn := g.gen_fixed_array_equality_fn(elem.typ)
		fn_builder.writeln('\t\tif (!${eq_fn}_arr_eq(a.arr.get(new int(i)), b.arr.get(new int(i))).val) {')
	} else if elem.sym.kind == .map && !elem.typ.is_ptr() {
		eq_fn := g.gen_map_equality_fn(elem.typ)
		fn_builder.writeln('\t\tif (!${eq_fn}_map_eq(a.arr.get(new int(i)), b.arr.get(new int(i))).val) {')
	} else if elem.sym.kind == .alias && !elem.typ.is_ptr() {
		eq_fn := g.gen_alias_equality_fn(elem.typ)
		fn_builder.writeln('\t\tif (!${eq_fn}_alias_eq(a.arr.get(new int(i)), b.arr.get(new int(i))).val) {')
	} else if elem.sym.kind == .function {
		fn_builder.writeln('\t\tif (a.arr.get(new int(i)) != b.arr.get(new int(i))) {')
	} else {
		fn_builder.writeln('\t\tif (!vEq(a.arr.get(new int(i)),b.arr.get(new int(i)))) {')
	}
	fn_builder.writeln('\t\t\treturn new bool(false);')
	fn_builder.writeln('\t\t}')
	fn_builder.writeln('\t}')
	fn_builder.writeln('\treturn new bool(true);')
	fn_builder.writeln('}')

	return ptr_styp
}

fn (mut g JsGen) gen_map_equality_fn(left_type ast.Type) string {
	left := g.unwrap(left_type)
	ptr_styp := g.typ(left.typ.set_nr_muls(0))
	if ptr_styp in g.map_fn_definitions {
		return ptr_styp
	}
	g.map_fn_definitions << ptr_styp
	value := g.unwrap(left.sym.map_info().value_type)

	mut fn_builder := strings.new_builder(512)
	defer {
		g.definitions.writeln(fn_builder.str())
	}
	fn_builder.writeln('function ${ptr_styp}_map_eq(a,b) {')
	fn_builder.writeln('\tif (Object.keys(a.map).length != Object.keys(b.map).length) {')
	fn_builder.writeln('\t\treturn false;')
	fn_builder.writeln('\t}')
	fn_builder.writeln('\tlet keys = Object.keys(a.map);')
	fn_builder.writeln('\tfor (let i = 0;i < keys.length;i++) {')
	fn_builder.writeln('\t\tlet key = keys[i]; let value = a.map[key];')
	fn_builder.writeln('\t\tif (!(key in b.map)) { return new bool(false); }')
	fn_builder.writeln('\t\tlet x = value; let y = b.map[key];')
	kind := g.table.type_kind(value.typ)
	if kind == .string {
		fn_builder.writeln('\t\tif (x.str != y.str) {')
	} else if kind == .sum_type {
		eq_fn := g.gen_sumtype_equality_fn(value.typ)
		fn_builder.writeln('\t\tif (!${eq_fn}_sumtype_eq(x,y).val) {')
	} else if kind == .struct_ {
		eq_fn := g.gen_struct_equality_fn(value.typ)
		fn_builder.writeln('\t\tif (!${eq_fn}_struct_eq(x,y).val) {')
	} else if kind == .array {
		eq_fn := g.gen_array_equality_fn(value.typ)
		fn_builder.writeln('\t\tif (!${eq_fn}_arr_eq(x,y).val) {')
	} else if kind == .array_fixed {
		eq_fn := g.gen_fixed_array_equality_fn(value.typ)
		fn_builder.writeln('\t\tif (!${eq_fn}_arr_eq(x,y).val) {')
	} else if kind == .map {
		eq_fn := g.gen_map_equality_fn(value.typ)
		fn_builder.writeln('\t\tif (!${eq_fn}_map_eq(x,y).val) {')
	} else if kind == .alias {
		eq_fn := g.gen_alias_equality_fn(value.typ)
		fn_builder.writeln('\t\tif (!${eq_fn}_alias_eq(x,y).val) {')
	} else if kind == .function {
		fn_builder.writeln('\t\tif (x !== y) {')
	} else {
		fn_builder.writeln('\t\tif (!vEq(x,y)) {')
	}
	fn_builder.writeln('\t\t\treturn new bool(false);')
	fn_builder.writeln('\t\t}')
	fn_builder.writeln('\t}')
	fn_builder.writeln('\treturn new bool(true);')
	fn_builder.writeln('}')

	return ptr_styp
}
