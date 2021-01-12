// Copyright (c) 2019-2021 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license that can be found in the LICENSE file.
module gen

import strings
import v.table

fn (mut g Gen) gen_struct_equality_fn(left table.Type) string {
	ptr_typ := g.typ(left).trim('*')
	if ptr_typ in g.struct_fn_definitions {
		return ptr_typ
	}
	g.struct_fn_definitions << ptr_typ
	left_sym := g.table.get_type_symbol(left)
	info := left_sym.struct_info()
	g.type_definitions.writeln('static bool ${ptr_typ}_struct_eq($ptr_typ a, $ptr_typ b); // auto')
	mut fn_builder := strings.new_builder(512)
	fn_builder.writeln('static bool ${ptr_typ}_struct_eq($ptr_typ a, $ptr_typ b) {')
	for field in info.fields {
		sym := g.table.get_type_symbol(field.typ)
		if sym.kind == .string {
			fn_builder.writeln('\tif (string_ne(a.$field.name, b.$field.name)) {')
		} else if sym.kind == .struct_ && !field.typ.is_ptr() {
			eq_fn := g.gen_struct_equality_fn(field.typ)
			fn_builder.writeln('\tif (!${eq_fn}_struct_eq(a.$field.name, b.$field.name)) {')
		} else if sym.kind == .array && !field.typ.is_ptr() {
			eq_fn := g.gen_array_equality_fn(field.typ)
			fn_builder.writeln('\tif (!${eq_fn}_arr_eq(a.$field.name, b.$field.name)) {')
		} else if sym.kind == .array_fixed && !field.typ.is_ptr() {
			eq_fn := g.gen_fixed_array_equality_fn(field.typ)
			fn_builder.writeln('\tif (!${eq_fn}_arr_eq(a.$field.name, b.$field.name)) {')
		} else if sym.kind == .map && !field.typ.is_ptr() {
			eq_fn := g.gen_map_equality_fn(field.typ)
			fn_builder.writeln('\tif (!${eq_fn}_map_eq(a.$field.name, b.$field.name)) {')
		} else if sym.kind == .function {
			fn_builder.writeln('\tif (*((voidptr*)(a.$field.name)) != *((voidptr*)(b.$field.name))) {')
		} else {
			fn_builder.writeln('\tif (a.$field.name != b.$field.name) {')
		}
		fn_builder.writeln('\t\treturn false;')
		fn_builder.writeln('\t}')
	}
	fn_builder.writeln('\treturn true;')
	fn_builder.writeln('}')
	g.auto_fn_definitions << fn_builder.str()
	return ptr_typ
}

fn (mut g Gen) gen_array_equality_fn(left table.Type) string {
	ptr_typ := g.typ(left).trim('*')
	if ptr_typ in g.array_fn_definitions {
		return ptr_typ
	}
	g.array_fn_definitions << ptr_typ
	left_sym := g.table.get_type_symbol(left)
	elem_typ := left_sym.array_info().elem_type
	ptr_elem_typ := g.typ(elem_typ)
	elem_sym := g.table.get_type_symbol(elem_typ)
	g.type_definitions.writeln('static bool ${ptr_typ}_arr_eq($ptr_typ a, $ptr_typ b); // auto')
	mut fn_builder := strings.new_builder(512)
	fn_builder.writeln('static bool ${ptr_typ}_arr_eq($ptr_typ a, $ptr_typ b) {')
	fn_builder.writeln('\tif (a.len != b.len) {')
	fn_builder.writeln('\t\treturn false;')
	fn_builder.writeln('\t}')
	fn_builder.writeln('\tfor (int i = 0; i < a.len; ++i) {')
	// compare every pair of elements of the two arrays
	if elem_sym.kind == .string {
		fn_builder.writeln('\t\tif (string_ne(*(($ptr_elem_typ*)((byte*)a.data+(i*a.element_size))), *(($ptr_elem_typ*)((byte*)b.data+(i*b.element_size))))) {')
	} else if elem_sym.kind == .struct_ && !elem_typ.is_ptr() {
		eq_fn := g.gen_struct_equality_fn(elem_typ)
		fn_builder.writeln('\t\tif (!${eq_fn}_struct_eq((($ptr_elem_typ*)a.data)[i], (($ptr_elem_typ*)b.data)[i])) {')
	} else if elem_sym.kind == .array && !elem_typ.is_ptr() {
		eq_fn := g.gen_array_equality_fn(elem_typ)
		fn_builder.writeln('\t\tif (!${eq_fn}_arr_eq((($ptr_elem_typ*)a.data)[i], (($ptr_elem_typ*)b.data)[i])) {')
	} else if elem_sym.kind == .array_fixed && !elem_typ.is_ptr() {
		eq_fn := g.gen_fixed_array_equality_fn(elem_typ)
		fn_builder.writeln('\t\tif (!${eq_fn}_arr_eq((($ptr_elem_typ*)a.data)[i], (($ptr_elem_typ*)b.data)[i])) {')
	} else if elem_sym.kind == .map && !elem_typ.is_ptr() {
		eq_fn := g.gen_map_equality_fn(elem_typ)
		fn_builder.writeln('\t\tif (!${eq_fn}_map_eq((($ptr_elem_typ*)a.data)[i], (($ptr_elem_typ*)b.data)[i])) {')
	} else if elem_sym.kind == .function {
		fn_builder.writeln('\t\tif (*((voidptr*)((byte*)a.data+(i*a.element_size))) != *((voidptr*)((byte*)b.data+(i*b.element_size)))) {')
	} else {
		fn_builder.writeln('\t\tif (*(($ptr_elem_typ*)((byte*)a.data+(i*a.element_size))) != *(($ptr_elem_typ*)((byte*)b.data+(i*b.element_size)))) {')
	}
	fn_builder.writeln('\t\t\treturn false;')
	fn_builder.writeln('\t\t}')
	fn_builder.writeln('\t}')
	fn_builder.writeln('\treturn true;')
	fn_builder.writeln('}')
	g.auto_fn_definitions << fn_builder.str()
	return ptr_typ
}

fn (mut g Gen) gen_fixed_array_equality_fn(left table.Type) string {
	ptr_typ := g.typ(left).trim('*')
	if ptr_typ in g.array_fn_definitions {
		return ptr_typ
	}
	g.array_fn_definitions << ptr_typ
	left_sym := g.table.get_type_symbol(left)
	elem_info := left_sym.array_fixed_info()
	elem_typ := elem_info.elem_type
	size := elem_info.size
	elem_sym := g.table.get_type_symbol(elem_typ)
	g.type_definitions.writeln('static bool ${ptr_typ}_arr_eq($ptr_typ a, $ptr_typ b); // auto')
	mut fn_builder := strings.new_builder(512)
	fn_builder.writeln('static bool ${ptr_typ}_arr_eq($ptr_typ a, $ptr_typ b) {')
	fn_builder.writeln('\tfor (int i = 0; i < $size; ++i) {')
	// compare every pair of elements of the two fixed arrays
	if elem_sym.kind == .string {
		fn_builder.writeln('\t\tif (string_ne(a[i], b[i])) {')
	} else if elem_sym.kind == .struct_ && !elem_typ.is_ptr() {
		eq_fn := g.gen_struct_equality_fn(elem_typ)
		fn_builder.writeln('\t\tif (!${eq_fn}_struct_eq(a[i], b[i])) {')
	} else if elem_sym.kind == .array && !elem_typ.is_ptr() {
		eq_fn := g.gen_array_equality_fn(elem_typ)
		fn_builder.writeln('\t\tif (!${eq_fn}_arr_eq(a[i], b[i])) {')
	} else if elem_sym.kind == .array_fixed && !elem_typ.is_ptr() {
		eq_fn := g.gen_fixed_array_equality_fn(elem_typ)
		fn_builder.writeln('\t\tif (!${eq_fn}_arr_eq(a[i], b[i])) {')
	} else if elem_sym.kind == .map && !elem_typ.is_ptr() {
		eq_fn := g.gen_map_equality_fn(elem_typ)
		fn_builder.writeln('\t\tif (!${eq_fn}_map_eq(a[i], b[i])) {')
	} else if elem_sym.kind == .function {
		fn_builder.writeln('\t\tif (a[i] != b[i]) {')
	} else {
		fn_builder.writeln('\t\tif (a[i] != b[i]) {')
	}
	fn_builder.writeln('\t\t\treturn false;')
	fn_builder.writeln('\t\t}')
	fn_builder.writeln('\t}')
	fn_builder.writeln('\treturn true;')
	fn_builder.writeln('}')
	g.auto_fn_definitions << fn_builder.str()
	return ptr_typ
}

fn (mut g Gen) gen_map_equality_fn(left table.Type) string {
	ptr_typ := g.typ(left).trim('*')
	if ptr_typ in g.map_fn_definitions {
		return ptr_typ
	}
	g.map_fn_definitions << ptr_typ
	left_sym := g.table.get_type_symbol(left)
	value_typ := left_sym.map_info().value_type
	ptr_value_typ := g.typ(value_typ)
	g.type_definitions.writeln('static bool ${ptr_typ}_map_eq($ptr_typ a, $ptr_typ b); // auto')
	mut fn_builder := strings.new_builder(512)
	fn_builder.writeln('static bool ${ptr_typ}_map_eq($ptr_typ a, $ptr_typ b) {')
	fn_builder.writeln('\tif (a.len != b.len) {')
	fn_builder.writeln('\t\treturn false;')
	fn_builder.writeln('\t}')
	fn_builder.writeln('\tfor (int i = 0; i < a.key_values.len; ++i) {')
	fn_builder.writeln('\t\tif (!DenseArray_has_index(&a.key_values, i)) continue;')
	fn_builder.writeln('\t\tvoidptr k = DenseArray_key(&a.key_values, i);')
	fn_builder.writeln('\t\tif (!map_exists_1(&b, k)) return false;')
	kind := g.table.type_kind(value_typ)
	if kind == .function {
		value_sym := g.table.get_type_symbol(value_typ)
		func := value_sym.info as table.FnType
		ret_styp := g.typ(func.func.return_type)
		fn_builder.write('\t\t$ret_styp (*v) (')
		arg_len := func.func.params.len
		for j, arg in func.func.params {
			arg_styp := g.typ(arg.typ)
			fn_builder.write('$arg_styp $arg.name')
			if j < arg_len - 1 {
				fn_builder.write(', ')
			}
		}
		fn_builder.writeln(') = *(voidptr*)map_get_1(&a, k, &(voidptr[]){ 0 });')
	} else {
		fn_builder.writeln('\t\t$ptr_value_typ v = *($ptr_value_typ*)map_get_1(&a, k, &($ptr_value_typ[]){ 0 });')
	}
	match kind {
		.string {
			fn_builder.writeln('\t\tif (!fast_string_eq(*(string*)map_get_1(&b, k, &(string[]){_SLIT("")}), v)) {')
		}
		.struct_ {
			eq_fn := g.gen_struct_equality_fn(value_typ)
			fn_builder.writeln('\t\tif (!${eq_fn}_struct_eq(*($ptr_value_typ*)map_get_1(&b, k, &($ptr_value_typ[]){ 0 }), v)) {')
		}
		.array {
			eq_fn := g.gen_array_equality_fn(value_typ)
			fn_builder.writeln('\t\tif (!${eq_fn}_arr_eq(*($ptr_value_typ*)map_get_1(&b, k, &($ptr_value_typ[]){ 0 }), v)) {')
		}
		.array_fixed {
			eq_fn := g.gen_fixed_array_equality_fn(value_typ)
			fn_builder.writeln('\t\tif (!${eq_fn}_arr_eq(*($ptr_value_typ*)map_get_1(&b, k, &($ptr_value_typ[]){ 0 }), v)) {')
		}
		.map {
			eq_fn := g.gen_map_equality_fn(value_typ)
			fn_builder.writeln('\t\tif (!${eq_fn}_map_eq(*($ptr_value_typ*)map_get_1(&b, k, &($ptr_value_typ[]){ 0 }), v)) {')
		}
		.function {
			fn_builder.writeln('\t\tif (*(voidptr*)map_get_1(&b, k, &(voidptr[]){ 0 }) != v) {')
		}
		else {
			fn_builder.writeln('\t\tif (*($ptr_value_typ*)map_get_1(&b, k, &($ptr_value_typ[]){ 0 }) != v) {')
		}
	}
	fn_builder.writeln('\t\t\treturn false;')
	fn_builder.writeln('\t\t}')
	fn_builder.writeln('\t}')
	fn_builder.writeln('\treturn true;')
	fn_builder.writeln('}')
	g.auto_fn_definitions << fn_builder.str()
	return ptr_typ
}
