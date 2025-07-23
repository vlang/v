// Copyright (c) 2019-2024 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license that can be found in the LICENSE file.
module c

import strings
import v.ast

fn (mut g Gen) equality_fn(typ ast.Type) string {
	g.needed_equality_fns << typ.set_nr_muls(0)
	t1 := g.unwrap_generic(typ)
	t2 := t1.set_nr_muls(0)
	st2 := g.styp(t2)
	res := st2.replace('struct ', '')
	return res
}

fn (mut g Gen) gen_equality_fns() {
	for needed_typ in g.needed_equality_fns {
		if needed_typ in g.generated_eq_fns {
			continue
		}
		sym := g.table.sym(needed_typ)
		match sym.kind {
			.sum_type {
				g.gen_sumtype_equality_fn(needed_typ)
			}
			.struct {
				g.gen_struct_equality_fn(needed_typ)
			}
			.array {
				g.gen_array_equality_fn(needed_typ)
			}
			.array_fixed {
				g.gen_fixed_array_equality_fn(needed_typ)
			}
			.map {
				g.gen_map_equality_fn(needed_typ)
			}
			.alias {
				g.gen_alias_equality_fn(needed_typ)
			}
			.interface {
				g.gen_interface_equality_fn(needed_typ)
			}
			else {
				verror('could not generate equality function for type ${sym.kind}')
			}
		}
	}
}

fn (mut g Gen) gen_sumtype_equality_fn(left_type ast.Type) string {
	left := g.unwrap(left_type)
	ptr_styp := g.styp(left.typ.set_nr_muls(0))

	left_no_ptr := left_type.set_nr_muls(0)
	if left_no_ptr in g.generated_eq_fns {
		return ptr_styp
	}
	g.generated_eq_fns << left_no_ptr

	info := left.sym.sumtype_info()
	g.definitions.writeln('bool ${ptr_styp}_sumtype_eq(${ptr_styp} a, ${ptr_styp} b);')

	left_typ := g.read_field(left_type, '_typ', 'a')
	right_typ := g.read_field(left_type, '_typ', 'b')

	mut fn_builder := strings.new_builder(512)
	fn_builder.writeln('inline bool ${ptr_styp}_sumtype_eq(${ptr_styp} a, ${ptr_styp} b) {')
	fn_builder.writeln('\tif (${left_typ} != ${right_typ}) { return false; }')
	fn_builder.writeln('\tif (${left_typ} == ${right_typ} && ${right_typ} == 0) { return true; } // uninitialized')
	for typ in info.variants {
		variant := g.unwrap(typ)
		fn_builder.writeln('\tif (${left_typ} == ${int(variant.typ)}) {')
		name := '_${g.get_sumtype_variant_name(variant.typ, variant.sym)}'

		left_arg := g.read_field(left_type, name, 'a')
		right_arg := g.read_field(left_type, name, 'b')

		if variant.typ.has_flag(.option) {
			fn_builder.writeln('\t\treturn ((*${left_arg}).state == 2 && (*${right_arg}).state == 2) || !memcmp(&(*${left_arg}).data, &(*${right_arg}).data, sizeof(${g.base_type(variant.typ)}));')
		} else if variant.sym.kind == .string {
			fn_builder.writeln('\t\treturn string__eq(*${left_arg}, *${right_arg});')
		} else if variant.sym.kind == .sum_type && !typ.is_ptr() {
			eq_fn := g.gen_sumtype_equality_fn(typ)
			fn_builder.writeln('\t\treturn ${eq_fn}_sumtype_eq(*${left_arg}, *${right_arg});')
		} else if variant.sym.kind == .struct && !typ.is_ptr() {
			eq_fn := g.gen_struct_equality_fn(typ)
			fn_builder.writeln('\t\treturn ${eq_fn}_struct_eq(*${left_arg}, *${right_arg});')
		} else if variant.sym.kind == .array && !typ.is_ptr() {
			eq_fn := g.gen_array_equality_fn(typ)
			fn_builder.writeln('\t\treturn ${eq_fn}_arr_eq(*${left_arg}, *${right_arg});')
		} else if variant.sym.kind == .array_fixed && !typ.is_ptr() {
			eq_fn := g.gen_fixed_array_equality_fn(typ)
			fn_builder.writeln('\t\treturn ${eq_fn}_arr_eq(*${left_arg}, *${right_arg});')
		} else if variant.sym.kind == .map && !typ.is_ptr() {
			eq_fn := g.gen_map_equality_fn(typ)
			fn_builder.writeln('\t\treturn ${eq_fn}_map_eq(*${left_arg}, *${right_arg});')
		} else if variant.sym.kind == .alias && !typ.is_ptr() {
			if g.no_eq_method_types[typ] {
				fn_builder.writeln('\t\treturn *${left_arg} == *${right_arg};')
			} else {
				eq_fn := g.gen_alias_equality_fn(typ)
				fn_builder.writeln('\t\treturn ${eq_fn}_alias_eq(*${left_arg}, *${right_arg});')
			}
		} else if variant.sym.kind == .function {
			fn_builder.writeln('\t\treturn *((voidptr*)(*${left_arg})) == *((voidptr*)(*${right_arg}));')
		} else {
			fn_builder.writeln('\t\treturn *${left_arg} == *${right_arg};')
		}
		fn_builder.writeln('\t}')
	}
	fn_builder.writeln('\treturn false;')
	fn_builder.writeln('}')
	g.auto_fn_definitions << fn_builder.str()
	return ptr_styp
}

// read_opt generates C code accessing option data
@[inline]
fn (mut g Gen) read_opt(typ ast.Type, var_name string) string {
	return '(${g.base_type(typ)}*)${var_name}.data'
}

// read_field generates C code for reading option/no-option struct field
@[inline]
fn (mut g Gen) read_field(struct_type ast.Type, field_name string, var_name string) string {
	return if struct_type.has_flag(.option) {
		'(${g.read_opt(struct_type, var_name)})->${field_name}'
	} else {
		'${var_name}.${field_name}'
	}
}

// read_map generates C code for reading option/no-option struct field
@[inline]
fn (mut g Gen) read_map_from_option(typ ast.Type, var_name string) string {
	return if typ.has_flag(.option) {
		return '(${g.base_type(typ)}*)&${var_name}.data'
	} else {
		var_name
	}
}

// read_map_field generates C code for reading option/no-option struct field
@[inline]
fn (mut g Gen) read_map_field_from_option(typ ast.Type, field_name string, var_name string) string {
	return if typ.has_flag(.option) {
		'(*(${g.base_type(typ)}*)${var_name}.data).${field_name}'
	} else {
		'${var_name}.${field_name}'
	}
}

// read_opt_field generates C code for reading option/no-option struct field
@[inline]
fn (mut g Gen) read_opt_field(struct_type ast.Type, field_name string, var_name string, field_typ ast.Type) string {
	return if field_typ.has_flag(.option) {
		'*(${g.base_type(field_typ)}*)${g.read_field(struct_type, field_name, var_name)}.data'
	} else {
		g.read_field(struct_type, field_name, var_name)
	}
}

// read_map_opt_field generates C code for reading option/no-option map field
@[inline]
fn (mut g Gen) read_map_opt_field(struct_type ast.Type, field_name string, var_name string, field_typ ast.Type) string {
	return if field_typ.has_flag(.option) {
		'*(${g.base_type(field_typ)}*)${g.read_field(struct_type, field_name, var_name)}.data'
	} else {
		g.read_field(struct_type, field_name, var_name)
	}
}

fn (mut g Gen) gen_struct_equality_fn(left_type ast.Type) string {
	left := g.unwrap(left_type)
	ptr_styp := g.styp(left.typ.set_nr_muls(0))
	fn_name := ptr_styp.replace('struct ', '')

	left_no_ptr := left_type.set_nr_muls(0)
	if left_no_ptr in g.generated_eq_fns {
		return fn_name
	}
	g.generated_eq_fns << left_no_ptr

	info := left.sym.struct_info()
	g.definitions.writeln('bool ${fn_name}_struct_eq(${ptr_styp} a, ${ptr_styp} b);')

	mut fn_builder := strings.new_builder(512)
	defer {
		g.auto_fn_definitions << fn_builder.str()
	}
	fn_builder.writeln('inline bool ${fn_name}_struct_eq(${ptr_styp} a, ${ptr_styp} b) {')

	// overloaded
	if left.sym.has_method('==') {
		if left.typ.has_flag(.option) {
			opt_ptr_styp := g.styp(left.typ.set_nr_muls(0).clear_flag(.option))
			opt_fn_name := opt_ptr_styp.replace('struct ', '')
			fn_builder.writeln('\treturn (a.state == b.state && b.state == 2) || ${opt_fn_name}__eq(*(${opt_ptr_styp}*)a.data, *(${opt_ptr_styp}*)b.data);')
		} else {
			fn_builder.writeln('\treturn ${fn_name}__eq(a, b);')
		}
		fn_builder.writeln('}')
		return fn_name
	}

	fn_builder.write_string('\treturn ')
	if info.fields.len > 0 {
		for i, field in info.fields {
			if i > 0 {
				fn_builder.write_string('\n\t\t&& ')
			}
			field_type := g.unwrap(field.typ)
			field_name := c_name(field.name)

			left_arg := g.read_field(left_type, field_name, 'a')
			right_arg := g.read_field(left_type, field_name, 'b')

			if field.typ.has_flag(.option) {
				fn_builder.write_string('((${left_arg}.state == ${right_arg}.state && ${right_arg}.state == 2) || ')
			}
			if field_type.sym.kind == .string {
				if field.typ.has_flag(.option) {
					left_arg_opt := g.read_opt_field(left_type, field_name, 'a', field.typ)
					right_arg_opt := g.read_opt_field(left_type, field_name, 'b', field.typ)
					fn_builder.write_string('(((${left_arg_opt}).len == (${right_arg_opt}).len && (${left_arg_opt}).len == 0) || fast_string_eq(${left_arg_opt}, ${right_arg_opt}))')
				} else if field.typ.is_ptr() {
					fn_builder.write_string('((${left_arg}->len == ${right_arg}->len && ${left_arg}->len == 0) || fast_string_eq(*(${left_arg}), *(${right_arg})))')
				} else {
					fn_builder.write_string('((${left_arg}.len == ${right_arg}.len && ${left_arg}.len == 0) || fast_string_eq(${left_arg}, ${right_arg}))')
				}
			} else if field_type.sym.kind == .sum_type && !field.typ.is_ptr() {
				eq_fn := g.gen_sumtype_equality_fn(field.typ)
				fn_builder.write_string('${eq_fn}_sumtype_eq(${left_arg}, ${right_arg})')
			} else if field_type.sym.kind == .struct && !field.typ.is_ptr() {
				eq_fn := g.gen_struct_equality_fn(field.typ)
				fn_builder.write_string('${eq_fn}_struct_eq(${left_arg}, ${right_arg})')
			} else if field_type.sym.kind == .array && !field.typ.is_ptr() {
				eq_fn := g.gen_array_equality_fn(field.typ)
				fn_builder.write_string('${eq_fn}_arr_eq(${left_arg}, ${right_arg})')
			} else if field_type.sym.kind == .array_fixed && !field.typ.is_ptr() {
				eq_fn := g.gen_fixed_array_equality_fn(field.typ)
				fn_builder.write_string('${eq_fn}_arr_eq(${left_arg}, ${right_arg})')
			} else if field_type.sym.kind == .map && !field.typ.is_ptr() {
				eq_fn := g.gen_map_equality_fn(field.typ)
				fn_builder.write_string('${eq_fn}_map_eq(${left_arg}, ${right_arg})')
			} else if field_type.sym.kind == .alias && !field.typ.is_ptr() {
				if g.no_eq_method_types[field.typ] {
					fn_builder.write_string('${left_arg} == ${right_arg}')
				} else {
					eq_fn := g.gen_alias_equality_fn(field.typ)
					fn_builder.write_string('${eq_fn}_alias_eq(${left_arg}, ${right_arg})')
				}
			} else if field_type.sym.kind == .function && !field.typ.has_flag(.option) {
				fn_builder.write_string('*((voidptr*)(${left_arg})) == *((voidptr*)(${right_arg}))')
			} else if field_type.sym.kind == .interface
				&& (!field.typ.has_flag(.option) || !field.typ.is_ptr()) {
				ptr := if field.typ.is_ptr() { '*'.repeat(field.typ.nr_muls()) } else { '' }
				eq_fn := g.gen_interface_equality_fn(field.typ)
				if ptr != '' {
					fn_builder.write_string('((${left_arg} == (void*)0 && ${right_arg} == (void*)0) || (${left_arg} != (void*)0 && ${right_arg} != (void*)0 && ')
				}
				fn_builder.write_string('${eq_fn}_interface_eq(${ptr}${left_arg}, ${ptr}${right_arg})')
				if ptr != '' {
					fn_builder.write_string('))')
				}
			} else if field.typ.has_flag(.option) {
				fn_builder.write_string('!memcmp(&${left_arg}.data, &${right_arg}.data, sizeof(${g.base_type(field.typ)}))')
			} else {
				fn_builder.write_string('${left_arg} == ${right_arg}')
			}
			if field.typ.has_flag(.option) {
				fn_builder.write_string(')')
			}
		}
	} else {
		fn_builder.write_string('true')
	}
	fn_builder.writeln(';')
	fn_builder.writeln('}')
	return fn_name
}

fn (mut g Gen) gen_alias_equality_fn(left_type ast.Type) string {
	left := g.unwrap(left_type)
	ptr_styp := g.styp(left.typ.set_nr_muls(0))

	left_no_ptr := left_type.set_nr_muls(0)
	if left_no_ptr in g.generated_eq_fns {
		return ptr_styp
	}
	g.generated_eq_fns << left_no_ptr

	info := left.sym.info as ast.Alias
	g.definitions.writeln('bool ${ptr_styp}_alias_eq(${ptr_styp} a, ${ptr_styp} b);')

	mut fn_builder := strings.new_builder(512)
	fn_builder.writeln('inline bool ${ptr_styp}_alias_eq(${ptr_styp} a, ${ptr_styp} b) {')

	is_option := left.typ.has_flag(.option)

	mut left_var := if is_option { '*' + g.read_opt(info.parent_type, 'a') } else { 'a' }
	mut right_var := if is_option { '*' + g.read_opt(info.parent_type, 'b') } else { 'b' }

	sym := g.table.sym(info.parent_type)
	if sym.kind == .string {
		if info.parent_type.has_flag(.option) {
			left_var = '*' + g.read_opt(info.parent_type, 'a')
			right_var = '*' + g.read_opt(info.parent_type, 'b')
			fn_builder.writeln('\treturn ((${left_var}).len == (${right_var}).len && (${left_var}).len == 0) || fast_string_eq(${left_var}, ${right_var});')
		} else {
			fn_builder.writeln('\treturn string__eq(a, b);')
		}
	} else if sym.kind == .sum_type && !left.typ.is_ptr() {
		eq_fn := g.gen_sumtype_equality_fn(info.parent_type)
		fn_builder.writeln('\treturn ${eq_fn}_sumtype_eq(${left_var}, ${right_var});')
	} else if sym.kind == .struct && !left.typ.is_ptr() {
		eq_fn := g.gen_struct_equality_fn(info.parent_type)
		fn_builder.writeln('\treturn ${eq_fn}_struct_eq(${left_var}, ${right_var});')
	} else if sym.kind == .interface && !left.typ.is_ptr() {
		eq_fn := g.gen_interface_equality_fn(info.parent_type)
		fn_builder.writeln('\treturn ${eq_fn}_interface_eq(${left_var}, ${right_var});')
	} else if sym.kind == .array && !left.typ.is_ptr() {
		eq_fn := g.gen_array_equality_fn(info.parent_type)
		fn_builder.writeln('\treturn ${eq_fn}_arr_eq(${left_var}, ${right_var});')
	} else if sym.kind == .array_fixed && !left.typ.is_ptr() {
		eq_fn := g.gen_fixed_array_equality_fn(info.parent_type)
		fn_builder.writeln('\treturn ${eq_fn}_arr_eq(${left_var}, ${right_var});')
	} else if sym.kind == .map && !left.typ.is_ptr() {
		eq_fn := g.gen_map_equality_fn(info.parent_type)
		fn_builder.writeln('\treturn ${eq_fn}_map_eq(${left_var}, ${right_var});')
	} else if sym.kind == .function && !left.typ.has_flag(.option) {
		fn_builder.writeln('\treturn *((voidptr*)(a)) == *((voidptr*)(b));')
	} else if info.parent_type.has_flag(.option) || left.typ.has_flag(.option) {
		fn_builder.writeln('\treturn a.state == b.state && !memcmp(&a.data, &b.data, sizeof(${g.base_type(info.parent_type)}));')
	} else {
		fn_builder.writeln('\treturn a == b;')
	}
	fn_builder.writeln('}')
	g.auto_fn_definitions << fn_builder.str()
	return ptr_styp
}

fn (mut g Gen) gen_array_equality_fn(left_type ast.Type) string {
	left := g.unwrap(left_type)
	ptr_styp := g.styp(left.typ.set_nr_muls(0))

	left_no_ptr := left_type.set_nr_muls(0)
	if left_no_ptr in g.generated_eq_fns {
		return ptr_styp
	}
	g.generated_eq_fns << left_no_ptr

	elem := g.unwrap(left.sym.array_info().elem_type)
	ptr_elem_styp := g.styp(elem.typ)
	g.definitions.writeln('bool ${ptr_styp}_arr_eq(${ptr_styp} a, ${ptr_styp} b);')

	mut fn_builder := strings.new_builder(512)
	fn_builder.writeln('inline bool ${ptr_styp}_arr_eq(${ptr_styp} a, ${ptr_styp} b) {')

	left_len := g.read_field(left_type, 'len', 'a')
	right_len := g.read_field(left_type, 'len', 'b')

	left_data := g.read_field(left_type, 'data', 'a')
	right_data := g.read_field(left_type, 'data', 'b')

	left_elem := g.read_field(left_type, 'element_size', 'a')
	right_elem := g.read_field(left_type, 'element_size', 'b')

	if left_type.has_flag(.option) {
		fn_builder.writeln('\tif (a.state != b.state) return false;')
		fn_builder.writeln('\tif (a.state == 2 && a.state == b.state) return true;')
	}

	fn_builder.writeln('\tif (${left_len} != ${right_len}) {')
	fn_builder.writeln('\t\treturn false;')
	fn_builder.writeln('\t}')
	fn_builder.writeln('\tfor (int i = 0; i < ${left_len}; ++i) {')
	// compare every pair of elements of the two arrays
	if elem.sym.kind == .string {
		fn_builder.writeln('\t\tif (!string__eq(*((${ptr_elem_styp}*)((byte*)${left_data}+(i*${left_elem}))), *((${ptr_elem_styp}*)((byte*)${right_data}+(i*${right_elem}))))) {')
	} else if elem.sym.kind == .sum_type && !elem.typ.is_ptr() {
		eq_fn := g.gen_sumtype_equality_fn(elem.typ)
		fn_builder.writeln('\t\tif (!${eq_fn}_sumtype_eq(((${ptr_elem_styp}*)${left_data})[i], ((${ptr_elem_styp}*)${right_data})[i])) {')
	} else if elem.sym.kind == .struct && !elem.typ.is_ptr() {
		eq_fn := g.gen_struct_equality_fn(elem.typ)
		fn_builder.writeln('\t\tif (!${eq_fn}_struct_eq(((${ptr_elem_styp}*)${left_data})[i], ((${ptr_elem_styp}*)${right_data})[i])) {')
	} else if elem.sym.kind == .interface && !elem.typ.is_ptr() {
		eq_fn := g.gen_interface_equality_fn(elem.typ)
		fn_builder.writeln('\t\tif (!${eq_fn}_interface_eq(((${ptr_elem_styp}*)${left_data})[i], ((${ptr_elem_styp}*)${right_data})[i])) {')
	} else if elem.sym.kind == .array && !elem.typ.is_ptr() {
		eq_fn := g.gen_array_equality_fn(elem.typ)
		fn_builder.writeln('\t\tif (!${eq_fn}_arr_eq(((${ptr_elem_styp}*)${left_data})[i], ((${ptr_elem_styp}*)${right_data})[i])) {')
	} else if elem.sym.kind == .array_fixed && !elem.typ.is_ptr() {
		eq_fn := g.gen_fixed_array_equality_fn(elem.typ)
		fn_builder.writeln('\t\tif (!${eq_fn}_arr_eq(((${ptr_elem_styp}*)${left_data})[i], ((${ptr_elem_styp}*)${right_data})[i])) {')
	} else if elem.sym.kind == .map && !elem.typ.is_ptr() {
		eq_fn := g.gen_map_equality_fn(elem.typ)
		fn_builder.writeln('\t\tif (!${eq_fn}_map_eq(((${ptr_elem_styp}*)${left_data})[i], ((${ptr_elem_styp}*)${right_data})[i])) {')
	} else if elem.sym.kind == .alias && !elem.typ.is_ptr() {
		if g.no_eq_method_types[elem.typ] {
			fn_builder.writeln('\t\tif (((${ptr_elem_styp}*)${left_data})[i] != ((${ptr_elem_styp}*)${right_data})[i]) {')
		} else {
			eq_fn := g.gen_alias_equality_fn(elem.typ)
			fn_builder.writeln('\t\tif (!${eq_fn}_alias_eq(((${ptr_elem_styp}*)${left_data})[i], ((${ptr_elem_styp}*)${right_data})[i])) {')
		}
	} else if elem.sym.kind == .function {
		fn_builder.writeln('\t\tif (*((voidptr*)((byte*)${left_data}+(i*${left_elem}))) != *((voidptr*)((byte*)${right_data}+(i*${right_elem})))) {')
	} else {
		if elem.typ.has_flag(.option) {
			fn_builder.writeln('\t\t${ptr_elem_styp}* left = ((${ptr_elem_styp}*)${left_data})+(i*${left_elem});')
			fn_builder.writeln('\t\t${ptr_elem_styp}* right = ((${ptr_elem_styp}*)${right_data})+(i*${right_elem});')
			fn_builder.writeln('\t\tif (!(left->state == 2 && left->state == right->state) && memcmp(left->data, right->data, sizeof(${g.base_type(elem.typ)}))) {')
		} else {
			fn_builder.writeln('\t\tif (*((${ptr_elem_styp}*)((byte*)${left_data}+(i*${left_elem}))) != *((${ptr_elem_styp}*)((byte*)${right_data}+(i*${right_elem})))) {')
		}
	}
	fn_builder.writeln('\t\t\treturn false;')
	fn_builder.writeln('\t\t}')
	fn_builder.writeln('\t}')
	fn_builder.writeln('\treturn true;')
	fn_builder.writeln('}')
	g.auto_fn_definitions << fn_builder.str()
	return ptr_styp
}

fn (mut g Gen) gen_fixed_array_equality_fn(left_type ast.Type) string {
	left_typ := g.unwrap(left_type)
	ptr_styp := g.styp(left_typ.typ.set_nr_muls(0))

	left_no_ptr := left_type.set_nr_muls(0)
	if left_no_ptr in g.generated_eq_fns {
		return ptr_styp
	}
	g.generated_eq_fns << left_no_ptr

	elem_info := left_typ.sym.array_fixed_info()
	elem := g.unwrap(elem_info.elem_type)
	size := elem_info.size
	mut arg_styp := ptr_styp
	if elem_info.is_fn_ret {
		arg_styp = ptr_styp[3..] // removes the _v_ prefix for returning fixed array
	}
	g.definitions.writeln('bool ${ptr_styp}_arr_eq(${arg_styp} a, ${arg_styp} b);')

	is_option := left_type.has_flag(.option)
	left := if is_option { 'a.data' } else { 'a' }
	right := if is_option { 'b.data' } else { 'b' }

	mut fn_builder := strings.new_builder(512)
	fn_builder.writeln('inline bool ${ptr_styp}_arr_eq(${arg_styp} a, ${arg_styp} b) {')
	if is_option {
		fn_builder.writeln('\tif (a.state != b.state) return false;')
		fn_builder.writeln('\tif (a.state == 2 && a.state == b.state) return true;')
	}
	if left_typ.sym.is_primitive_fixed_array() {
		suffix := if is_option { '.data' } else { '[0]' }
		size_styp := if is_option {
			g.base_type(left_typ.typ.set_nr_muls(0))
		} else {
			arg_styp
		}
		fn_builder.writeln('\tif (!memcmp(&a${suffix}, &b${suffix}, sizeof(${size_styp}))) {')
		fn_builder.writeln('\t\treturn true;')
		fn_builder.writeln('\t}')
	}
	fn_builder.writeln('\tfor (int i = 0; i < ${size}; ++i) {')
	// compare every pair of elements of the two fixed arrays
	if elem.sym.kind == .string {
		fn_builder.writeln('\t\tif (!string__eq(((string*)${left})[i], ((string*)${right})[i])) {')
	} else if elem.sym.kind == .sum_type && !elem.typ.is_ptr() {
		eq_fn := g.gen_sumtype_equality_fn(elem.typ)
		fn_builder.writeln('\t\tif (!${eq_fn}_sumtype_eq(${left}[i], ${right}[i])) {')
	} else if elem.sym.kind == .struct && !elem.typ.is_ptr() {
		eq_fn := g.gen_struct_equality_fn(elem.typ)
		fn_builder.writeln('\t\tif (!${eq_fn}_struct_eq(${left}[i], ${right}[i])) {')
	} else if elem.sym.kind == .interface && !elem.typ.is_ptr() {
		eq_fn := g.gen_interface_equality_fn(elem.typ)
		fn_builder.writeln('\t\tif (!${eq_fn}_interface_eq(${left}[i], ${right}[i])) {')
	} else if elem.sym.kind == .array && !elem.typ.is_ptr() {
		eq_fn := g.gen_array_equality_fn(elem.typ)
		fn_builder.writeln('\t\tif (!${eq_fn}_arr_eq(${left}[i], ${right}[i])) {')
	} else if elem.sym.kind == .array_fixed && !elem.typ.is_ptr() {
		eq_fn := g.gen_fixed_array_equality_fn(elem.typ)
		fn_builder.writeln('\t\tif (!${eq_fn}_arr_eq(${left}[i], ${right}[i])) {')
	} else if elem.sym.kind == .map && !elem.typ.is_ptr() {
		eq_fn := g.gen_map_equality_fn(elem.typ)
		fn_builder.writeln('\t\tif (!${eq_fn}_map_eq(${left}[i], ${right}[i])) {')
	} else if elem.sym.kind == .alias && !elem.typ.is_ptr() {
		eq_fn := g.gen_alias_equality_fn(elem.typ)
		fn_builder.writeln('\t\tif (!${eq_fn}_alias_eq(${left}[i], ${right}[i])) {')
	} else if elem.sym.kind == .function {
		fn_builder.writeln('\t\tif (${left}[i] != ${right}[i]) {')
	} else {
		fn_builder.writeln('\t\tif (${left}[i] != ${right}[i]) {')
	}
	fn_builder.writeln('\t\t\treturn false;')
	fn_builder.writeln('\t\t}')
	fn_builder.writeln('\t}')
	fn_builder.writeln('\treturn true;')
	fn_builder.writeln('}')
	g.auto_fn_definitions << fn_builder.str()
	return ptr_styp
}

fn (mut g Gen) gen_map_equality_fn(left_type ast.Type) string {
	left := g.unwrap(left_type)
	ptr_styp := g.styp(left.typ.set_nr_muls(0))

	left_no_ptr := left_type.set_nr_muls(0)
	if left_no_ptr in g.generated_eq_fns {
		return ptr_styp
	}
	g.generated_eq_fns << left_no_ptr

	value := g.unwrap(left.sym.map_info().value_type)
	ptr_value_styp := g.styp(value.typ)
	g.definitions.writeln('bool ${ptr_styp}_map_eq(${ptr_styp} a, ${ptr_styp} b);')

	left_len := g.read_map_field_from_option(left.typ, 'len', 'a')
	right_len := g.read_map_field_from_option(left.typ, 'len', 'b')
	key_values := g.read_map_field_from_option(left.typ, 'key_values', 'a')

	a := if left.typ.has_flag(.option) { g.read_map_from_option(left.typ, 'a') } else { '&a' }
	b := if left.typ.has_flag(.option) { g.read_map_from_option(left.typ, 'b') } else { '&b' }

	mut fn_builder := strings.new_builder(512)
	fn_builder.writeln('inline bool ${ptr_styp}_map_eq(${ptr_styp} a, ${ptr_styp} b) {')
	fn_builder.writeln('\tif (${left_len} != ${right_len}) {')
	fn_builder.writeln('\t\treturn false;')
	fn_builder.writeln('\t}')
	fn_builder.writeln('\tfor (int i = 0; i < ${key_values}.len; ++i) {')
	fn_builder.writeln('\t\tif (!DenseArray_has_index(&${key_values}, i)) continue;')
	fn_builder.writeln('\t\tvoidptr k = DenseArray_key(&${key_values}, i);')
	fn_builder.writeln('\t\tif (!map_exists(${b}, k)) return false;')
	kind := g.table.type_kind(value.typ)
	if kind == .function {
		info := value.sym.info as ast.FnType
		sig := g.fn_var_signature(info.func.return_type, info.func.params.map(it.typ),
			'v')
		fn_builder.writeln('\t\t${sig} = *(voidptr*)map_get(${a}, k, &(voidptr[]){ 0 });')
	} else {
		fn_builder.writeln('\t\t${ptr_value_styp} v = *(${ptr_value_styp}*)map_get(${a}, k, &(${ptr_value_styp}[]){ 0 });')
	}
	match kind {
		.string {
			fn_builder.writeln('\t\tif (!fast_string_eq(*(string*)map_get(${b}, k, &(string[]){_S("")}), v)) {')
		}
		.sum_type {
			eq_fn := g.gen_sumtype_equality_fn(value.typ)
			fn_builder.writeln('\t\tif (!${eq_fn}_sumtype_eq(*(${ptr_value_styp}*)map_get(${b}, k, &(${ptr_value_styp}[]){ 0 }), v)) {')
		}
		.struct {
			eq_fn := g.gen_struct_equality_fn(value.typ)
			fn_builder.writeln('\t\tif (!${eq_fn}_struct_eq(*(${ptr_value_styp}*)map_get(${b}, k, &(${ptr_value_styp}[]){ 0 }), v)) {')
		}
		.interface {
			eq_fn := g.gen_interface_equality_fn(value.typ)
			fn_builder.writeln('\t\tif (!${eq_fn}_interface_eq(*(${ptr_value_styp}*)map_get(${b}, k, &(${ptr_value_styp}[]){ 0 }), v)) {')
		}
		.array {
			eq_fn := g.gen_array_equality_fn(value.typ)
			fn_builder.writeln('\t\tif (!${eq_fn}_arr_eq(*(${ptr_value_styp}*)map_get(${b}, k, &(${ptr_value_styp}[]){ 0 }), v)) {')
		}
		.array_fixed {
			eq_fn := g.gen_fixed_array_equality_fn(value.typ)
			fn_builder.writeln('\t\tif (!${eq_fn}_arr_eq(*(${ptr_value_styp}*)map_get(${b}, k, &(${ptr_value_styp}[]){ 0 }), v)) {')
		}
		.map {
			eq_fn := g.gen_map_equality_fn(value.typ)
			fn_builder.writeln('\t\tif (!${eq_fn}_map_eq(*(${ptr_value_styp}*)map_get(${b}, k, &(${ptr_value_styp}[]){ 0 }), v)) {')
		}
		.alias {
			eq_fn := g.gen_alias_equality_fn(value.typ)
			fn_builder.writeln('\t\tif (!${eq_fn}_alias_eq(*(${ptr_value_styp}*)map_get(${b}, k, &(${ptr_value_styp}[]){ 0 }), v)) {')
		}
		.function {
			fn_builder.writeln('\t\tif (*(voidptr*)map_get(${b}, k, &(voidptr[]){ 0 }) != v) {')
		}
		else {
			if value.typ.has_flag(.option) {
				fn_builder.writeln('\t\tif (memcmp(v.data, ((${ptr_value_styp}*)map_get(${b}, k, &(${ptr_value_styp}[]){ 0 }))->data, sizeof(${g.base_type(value.typ)})) != 0) {')
			} else {
				fn_builder.writeln('\t\tif (*(${ptr_value_styp}*)map_get(${b}, k, &(${ptr_value_styp}[]){ 0 }) != v) {')
			}
		}
	}
	fn_builder.writeln('\t\t\treturn false;')
	fn_builder.writeln('\t\t}')
	fn_builder.writeln('\t}')
	fn_builder.writeln('\treturn true;')
	fn_builder.writeln('}')
	g.auto_fn_definitions << fn_builder.str()
	return ptr_styp
}

fn (mut g Gen) gen_interface_equality_fn(left_type ast.Type) string {
	left := g.unwrap(left_type)
	ptr_styp := g.styp(left.typ.set_nr_muls(0))
	idx_fn := g.styp(left.typ.set_nr_muls(0).clear_flag(.option))
	fn_name := ptr_styp.replace('interface ', '')

	left_no_ptr := left_type.set_nr_muls(0)
	if left_no_ptr in g.generated_eq_fns {
		return fn_name
	}
	g.generated_eq_fns << left_no_ptr

	info := left.sym.info
	g.definitions.writeln('${g.static_non_parallel}bool ${ptr_styp}_interface_eq(${ptr_styp} a, ${ptr_styp} b);')

	mut fn_builder := strings.new_builder(512)
	defer {
		g.auto_fn_definitions << fn_builder.str()
	}

	left_arg := g.read_field(left_type, '_typ', 'a')
	right_arg := g.read_field(left_type, '_typ', 'b')

	fn_builder.writeln('${g.static_non_parallel}inline bool ${fn_name}_interface_eq(${ptr_styp} a, ${ptr_styp} b) {')
	fn_builder.writeln('\tif (${left_arg} == ${right_arg}) {')
	fn_builder.writeln('\t\tint idx = v_typeof_interface_idx_${idx_fn}(${left_arg});')
	if info is ast.Interface {
		for typ in info.types {
			sym := g.table.sym(typ.set_nr_muls(0))
			if g.pref.skip_unused && sym.idx !in g.table.used_features.used_syms {
				continue
			}
			fn_builder.writeln('\t\tif (idx == ${typ.idx()}) {')
			fn_builder.write_string('\t\t\treturn ')
			match sym.kind {
				.struct {
					eq_fn := g.gen_struct_equality_fn(typ)
					l_eqfn := g.read_field(left_type, '_${eq_fn}', 'a')
					r_eqfn := g.read_field(left_type, '_${eq_fn}', 'b')
					fn_builder.write_string('${eq_fn}_struct_eq(*(${l_eqfn}), *(${r_eqfn}))')
				}
				.string {
					l_str := g.read_field(left_type, '_string', 'a')
					r_str := g.read_field(left_type, '_string', 'b')
					fn_builder.write_string('string__eq(*(${l_str}), *(${r_str}))')
				}
				.sum_type {
					eq_fn := g.gen_sumtype_equality_fn(typ)
					l_eqfn := g.read_field(left_type, '_${eq_fn}', 'a')
					r_eqfn := g.read_field(left_type, '_${eq_fn}', 'b')
					fn_builder.write_string('${eq_fn}_sumtype_eq(*(${l_eqfn}), *(${r_eqfn}))')
				}
				.array {
					eq_fn := g.gen_array_equality_fn(typ)
					l_eqfn := g.read_field(left_type, '_${eq_fn}', 'a')
					r_eqfn := g.read_field(left_type, '_${eq_fn}', 'b')
					fn_builder.write_string('${eq_fn}_arr_eq(*(${l_eqfn}), *(${r_eqfn}))')
				}
				.array_fixed {
					eq_fn := g.gen_fixed_array_equality_fn(typ)
					l_eqfn := g.read_field(left_type, '_${eq_fn}', 'a')
					r_eqfn := g.read_field(left_type, '_${eq_fn}', 'b')
					fn_builder.write_string('${eq_fn}_arr_eq(*(${l_eqfn}), *(${r_eqfn}))')
				}
				.map {
					eq_fn := g.gen_map_equality_fn(typ)
					l_eqfn := g.read_field(left_type, '_${eq_fn}', 'a')
					r_eqfn := g.read_field(left_type, '_${eq_fn}', 'b')
					fn_builder.write_string('${eq_fn}_map_eq(*(${l_eqfn}), *(${r_eqfn}))')
				}
				.alias {
					eq_fn := g.gen_alias_equality_fn(typ)
					l_eqfn := g.read_field(left_type, '_${eq_fn}', 'a')
					r_eqfn := g.read_field(left_type, '_${eq_fn}', 'b')
					fn_builder.write_string('${eq_fn}_alias_eq(*(${l_eqfn}), *(${r_eqfn}))')
				}
				else {
					fn_builder.write_string('true')
				}
			}
			fn_builder.writeln(';')
			fn_builder.writeln('\t\t}')
		}
	}
	fn_builder.writeln('\t}')
	fn_builder.writeln('\treturn false;')
	fn_builder.writeln('}')

	return fn_name
}
