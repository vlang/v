// Copyright (c) 2019-2024 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license that can be found in the LICENSE file.
module c

import v.ast
import strings

@[inline]
fn (mut g Gen) register_free_method(typ ast.Type) {
	if typ.has_flag(.shared_f) {
		g.get_free_method(typ.clear_flag(.shared_f).set_nr_muls(0))
	} else {
		g.get_free_method(typ)
	}
}

fn (mut g Gen) get_free_method(typ ast.Type) string {
	if typ in g.autofree_methods {
		return g.autofree_methods[typ]
	}
	mut sym := g.table.sym(g.unwrap_generic(typ))
	if mut sym.info is ast.Alias {
		if sym.info.is_import {
			sym = g.table.sym(sym.info.parent_type)
		}
	}
	styp := g.styp(typ).replace('*', '')
	fn_name := styp_to_free_fn_name(styp)
	if sym.has_method_with_generic_parent('free') {
		g.autofree_methods[typ] = fn_name
		return fn_name
	}
	g.autofree_methods[typ] = fn_name
	return fn_name
}

fn (mut g Gen) gen_free_methods() {
	for typ, _ in g.autofree_methods {
		g.gen_free_method(typ)
	}
}

fn (mut g Gen) gen_free_method(typ ast.Type) string {
	styp := g.styp(typ).replace('*', '')
	mut fn_name := styp_to_free_fn_name(styp)
	deref_typ := if typ.has_flag(.option) { typ } else { typ.set_nr_muls(0) }
	if deref_typ in g.generated_free_methods {
		return fn_name
	}
	g.generated_free_methods[deref_typ] = true

	objtyp := g.unwrap_generic(typ)
	mut sym := g.table.sym(objtyp)
	if mut sym.info is ast.Alias {
		if sym.info.is_import {
			sym = g.table.sym(sym.info.parent_type)
		}
	}
	if sym.kind != .interface && sym.has_method_with_generic_parent('free') {
		return fn_name
	}

	match mut sym.info {
		ast.Struct {
			g.gen_free_for_struct(objtyp, sym.info, styp, fn_name)
		}
		ast.Array {
			g.gen_free_for_array(sym.info, styp, fn_name)
		}
		ast.Map {
			g.gen_free_for_map(objtyp, styp, fn_name)
		}
		ast.Interface {
			g.gen_free_for_interface(sym, sym.info, styp, fn_name)
		}
		else {
			println(g.table.type_str(typ))
			// print_backtrace()
			println("could not generate free method '${fn_name}' for type '${styp}'")
			// verror("could not generate free method '${fn_name}' for type '${styp}'")
		}
	}
	return fn_name
}

fn (mut g Gen) gen_free_for_interface(sym ast.TypeSymbol, info ast.Interface, styp string, fn_name string) {
	g.definitions.writeln('${g.static_non_parallel}void ${fn_name}(${styp}* it);')
	mut fn_builder := strings.new_builder(128)
	defer {
		g.auto_fn_definitions << fn_builder.str()
	}
	fn_builder.writeln('${g.static_non_parallel}void ${fn_name}(${styp}* it) {')
	for t in info.types {
		typ_ := g.unwrap_generic(t)
		sub_sym := g.table.sym(typ_)
		if sub_sym.kind !in [.string, .array, .map, .struct] {
			continue
		}
		if !sub_sym.has_method_with_generic_parent('free') {
			continue
		}
		type_styp := g.gen_type_name_for_free_call(typ_)
		fn_builder.writeln('\tif (it->_typ == _${sym.cname}_${sub_sym.cname}_index) { ${type_styp}_free(it->_${sub_sym.cname}); return; }')
	}
	fn_builder.writeln('}')
}

fn (mut g Gen) gen_free_for_struct(typ ast.Type, info ast.Struct, styp string, fn_name string) {
	g.definitions.writeln('${g.static_non_parallel}void ${fn_name}(${styp}* it);')
	mut fn_builder := strings.new_builder(128)
	defer {
		g.auto_fn_definitions << fn_builder.str()
	}
	fn_builder.writeln('${g.static_non_parallel}void ${fn_name}(${styp}* it) {')
	for field in info.fields {
		field_name := c_name(field.name)
		sym := g.table.sym(g.unwrap_generic(field.typ))

		if sym.kind !in [.string, .array, .map, .struct] {
			continue
		}
		field_styp := g.gen_type_name_for_free_call(field.typ)
		is_struct_option := typ.has_flag(.option)
		field_styp_fn_name := if sym.has_method('free') {
			'${field_styp}_free'
		} else {
			g.gen_free_method(field.typ)
		}
		is_field_option := field.typ.has_flag(.option)
		expects_opt := field_styp_fn_name.starts_with('_option_')
		if field.typ.has_flag(.shared_f) {
			fn_builder.writeln('\t${field_styp_fn_name}(&(it->${field_name}->val));')
		} else if is_struct_option {
			opt_styp := g.base_type(typ)
			prefix := if field.typ.is_ptr() { '' } else { '&' }
			if is_field_option {
				opt_field_styp := if expects_opt {
					g.styp(field.typ)
				} else {
					g.base_type(field.typ)
				}
				suffix := if expects_opt { '' } else { '.data' }

				fn_builder.writeln('\tif (((${opt_styp}*)&it->data)->${field_name}.state != 2) {')
				fn_builder.writeln('\t\t${field_styp_fn_name}((${opt_field_styp}*)${prefix}((${opt_styp}*)&it->data)->${field_name}${suffix});')
				fn_builder.writeln('\t}')
			} else {
				fn_builder.writeln('\t${field_styp_fn_name}(${prefix}((${opt_styp}*)&it->data)->${field_name});')
			}
		} else {
			if is_field_option {
				opt_field_styp := if expects_opt {
					g.styp(field.typ)
				} else {
					g.base_type(field.typ)
				}
				suffix := if expects_opt { '' } else { '.data' }

				fn_builder.writeln('\tif (it->${field_name}.state != 2) {')
				fn_builder.writeln('\t\t${field_styp_fn_name}((${opt_field_styp}*)&(it->${field_name}${suffix}));')
				fn_builder.writeln('\t}')
			} else {
				fn_builder.writeln('\t${field_styp_fn_name}(&(it->${field_name}));')
			}
		}
	}
	fn_builder.writeln('}')
}

fn (mut g Gen) gen_type_name_for_free_call(typ ast.Type) string {
	mut styp := g.styp(typ.set_nr_muls(0).clear_flag(.option)).replace('*', '')
	if styp.starts_with('__shared') {
		styp = styp.all_after('__shared__')
	}
	return styp
}

fn (mut g Gen) gen_free_for_array(info ast.Array, styp string, fn_name string) {
	g.definitions.writeln('${g.static_non_parallel}void ${fn_name}(${styp}* it);')
	mut fn_builder := strings.new_builder(128)
	defer {
		g.auto_fn_definitions << fn_builder.str()
	}
	fn_builder.writeln('${g.static_non_parallel}void ${fn_name}(${styp}* it) {')

	sym := g.table.sym(g.unwrap_generic(info.elem_type))
	if sym.kind in [.string, .array, .map, .struct] {
		fn_builder.writeln('\tfor (int i = 0; i < it->len; i++) {')

		mut elem_styp := g.styp(info.elem_type).replace('*', '')
		elem_styp_fn_name := if sym.has_method('free') {
			'${elem_styp}_free'
		} else {
			g.gen_free_method(info.elem_type)
		}
		fn_builder.writeln('\t\t${elem_styp_fn_name}(&(((${elem_styp}*)it->data)[i]));')
		fn_builder.writeln('\t}')
	}
	fn_builder.writeln('\tarray_free(it);')
	fn_builder.writeln('}')
}

fn (mut g Gen) gen_free_for_map(typ ast.Type, styp string, fn_name string) {
	g.definitions.writeln('${g.static_non_parallel}void ${fn_name}(${styp}* it);')
	mut fn_builder := strings.new_builder(128)
	defer {
		g.auto_fn_definitions << fn_builder.str()
	}
	fn_builder.writeln('${g.static_non_parallel}void ${fn_name}(${styp}* it) {')

	if typ.has_flag(.option) {
		fn_builder.writeln('\tif (it->state != 2) {')
		fn_builder.writeln('\t\tmap_free((map*)&it->data);')
		fn_builder.writeln('\t}')
	} else {
		fn_builder.writeln('\tmap_free(it);')
	}
	fn_builder.writeln('}')
}

@[inline]
fn styp_to_free_fn_name(styp string) string {
	return styp.replace_each(['*', '', '.', '__', ' ', '__']) + '_free'
}
