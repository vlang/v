// Copyright (c) 2019-2020 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module parser

import strings
import v.scanner
import v.table

fn (mut p Parser) gen_json_for_type(typ table.Type) {
	if typ in p.json_types {
		return
	}
	typ_str := p.table.type_to_str(typ)
	if is_prim(typ_str.all_after('[]')) || is_js_prim(typ_str.all_after('[]')) {
		return
	}
	sym := p.table.get_type_symbol(typ)
	if sym.kind == .enum_ {
		return
	}
	mut enc_dec := strings.new_builder(100)
	p.json_types << typ
	if !sym.has_method('from_json') {
		enc_dec.writeln(p.gen_from_json(typ, sym))
	}
	if !sym.has_method('to_json') {
		enc_dec.writeln(p.gen_to_json(typ, sym))
	}
	p.scanner.codegen(enc_dec.str())
}

fn (p Parser) gen_json_value_cast(var_name string, typ table.Type) string {
	sym := p.table.get_type_symbol(typ)
	typ_name := p.table.type_to_str(typ)
	if typ.is_number() || sym.kind == .enum_ {
		cast_typ := '${var_name}.' + if sym.is_int() || sym.kind == .enum_ { 'as_int()' } else { 'as_float()' }
		exp_typ_kind := if sym.is_int() { table.Kind.int } else { table.Kind.f64 }
		if sym.kind != exp_typ_kind {
			return '${typ_name}(' + cast_typ + ')'
		}
		return cast_typ
	}

	match sym.kind {
		.map {
			return '${var_name}.as_map()'
		}
		.array {
			return '${var_name}.as_array()'
		}
		.string, .ustring {
			return '${var_name}.as_string()'
		}
		.bool {
			return '${var_name}.as_bool()'
		}
		else {
			return typ_name + '(' + var_name + '.as_string())'
		}
	}
}

fn (mut p Parser) gen_from_json(typ table.Type, sym table.TypeSymbol) string {
	mut dec := strings.new_builder(200)
	pub_fn := if p.mod == 'main' { 'fn' } else { 'pub fn' }
	typ_name := p.table.type_to_str(typ)
	mut defs := strings.new_builder(200)
	defs.writeln('
$pub_fn (mut t $typ_name) from_json(a json.Any) {')
	match sym.kind {
		.struct_ {
			defs.writeln('obj := a.as_map()')
			dec.writeln('final := &$typ_name{')
			info := sym.info as table.Struct
			for field in info.fields {
				if 'skip' in field.attrs {
					continue
				}
				field_name := field.name
				mut name := field_name
				for attr in field.attrs {
					if attr.starts_with('json:') {
						name = attr.all_after('json:')
						break
					}
				}
				// todo arrays
				dec.write("$field_name: ")
				field_typ := field.typ
				field_typ_str := p.table.type_to_str(field_typ)
				field_sym := p.table.get_type_symbol(field_typ)
				if field_sym.kind in [.struct_, .array] {
					if field_sym.kind == .array && (is_prim(field_typ_str.all_after('[]')) || is_js_prim(field_typ_str.all_after('[]'))) {
						value_typ := p.table.value_type(field_typ)
						cast_array_item_typ := p.gen_json_value_cast('it', value_typ)
						dec.writeln('obj["$name"].as_array().map($cast_array_item_typ)')
					} else if field_typ !in p.json_types {
						p.gen_json_for_type(field_typ)
						field_var_name := typ_name.to_lower() +'_'+field_name
						defs.writeln('mut $field_var_name := ${field_typ_str}{}')
						defs.writeln('${field_var_name}.from_json(obj["$name"])')
						dec.writeln('$field_var_name')
					}
				} else if field_sym.kind == .enum_ {
					field_var_name := typ_name.to_lower() +'_'+field_name
					defs.writeln('$field_var_name := obj["$name"].as_int()')
					dec.writeln('${field_typ_str}($field_var_name)')
				} else if 'raw' in field.attrs {
					dec.writeln('obj["$name"].str()')
				} else if (is_prim(field_typ_str) || is_js_prim(field_typ_str)) || field_sym.kind == .enum_ {
					dec.writeln(p.gen_json_value_cast('obj["$name"]', field_typ))
				}
			}
			dec.writeln('}')
			dec.writeln('t = final')
		}
		.map {
			dec.writeln('t = a.as_map()')
		}
		.array {
			dec.writeln('arr := a.as_array()')
			dec.writeln('for v in arr {')
			value_typ := p.table.value_type(typ)
			value_typ_str := p.table.type_to_str(value_typ)
			if is_prim(value_typ_str) {
				dec.write('t << ')
				dec.writeln(p.gen_json_value_cast('v', value_typ))
			} else {
				if value_typ !in p.json_types {
					p.gen_json_for_type(value_typ)
				}
				dec.writeln('mut item := ${value_typ_str}{}')
				dec.writeln('item.from_json(v)')
				dec.writeln('t << item')
			}
			dec.write('}')
		}
		else {
			dec.write('t = ')
			if is_prim(typ_name) || is_js_prim(typ_name) || sym.kind == .enum_ {
				dec.writeln(p.gen_json_value_cast('a', typ))
			} else {
				// todo
				panic('json: unsupported type $typ_name')
			}
		}
	}
	dec.writeln('}')
	op := defs.str() + dec.str()
	return op
}

fn (mut p Parser) gen_to_json(typ table.Type, sym table.TypeSymbol) string {
	mut enc := strings.new_builder(200)
	pub_fn := if p.mod == 'main' { 'fn' } else { 'pub fn' }
	typ_name := p.table.type_to_str(typ)
	enc.writeln('
$pub_fn (t $typ_name) to_json() string {')
	match sym.kind {
		.struct_ {
			enc.writeln('mut obj := map[string]json.Any')
			info := sym.info as table.Struct
			for field in info.fields {
				if 'skip' in field.attrs {
					continue
				}
				mut name := field.name
				for attr in field.attrs {
					if attr.starts_with('json:') {
						name = attr.all_after('json:')
						break
					}
				}
				field_typ := field.typ
				field_name := field.name
				field_sym := p.table.get_type_symbol(field_typ)
				field_typ_str := p.table.type_to_str(field_typ)
				enc.write("obj['$name'] = ")
				if is_js_prim(field_typ_str) {
					enc.writeln('t.${field_name}')
				} else if is_prim(field_typ_str) || field_sym.kind == .enum_ {
					if field_sym.is_int() || field_sym.kind == .enum_ { 
						enc.writeln('int(t.${field_name})')
					} else { 
						enc.writeln('f64(t.${field_name})')
					}
				} else if is_js_prim(field_typ_str.all_after('[]')) || is_prim(field_typ_str.all_after('[]')) {
					enc.writeln('t.${field_name}.map(json.Any(it))') 
				} else {
					if field_typ !in p.json_types {
						p.gen_json_for_type(field_typ)
					}
					enc.writeln("t.${field_name}.to_json()")
				}
			}
		}
		.array {
			value_typ := p.table.value_type(typ)
			value_typ_str := p.table.type_to_str(value_typ)
			value_sym := p.table.get_type_symbol(value_typ)
			enc.write('obj := t.map(')
			if is_js_prim(value_typ_str) {
				enc.write('json.Any(it)')
			} else if is_prim(value_typ_str) {
				enc.write('json.Any(')
				if value_sym.is_int() { 
					enc.write('it')
				} else if value_sym.kind == .enum_ {
					enc.write('int(it)')
				} else { 
					enc.write('f64(it)')
				}
				enc.write(')')
			} else {
				if value_typ !in p.json_types {
					p.gen_json_for_type(value_typ)
				}
				enc.write("it.to_json()")
			}
			enc.writeln(')')
		}
		else {
			enc.writeln('obj := json.Any(t)')
		}
	}
	enc.writeln('return obj.str() \n}')
	op := enc.str()
	println(op)
	return op
}

fn is_prim(typ string) bool {
	return typ in ['f32', 'i8', 'i16', 'i64', 'u16', 'u32', 'u64', 'byte']
}

fn is_js_prim(typ string) bool {
	return typ in ['string', 'int', 'f64', 'any_int', 'any_float', 'bool', 'Null', '[]Any', 'map[string]Any']
}