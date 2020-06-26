// Copyright (c) 2019-2020 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module parser

import strings
import v.ast
import v.scanner
import v.table

fn (mut p Parser) gen_json_for_type(typ table.Type) {
	mut enc_dec := strings.new_builder(100)
	sym := p.table.get_type_symbol(typ)
	if is_js_prim(sym.name) || sym.kind == .enum_{
		return
	}
	if sym.kind == .array {
		// return
	}
	if typ in p.json_types {
		return
	}
	p.json_types << typ
	if !sym.has_method('from_json') {
		enc_dec.str(p.gen_from_json(typ, sym))
	}
	if !sym.has_method('to_json') {
		enc_dec.str(p.gen_to_json(typ, sym))
	}
	p.scanner.codegen(enc_dec.str())
}

fn gen_json_value_cast(f_name string, var_name string, sym table.TypeSymbol) {
	typ_name := p.table.typ_to_str(sym.name)
	if sym.is_numeric() || sym.kind == .enum_ {
		cast_typ := '${var_name}.' + if sym.is_int() { 'as_int()' } else { 'as_float()' }
		exp_typ_kind := if sym.is_int() { token.Kind.int } else { token.Kind.f64 }
		if sym.kind != exp_typ_kind {
			return '$typ_name($cast_typ)'
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
			return '$typ_name(${var_name}.as_string())'
		}
	}
}

fn (mut p Parser) gen_from_json(typ table.Type, sym table.TypeSymbol) string {
	mut enc := strings.new_builder(200)
	pubfn := if p.mod == 'main' { 'fn' } else { 'pub fn' }
	typ_name := p.table.typ_to_str(sym.name)

	enc.writeln('
// autogen JSON from_json $typ_name
$pubfn (mut t $typ_name) from_json(a json.Any) {
	')
	match sym.kind {
		.struct_, .map {
			enc.writeln('   obj := a.as_map()')
			if sym.kind == .struct_ {
				enc.writeln('    for k, v in obj {\n        match k {')
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
					enc.writeln("            '$name' {")
					field_typ := field.typ
					table.value_type(typ)
					enc.writeln("            }")
				}
				enc.writeln("            else {}")
				enc.writeln('        }\n    }')
			}
		}
		.array {
			enc.writeln('   obj := a.as_array()')
		}
		.string, .ustring {
			enc.writeln('   t = a.as_string()')
		}
		.bool {
			enc.writeln('   obj := a.as_bool()')
		}
		else {
			if sym.is_int() || sym.kind == .enum_ {
				enc.write('   t = ')
				if sym.kind != .i32 {
					enc.writeln('$typ_name(a.as_int())')
				} else {
					enc.writeln('a.as_int()')
				}
			} else if sym.is_float() {
				enc.write('   t = ')
				if sym.kind != .f64 {
					enc.writeln('$typ_name(a.as_float())')
				} else {
					enc.writeln('a.as_int()')
				}
			} else {
				// todo
				panic('json: unsupported type $typ_name')
			}
		}
	}
	enc.writeln('}')
	return enc.str()
}

fn (mut p Parser) gen_to_json(typ table.Type, sym table.TypeSymbol) {
		// Code gen encoder
	// encode_TYPE funcs receive an object to encode
	enc_fn_name := js_enc_name(sym.name)
	enc_fn_dec := 'cJSON* ${enc_fn_name}($styp val)'
	g.json_forward_decls.writeln('$enc_fn_dec;\n')
	enc.writeln('
$enc_fn_dec {
\tcJSON *o = cJSON_CreateObject();')
	if sym.kind == .array {
		// Handle arrays
		value_type := g.table.value_type(typ)
		g.gen_json_for_type(value_type)
		dec.writeln(g.decode_array(value_type))
		enc.writeln(g.encode_array(value_type))
		// enc += g.encode_array(t)
	} else {
		// Structs. Range through fields
		if !(sym.info is table.Struct) {
			verror('json: $sym.name is not struct')
		}
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
			field_type := g.typ(field.typ)
			if 'raw' in field.attrs {
				dec.writeln(' res . ${c_name(field.name)} = tos2(cJSON_PrintUnformatted(' + 'js_get(root, "$name")));')
			} else {
				// Now generate decoders for all field types in this struct
				// need to do it here so that these functions are generated first
				g.gen_json_for_type(field.typ)
				dec_name := js_dec_name(field_type)
				if is_js_prim(field_type) {
					dec.writeln(' res . ${c_name(field.name)} = $dec_name (js_get(root, "$name"));')
				} else if g.table.get_type_symbol(field.typ).kind == .enum_ {
					dec.writeln(' res . ${c_name(field.name)} = json__decode_u64(js_get(root, "$name"));')
				} else {
					// dec.writeln(' $dec_name (js_get(root, "$name"), & (res . $field.name));')
					dec.writeln('  res . ${c_name(field.name)} = *($field_type*) $dec_name (js_get(root,"$name")).data;')
				}
			}

			mut enc_name := js_enc_name(field_type)
			if g.table.get_type_symbol(field.typ).kind == .enum_ {
				enc.writeln('\tcJSON_AddItemToObject(o, "$name", json__encode_u64(val.${c_name(field.name)}));')
				
			} else {
				enc.writeln('\tcJSON_AddItemToObject(o, "$name", ${enc_name}(val.${c_name(field.name)}));')
			}
		}
	}
}

fn is_js_prim(typ string) bool {
	return typ in ['int', 'string', 'bool', 'f32', 'f64', 'i8', 'i16', 'i64', 'u16', 'u32', 'u64', 'byte']
}

fn (mut p Parser) decode_array(value_type table.Type) string {
	styp := g.typ(value_type)
	fn_name := js_dec_name(styp)
	// If we have `[]Profile`, have to register a Profile en(de)coder first
	g.gen_json_for_type(value_type)
	mut s := ''
	if is_js_prim(styp) {
		s = '$styp val = ${fn_name}(jsval); '
	} else {
		s = '\t$styp val = *($styp*) ${fn_name}(jsval).data; '
	}
	return '
res = __new_array(0, 0, sizeof($styp));
const cJSON *jsval = NULL;
cJSON_ArrayForEach(jsval, root)
{
$s
  array_push(&res, &val);
}
'
}

fn (mut g Gen) encode_array(value_type table.Type) string {
	styp := g.typ(value_type)
	fn_name := js_enc_name(styp)
	return '
o = cJSON_CreateArray();
for (int i = 0; i < val.len; i++){
  cJSON_AddItemToArray(o, $fn_name (  (($styp*)val.data)[i]  ));
}
'
}
