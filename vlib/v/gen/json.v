// Copyright (c) 2019-2020 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module gen

import v.table
import v.util
import strings

// TODO replace with comptime code generation.
// TODO remove cJSON dependency.
// OLD: User decode_User(string js) {
// now it's
// User decode_User(cJSON* root) {
// User res;
// res.name = decode_string(js_get(root, "name"));
// res.profile = decode_Profile(js_get(root, "profile"));
// return res;
// }
// Codegen json_decode/encode funcs
fn (mut g Gen) gen_json_for_type(typ table.Type) {
	mut dec := strings.new_builder(100)
	mut enc := strings.new_builder(100)
	sym := g.table.get_type_symbol(typ)
	styp := g.typ(typ)
	if is_js_prim(sym.name) || sym.kind == .enum_ {
		return
	}
	if sym.kind == .array {
		// return
	}
	if sym.name in g.json_types {
		return
	}
	g.json_types << sym.name
	// println('gen_json_for_type($sym.name)')
	// decode_TYPE funcs receive an actual cJSON* object to decode
	// cJSON_Parse(str) call is added by the compiler
	// Code gen decoder
	dec_fn_name := js_dec_name(sym.name)
	// Make sure that this optional type actually exists
	g.register_optional(typ)
	dec_fn_dec := 'Option_$styp ${dec_fn_name}(cJSON* root)'
	dec.writeln('
//Option_$styp ${dec_fn_name}(cJSON* root, $styp* res) {
$dec_fn_dec {
	$styp res;
	if (!root) {
		const char *error_ptr = cJSON_GetErrorPtr();
		if (error_ptr != NULL)	{
			// fprintf(stderr, "Error in decode() for $styp error_ptr=: %%s\\n", error_ptr);
			// printf("\\nbad js=%%s\\n", js.str);
			Option err = v_error(tos2(error_ptr));
			return *(Option_$styp *)&err;
		}
	}
')
	g.json_forward_decls.writeln('$dec_fn_dec;')
	// Code gen encoder
	// encode_TYPE funcs receive an object to encode
	enc_fn_name := js_enc_name(sym.name)
	enc_fn_dec := 'cJSON* ${enc_fn_name}($styp val)'
	g.json_forward_decls.writeln('$enc_fn_dec;\n')
	enc.writeln('
$enc_fn_dec {
\tcJSON *o;')
	if sym.kind == .array {
		// Handle arrays
		value_type := g.table.value_type(typ)
		g.gen_json_for_type(value_type)
		dec.writeln(g.decode_array(value_type))
		enc.writeln(g.encode_array(value_type))
		// enc += g.encode_array(t)
	} else if sym.kind == .map {
		// Handle maps
		m := sym.info as table.Map		
		g.gen_json_for_type(m.key_type)
		g.gen_json_for_type(m.value_type)
		//dec.writeln(g.decode_map(m.key_type, m.value_type))
		enc.writeln(g.encode_map(m.key_type, m.value_type))
	} else {
		enc.writeln('\to = cJSON_CreateObject();')
		// Structs. Range through fields
		if sym.info !is table.Struct {
			verror('json: $sym.name is not struct')
		}
		info := sym.info as table.Struct
		for field in info.fields {
			if field.attrs.contains('skip') {
				continue
			}
			mut name := field.name
			for attr in field.attrs {
				if attr.name == 'json' {
					name = attr.arg
					break
				}
			}
			field_type := g.typ(field.typ)
			if field.attrs.contains('raw') {
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
	// cJSON_delete
	// p.cgen.fns << '$dec return opt_ok(res); \n}'
	dec.writeln('Option_$styp ret;')
	dec.writeln('opt_ok2(&res, (OptionBase*)&ret, sizeof(res));')
	dec.writeln('return ret;\n}')
	enc.writeln('\treturn o;\n}')
	g.definitions.writeln(dec.str())
	g.gowrappers.writeln(enc.str())
}

fn js_enc_name(typ string) string {
	name := 'json__encode_$typ'
	return util.no_dots(name)
}

fn js_dec_name(typ string) string {
	name := 'json__decode_$typ'
	return util.no_dots(name)
}

fn is_js_prim(typ string) bool {
	return typ == 'int' || typ == 'string' || typ == 'bool' || typ == 'f32' || typ == 'f64' ||
		typ == 'i8' || typ == 'i16' || typ == 'i64' || typ == 'u16' || typ == 'u32' || typ == 'u64' ||
		typ == 'byte'
}

fn (mut g Gen) decode_array(value_type table.Type) string {
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

fn (mut g Gen) decode_map(key_type, value_type table.Type) string {
	// TODO
	return ''
}

fn (mut g Gen) encode_map(key_type, value_type table.Type) string {
	styp := g.typ(key_type)

	styp_v := g.typ(value_type)
	fn_name_v := js_enc_name(styp_v)

	zero := g.type_default(value_type)

	keys_tmp := g.new_tmp_var()

	mut key := 'string key = '
	if key_type.is_string() {
		key += '(($styp*)${keys_tmp}.data)[i];'
	} else {
		// g.gen_str_for_type(key_type)
		// key += '${styp}_str((($styp*)${keys_tmp}.data)[i]);'
		verror('json: encode only maps with string keys')
	}

	return '
	o = cJSON_CreateObject();
	array_$styp $keys_tmp = map_keys(&val);
	for (int i = 0; i < ${keys_tmp}.len; ++i) {
		$key
		cJSON_AddItemToObject(o, (char*) key.str, $fn_name_v ( *($styp_v*) map_get(val, key, &($styp_v[]) { $zero } ) ) );
	}
	array_free(&$keys_tmp);
'
}
