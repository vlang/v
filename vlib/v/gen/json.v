// Copyright (c) 2019-2021 Alexander Medvednikov. All rights reserved.
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
	utyp := g.unwrap_generic(typ)
	mut dec := strings.new_builder(100)
	mut enc := strings.new_builder(100)
	sym := g.table.get_type_symbol(utyp)
	styp := g.typ(utyp)
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
	dec_fn_name := js_dec_name(styp)
	// Make sure that this optional type actually exists
	g.register_optional(utyp)
	dec_fn_dec := 'Option_$styp ${dec_fn_name}(cJSON* root)'
	dec.writeln('
$dec_fn_dec {
	$styp res;
	if (!root) {
		const char *error_ptr = cJSON_GetErrorPtr();
		if (error_ptr != NULL)	{
			// fprintf(stderr, "Error in decode() for $styp error_ptr=: %s\\n", error_ptr);
			// printf("\\nbad js=%%s\\n", js.str);
			Option err = v_error(tos2(error_ptr));
			return *(Option_$styp *)&err;
		}
	}
')
	g.json_forward_decls.writeln('$dec_fn_dec;')
	// Code gen encoder
	// encode_TYPE funcs receive an object to encode
	enc_fn_name := js_enc_name(styp)
	enc_fn_dec := 'cJSON* ${enc_fn_name}($styp val)'
	g.json_forward_decls.writeln('$enc_fn_dec;\n')
	enc.writeln('
$enc_fn_dec {
\tcJSON *o;')
	if sym.kind == .array {
		// Handle arrays
		value_type := g.table.value_type(utyp)
		// If we have `[]Profile`, have to register a Profile en(de)coder first
		g.gen_json_for_type(value_type)
		dec.writeln(g.decode_array(value_type))
		enc.writeln(g.encode_array(value_type))
		// enc += g.encode_array(t)
	} else if sym.kind == .map {
		// Handle maps
		m := sym.info as table.Map
		g.gen_json_for_type(m.key_type)
		g.gen_json_for_type(m.value_type)
		dec.writeln(g.decode_map(m.key_type, m.value_type))
		enc.writeln(g.encode_map(m.key_type, m.value_type))
	} else if sym.kind == .alias {
		a := sym.info as table.Alias
		parent_typ := a.parent_type
		psym := g.table.get_type_symbol(parent_typ)
		if is_js_prim(g.typ(parent_typ)) {
			g.gen_json_for_type(parent_typ)
			return
		}
		enc.writeln('\to = cJSON_CreateObject();')
		if psym.info !is table.Struct {
			verror('json: $sym.name is not struct')
		}
		g.gen_struct_enc_dec(psym.info, styp, mut enc, mut dec)
	} else {
		enc.writeln('\to = cJSON_CreateObject();')
		// Structs. Range through fields
		if sym.info !is table.Struct {
			verror('json: $sym.name is not struct')
		}
		g.gen_struct_enc_dec(sym.info, styp, mut enc, mut dec)
	}
	// cJSON_delete
	// p.cgen.fns << '$dec return opt_ok(res); \n}'
	dec.writeln('\tOption_$styp ret;')
	dec.writeln('\topt_ok2(&res, (OptionBase*)&ret, sizeof(res));')
	dec.writeln('\treturn ret;\n}')
	enc.writeln('\treturn o;\n}')
	g.definitions.writeln(dec.str())
	g.gowrappers.writeln(enc.str())
}

[inline]
fn (mut g Gen) gen_struct_enc_dec(type_info table.TypeInfo, styp string, mut enc strings.Builder, mut dec strings.Builder) {
	info := type_info as table.Struct
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
		field_sym := g.table.get_type_symbol(field.typ)
		// First generate decoding
		if field.attrs.contains('raw') {
			dec.writeln('\tres.${c_name(field.name)} = tos4(cJSON_PrintUnformatted(' + 'js_get(root, "$name")));')
		} else {
			// Now generate decoders for all field types in this struct
			// need to do it here so that these functions are generated first
			g.gen_json_for_type(field.typ)
			dec_name := js_dec_name(field_type)
			if is_js_prim(field_type) {
				dec.writeln('\tres.${c_name(field.name)} = $dec_name (js_get(root, "$name"));')
			} else if field_sym.kind == .enum_ {
				dec.writeln('\tres.${c_name(field.name)} = json__decode_u64(js_get(root, "$name"));')
			} else if field_sym.name == 'time.Time' {
				// time struct requires special treatment
				// it has to be decoded from a unix timestamp number
				dec.writeln('\tres.${c_name(field.name)} = time__unix(json__decode_u64(js_get(root, "$name")));')
			} else if field_sym.kind == .alias {
				alias := field_sym.info as table.Alias
				parent_type := g.typ(alias.parent_type)
				parent_dec_name := js_dec_name(parent_type)
				if is_js_prim(parent_type) {
					dec.writeln('\tres.${c_name(field.name)} = $parent_dec_name (js_get(root, "$name"));')
				} else {
					g.gen_json_for_type(field.typ)
					tmp := g.new_tmp_var()
					dec.writeln('\tOption_$field_type $tmp = $dec_name (js_get(root,"$name"));')
					dec.writeln('\tif(!${tmp}.ok) {')
					dec.writeln('\t\treturn *(Option_$styp*) &$tmp;')
					dec.writeln('\t}')
					dec.writeln('\tres.${c_name(field.name)} = *($field_type*) ${tmp}.data;')
				}
			} else {
				// dec.writeln(' $dec_name (js_get(root, "$name"), & (res . $field.name));')
				tmp := g.new_tmp_var()
				dec.writeln('\tOption_$field_type $tmp = $dec_name (js_get(root,"$name"));')
				dec.writeln('\tif(!${tmp}.ok) {')
				dec.writeln('\t\treturn *(Option_$styp*) &$tmp;')
				dec.writeln('\t}')
				dec.writeln('\tres.${c_name(field.name)} = *($field_type*) ${tmp}.data;')
			}
		}
		// Encoding
		mut enc_name := js_enc_name(field_type)
		if !is_js_prim(field_type) {
			if field_sym.kind == .alias {
				ainfo := field_sym.info as table.Alias
				enc_name = js_enc_name(g.typ(ainfo.parent_type))
			}
		}
		if field_sym.kind == .enum_ {
			enc.writeln('\tcJSON_AddItemToObject(o, "$name", json__encode_u64(val.${c_name(field.name)}));')
		} else {
			if field_sym.name == 'time.Time' {
				// time struct requires special treatment
				// it has to be encoded as a unix timestamp number
				enc.writeln('\tcJSON_AddItemToObject(o, "$name", json__encode_u64(val.${c_name(field.name)}.v_unix));')
			} else {
				enc.writeln('\tcJSON_AddItemToObject(o, "$name", ${enc_name}(val.${c_name(field.name)}));')
			}
		}
	}
}

fn js_enc_name(typ string) string {
	suffix := if typ.ends_with('*') { typ.replace('*', '') } else { typ }
	name := 'json__encode_$suffix'
	return util.no_dots(name)
}

fn js_dec_name(typ string) string {
	name := 'json__decode_$typ'
	return util.no_dots(name)
}

fn is_js_prim(typ string) bool {
	return typ in
		['int', 'string', 'bool', 'f32', 'f64', 'i8', 'i16', 'i64', 'u16', 'u32', 'u64', 'byte']
}

fn (mut g Gen) decode_array(value_type table.Type) string {
	styp := g.typ(value_type)
	fn_name := js_dec_name(styp)
	mut s := ''
	if is_js_prim(styp) {
		s = '$styp val = ${fn_name}(jsval); '
	} else {
		s = '
		Option_$styp val2 = $fn_name (jsval);
		if(!val2.ok) {
			array_free(&res);
			return *(Option_array_$styp*)&val2;
		}
		$styp val = *($styp*)val2.data;
'
	}
	return '
	if(root && !cJSON_IsArray(root) && !cJSON_IsNull(root)) {
		Option err = v_error( string_add(_SLIT("Json element is not an array: "), tos2(cJSON_PrintUnformatted(root))) );
		return *(Option_array_$styp *)&err;
	}
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

fn (mut g Gen) decode_map(key_type table.Type, value_type table.Type) string {
	styp := g.typ(key_type)
	styp_v := g.typ(value_type)
	key_type_symbol := g.table.get_type_symbol(key_type)
	hash_fn, key_eq_fn, clone_fn, free_fn := g.map_fn_ptrs(key_type_symbol)
	fn_name_v := js_dec_name(styp_v)
	mut s := ''
	if is_js_prim(styp_v) {
		s = '$styp_v val = $fn_name_v (js_get(root, jsval->string));'
	} else {
		s = '
		Option_$styp_v val2 = $fn_name_v (js_get(root, jsval->string));
		if(!val2.ok) {
			map_free(&res);
			return *(Option_map_${styp}_$styp_v*)&val2;
		}
		$styp_v val = *($styp_v*)val2.data;
'
	}
	return '
	if(!cJSON_IsObject(root) && !cJSON_IsNull(root)) {
		Option err = v_error( string_add(_SLIT("Json element is not an object: "), tos2(cJSON_PrintUnformatted(root))) );
		return *(Option_map_${styp}_$styp_v *)&err;
	}
	res = new_map_2(sizeof($styp), sizeof($styp_v), $hash_fn, $key_eq_fn, $clone_fn, $free_fn);
	cJSON *jsval = NULL;
	cJSON_ArrayForEach(jsval, root)
	{
		$s
		string key = tos2( (byteptr) jsval->string );
		map_set_1(&res, &key, &val);
	}
'
}

fn (mut g Gen) encode_map(key_type table.Type, value_type table.Type) string {
	styp := g.typ(key_type)
	styp_v := g.typ(value_type)
	fn_name_v := js_enc_name(styp_v)
	zero := g.type_default(value_type)
	keys_tmp := g.new_tmp_var()
	mut key := 'string key = '
	if key_type.is_string() {
		key += '(($styp*)${keys_tmp}.data)[i];'
	} else {
		// key += '${styp}_str((($styp*)${keys_tmp}.data)[i]);'
		verror('json: encode only maps with string keys')
	}
	return '
	o = cJSON_CreateObject();
	array_$styp $keys_tmp = map_keys(&val);
	for (int i = 0; i < ${keys_tmp}.len; ++i) {
		$key
		cJSON_AddItemToObject(o, (char*) key.str, $fn_name_v ( *($styp_v*) map_get_1(&val, &key, &($styp_v[]) { $zero } ) ) );
	}
	array_free(&$keys_tmp);
'
}
