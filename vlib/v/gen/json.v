// Copyright (c) 2019-2020 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module gen

import v.table
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
	if sym.name in ['int', 'string', 'bool', 'f32'] {
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
	dec.writeln('
//Option_$styp ${dec_fn_name}(cJSON* root, $styp* res) {
Option_$styp ${dec_fn_name}(cJSON* root) {
  $styp res;
  if (!root) {
    const char *error_ptr = cJSON_GetErrorPtr();
    if (error_ptr != NULL)	{
//      fprintf(stderr, "Error in decode() for $styp error_ptr=: %%s\\n", error_ptr);
//      printf("\\nbad js=%%s\\n", js.str);
		Option err = v_error(tos2(error_ptr));
      return *(Option_$styp *)&err;
    }
  }
')
	// Code gen encoder
	// encode_TYPE funcs receive an object to encode
	enc_fn_name := js_enc_name(sym.name)
	enc.writeln('
cJSON* ${enc_fn_name}($styp val) {
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
					name = attr[5..]
					break
				}
			}
			field_type := g.typ(field.typ)
			enc_name := js_enc_name(field_type)
			if 'raw' in field.attrs {
				dec.writeln(' res . ${c_name(field.name)} = tos2(cJSON_PrintUnformatted(' + 'js_get(root, "$name")));')
			} else {
				// Now generate decoders for all field types in this struct
				// need to do it here so that these functions are generated first
				g.gen_json_for_type(field.typ)
				dec_name := js_dec_name(field_type)
				if is_js_prim(field_type) {
					dec.writeln(' res . ${c_name(field.name)} = $dec_name (js_get(root, "$name"));')
				} else {
					// dec.writeln(' $dec_name (js_get(root, "$name"), & (res . $field.name));')
					dec.writeln('  res . ${c_name(field.name)} = *($field_type*) $dec_name (js_get(root,"$name")).data;')
				}
			}
			enc.writeln('\tcJSON_AddItemToObject(o, "$name", ${enc_name}(val.${c_name(field.name)}));')
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
	return name.replace('.', '__')
}

fn js_dec_name(typ string) string {
	name := 'json__decode_$typ'
	return name.replace('.', '__')
}

fn is_js_prim(typ string) bool {
	return typ == 'int' || typ == 'string' || typ == 'bool' || typ == 'f32' || typ == 'f64' ||
		typ == 'i8' || typ == 'i16' || typ == 'i64' || typ == 'u16' || typ == 'u32' || typ == 'u64'
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
