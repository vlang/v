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
	if sym.name in ['int', 'string', 'bool'] {
		return
	}
	// println('gen_json_for_type($typ.name)')
	// decode_TYPE funcs receive an actual cJSON* object to decode
	// cJSON_Parse(str) call is added by the compiler
	// Code gen decoder
	dec_fn_name := js_dec_name(sym.name)
	dec.writeln('
Option ${dec_fn_name}(cJSON* root, $styp* res) {
//  $styp res;
  if (!root) {
    const char *error_ptr = cJSON_GetErrorPtr();
    if (error_ptr != NULL)	{
      fprintf(stderr, "Error in decode() for $styp error_ptr=: %%s\\n", error_ptr);
//      printf("\\nbad js=%%s\\n", js.str);
      return v_error(tos2(error_ptr));
    }
  }
')
	// Code gen encoder
	// encode_TYPE funcs receive an object to encode
	enc_fn_name := js_enc_name(sym.name)
	enc.writeln('
cJSON* ${enc_fn_name}($styp val) {
\tcJSON *o = cJSON_CreateObject();')
	// Handle arrays
	if sym.kind == .array {
		// dec += p.decode_array(t)
		// enc += p.encode_array(t)
	}
	// Range through fields
	info := sym.info as table.Struct
	for field in info.fields {
		if field.attr == 'skip' {
			continue
		}
		name := if field.attr.starts_with('json:') { field.attr[5..] } else { field.name }
		field_type := g.typ(field.typ)
		enc_name := js_enc_name(field_type)
		if field.attr == 'raw' {
			dec.writeln(' res->$field.name = tos2(cJSON_PrintUnformatted(' + 'js_get(root, "$name")));')
		} else {
			// Now generate decoders for all field types in this struct
			// need to do it here so that these functions are generated first
			g.gen_json_for_type(field.typ)
			dec_name := js_dec_name(field_type)
			if is_js_prim(field_type) {
				dec.writeln(' res->$field.name = $dec_name (js_get(' + 'root, "$name"))')
			} else {
				dec.writeln(' $dec_name (js_get(root, "$name"), & (res->$field.name))')
			}
			dec.writeln(';')
		}
		enc.writeln('\tcJSON_AddItemToObject(o, "$name", ${enc_name}(val.$field.name));')
	}
	// cJSON_delete
	// p.cgen.fns << '$dec return opt_ok(res); \n}'
	dec.writeln('return opt_ok(res, sizeof(*res)); \n}')
	enc.writeln('\treturn o;\n}')
	g.definitions.writeln(dec.str())
	g.gowrappers.writeln(enc.str())
}

fn js_enc_name(typ string) string {
	name := 'json__encode_$typ'
	return name
}

fn js_dec_name(typ string) string {
	name := 'json__decode_$typ'
	return name
}

fn is_js_prim(typ string) bool {
	return typ == 'int' || typ == 'string' || typ == 'bool' || typ == 'f32' || typ == 'f64' ||
		typ == 'i8' || typ == 'i16' || typ == 'i64' || typ == 'u16' || typ == 'u32' || typ == 'u64'
}
