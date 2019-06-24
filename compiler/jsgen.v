// Copyright (c) 2019 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.

module main

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
fn (p mut Parser) gen_json_for_type(typ Type) {
	mut dec := ''
	mut enc := ''
	t := typ.name
	if t == 'int' || t == 'string' || t == 'bool' {
		return
	}
	if p.first_run() {
		return
	}
	// println('gen_json_for_type( $typ.name )')
	// Register decoder fn
	mut dec_fn := Fn {
		pkg: p.pkg
		typ: 'Option_$typ.name'
		name: js_dec_name(t)
	}
	// Already registered? Skip.
	if p.table.known_fn(dec_fn.name) {
		return
	}
	// decode_TYPE funcs receive an actual cJSON* object to decode
	// cJSON_Parse(str) call is added by the compiler
	arg := Var {
		typ: 'cJSON*'
	}
	dec_fn.args << arg
	p.table.register_fn(dec_fn)
	// Register encoder fn
	mut enc_fn := Fn {
		pkg: p.pkg
		typ: 'cJSON*'
		name: js_enc_name(t)
	}
	// encode_TYPE funcs receive an object to encode
	enc_arg := Var {
		typ: t
	}
	enc_fn.args << enc_arg
	p.table.register_fn(enc_fn)
	// Code gen decoder
	dec += '
//$t $dec_fn.name(cJSON* root) {  
Option $dec_fn.name(cJSON* root, $t* res) {  
//  $t res; 
  if (!root) {
    const char *error_ptr = cJSON_GetErrorPtr();
    if (error_ptr != NULL)	{
      fprintf(stderr, "Error in decode() for $t error_ptr=: %%s\\n", error_ptr);
//      printf("\\nbad js=%%s\\n", js.str); 
      return v_error(tos2(error_ptr));
    }
  }
'
	// Code gen encoder
	enc += '
cJSON* $enc_fn.name($t val) {  
cJSON *o = cJSON_CreateObject();
string res = tos2(""); 
'
	// Handle arrays
	if t.starts_with('array_') {
		dec += p.decode_array(t)
		enc += p.encode_array(t)
	}
	// Range through fields
	for field in typ.fields {
		if field.attr == 'skip' {
			continue
		}
		field_type := p.table.find_type(field.typ)
		// Now generate decoders for all field types in this struct
		// need to do it here so that these functions are generated first
		p.gen_json_for_type(field_type)
		name := field.name
		_typ := field.typ.replace('*', '')
		enc_name := js_enc_name(_typ)
		dec_name := js_dec_name(_typ)
		if is_js_prim(_typ) {
			dec += ' /*prim*/ res->$name = $dec_name(js_get(root, "$field.name"))'
			// dec += '.data'
		}
		else {
			dec += ' /*!!*/ $dec_name(js_get(root, "$field.name"), & (res->$name))'
		}
		dec += ';\n'
		enc += '  cJSON_AddItemToObject(o,  "$name", $enc_name(val.$name)); \n'
	}
	// cJSON_delete
	p.cgen.fns << '$dec return opt_ok(res); \n}'
	p.cgen.fns << '/*enc start*/ $enc return o;}'
}

fn is_js_prim(typ string) bool {
	return typ == 'int' || typ == 'string' ||
	typ == 'bool' || typ == 'float' || typ == 'f32' || typ == 'f64' ||
	typ == 'i8' || typ == 'i16' || typ == 'i32' || typ == 'i64'
}

fn (p mut Parser) decode_array(typ string) string {
	typ = typ.replace('array_', '')
	t := p.table.find_type(typ)
	fn_name := js_dec_name(typ)
	// If we have `[]Profile`, have to register a Profile en(de)coder first
	p.gen_json_for_type(t)
	mut s := ''
	if is_js_prim(typ) {
		s = '$typ val= $fn_name(jsval); '
	}
	else {
		s = '  $typ val; $fn_name(jsval, &val); '
	}
	return '
*res = new_array(0, 0, sizeof($typ));
const cJSON *jsval = NULL;
cJSON_ArrayForEach(jsval, root)
{
$s 
  array__push(res, &val);
}
'
}

fn js_enc_name(typ string) string {
	name := 'json__jsencode_$typ'
	return name
}

fn js_dec_name(typ string) string {
	name := 'json__jsdecode_$typ'
	return name
}

fn (p &Parser) encode_array(typ string) string {
	typ = typ.replace('array_', '')
	fn_name := js_enc_name(typ)
	return '
o = cJSON_CreateArray();
for (int i = 0; i < val.len; i++){
  cJSON_AddItemToArray(o, $fn_name(  (($typ*)val.data)[i]  ));
} 
'
}

