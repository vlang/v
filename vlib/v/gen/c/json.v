// Copyright (c) 2019-2022 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module c

import v.ast
import v.util
import strings

// TODO: replace with comptime code generation.
// TODO: remove cJSON dependency.

// Old:
// `User decode_User(string js) {`
// now it's:
// ```
// User decode_User(cJSON* root) {
//     User res;
//     res.name = decode_string(js_get(root, "name"));
//     res.profile = decode_Profile(js_get(root, "profile"));
//     return res;
// }
// ```

// Codegen json_decode/encode funcs
fn (mut g Gen) gen_json_for_type(typ ast.Type) {
	utyp := g.unwrap_generic(typ).set_nr_muls(0)
	sym := g.table.sym(utyp)
	if is_js_prim(sym.name) || sym.kind == .enum_ {
		return
	}
	g.json_types << utyp
}

fn (mut g Gen) gen_jsons() {
	mut done := []ast.Type{}
	for i := 0; i < g.json_types.len; i++ {
		utyp := g.json_types[i]
		if utyp in done {
			continue
		}
		done << utyp
		mut dec := strings.new_builder(100)
		mut enc := strings.new_builder(100)
		sym := g.table.sym(utyp)
		styp := g.typ(utyp)
		g.register_result(utyp)
		// decode_TYPE funcs receive an actual cJSON* object to decode
		// cJSON_Parse(str) call is added by the compiler
		// Codegen decoder
		dec_fn_name := js_dec_name(styp)
		dec_fn_dec := '${result_name}_$styp ${dec_fn_name}(cJSON* root)'

		mut init_styp := '$styp res'
		if sym.kind == .struct_ {
			init_styp += ' = '
			init_styp += g.expr_string(ast.Expr(ast.StructInit{
				typ: utyp
				typ_str: styp
			}))
		}

		dec.writeln('
$dec_fn_dec {
	$init_styp;
	if (!root) {
		const char *error_ptr = cJSON_GetErrorPtr();
		if (error_ptr != NULL)	{
			const int error_pos = (int)cJSON_GetErrorPos();
			int maxcontext_chars = 30;
			byte *buf = vcalloc_noscan(maxcontext_chars + 10);
			if(error_pos > 0) {
				int backlines = 1;
				int backchars = error_pos < maxcontext_chars-7 ? (int)error_pos : maxcontext_chars-7 ;
				char *prevline_ptr = (char*)error_ptr;
				while(backchars--){
					char prevc = *(prevline_ptr - 1);
					if(0==prevc){
						break;
					}
					if(10==prevc && !backlines--){
						break;
					}
					prevline_ptr--;
					if(123==prevc) {
						break; // stop at `{` too
					}
				}
				int maxchars = vstrlen_char(prevline_ptr);
				vmemcpy(buf, prevline_ptr, (maxchars < maxcontext_chars ? maxchars : maxcontext_chars));
			}
			return (${result_name}_$styp){.is_error = true,.err = _v_error(tos2(buf)),.data = {0}};
		}
	}
')
		g.json_forward_decls.writeln('$dec_fn_dec;')
		// Codegen encoder
		// encode_TYPE funcs receive an object to encode
		enc_fn_name := js_enc_name(styp)
		enc_fn_dec := 'cJSON* ${enc_fn_name}($styp val)'
		g.json_forward_decls.writeln('$enc_fn_dec;\n')
		enc.writeln('
$enc_fn_dec {
\tcJSON *o;')
		if sym.kind == .array || sym.kind == .array_fixed {
			array_size := if sym.kind == .array_fixed {
				(sym.info as ast.ArrayFixed).size
			} else {
				-1
			}
			// Handle arrays
			value_type := g.table.value_type(utyp)
			// If we have `[]Profile`, have to register a Profile en(de)coder first
			g.gen_json_for_type(value_type)
			dec.writeln(g.decode_array(value_type, array_size))
			enc.writeln(g.encode_array(value_type, array_size))
		} else if sym.kind == .map {
			// Handle maps
			m := sym.info as ast.Map
			g.gen_json_for_type(m.key_type)
			g.gen_json_for_type(m.value_type)
			dec.writeln(g.decode_map(m.key_type, m.value_type))
			enc.writeln(g.encode_map(m.key_type, m.value_type))
		} else if sym.kind == .alias {
			a := sym.info as ast.Alias
			parent_typ := a.parent_type
			psym := g.table.sym(parent_typ)
			if is_js_prim(g.typ(parent_typ)) {
				g.gen_json_for_type(parent_typ)
				continue
			}
			enc.writeln('\to = cJSON_CreateObject();')
			if psym.info is ast.Struct {
				g.gen_struct_enc_dec(psym.info, styp, mut enc, mut dec)
			} else if psym.kind == .sum_type {
				verror('json: $sym.name aliased sumtypes does not work at the moment')
			} else {
				verror('json: $sym.name is not struct')
			}
		} else if sym.kind == .sum_type {
			enc.writeln('\to = cJSON_CreateObject();')
			// Sumtypes. Range through variants of sumtype
			if sym.info !is ast.SumType {
				verror('json: $sym.name is not a sumtype')
			}
			g.gen_sumtype_enc_dec(sym, mut enc, mut dec)
		} else {
			enc.writeln('\to = cJSON_CreateObject();')
			// Structs. Range through fields
			if sym.info !is ast.Struct {
				verror('json: $sym.name is not struct')
			}
			g.gen_struct_enc_dec(sym.info, styp, mut enc, mut dec)
		}
		// cJSON_delete
		dec.writeln('\t${result_name}_$styp ret;')
		dec.writeln('\t_result_ok(&res, ($result_name*)&ret, sizeof(res));')
		dec.writeln('\treturn ret;\n}')
		enc.writeln('\treturn o;\n}')
		g.gowrappers.writeln(dec.str())
		g.gowrappers.writeln(enc.str())
	}
}

[inline]
fn (mut g Gen) gen_sumtype_enc_dec(sym ast.TypeSymbol, mut enc strings.Builder, mut dec strings.Builder) {
	info := sym.info as ast.SumType
	type_var := g.new_tmp_var()
	typ := g.table.type_idxs[sym.name]

	// DECODING (inline)
	$if !json_no_inline_sumtypes ? {
		type_tmp := g.new_tmp_var()
		dec.writeln('\tif (cJSON_IsObject(root)) {')
		dec.writeln('\t\tcJSON* $type_tmp = js_get(root, "_type");')
		dec.writeln('\t\tif ($type_tmp != 0) {')
		dec.writeln('\t\t\tchar* $type_var = cJSON_GetStringValue($type_tmp);')
		// dec.writeln('\t\t\tcJSON_DeleteItemFromObjectCaseSensitive(root, "_type");')
	}

	mut variant_types := []string{}
	mut variant_symbols := []ast.TypeSymbol{}
	mut at_least_one_prim := false
	for variant in info.variants {
		variant_typ := g.typ(variant)
		variant_types << variant_typ
		variant_sym := g.table.sym(variant)
		variant_symbols << variant_sym
		at_least_one_prim = at_least_one_prim || is_js_prim(variant_typ)
			|| variant_sym.kind == .enum_ || variant_sym.name == 'time.Time'
		unmangled_variant_name := variant_sym.name.split('.').last()

		// TODO: Do not generate dec/enc for 'time.Time', because we handle it by saving it as u64
		g.gen_json_for_type(variant)

		// Helpers for decoding
		g.get_sumtype_casting_fn(variant, typ)
		g.definitions.writeln('static inline $sym.cname ${variant_typ}_to_sumtype_${sym.cname}($variant_typ* x);')

		// ENCODING
		enc.writeln('\tif (val._typ == $variant.idx()) {')
		$if json_no_inline_sumtypes ? {
			if variant_sym.kind == .enum_ {
				enc.writeln('\t\tcJSON_AddItemToObject(o, "$unmangled_variant_name", ${js_enc_name('u64')}(*val._$variant_typ));')
			} else if variant_sym.name == 'time.Time' {
				enc.writeln('\t\tcJSON_AddItemToObject(o, "$unmangled_variant_name", ${js_enc_name('i64')}(val._$variant_typ->_v_unix));')
			} else {
				enc.writeln('\t\tcJSON_AddItemToObject(o, "$unmangled_variant_name", ${js_enc_name(variant_typ)}(*val._$variant_typ));')
			}
		} $else {
			if is_js_prim(variant_typ) {
				enc.writeln('\t\to = ${js_enc_name(variant_typ)}(*val._$variant_typ);')
			} else if variant_sym.kind == .enum_ {
				enc.writeln('\t\to = ${js_enc_name('u64')}(*val._$variant_typ);')
			} else if variant_sym.name == 'time.Time' {
				enc.writeln('\t\tcJSON_AddItemToObject(o, "_type", cJSON_CreateString("$unmangled_variant_name"));')
				enc.writeln('\t\tcJSON_AddItemToObject(o, "value", ${js_enc_name('i64')}(val._$variant_typ->_v_unix));')
			} else {
				enc.writeln('\t\to = ${js_enc_name(variant_typ)}(*val._$variant_typ);')
				enc.writeln('\t\tcJSON_AddItemToObject(o, "_type", cJSON_CreateString("$unmangled_variant_name"));')
			}
		}
		enc.writeln('\t}')

		// DECODING
		tmp := g.new_tmp_var()
		$if json_no_inline_sumtypes ? {
			dec.writeln('\tif (strcmp("$unmangled_variant_name", root->child->string) == 0) {')
			if is_js_prim(variant_typ) {
				gen_js_get(variant_typ, tmp, unmangled_variant_name, mut dec, true)
				dec.writeln('\t\t$variant_typ value = ${js_dec_name(variant_typ)}(jsonroot_$tmp);')
			} else if variant_sym.kind == .enum_ {
				gen_js_get(variant_typ, tmp, unmangled_variant_name, mut dec, true)
				dec.writeln('\t\t$variant_typ value = ${js_dec_name('u64')}(jsonroot_$tmp);')
			} else if variant_sym.name == 'time.Time' {
				gen_js_get(variant_typ, tmp, unmangled_variant_name, mut dec, true)
				dec.writeln('\t\t$variant_typ value = time__unix(${js_dec_name('i64')}(jsonroot_$tmp));')
			} else {
				gen_js_get_opt(js_dec_name(variant_typ), variant_typ, sym.cname, tmp,
					unmangled_variant_name, mut dec, true)
				dec.writeln('\t\t$variant_typ value = *($variant_typ*)(${tmp}.data);')
			}
			dec.writeln('\t\tres = ${variant_typ}_to_sumtype_${sym.cname}(&value);')
			dec.writeln('\t}')
		} $else {
			if variant_sym.name == 'time.Time' {
				dec.writeln('\t\t\tif (strcmp("Time", $type_var) == 0) {')
				gen_js_get(sym.cname, tmp, 'value', mut dec, true)
				dec.writeln('\t\t\t\t$variant_typ $tmp = time__unix(${js_dec_name('i64')}(jsonroot_$tmp));')
				dec.writeln('\t\t\t\tres = ${variant_typ}_to_sumtype_${sym.cname}(&$tmp);')
				dec.writeln('\t\t\t}')
			} else if !is_js_prim(variant_typ) && variant_sym.kind != .enum_ {
				dec.writeln('\t\t\tif (strcmp("$unmangled_variant_name", $type_var) == 0) {')
				dec.writeln('\t\t\t\t${result_name}_$variant_typ $tmp = ${js_dec_name(variant_typ)}(root);')
				dec.writeln('\t\t\t\tif (${tmp}.is_error) {')
				dec.writeln('\t\t\t\t\treturn (${result_name}_$sym.cname){ .is_error = true, .err = ${tmp}.err, .data = {0} };')
				dec.writeln('\t\t\t\t}')
				dec.writeln('\t\t\t\tres = ${variant_typ}_to_sumtype_${sym.cname}(($variant_typ*)${tmp}.data);')
				dec.writeln('\t\t\t}')
			}
		}
	}

	// DECODING (inline)
	$if !json_no_inline_sumtypes ? {
		dec.writeln('\t\t}')

		mut number_is_met := false
		mut string_is_met := false
		mut last_number_type := ''

		if at_least_one_prim {
			dec.writeln('\t} else {')

			if 'bool' in variant_types {
				var_t := 'bool'
				dec.writeln('\t\tif (cJSON_IsBool(root)) {')
				dec.writeln('\t\t\t$var_t value = ${js_dec_name(var_t)}(root);')
				dec.writeln('\t\t\tres = ${var_t}_to_sumtype_${sym.cname}(&value);')
				dec.writeln('\t\t}')
			}

			for i, var_t in variant_types {
				if variant_symbols[i].kind == .enum_ {
					if number_is_met {
						var_num := var_t.replace('__', '.')
						last_num := last_number_type.replace('__', '.')
						verror('json: can not decode `$sym.name` sumtype, too many numeric types (conflict of `$last_num` and `$var_num`), you can try to use alias for `$var_num` or compile v with `json_no_inline_sumtypes` flag')
					}
					number_is_met = true
					last_number_type = var_t
					dec.writeln('\t\tif (cJSON_IsNumber(root)) {')
					dec.writeln('\t\t\t$var_t value = ${js_dec_name('u64')}(root);')
					dec.writeln('\t\t\tres = ${var_t}_to_sumtype_${sym.cname}(&value);')
					dec.writeln('\t\t}')
				}

				if var_t in ['string', 'rune'] {
					if string_is_met {
						var_num := var_t.replace('__', '.')
						verror('json: can not decode `$sym.name` sumtype, too many string types (conflict of `string` and `rune`), you can try to use alias for `$var_num` or compile v with `json_no_inline_sumtypes` flag')
					}
					string_is_met = true
					dec.writeln('\t\tif (cJSON_IsString(root)) {')
					dec.writeln('\t\t\t$var_t value = ${js_dec_name(var_t)}(root);')
					dec.writeln('\t\t\tres = ${var_t}_to_sumtype_${sym.cname}(&value);')
					dec.writeln('\t\t}')
				}

				if var_t.starts_with('Array_') {
					tmp := g.new_tmp_var()
					judge_elem_typ := if var_t.ends_with('string') {
						'cJSON_IsString(root->child)'
					} else if var_t.ends_with('bool') {
						'cJSON_IsBool(root->child)'
					} else {
						'cJSON_IsNumber(root->child)'
					}
					dec.writeln('\t\tif (cJSON_IsArray(root) && $judge_elem_typ) {')
					dec.writeln('\t\t\t${result_name}_$var_t $tmp = ${js_dec_name(var_t)}(root);')
					dec.writeln('\t\t\tif (${tmp}.is_error) {')
					dec.writeln('\t\t\t\treturn (${result_name}_$sym.cname){ .is_error = true, .err = ${tmp}.err, .data = {0} };')
					dec.writeln('\t\t\t}')
					dec.writeln('\t\t\tres = ${var_t}_to_sumtype_${sym.cname}(($var_t*)${tmp}.data);')
					dec.writeln('\t\t}')
				}

				if var_t in ['i64', 'int', 'i8', 'u64', 'u32', 'u16', 'byte', 'u8', 'rune', 'f64',
					'f32'] {
					if number_is_met {
						var_num := var_t.replace('__', '.')
						last_num := last_number_type.replace('__', '.')
						verror('json: can not decode `$sym.name` sumtype, too many numeric types (conflict of `$last_num` and `$var_num`), you can try to use alias for `$var_num` or compile v with `json_no_inline_sumtypes` flag')
					}
					number_is_met = true
					last_number_type = var_t
					dec.writeln('\t\tif (cJSON_IsNumber(root)) {')
					dec.writeln('\t\t\t$var_t value = ${js_dec_name(var_t)}(root);')
					dec.writeln('\t\t\tres = ${var_t}_to_sumtype_${sym.cname}(&value);')
					dec.writeln('\t\t}')
				}
			}
		}
		dec.writeln('\t}')
	}
}

[inline]
fn (mut g Gen) gen_struct_enc_dec(type_info ast.TypeInfo, styp string, mut enc strings.Builder, mut dec strings.Builder) {
	info := type_info as ast.Struct
	for field in info.fields {
		mut name := field.name
		mut is_raw := false
		mut is_skip := false
		mut is_required := false
		mut is_omit_empty := false

		for attr in field.attrs {
			match attr.name {
				'json' {
					name = attr.arg
				}
				'skip' {
					is_skip = true
				}
				'raw' {
					is_raw = true
				}
				'required' {
					is_required = true
				}
				'omitempty' {
					is_omit_empty = true
				}
				else {}
			}
		}
		if is_skip {
			continue
		}
		field_type := g.typ(field.typ)
		field_sym := g.table.sym(field.typ)
		// First generate decoding
		if is_raw {
			dec.writeln('\tres.${c_name(field.name)} = tos5(cJSON_PrintUnformatted(' +
				'js_get(root, "$name")));')
		} else {
			// Now generate decoders for all field types in this struct
			// need to do it here so that these functions are generated first
			g.gen_json_for_type(field.typ)
			dec_name := js_dec_name(field_type)
			if is_js_prim(field_type) {
				tmp := g.new_tmp_var()
				gen_js_get(styp, tmp, name, mut dec, is_required)
				dec.writeln('\tif (jsonroot_$tmp) {')
				dec.writeln('\t\tres.${c_name(field.name)} = ${dec_name}(jsonroot_$tmp);')
				if field.has_default_expr {
					dec.writeln('\t} else {')
					dec.writeln('\t\tres.${c_name(field.name)} = ${g.expr_string(field.default_expr)};')
				}
				dec.writeln('\t}')
			} else if field_sym.kind == .enum_ {
				tmp := g.new_tmp_var()
				gen_js_get(styp, tmp, name, mut dec, is_required)
				dec.writeln('\tif (jsonroot_$tmp) {')
				dec.writeln('\t\tres.${c_name(field.name)} = json__decode_u64(jsonroot_$tmp);')
				if field.has_default_expr {
					dec.writeln('\t} else {')
					dec.writeln('\t\tres.${c_name(field.name)} = ${g.expr_string(field.default_expr)};')
				}
				dec.writeln('\t}')
			} else if field_sym.name == 'time.Time' {
				// time struct requires special treatment
				// it has to be decoded from a unix timestamp number
				tmp := g.new_tmp_var()
				gen_js_get(styp, tmp, name, mut dec, is_required)
				dec.writeln('\tif (jsonroot_$tmp) {')
				dec.writeln('\t\tres.${c_name(field.name)} = time__unix(json__decode_u64(jsonroot_$tmp));')
				if field.has_default_expr {
					dec.writeln('\t} else {')
					dec.writeln('\t\tres.${c_name(field.name)} = ${g.expr_string(field.default_expr)};')
				}
				dec.writeln('\t}')
			} else if field_sym.kind == .alias {
				alias := field_sym.info as ast.Alias
				parent_type := g.typ(alias.parent_type)
				parent_dec_name := js_dec_name(parent_type)
				if is_js_prim(parent_type) {
					tmp := g.new_tmp_var()
					gen_js_get(styp, tmp, name, mut dec, is_required)
					dec.writeln('\tif (jsonroot_$tmp) {')
					dec.writeln('\t\tres.${c_name(field.name)} = $parent_dec_name (jsonroot_$tmp);')
					if field.has_default_expr {
						dec.writeln('\t} else {')
						dec.writeln('\t\tres.${c_name(field.name)} = ${g.expr_string(field.default_expr)};')
					}
					dec.writeln('\t}')
				} else {
					g.gen_json_for_type(alias.parent_type)
					tmp := g.new_tmp_var()
					gen_js_get_opt(dec_name, field_type, styp, tmp, name, mut dec, is_required)
					dec.writeln('\tif (jsonroot_$tmp) {')
					dec.writeln('\t\tres.${c_name(field.name)} = *($field_type*) ${tmp}.data;')
					if field.has_default_expr {
						dec.writeln('\t} else {')
						dec.writeln('\t\tres.${c_name(field.name)} = ${g.expr_string(field.default_expr)};')
					}
					dec.writeln('\t}')
				}
			} else {
				tmp := g.new_tmp_var()
				gen_js_get_opt(dec_name, field_type, styp, tmp, name, mut dec, is_required)
				dec.writeln('\tif (jsonroot_$tmp) {')
				if field_sym.kind == .array_fixed {
					dec.writeln('\t\tvmemcpy(res.${c_name(field.name)},*($field_type*)${tmp}.data,sizeof($field_type));')
				} else {
					dec.writeln('\t\tres.${c_name(field.name)} = *($field_type*) ${tmp}.data;')
				}
				if field.has_default_expr {
					dec.writeln('\t} else {')
					dec.writeln('\t\tres.${c_name(field.name)} = ${g.expr_string(field.default_expr)};')
				}
				dec.writeln('\t}')
			}
		}
		// Encoding
		mut enc_name := js_enc_name(field_type)
		if is_omit_empty {
			enc.writeln('\t if (val.${c_name(field.name)} != ${g.type_default(field.typ)}) \n')
		}
		if !is_js_prim(field_type) {
			if field_sym.kind == .alias {
				ainfo := field_sym.info as ast.Alias
				enc_name = js_enc_name(g.typ(ainfo.parent_type))
			}
		}
		if field_sym.kind == .enum_ {
			enc.writeln('\tcJSON_AddItemToObject(o, "$name", json__encode_u64(val.${c_name(field.name)}));\n')
		} else {
			if field_sym.name == 'time.Time' {
				// time struct requires special treatment
				// it has to be encoded as a unix timestamp number
				enc.writeln('\tcJSON_AddItemToObject(o, "$name", json__encode_u64(val.${c_name(field.name)}._v_unix));')
			} else {
				if !field.typ.is_real_pointer() {
					enc.writeln('\tcJSON_AddItemToObject(o, "$name", ${enc_name}(val.${c_name(field.name)}));\n')
				} else {
					sptr_value := 'val.${c_name(field.name)}'
					enc.writeln('\tif ($sptr_value != 0) {')
					enc.writeln('\t\tcJSON_AddItemToObject(o, "$name", ${enc_name}(*$sptr_value));')
					enc.writeln('\t}\n')
				}
			}
		}
	}
}

fn gen_js_get(styp string, tmp string, name string, mut dec strings.Builder, is_required bool) {
	dec.writeln('\tcJSON *jsonroot_$tmp = js_get(root, "$name");')
	if is_required {
		dec.writeln('\tif (jsonroot_$tmp == 0) {')
		dec.writeln('\t\treturn (${result_name}_$styp){ .is_error = true, .err = _v_error(_SLIT("expected field \'$name\' is missing")), .data = {0} };')
		dec.writeln('\t}')
	}
}

fn gen_js_get_opt(dec_name string, field_type string, styp string, tmp string, name string, mut dec strings.Builder, is_required bool) {
	gen_js_get(styp, tmp, name, mut dec, is_required)
	value_field_type := field_type.trim_right('*')
	dec.writeln('\t${result_name}_$value_field_type $tmp;')
	dec.writeln('\tif (jsonroot_$tmp) {')
	dec.writeln('\t\t$tmp = ${dec_name}(jsonroot_$tmp);')
	dec.writeln('\t\tif (${tmp}.is_error) {')
	dec.writeln('\t\t\treturn (${result_name}_$styp){ .is_error = true, .err = ${tmp}.err, .data = {0} };')
	dec.writeln('\t\t}')
	dec.writeln('\t}')
}

fn js_enc_name(typ string) string {
	suffix := if typ.ends_with('*') { typ.trim_right('*') } else { typ }
	name := 'json__encode_$suffix'
	return util.no_dots(name)
}

fn js_dec_name(typ string) string {
	suffix := if typ.ends_with('*') { typ.trim_right('*') } else { typ }
	name := 'json__decode_$suffix'
	return util.no_dots(name)
}

fn is_js_prim(typ string) bool {
	return typ in ['int', 'rune', 'string', 'bool', 'f32', 'f64', 'i8', 'i16', 'i64', 'u8', 'u16',
		'u32', 'u64', 'byte']
}

fn (mut g Gen) decode_array(value_type ast.Type, fixed_array_size int) string {
	styp := g.typ(value_type)
	fn_name := js_dec_name(styp)
	noscan := g.check_noscan(value_type)

	fixed_array_str, fixed_array_size_str, res_str, array_free_str := if fixed_array_size > -1 {
		// fixed array
		'fixed_', '_$fixed_array_size', '', ''
	} else {
		'', '', 'res = __new_array${noscan}(0, 0, sizeof($styp));', 'array_free(&res);'
	}

	fixed_array_idx, array_element_assign, fixed_array_idx_increment := if fixed_array_size > -1 {
		// fixed array
		'int fixed_array_idx = 0;', 'res[fixed_array_idx] = val;', 'fixed_array_idx++;'
	} else {
		'', 'array_push${noscan}((array*)&res, &val);', ''
	}

	mut s := ''
	if is_js_prim(styp) {
		s = '$styp val = ${fn_name}((cJSON *)jsval); '
	} else {
		s = '
		${result_name}_$styp val2 = $fn_name ((cJSON *)jsval);
		if(val2.is_error) {
			$array_free_str
			return *(${result_name}_Array_$fixed_array_str$styp$fixed_array_size_str*)&val2;
		}
		$styp val = *($styp*)val2.data;
'
	}

	return '
	if(root && !cJSON_IsArray(root) && !cJSON_IsNull(root)) {
		return (${result_name}_Array_$fixed_array_str$styp$fixed_array_size_str){.is_error = true, .err = _v_error(string__plus(_SLIT("Json element is not an array: "), tos2((byteptr)cJSON_PrintUnformatted(root)))), .data = {0}};
	}
	$res_str
	const cJSON *jsval = NULL;
	$fixed_array_idx
	cJSON_ArrayForEach(jsval, root)
	{
	    $s
		$array_element_assign
		$fixed_array_idx_increment
	}
'
}

fn (mut g Gen) encode_array(value_type ast.Type, fixed_array_size int) string {
	styp := g.typ(value_type)
	fn_name := js_enc_name(styp)

	data_str, size_str := if fixed_array_size > -1 {
		// fixed array
		'', '$fixed_array_size'
	} else {
		'.data', 'val.len'
	}

	return '
	o = cJSON_CreateArray();
	for (int i = 0; i < $size_str; i++){
		cJSON_AddItemToArray(o, $fn_name (  (($styp*)val$data_str)[i]  ));
	}
'
}

fn (mut g Gen) decode_map(key_type ast.Type, value_type ast.Type) string {
	styp := g.typ(key_type)
	styp_v := g.typ(value_type)
	key_type_symbol := g.table.sym(key_type)
	hash_fn, key_eq_fn, clone_fn, free_fn := g.map_fn_ptrs(key_type_symbol)
	fn_name_v := js_dec_name(styp_v)
	mut s := ''
	if is_js_prim(styp_v) {
		s = '$styp_v val = $fn_name_v (js_get(root, jsval->string));'
	} else {
		s = '
		${result_name}_$styp_v val2 = $fn_name_v (js_get(root, jsval->string));
		if(val2.is_error) {
			map_free(&res);
			return *(${result_name}_Map_${styp}_$styp_v*)&val2;
		}
		$styp_v val = *($styp_v*)val2.data;
'
	}
	return '
	if(!cJSON_IsObject(root) && !cJSON_IsNull(root)) {
		return (${result_name}_Map_${styp}_$styp_v){ .is_error = true, .err = _v_error(string__plus(_SLIT("Json element is not an object: "), tos2((byteptr)cJSON_PrintUnformatted(root)))), .data = {0}};
	}
	res = new_map(sizeof($styp), sizeof($styp_v), $hash_fn, $key_eq_fn, $clone_fn, $free_fn);
	cJSON *jsval = NULL;
	cJSON_ArrayForEach(jsval, root)
	{
		$s
		string key = tos2((byteptr)jsval->string);
		map_set(&res, &key, &val);
	}
'
}

fn (mut g Gen) encode_map(key_type ast.Type, value_type ast.Type) string {
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
	Array_$styp $keys_tmp = map_keys(&val);
	for (int i = 0; i < ${keys_tmp}.len; ++i) {
		$key
		cJSON_AddItemToObject(o, (char*) key.str, $fn_name_v ( *($styp_v*) map_get(&val, &key, &($styp_v[]) { $zero } ) ) );
	}
	array_free(&$keys_tmp);
'
}
