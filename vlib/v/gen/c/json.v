// Copyright (c) 2019-2024 Alexander Medvednikov. All rights reserved.
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
	utyp := g.unwrap_generic(typ)
	sym := g.table.sym(utyp)
	if is_js_prim(sym.name) && !utyp.has_flag(.option) && !typ.is_ptr() {
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
		styp := g.styp(utyp)
		ret_styp := styp.replace('*', '_ptr')
		if utyp.is_ptr() && utyp.has_flag(.option) {
			g.register_option(utyp.set_nr_muls(0))
		}
		g.register_result(utyp)

		// decode_TYPE funcs receive an actual cJSON* object to decode
		// cJSON_Parse(str) call is added by the compiler
		// Codegen decoder
		dec_fn_name := js_dec_name(styp)
		dec_fn_dec := '${result_name}_${ret_styp} ${dec_fn_name}(cJSON* root)'

		mut init_styp := '${styp} res'
		if utyp.has_flag(.option) {
			none_str := g.expr_string(ast.None{})
			init_styp += ' = (${styp}){ .state=2, .err=${none_str}, .data={E_STRUCT} }'
		} else {
			if !utyp.is_ptr() {
				init_styp += ' = '
				g.set_current_pos_as_last_stmt_pos()
				pos := g.out.len
				g.write(g.type_default(utyp))
				init_generated := g.out.cut_to(pos).trim_space()
				if g.type_default_vars.len > 0 {
					saved_init := init_styp
					init_styp = g.type_default_vars.bytestr()
					init_styp += '\n'
					init_styp += saved_init
					g.type_default_vars.clear()
				}
				init_styp += init_generated
			} else if utyp.is_ptr() {
				ptr_styp := g.styp(utyp.set_nr_muls(utyp.nr_muls() - 1))
				init_styp += ' = HEAP(${ptr_styp}, {0})'
			}
		}

		dec.writeln('
${dec_fn_dec} {
	${init_styp};
	if (!root) {
		const char *error_ptr = cJSON_GetErrorPtr();
		if (error_ptr != NULL) {
			const int error_pos = (int)cJSON_GetErrorPos();
			int maxcontext_chars = 30;
			byte *buf = vcalloc_noscan(maxcontext_chars + 10);
			if (error_pos > 0) {
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
			string msg;
			msg = _S("failed to decode JSON string");
			if (buf[0] != \'\\0\') {
				msg = string__plus(msg, _S(": "));
			}
			return (${result_name}_${ret_styp}){.is_error = true,.err = _v_error(string__plus(msg, tos2(buf))),.data = {0}};
		}
	}
')
		extern_str := if g.pref.parallel_cc { 'extern ' } else { '' }
		g.json_forward_decls.writeln('${extern_str}${dec_fn_dec};')
		// Codegen encoder
		// encode_TYPE funcs receive an object to encode
		enc_fn_name := js_enc_name(styp)
		enc_fn_dec := 'cJSON* ${enc_fn_name}(${styp} val)'
		g.json_forward_decls.writeln('${extern_str}${enc_fn_dec};\n')
		enc.writeln('
${enc_fn_dec} {
\tcJSON *o;')
		if is_js_prim(sym.name) && utyp.is_ptr() {
			g.gen_prim_enc_dec(utyp, mut enc, mut dec)
		} else if sym.kind in [.array, .array_fixed] {
			array_size := if sym.kind == .array_fixed {
				(sym.info as ast.ArrayFixed).size
			} else {
				-1
			}
			// Handle arrays
			value_type := g.table.value_type(utyp)
			// If we have `[]Profile`, have to register a Profile en(de)coder first
			g.gen_json_for_type(value_type)
			dec.writeln(g.decode_array(utyp, value_type, array_size, ret_styp))
			enc.writeln(g.encode_array(utyp, value_type, array_size))
		} else if sym.kind == .map {
			// Handle maps
			m := sym.info as ast.Map
			g.gen_json_for_type(m.key_type)
			g.gen_json_for_type(m.value_type)
			dec.writeln(g.decode_map(utyp, m.key_type, m.value_type, ret_styp))
			enc.writeln(g.encode_map(utyp, m.key_type, m.value_type))
		} else if sym.kind == .alias {
			a := sym.info as ast.Alias
			parent_typ := a.parent_type
			psym := g.table.sym(parent_typ)
			if is_js_prim(g.styp(parent_typ)) {
				if utyp.has_flag(.option) {
					g.gen_json_for_type(parent_typ.set_flag(.option))
					g.gen_option_enc_dec(parent_typ.set_flag(.option), mut enc, mut dec)
				} else {
					g.gen_json_for_type(parent_typ)
					g.gen_prim_enc_dec(parent_typ, mut enc, mut dec)
				}
			} else if psym.info is ast.Struct {
				enc.writeln('\to = cJSON_CreateObject();')
				g.gen_struct_enc_dec(utyp, psym.info, ret_styp, mut enc, mut dec, '')
			} else if psym.kind == .enum {
				g.gen_enum_enc_dec(utyp, psym, mut enc, mut dec)
			} else if psym.kind == .sum_type {
				verror('json: ${sym.name} aliased sumtypes does not work at the moment')
			} else if psym.kind == .map {
				m := psym.info as ast.Map
				g.gen_json_for_type(m.key_type)
				g.gen_json_for_type(m.value_type)
				dec.writeln(g.decode_map(utyp, m.key_type, m.value_type, ret_styp))
				enc.writeln(g.encode_map(utyp, m.key_type, m.value_type))
			} else if utyp.has_flag(.option) {
				g.gen_option_enc_dec(utyp, mut enc, mut dec)
			} else {
				verror('json: ${sym.name} is not struct')
			}
		} else if sym.kind == .sum_type {
			enc.writeln('\to = cJSON_CreateObject();')
			// Sumtypes. Range through variants of sumtype
			if sym.info !is ast.SumType {
				verror('json: ${sym.name} is not a sumtype')
			}
			g.gen_sumtype_enc_dec(utyp, sym, mut enc, mut dec, ret_styp)
		} else if sym.kind == .enum {
			g.gen_enum_enc_dec(utyp, sym, mut enc, mut dec)
		} else if utyp.has_flag(.option)
			&& (is_js_prim(g.styp(utyp.clear_flag(.option))) || sym.info !is ast.Struct) {
			g.gen_option_enc_dec(utyp, mut enc, mut dec)
		} else {
			enc.writeln('\to = cJSON_CreateObject();')
			// Structs. Range through fields
			if sym.info !is ast.Struct {
				verror('json: ${sym.name} is not struct')
			}
			g.gen_struct_enc_dec(utyp, sym.info, ret_styp, mut enc, mut dec, '')
		}
		// cJSON_delete
		dec.writeln('\t${result_name}_${ret_styp} ret;')
		dec.writeln('\t_result_ok(&res, (${result_name}*)&ret, sizeof(res));')
		dec.writeln('\treturn ret;\n}')
		enc.writeln('\treturn o;\n}')
		g.gowrappers.writeln(dec.str())
		g.gowrappers.writeln(enc.str())
	}
}

@[inline]
fn (mut g Gen) gen_enum_to_str(utyp ast.Type, sym ast.TypeSymbol, enum_var string, result_var string, ident string,
	mut enc strings.Builder) {
	enum_prefix := g.gen_enum_prefix(utyp.clear_flag(.option))
	enc.writeln('${ident}switch (${enum_var}) {')
	for val in (sym.info as ast.Enum).vals {
		enc.write_string('${ident}\tcase ${enum_prefix}${val}:\t')
		// read [json:] attr from the Enum value
		attr := g.table.enum_decls[sym.name].fields.filter(it.name == val)[0].attrs.find_first('json') or {
			ast.Attr{}
		}
		if attr.has_arg {
			enc.writeln('${result_var} = json__encode_string(_S("${attr.arg}")); break;')
		} else {
			enc.writeln('${result_var} = json__encode_string(_S("${val}")); break;')
		}
	}
	enc.writeln('${ident}}')
}

@[inline]
fn (mut g Gen) gen_str_to_enum(utyp ast.Type, sym ast.TypeSymbol, val_var string, result_var string, ident string,
	mut dec strings.Builder) {
	enum_prefix := g.gen_enum_prefix(utyp.clear_flag(.option))
	is_option := utyp.has_flag(.option)
	for k, val in (sym.info as ast.Enum).vals {
		// read [json:] attr from the Enum value
		attr := g.table.enum_decls[sym.name].fields.filter(it.name == val)[0].attrs.find_first('json') or {
			ast.Attr{}
		}
		if k == 0 {
			dec.write_string('${ident}if (string__eq(_S("${val}"), ${val_var})')
		} else {
			dec.write_string('${ident}else if (string__eq(_S("${val}"), ${val_var})')
		}
		if attr.has_arg {
			dec.write_string(' || string__eq(_S("${attr.arg}"), ${val_var})')
		}
		dec.write_string(')\t')
		if is_option {
			base_typ := g.base_type(utyp)
			dec.writeln('_option_ok(&(${base_typ}[]){ ${enum_prefix}${val} }, (${option_name}*)${result_var}, sizeof(${base_typ}));')
		} else {
			dec.writeln('${result_var} = ${enum_prefix}${val};')
		}
	}
}

@[inline]
fn (mut g Gen) is_enum_as_int(sym ast.TypeSymbol) bool {
	if enum_decl := g.table.enum_decls[sym.name] {
		if _ := enum_decl.attrs.find_first('json_as_number') {
			return true
		}
	}
	return false
}

@[inline]
fn (mut g Gen) gen_enum_enc_dec(utyp ast.Type, sym ast.TypeSymbol, mut enc strings.Builder, mut dec strings.Builder) {
	is_option := utyp.has_flag(.option)

	if g.is_enum_as_int(sym) {
		if is_option {
			base_typ := g.styp(utyp.clear_flag(.option))
			enc.writeln('\to = ${js_enc_name('u64')}(*val.data);')
			dec.writeln('\t_option_ok(&(${base_typ}[]){ ${js_dec_name('u64')}(root) }, (${option_name}*)&res, sizeof(${base_typ}));')
		} else {
			dec.writeln('\tres = ${js_dec_name('u64')}(root);')
			enc.writeln('\to = ${js_enc_name('u64')}(val);')
		}
	} else {
		tmp := g.new_tmp_var()
		dec.writeln('\tstring ${tmp} = ${js_dec_name('string')}(root);')
		if is_option {
			g.gen_str_to_enum(utyp, sym, tmp, '&res', '\t', mut dec)
			g.gen_enum_to_str(utyp, sym, '*(${g.base_type(utyp)}*)val.data', 'o', '\t\t', mut
				enc)
		} else {
			g.gen_str_to_enum(utyp, sym, tmp, 'res', '\t', mut dec)
			g.gen_enum_to_str(utyp, sym, 'val', 'o', '\t', mut enc)
		}
	}
}

@[inline]
fn (mut g Gen) gen_prim_enc_dec(typ ast.Type, mut enc strings.Builder, mut dec strings.Builder) {
	if typ.is_ptr() {
		type_str := g.styp(typ.clear_flag(.option).set_nr_muls(typ.nr_muls() - 1))
		type_str_0 := g.styp(typ.clear_flag(.option).set_nr_muls(0))
		encode_name := js_enc_name(type_str_0)
		dec_name := js_dec_name(type_str)
		if typ.has_flag(.option) {
			enc.writeln('\to = ${encode_name}(${'*'.repeat(typ.nr_muls() + 1)}(${type_str_0}${'*'.repeat(typ.nr_muls())}*)&val.data);')
		} else {
			enc.writeln('\to = ${encode_name}(${'*'.repeat(typ.nr_muls())}val);')
		}

		if typ.nr_muls() > 1 {
			g.gen_json_for_type(typ.clear_flag(.option).set_nr_muls(typ.nr_muls() - 1))
			if typ.has_flag(.option) {
				tmp_var := g.new_tmp_var()
				dec.writeln('${type_str}* ${tmp_var} = HEAP(${type_str}, *(${type_str}*) ${dec_name}(root).data);')
				dec.writeln('\t_option_ok(&(${type_str}*[]) { &(*(${tmp_var})) }, (${option_name}*)&res, sizeof(${type_str}*));')
			} else {
				dec.writeln('\tres = HEAP(${type_str}, *(${type_str}*) ${dec_name}(root).data);')
			}
		} else {
			if typ.has_flag(.option) {
				tmp_var := g.new_tmp_var()
				dec.writeln('${type_str}* ${tmp_var} = HEAP(${type_str}, ${dec_name}(root));')
				dec.writeln('\t_option_ok(&(${type_str}*[]) { &(*(${tmp_var})) }, (${option_name}*)&res, sizeof(${type_str}*));')
			} else {
				dec.writeln('\tres = HEAP(${type_str}, ${dec_name}(root));')
			}
		}
	} else {
		type_str := g.styp(typ.clear_flag(.option))
		encode_name := js_enc_name(type_str)
		dec_name := js_dec_name(type_str)
		enc.writeln('\to = ${encode_name}(val);')
		dec.writeln('\tres = ${dec_name}(root);')
	}
}

@[inline]
fn (mut g Gen) gen_option_enc_dec(typ ast.Type, mut enc strings.Builder, mut dec strings.Builder) {
	enc.writeln('\tif (val.state == 2) {')
	enc.writeln('\t\treturn NULL;')
	enc.writeln('\t}')
	type_str := g.styp(typ.clear_flag(.option))
	encode_name := js_enc_name(type_str)
	enc.writeln('\to = ${encode_name}(*(${type_str}*)val.data);')

	dec_name := js_dec_name(type_str)
	dec.writeln('\tif (!cJSON_IsNull(root)) {')
	dec.writeln('\t\t_option_ok(&(${type_str}[]){ ${dec_name}(root) }, (${option_name}*)&res, sizeof(${type_str}));')
	dec.writeln('\t} else {')
	default_init := if typ.is_int() || typ.is_float() || typ.is_bool() { '0' } else { '{0}' }
	dec.writeln('\t\t_option_none(&(${type_str}[]){ ${default_init} }, (${option_name}*)&res, sizeof(${type_str}));')
	dec.writeln('\t}')
}

@[inline]
fn (mut g Gen) gen_sumtype_enc_dec(utyp ast.Type, sym ast.TypeSymbol, mut enc strings.Builder, mut dec strings.Builder,
	ret_styp string) {
	info := sym.info as ast.SumType
	type_var := g.new_tmp_var()
	typ := ast.idx_to_type(g.table.type_idxs[sym.name])
	prefix := if utyp.is_ptr() { '*' } else { '' }
	field_op := if utyp.is_ptr() { '->' } else { '.' }
	is_option := utyp.has_flag(.option)
	var_data := if is_option { '(*(${g.base_type(utyp)}*)val.data)' } else { 'val' }

	// DECODING (inline)
	$if !json_no_inline_sumtypes ? {
		// Handle "key": null
		// In this case the first variant must be used (something like InvalidExpr for example)
		// An empty instance of the first variant is generated.
		// This way the user can easily check if the sum type was not provided in json ("key":null):
		// `if node.expr is InvalidExpr { ... }`
		// (Do this only for structs)
		type_tmp := g.new_tmp_var()
		first_variant := info.variants[0]
		variant_typ := g.styp(first_variant)
		fv_sym := g.table.sym(first_variant)
		first_variant_name := fv_sym.cname
		// println('KIND=${fv_sym.kind}')
		if fv_sym.kind == .struct && !is_option && field_op != '->' {
			dec.writeln('\tif (root->type == cJSON_NULL) { ')
			dec.writeln('\t\tstruct ${first_variant_name} empty = {0};')
			dec.writeln('res = ${variant_typ}_to_sumtype_${ret_styp}(&empty); } \n else ')
			// dec.writeln('res = ${variant_typ}_to_sumtype_${sym.cname}(&empty); } \n else ')
		}
		//
		dec.writeln('if (cJSON_IsObject(root) || (cJSON_IsArray(root) && cJSON_IsObject(root->child))) {')
		dec.writeln('\t\tcJSON* ${type_tmp} = cJSON_IsObject(root) ? js_get(root, "_type") : js_get(root->child, "_type");')
		dec.writeln('\t\tif (${type_tmp} != 0) {')
		dec.writeln('\t\t\tchar* ${type_var} = cJSON_GetStringValue(${type_tmp});')
		// dec.writeln('\t\t\tcJSON_DeleteItemFromObjectCaseSensitive(root, "_type");')
	}

	mut variant_types := []string{}
	mut variant_symbols := []ast.TypeSymbol{}
	mut at_least_one_prim := false
	for variant in info.variants {
		variant_typ := g.styp(variant)
		variant_types << variant_typ
		variant_sym := g.table.sym(variant)
		variant_symbols << variant_sym
		at_least_one_prim = at_least_one_prim || is_js_prim(variant_typ)
			|| variant_sym.kind == .enum || variant_sym.name == 'time.Time'
		unmangled_variant_name := variant_sym.name.split('.').last()

		// TODO: Do not generate dec/enc for 'time.Time', because we handle it by saving it as u64
		g.gen_json_for_type(variant)

		// Helpers for decoding
		g.get_sumtype_casting_fn(variant, typ)
		g.definitions.writeln('static inline ${sym.cname} ${variant_typ}_to_sumtype_${sym.cname}(${variant_typ}* x);')

		// ENCODING
		enc.writeln('\tif (${var_data}${field_op}_typ == ${int(variant.idx())}) {')
		$if json_no_inline_sumtypes ? {
			if variant_sym.kind == .enum {
				enc.writeln('\t\tcJSON_AddItemToObject(o, "${unmangled_variant_name}", ${js_enc_name('u64')}(*${var_data}${field_op}_${variant_typ}));')
			} else if variant_sym.name == 'time.Time' {
				enc.writeln('\t\tcJSON_AddItemToObject(o, "${unmangled_variant_name}", ${js_enc_name('i64')}(${var_data}${field_op}_${variant_typ}->__v_unix));')
			} else {
				enc.writeln('\t\tcJSON_AddItemToObject(o, "${unmangled_variant_name}", ${js_enc_name(variant_typ)}(*${var_data}${field_op}_${variant_typ}));')
			}
		} $else {
			if is_js_prim(variant_typ) {
				enc.writeln('\t\tcJSON_free(o); return ${js_enc_name(variant_typ)}(*${var_data}${field_op}_${variant_typ});')
			} else if variant_sym.kind == .enum {
				if g.is_enum_as_int(variant_sym) {
					enc.writeln('\t\tcJSON_free(o); return ${js_enc_name('u64')}(*${var_data}${field_op}_${variant_typ});')
				} else {
					enc.writeln('\t\tcJSON_free(o);')
					tmp2 := g.new_tmp_var()
					if utyp.has_flag(.option) {
						enc.writeln('\t\tu64 ${tmp2} = *${var_data}${field_op}_${variant_typ};')
						g.gen_enum_to_str(variant, variant_sym, tmp2, 'o', '\t\t', mut
							enc)
					} else {
						enc.writeln('\t\tu64 ${tmp2} = *${var_data}${field_op}_${variant_typ};')
						g.gen_enum_to_str(variant, variant_sym, tmp2, 'o', '\t\t', mut
							enc)
					}
				}
			} else if variant_sym.name == 'time.Time' {
				enc.writeln('\t\tcJSON_AddItemToObject(o, "_type", cJSON_CreateString("${unmangled_variant_name}"));')
				enc.writeln('\t\tcJSON_AddItemToObject(o, "value", ${js_enc_name('i64')}(${var_data}${field_op}_${variant_typ}->__v_unix));')
			} else {
				enc.writeln('\t\tcJSON_free(o);')
				enc.writeln('\t\to = ${js_enc_name(variant_typ)}(*${var_data}${field_op}_${variant_typ});')
				if variant_sym.kind == .array {
					enc.writeln('\t\tif (cJSON_IsObject(o->child)) {')
					enc.writeln('\t\t\tcJSON_AddItemToObject(o->child, "_type", cJSON_CreateString("${unmangled_variant_name}"));')
					enc.writeln('\t\t}')
				} else {
					enc.writeln('\t\tcJSON_AddItemToObject(o, "_type", cJSON_CreateString("${unmangled_variant_name}"));')
				}
			}
		}
		enc.writeln('\t}')

		// DECODING
		tmp := g.new_tmp_var()
		$if json_no_inline_sumtypes ? {
			dec.writeln('\tif (strcmp("${unmangled_variant_name}", root->child->string) == 0) {')
			if is_js_prim(variant_typ) {
				gen_js_get(ret_styp, tmp, unmangled_variant_name, mut dec, true)
				dec.writeln('\t\t${variant_typ} value = ${js_dec_name(variant_typ)}(jsonroot_${tmp});')
			} else if variant_sym.kind == .enum {
				if g.is_enum_as_int(variant_sym) {
					gen_js_get(ret_styp, tmp, unmangled_variant_name, mut dec, true)
					dec.writeln('\t\t${variant_typ} value = ${js_dec_name('u64')}(jsonroot_${tmp});')
				} else {
					gen_js_get(ret_styp, tmp, unmangled_variant_name, mut dec, true)
					dec.writeln('\t\t${variant_typ} value;')
					tmp2 := g.new_tmp_var()
					dec.writeln('\t\tstring ${tmp2} = json__decode_string(jsonroot_${tmp});')
					g.gen_enum_to_str(variant, variant_sym, tmp2, 'value', '\t\t', mut
						dec)
				}
			} else if variant_sym.name == 'time.Time' {
				gen_js_get(ret_styp, tmp, unmangled_variant_name, mut dec, true)
				dec.writeln('\t\t${variant_typ} value = time__unix(${js_dec_name('i64')}(jsonroot_${tmp}));')
			} else {
				gen_js_get_opt(js_dec_name(variant_typ), variant_typ, ret_styp, tmp, unmangled_variant_name, mut
					dec, true)
				dec.writeln('\t\t${variant_typ} value = *(${variant_typ}*)(${tmp}.data);')
			}
			if is_option {
				dec.writeln('\t\t\t_option_ok(&(${sym.cname}[]){ ${variant_typ}_to_sumtype_${sym.cname}(&value) }, (${option_name}*)&res, sizeof(${sym.cname}));')
			} else {
				dec.writeln('\t\tres = ${variant_typ}_to_sumtype_${ret_styp}(&value);')
			}
			dec.writeln('\t}')
		} $else {
			if variant_sym.name == 'time.Time' {
				dec.writeln('\t\t\tif (strcmp("Time", ${type_var}) == 0) {')
				gen_js_get(ret_styp, tmp, 'value', mut dec, true)
				dec.writeln('\t\t\t\t${variant_typ} ${tmp} = time__unix(${js_dec_name('i64')}(jsonroot_${tmp}));')
				if utyp.has_flag(.option) {
					dec.writeln('\t\t\t\t${prefix}res.state = 0;')
					tmp_time_var := g.new_tmp_var()
					dec.writeln('\t\t\t\t${g.base_type(utyp)} ${tmp_time_var} = ${variant_typ}_to_sumtype_${sym.cname}(&${tmp});')
					dec.writeln('\t\t\t\tvmemcpy(&${prefix}res.data, ${tmp_time_var}._time__Time, sizeof(${variant_typ}));')
				} else {
					dec.writeln('\t\t\t\t${prefix}res = ${variant_typ}_to_sumtype_${sym.cname}(&${tmp});')
				}
				dec.writeln('\t\t\t}')
			} else if !is_js_prim(variant_typ) && variant_sym.kind != .enum {
				dec.writeln('\t\t\tif (strcmp("${unmangled_variant_name}", ${type_var}) == 0 && ${variant_sym.kind == .array} == cJSON_IsArray(root)) {')
				dec.writeln('\t\t\t\t${result_name}_${variant_typ} ${tmp} = ${js_dec_name(variant_typ)}(root);')
				dec.writeln('\t\t\t\tif (${tmp}.is_error) {')

				dec.writeln('\t\t\t\t\treturn (${result_name}_${ret_styp}){ .is_error = true, .err = ${tmp}.err, .data = {0} };')
				dec.writeln('\t\t\t\t}')
				if is_option {
					dec.writeln('\t\t\t\t_option_ok(&(${sym.cname}[]){ ${variant_typ}_to_sumtype_${sym.cname}((${variant_typ}*)${tmp}.data) }, (${option_name}*)&res, sizeof(${sym.cname}));')
				} else {
					dec.writeln('\t\t\t\t${prefix}res = ${variant_typ}_to_sumtype_${sym.cname}((${variant_typ}*)${tmp}.data);')
				}
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
				dec.writeln('\t\t\t${var_t} value = ${js_dec_name(var_t)}(root);')
				dec.writeln('\t\t\t${prefix}res = ${var_t}_to_sumtype_${sym.cname}(&value);')
				dec.writeln('\t\t}')
			}

			for i, var_t in variant_types {
				if variant_symbols[i].kind == .enum {
					if number_is_met {
						var_num := var_t.replace('__', '.')
						last_num := last_number_type.replace('__', '.')
						verror_suggest_json_no_inline_sumtypes(sym.name, last_num, var_num)
					}
					number_is_met = true
					last_number_type = var_t
					dec.writeln('\t\tif (cJSON_IsNumber(root)) {')
					dec.writeln('\t\t\t${var_t} value = ${js_dec_name('u64')}(root);')
					if utyp.has_flag(.option) {
						dec.writeln('\t\t\t_option_ok(&(${sym.cname}[]){ ${var_t}_to_sumtype_${sym.cname}(&value) }, (${option_name}*)&${prefix}res, sizeof(${sym.cname}));')
					} else {
						dec.writeln('\t\t\t${prefix}res = ${var_t}_to_sumtype_${sym.cname}(&value);')
					}
					dec.writeln('\t\t}')
				}

				if var_t in ['string', 'rune'] {
					if string_is_met {
						var_num := var_t.replace('__', '.')
						verror_suggest_json_no_inline_sumtypes(sym.name, 'string', var_num)
					}
					string_is_met = true
					dec.writeln('\t\tif (cJSON_IsString(root)) {')
					dec.writeln('\t\t\t${var_t} value = ${js_dec_name(var_t)}(root);')
					if utyp.has_flag(.option) {
						dec.writeln('\t\t\t_option_ok(&(${sym.cname}[]){ ${var_t}_to_sumtype_${sym.cname}(&value) }, (${option_name}*)&${prefix}res, sizeof(${sym.cname}));')
					} else {
						dec.writeln('\t\t\t${prefix}res = ${var_t}_to_sumtype_${sym.cname}(&value);')
					}
					dec.writeln('\t\t}')
				}

				if var_t.starts_with('Array_') {
					tmp := g.new_tmp_var()
					judge_elem_typ := if var_t.ends_with('string') {
						'cJSON_IsString(root->child)'
					} else if var_t.ends_with('bool') {
						'cJSON_IsBool(root->child)'
					} else if g.table.sym(g.table.value_type(ast.idx_to_type(variant_symbols[i].idx))).kind == .struct {
						'cJSON_IsObject(root->child)'
					} else {
						'cJSON_IsNumber(root->child)'
					}
					dec.writeln('\t\tif (cJSON_IsArray(root) && ${judge_elem_typ}) {')
					dec.writeln('\t\t\t${result_name}_${var_t} ${tmp} = ${js_dec_name(var_t)}(root);')
					dec.writeln('\t\t\tif (${tmp}.is_error) {')
					dec.writeln('\t\t\t\treturn (${result_name}_${ret_styp}){ .is_error = true, .err = ${tmp}.err, .data = {0} };')
					dec.writeln('\t\t\t}')
					if utyp.has_flag(.option) {
						dec.writeln('\t\t\t_option_ok(&(${sym.cname}[]){ ${var_t}_to_sumtype_${sym.cname}((${var_t}*)${tmp}.data) }, (${option_name}*)&${prefix}res, sizeof(${sym.cname}));')
					} else {
						dec.writeln('\t\t\t${prefix}res = ${var_t}_to_sumtype_${sym.cname}((${var_t}*)${tmp}.data);')
					}
					dec.writeln('\t\t}')
				}

				if var_t in ['i64', 'int', 'i8', 'u64', 'u32', 'u16', 'byte', 'u8', 'rune', 'f64',
					'f32'] {
					if number_is_met {
						var_num := var_t.replace('__', '.')
						last_num := last_number_type.replace('__', '.')
						verror_suggest_json_no_inline_sumtypes(sym.name, last_num, var_num)
					}
					number_is_met = true
					last_number_type = var_t
					dec.writeln('\t\tif (cJSON_IsNumber(root)) {')
					dec.writeln('\t\t\t${var_t} value = ${js_dec_name(var_t)}(root);')
					if utyp.has_flag(.option) {
						dec.writeln('\t\t\t_option_ok(&(${sym.cname}[]){ ${var_t}_to_sumtype_${sym.cname}(&value) }, (${option_name}*)&${prefix}res, sizeof(${sym.cname}));')
					} else {
						dec.writeln('\t\t\t${prefix}res = ${var_t}_to_sumtype_${sym.cname}(&value);')
					}
					dec.writeln('\t\t}')
				}
			}
		}
		dec.writeln('\t}')
	}
}

fn (mut g Gen) gen_prim_type_validation(name string, typ ast.Type, tmp string, is_required bool, ret_styp string, mut dec strings.Builder) {
	none_check := if !is_required {
		'cJSON_IsNull(jsonroot_${tmp}) || '
	} else {
		''
	}
	type_check := if typ.is_int() || typ.is_float() {
		'${none_check}cJSON_IsNumber(jsonroot_${tmp}) || (cJSON_IsString(jsonroot_${tmp}) && strlen(jsonroot_${tmp}->valuestring))'
	} else if typ.is_string() {
		'${none_check}cJSON_IsString(jsonroot_${tmp})'
	} else if typ.is_bool() {
		'${none_check}cJSON_IsBool(jsonroot_${tmp})'
	} else {
		''
	}
	if type_check == '' {
		return
	}
	dec.writeln('\t\tif (!(${type_check})) {')
	dec.writeln('\t\t\treturn (${ret_styp}){ .is_error = true, .err = _v_error(string__plus(_S("type mismatch for field \'${name}\', expecting `${g.table.type_to_str(typ)}` type, got: "), json__json_print(jsonroot_${tmp}))), .data = {0} };')
	dec.writeln('\t\t}')
}

@[inline]
fn (mut g Gen) gen_struct_enc_dec(utyp ast.Type, type_info ast.TypeInfo, styp string, mut enc strings.Builder,
	mut dec strings.Builder, embed_prefix string) {
	info := type_info as ast.Struct
	for field in info.fields {
		mut name := field.name
		mut is_raw := false
		mut is_skip := false
		mut is_required := false
		mut is_omit_empty := false
		mut skip_embed := false
		mut is_json_null := false

		for attr in field.attrs {
			match attr.name {
				'json' {
					if attr.arg == '-' {
						// [json:'-']
						is_skip = true
					} else {
						name = attr.arg
					}
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
				'json_null' {
					is_json_null = true
				}
				else {}
			}
		}
		if is_skip {
			continue
		}
		field_type := g.styp(field.typ)
		field_sym := g.table.sym(field.typ)
		op := if utyp.is_ptr() { '->' } else { '.' }
		embed_member := if embed_prefix.len > 0 { '.${embed_prefix}' } else { '' }
		prefix := if utyp.has_flag(.option) {
			'(*(${g.base_type(utyp)}*)res${embed_member}.data)'
		} else {
			'res${embed_member}'
		}
		// First generate decoding
		if is_raw {
			if field.typ.has_flag(.option) {
				g.gen_json_for_type(field.typ)
				base_typ := g.base_type(field.typ)
				dec.writeln('\tif (js_get(root, "${name}") == NULL)')
				default_init := if field.typ.is_int() || field.typ.is_float() || field.typ.is_bool() {
					'0'
				} else {
					'{0}'
				}
				dec.writeln('\t\t_option_none(&(${base_typ}[]) { ${default_init} }, (${option_name}*)&${prefix}${op}${c_name(field.name)}, sizeof(${base_typ}));')
				dec.writeln('\telse')
				dec.writeln('\t\t_option_ok(&(${base_typ}[]) {  json__json_print(js_get(root, "${name}")) }, (${option_name}*)&${prefix}${op}${c_name(field.name)}, sizeof(${base_typ}));')
			} else {
				dec.writeln('\t${prefix}${op}${c_name(field.name)} = json__json_print(js_get(root, "${name}"));')
			}
		} else {
			// Now generate decoders for all field types in this struct
			// need to do it here so that these functions are generated first
			g.gen_json_for_type(field.typ)
			dec_name := js_dec_name(field_type)
			if is_js_prim(field_type) {
				tmp := g.new_tmp_var()
				gen_js_get(styp, tmp, name, mut dec, is_required)
				dec.writeln('\tif (jsonroot_${tmp}) {')
				if utyp.has_flag(.option) {
					dec.writeln('\t\tres.state = 0;')
				}
				g.gen_prim_type_validation(field.name, field.typ, tmp, is_required, '${result_name}_${styp}', mut
					dec)
				dec.writeln('\t\t${prefix}${op}${c_name(field.name)} = ${dec_name}(jsonroot_${tmp});')
				if field.has_default_expr {
					dec.writeln('\t} else {')
					dec.writeln('\t\t${prefix}${op}${c_name(field.name)} = ${g.expr_string_opt(field.typ,
						field.default_expr)};')
				}
				dec.writeln('\t}')
			} else if field_sym.kind == .enum {
				tmp := g.new_tmp_var()
				is_option_field := field.typ.has_flag(.option)
				if field.typ.has_flag(.option) {
					gen_js_get_opt(js_dec_name(field_type), field_type, styp, tmp, name, mut
						dec, is_required)
					dec.writeln('\tif (jsonroot_${tmp} && !cJSON_IsNull(jsonroot_${tmp})) {')
				} else {
					gen_js_get(styp, tmp, name, mut dec, is_required)
					dec.writeln('\tif (jsonroot_${tmp}) {')
				}
				if g.is_enum_as_int(field_sym) {
					if is_option_field {
						base_typ := g.base_type(field.typ)
						dec.writeln('\t\t_option_ok(&(${base_typ}[]) { ${js_dec_name('u64')}(jsonroot_${tmp}) }, (${option_name}*)&${prefix}${op}${c_name(field.name)}, sizeof(${base_typ}));')
					} else {
						dec.writeln('\t\t${prefix}${op}${c_name(field.name)} = ${js_dec_name('u64')}(jsonroot_${tmp});')
					}
				} else {
					if is_option_field {
						base_typ := g.base_type(field.typ)
						dec.writeln('\t\t_option_ok(&(${base_typ}[]) { *(${base_typ}*)((${g.styp(field.typ)}*)${tmp}.data)->data }, (${option_name}*)&${prefix}${op}${c_name(field.name)}, sizeof(${base_typ}));')
					} else {
						tmp2 := g.new_tmp_var()
						dec.writeln('\t\tstring ${tmp2} = json__decode_string(jsonroot_${tmp});')
						g.gen_str_to_enum(field.typ, field_sym, tmp2, '${prefix}${op}${c_name(field.name)}',
							'\t\t', mut dec)
					}
				}
				if field.has_default_expr {
					dec.writeln('\t} else {')
					dec.writeln('\t\t${prefix}${op}${c_name(field.name)} = ${g.expr_string_opt(field.typ,
						field.default_expr)};')
				}
				dec.writeln('\t}')
			} else if field_sym.name == 'time.Time' {
				// time struct requires special treatment
				// it has to be decoded from a unix timestamp number
				tmp := g.new_tmp_var()
				gen_js_get(styp, tmp, name, mut dec, is_required)
				dec.writeln('\tif (jsonroot_${tmp}) {')
				if field.typ.has_flag(.option) {
					dec.writeln('\t\tif (!(cJSON_IsNull(jsonroot_${tmp}))) {\n')
					dec.writeln('\t\t\t${prefix}${op}${c_name(field.name)}.state = 0;\n')
					tmp_time_var := g.new_tmp_var()
					dec.writeln('\t\t\t${g.base_type(field.typ)} ${tmp_time_var} = time__unix(json__decode_u64(jsonroot_${tmp}));\n')
					dec.writeln('\t\t\tvmemcpy(&${prefix}${op}${c_name(field.name)}.data, &${tmp_time_var}, sizeof(${g.base_type(field.typ)}));')
					dec.writeln('\t\t}\n')
				} else {
					dec.writeln('\t\t${prefix}${op}${c_name(field.name)} = time__unix(json__decode_u64(jsonroot_${tmp}));')
					if field.has_default_expr {
						dec.writeln('\t} else {')
						dec.writeln('\t\t${prefix}${op}${c_name(field.name)} = ${g.expr_string_opt(field.typ,
							field.default_expr)};')
					}
				}
				dec.writeln('\t}')
			} else if field_sym.kind == .alias {
				alias := field_sym.info as ast.Alias
				parent_type := if field.typ.has_flag(.option) {
					alias.parent_type.set_flag(.option)
				} else {
					alias.parent_type
				}
				sparent_type := g.styp(parent_type)
				parent_dec_name := js_dec_name(sparent_type)
				if is_js_prim(sparent_type) {
					tmp := g.new_tmp_var()
					gen_js_get(styp, tmp, name, mut dec, is_required)
					dec.writeln('\tif (jsonroot_${tmp}) {')
					g.gen_prim_type_validation(field.name, parent_type, tmp, is_required,
						'${result_name}_${styp}', mut dec)
					dec.writeln('\t\t${prefix}${op}${c_name(field.name)} = ${parent_dec_name} (jsonroot_${tmp});')
					if field.has_default_expr {
						dec.writeln('\t} else {')
						dec.writeln('\t\t${prefix}${op}${c_name(field.name)} = ${g.expr_string_opt(field.typ,
							field.default_expr)};')
					}
					dec.writeln('\t}')
				} else {
					g.gen_json_for_type(parent_type)
					tmp := g.new_tmp_var()
					gen_js_get_opt(dec_name, field_type, styp, tmp, name, mut dec, is_required)
					dec.writeln('\tif (jsonroot_${tmp}) {')
					dec.writeln('\t\t${prefix}${op}${c_name(field.name)} = *(${field_type}*) ${tmp}.data;')
					if field.has_default_expr {
						dec.writeln('\t} else {')
						dec.writeln('\t\t${prefix}${op}${c_name(field.name)} = ${g.expr_string_opt(field.typ,
							field.default_expr)};')
					}
					dec.writeln('\t}')
				}
			} else {
				// embeded
				if field.is_embed && field_sym.info is ast.Struct {
					for embed in info.embeds {
						if embed == int(field.typ) {
							prefix_embed := if embed_prefix != '' {
								'${embed_prefix}.${name}'
							} else {
								name
							}
							g.gen_struct_enc_dec(field.typ, g.table.sym(field.typ).info,
								styp, mut enc, mut dec, prefix_embed)
							skip_embed = true
							break
						}
					}
				}
				tmp := g.new_tmp_var()
				gen_js_get_opt(dec_name, field_type, styp, tmp, name, mut dec, is_required)
				dec.writeln('\tif (jsonroot_${tmp}) {')
				if is_js_prim(g.styp(field.typ.clear_option_and_result())) {
					g.gen_prim_type_validation(field.name, field.typ, tmp, is_required,
						'${result_name}_${styp}', mut dec)
				}
				if field.typ.has_flag(.option) {
					dec.writeln('\t\tvmemcpy(&${prefix}${op}${c_name(field.name)}, (${field_type}*)${tmp}.data, sizeof(${field_type}));')
				} else {
					if field_sym.kind == .array_fixed {
						dec.writeln('\t\tvmemcpy(${prefix}${op}${c_name(field.name)},*(${field_type}*)${tmp}.data,sizeof(${field_type}));')
					} else {
						dec.writeln('\t\t${prefix}${op}${c_name(field.name)} = *(${field_type}*) ${tmp}.data;')
					}
				}
				if field.has_default_expr {
					dec.writeln('\t} else {')
					default_str := g.expr_string_opt(field.typ, field.default_expr)
					if default_str.count(';\n') > 1 {
						dec.writeln(default_str.all_before_last('\n'))
						dec.writeln('\t\t${prefix}${op}${c_name(field.name)} = ${default_str.all_after_last('\n')};')
					} else {
						dec.writeln('\t\t${prefix}${op}${c_name(field.name)} = ${default_str};')
					}
				}
				dec.writeln('\t}')
			}
		}
		if skip_embed {
			continue
		}
		// Encoding
		mut enc_name := js_enc_name(field_type)
		prefix_enc := if utyp.has_flag(.option) {
			'(*(${g.base_type(utyp)}*)val${embed_member}.data)'
		} else {
			'val${embed_member}'
		}
		is_option := field.typ.has_flag(.option)
		indent := if is_option { '\t\t' } else { '\t' }
		if is_option {
			enc.writeln('\tif (${prefix_enc}${op}${c_name(field.name)}.state != 2) {')
		}
		if is_omit_empty {
			if field.typ.has_flag(.option) {
				enc.writeln('${indent}if (${prefix_enc}${op}${c_name(field.name)}.state != 2)')
			} else if field.typ == ast.string_type {
				enc.writeln('${indent}if (${prefix_enc}${op}${c_name(field.name)}.len != 0)')
			} else {
				if !field.typ.is_ptr()
					&& field_sym.kind in [.alias, .sum_type, .map, .array, .struct] {
					ptr_typ := g.equality_fn(field.typ)
					if field_sym.kind == .alias {
						enc.writeln('${indent}if (!${ptr_typ}_alias_eq(${prefix_enc}${op}${c_name(field.name)}, ${g.type_default(field.typ)}))')
					} else if field_sym.kind == .sum_type {
						enc.writeln('${indent}if (${prefix_enc}${op}${c_name(field.name)}._typ != 0)')
					} else if field_sym.kind == .map {
						enc.writeln('${indent}if (!${ptr_typ}_map_eq(${prefix_enc}${op}${c_name(field.name)}, ${g.type_default(field.typ)}))')
					} else if field_sym.kind == .array {
						enc.writeln('${indent}if (!${ptr_typ}_arr_eq(${prefix_enc}${op}${c_name(field.name)}, ${g.type_default(field.typ)}))')
					} else if field_sym.kind == .struct {
						enc.writeln('${indent}if (!${ptr_typ}_struct_eq(${prefix_enc}${op}${c_name(field.name)}, ${g.type_default(field.typ)}))')
					}
				} else {
					enc.writeln('${indent}if (${prefix_enc}${op}${c_name(field.name)} != ${g.type_default(field.typ)})')
				}
			}
		}
		if !is_js_prim(field_type) {
			if field_sym.kind == .alias {
				ainfo := field_sym.info as ast.Alias
				if field.typ.has_flag(.option) {
					enc_name = js_enc_name(g.styp(ainfo.parent_type.set_flag(.option)))
				} else {
					enc_name = js_enc_name(g.styp(ainfo.parent_type))
				}
			}
		}
		if field_sym.kind == .enum {
			if g.is_enum_as_int(field_sym) {
				if field.typ.has_flag(.option) {
					enc.writeln('${indent}\tcJSON_AddItemToObject(o, "${name}", json__encode_u64(*${prefix_enc}${op}${c_name(field.name)}.data));\n')
				} else {
					enc.writeln('${indent}\tcJSON_AddItemToObject(o, "${name}", json__encode_u64(${prefix_enc}${op}${c_name(field.name)}));\n')
				}
			} else {
				if field.typ.has_flag(.option) {
					enc.writeln('${indent}\t{')
					enc.writeln('${indent}\t\tcJSON *enum_val;')
					g.gen_enum_to_str(field.typ, field_sym, '*(${g.base_type(field.typ)}*)${prefix_enc}${op}${c_name(field.name)}.data',
						'enum_val', '${indent}\t\t', mut enc)
					enc.writeln('${indent}\t\tcJSON_AddItemToObject(o, "${name}", enum_val);')
					enc.writeln('${indent}\t}')
				} else {
					enc.writeln('${indent}\t{')
					enc.writeln('${indent}\t\tcJSON *enum_val;')
					g.gen_enum_to_str(field.typ, field_sym, '${prefix_enc}${op}${c_name(field.name)}',
						'enum_val', '${indent}\t\t', mut enc)
					enc.writeln('${indent}\t\tcJSON_AddItemToObject(o, "${name}", enum_val);')
					enc.writeln('${indent}\t}')
				}
			}
		} else {
			if field_sym.name == 'time.Time' {
				// time struct requires special treatment
				// it has to be encoded as a unix timestamp number
				if is_option {
					enc.writeln('${indent}cJSON_AddItemToObject(o, "${name}", json__encode_u64((*(${g.base_type(field.typ)}*)(${prefix_enc}${op}${c_name(field.name)}.data)).__v_unix));')
				} else {
					enc.writeln('${indent}cJSON_AddItemToObject(o, "${name}", json__encode_u64(${prefix_enc}${op}${c_name(field.name)}.__v_unix));')
				}
			} else {
				if !field.typ.is_any_kind_of_pointer() {
					if field_sym.kind == .alias && field.typ.has_flag(.option) {
						parent_type := g.table.unaliased_type(field.typ).set_flag(.option)
						enc.writeln('${indent}\tcJSON_AddItemToObject(o, "${name}", ${enc_name}(*(${g.styp(parent_type)}*)&${prefix_enc}${op}${c_name(field.name)}));')
					} else {
						enc.writeln('${indent}\tcJSON_AddItemToObject(o, "${name}", ${enc_name}(${prefix_enc}${op}${c_name(field.name)}));')
					}
				} else {
					arg_prefix := if field.typ.is_ptr() { '' } else { '*' }
					sptr_value := '${prefix_enc}${op}${c_name(field.name)}'
					if !field.typ.has_flag(.option) {
						enc.writeln('${indent}if (${sptr_value} != 0) {')
						enc.writeln('${indent}\tcJSON_AddItemToObject(o, "${name}", ${enc_name}(${arg_prefix}${sptr_value}));')
						enc.writeln('${indent}}\n')
					} else {
						enc.writeln('${indent}cJSON_AddItemToObject(o, "${name}", ${enc_name}(${arg_prefix}${sptr_value}));')
					}
				}
			}
		}

		if is_option {
			if is_json_null {
				enc.writeln('\t} else {')
				enc.writeln('\t\tcJSON_AddItemToObject(o, "${name}", cJSON_CreateNull());')
			}
			enc.writeln('\t}')
		}
	}
}

fn gen_js_get(styp string, tmp string, name string, mut dec strings.Builder, is_required bool) {
	dec.writeln('\tcJSON *jsonroot_${tmp} = js_get(root, "${name}");')
	if is_required {
		dec.writeln('\tif (jsonroot_${tmp} == 0) {')
		dec.writeln('\t\treturn (${result_name}_${styp}){ .is_error = true, .err = _v_error(_S("expected field \'${name}\' is missing")), .data = {0} };')
		dec.writeln('\t}')
	}
}

fn gen_js_get_opt(dec_name string, field_type string, styp string, tmp string, name string, mut dec strings.Builder,
	is_required bool) {
	gen_js_get(styp, tmp, name, mut dec, is_required)
	value_field_type := field_type.replace('*', '_ptr')
	dec.writeln('\t${result_name}_${value_field_type.replace('*', '_ptr')} ${tmp} = {0};')
	dec.writeln('\tif (jsonroot_${tmp}) {')
	// dec.writeln('\t\tif (jsonroot_${tmp}->type == cJSON_NULL) { puts("${name} IS JSON_NULL"); }')
	dec.writeln('\t\t${tmp} = ${dec_name}(jsonroot_${tmp});')
	dec.writeln('\t\tif (${tmp}.is_error) {')
	dec.writeln('\t\t\treturn (${result_name}_${styp}){ .is_error = true, .err = ${tmp}.err, .data = {0} };')
	dec.writeln('\t\t}')
	dec.writeln('\t}')
}

fn js_enc_name(typ string) string {
	mut suffix := typ.replace('*', '_ptr')
	if typ == 'i32' {
		suffix = typ.replace('i32', 'int')
	}
	name := 'json__encode_${suffix}'
	return util.no_dots(name)
}

fn js_dec_name(typ string) string {
	mut suffix := typ.replace('*', '_ptr')
	if typ == 'i32' {
		suffix = typ.replace('i32', 'int')
	}
	name := 'json__decode_${suffix}'
	return util.no_dots(name)
}

fn is_js_prim(typ string) bool {
	return typ in ['int', 'rune', 'string', 'bool', 'f32', 'f64', 'i8', 'i16', 'i32', 'i64', 'u8',
		'u16', 'u32', 'u64', 'byte']
}

fn (mut g Gen) decode_array(utyp ast.Type, value_type ast.Type, fixed_array_size int, ret_styp string) string {
	styp := g.styp(value_type)
	fn_name := js_dec_name(styp)
	noscan := g.check_noscan(value_type)

	mut res_str := ''
	mut array_free_str := ''
	mut fixed_array_idx := ''
	mut fixed_array_idx_increment := ''
	mut array_element_assign := ''
	is_array_fixed_val := g.table.final_sym(value_type).kind == .array_fixed
	if utyp.has_flag(.option) {
		if fixed_array_size > -1 {
			fixed_array_idx += 'int fixed_array_idx = 0;'
			array_element_assign += '((${styp}*)res.data)[fixed_array_idx] = val;'
			fixed_array_idx_increment += 'fixed_array_idx++; res.state = 0;'
		} else {
			array_element_assign += 'array_push${noscan}((array*)&res.data, &val);'
			res_str += '_option_ok(&(${g.base_type(utyp)}[]) { __new_array${noscan}(0, 0, sizeof(${styp})) }, (${option_name}*)&res, sizeof(${g.base_type(utyp)}));'
			array_free_str += 'array_free(&res.data);'
		}
	} else {
		if is_array_fixed_val {
			fixed_array_idx += 'int fixed_array_idx = 0;'
			array_element_assign += 'memcpy(res[fixed_array_idx], val, sizeof(${styp}));'
			fixed_array_idx_increment += 'fixed_array_idx++;'
		} else if fixed_array_size > -1 {
			fixed_array_idx += 'int fixed_array_idx = 0;'
			array_element_assign += 'res[fixed_array_idx] = val;'
			fixed_array_idx_increment += 'fixed_array_idx++;'
		} else {
			array_element_assign += 'array_push${noscan}((array*)&res, &val);'
			res_str += 'res = __new_array${noscan}(0, 0, sizeof(${styp}));'
			array_free_str += 'array_free(&res);'
		}
	}

	mut s := ''
	if is_js_prim(styp) {
		s = '${styp} val = ${fn_name}((cJSON *)jsval); '
	} else if is_array_fixed_val {
		s = '
		${result_name}_${styp.replace('*', '_ptr')} val2 = ${fn_name} ((cJSON *)jsval);
		if(val2.is_error) {
			${array_free_str}
			return *(${result_name}_${ret_styp}*)&val2;
		}
		${styp} val;
		memcpy(&val, (${styp}*)val2.data, sizeof(${styp}));'
	} else {
		s = '
		${result_name}_${styp.replace('*', '_ptr')} val2 = ${fn_name} ((cJSON *)jsval);
		if(val2.is_error) {
			${array_free_str}
			return *(${result_name}_${ret_styp}*)&val2;
		}
		${styp} val = *(${styp}*)val2.data;
'
	}

	return '
	if(root && !cJSON_IsArray(root) && !cJSON_IsNull(root)) {
		return (${result_name}_${ret_styp}){.is_error = true, .err = _v_error(string__plus(_S("Json element is not an array: "), json__json_print(root))), .data = {0}};
	}
	${res_str}
	const cJSON *jsval = NULL;
	${fixed_array_idx}
	cJSON_ArrayForEach(jsval, root)
	{
		${s}
		${array_element_assign}
		${fixed_array_idx_increment}
	}
'
}

fn (mut g Gen) encode_array(utyp ast.Type, value_type ast.Type, fixed_array_size int) string {
	styp := g.styp(value_type)
	fn_name := js_enc_name(styp)

	mut data_str := ''
	mut size_str := ''

	if utyp.has_flag(.option) {
		data_str, size_str = if fixed_array_size > -1 {
			// fixed array
			'(${styp}*)(*(${g.base_type(utyp)}*)val.data)', '${fixed_array_size}'
		} else {
			'(${styp}*)(*(${g.base_type(utyp)}*)val.data).data', '(*(${g.base_type(utyp)}*)val.data).len'
		}
	} else {
		data_str, size_str = if fixed_array_size > -1 {
			// fixed array
			'(${styp}*)val', '${fixed_array_size}'
		} else {
			'(${styp}*)val.data', 'val.len'
		}
	}

	return '
	o = cJSON_CreateArray();
	for (int i = 0; i < ${size_str}; i++){
		cJSON_AddItemToArray(o, ${fn_name}( (${data_str})[i] ));
	}
'
}

fn (mut g Gen) decode_map(utyp ast.Type, key_type ast.Type, value_type ast.Type, ustyp string) string {
	styp := g.styp(key_type)
	mut styp_v := g.styp(value_type)
	ret_styp := styp_v.replace('*', '_ptr')
	key_type_symbol := g.table.sym(key_type)
	hash_fn, key_eq_fn, clone_fn, free_fn := g.map_fn_ptrs(key_type_symbol)
	fn_name_v := js_dec_name(styp_v)
	mut s := ''
	if is_js_prim(styp_v) {
		s = '${styp_v} val = ${fn_name_v} (js_get(root, jsval->string));'
	} else {
		s = '
		${result_name}_${ret_styp} val2 = ${fn_name_v} (js_get(root, jsval->string));
		if(val2.is_error) {
			map_free(&res);
			return *(${result_name}_${ustyp}*)&val2;
		}
		${styp_v} val = *(${styp_v}*)val2.data;
'
	}

	if utyp.has_flag(.option) {
		return '
		if(!cJSON_IsObject(root) && !cJSON_IsNull(root)) {
			return (${result_name}_${ustyp}){ .is_error = true, .err = _v_error(string__plus(_S("Json element is not an object: "), json__json_print(root))), .data = {0}};
		}
		_option_ok(&(${g.base_type(utyp)}[]) { new_map(sizeof(${styp}), sizeof(${styp_v}), ${hash_fn}, ${key_eq_fn}, ${clone_fn}, ${free_fn}) }, (${option_name}*)&res, sizeof(${g.base_type(utyp)}));
		cJSON *jsval = NULL;
		cJSON_ArrayForEach(jsval, root)
		{
			${s}
			string key = tos2((byteptr)jsval->string);
			map_set((map*)res.data, &key, &val);
		}
'
	} else {
		return '
		if(!cJSON_IsObject(root) && !cJSON_IsNull(root)) {
			return (${result_name}_${ustyp}){ .is_error = true, .err = _v_error(string__plus(_S("Json element is not an object: "), json__json_print(root))), .data = {0}};
		}
		res = new_map(sizeof(${styp}), sizeof(${styp_v}), ${hash_fn}, ${key_eq_fn}, ${clone_fn}, ${free_fn});
		cJSON *jsval = NULL;
		cJSON_ArrayForEach(jsval, root)
		{
			${s}
			string key = tos2((byteptr)jsval->string);
			map_set(&res, &key, &val);
		}
'
	}
}

fn (mut g Gen) encode_map(utyp ast.Type, key_type ast.Type, value_type ast.Type) string {
	styp := g.styp(key_type)
	styp_v := g.styp(value_type)
	fn_name_v := js_enc_name(styp_v)
	zero := g.type_default(value_type)
	keys_tmp := g.new_tmp_var()
	mut key := 'string key = '
	if key_type.is_string() {
		key += '((${styp}*)${keys_tmp}.data)[i];'
	} else {
		// key += '${styp}_str((($styp*)${keys_tmp}.data)[i]);'
		verror('json: encode only maps with string keys')
	}
	if utyp.has_flag(.option) {
		return '
		o = cJSON_CreateObject();
		Array_${styp} ${keys_tmp} = map_keys((map*)val.data);
		for (int i = 0; i < ${keys_tmp}.len; ++i) {
			${key}
			cJSON_AddItemToObject(o, (char*) key.str, ${fn_name_v} ( *(${styp_v}*) map_get((map*)val.data, &key, &(${styp_v}[]) { ${zero} } ) ) );
		}
		array_free(&${keys_tmp});
'
	} else {
		return '
		o = cJSON_CreateObject();
		Array_${styp} ${keys_tmp} = map_keys(&val);
		for (int i = 0; i < ${keys_tmp}.len; ++i) {
			${key}
			cJSON_AddItemToObject(o, (char*) key.str, ${fn_name_v} ( *(${styp_v}*) map_get(&val, &key, &(${styp_v}[]) { ${zero} } ) ) );
		}
		array_free(&${keys_tmp});
'
	}
}

@[noreturn]
fn verror_suggest_json_no_inline_sumtypes(sumtype_name string, type_name1 string, type_name2 string) {
	verror('json: can not decode `${sumtype_name}` sumtype, too many numeric types (conflict of `${type_name1}` and `${type_name2}`), you can try to use alias for `${type_name2}` or compile v with `json_no_inline_sumtypes` flag')
}
