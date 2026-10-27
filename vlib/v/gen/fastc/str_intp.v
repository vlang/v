module fastc

import v.flat
import strings
import v.token

const fastc_type_ptrdiff_t = flat.source_type_id('ptrdiff_t')

// fastc_default_struct_str_name returns the C name of an auto-generated default `str()`
// for the struct/union `c_type` — V's `Type{\n    field: value\n ...}` format — used when
// a struct with no user `str()` is interpolated. The function (and any nested struct
// fields' defaults) is generated into g.spawn_helpers on first use. Returns none if
// `c_type` is not a struct/union.
fn (g &Parser) can_generate_default_struct_str(c_type u32) bool {
	key := g.semantic_type_key(fastc_trim_pointer_type(c_type))
	return flat.source_type_name(key) in g.declared_kinds && g.declared_kinds[flat.source_type_name(key)] in [
		.struct_,
		.union_,
	]
}

fn (mut g Parser) fastc_default_struct_str_name(c_type u32) ?string {
	key := g.semantic_type_key(c_type)
	// A missing key defaults to `.struct_` (the zero enum value), so require it to be
	// present before treating `c_type` as a struct.
	if flat.source_type_name(key) !in g.declared_kinds || g.declared_kinds[flat.source_type_name(key)] !in [
		.struct_,
		.union_,
	] {
		return none
	}
	c_name := fastc_c_declared_type_name(flat.source_type_name(key))
	fn_name := 'v_fastc_default_str_${c_name}'
	if fn_name in g.spawn_helpers {
		return fn_name
	}
	// Reserve the name first so a self-referential field does not recurse forever.
	g.spawn_helpers[fn_name] = ''
	g.protos.writeln('string ${fn_name}(${c_name} it);')
	display := fastc_default_struct_str_display_name(flat.source_type_name(key), c_name)
	mut body := 'string ${fn_name}(${c_name} it) {\n'
	body += '\tstring res = _S("${display}{\\n");\n'
	for field in g.struct_field_info[c_name] {
		c_field := fastc_c_identifier(field.name)
		mut formatted := ''
		mut needs_indent := false
		if field.option_value_type != 0 {
			option_helper := g.fastc_option_str_name(field.option_value_type) or { '' }
			if option_helper != '' {
				formatted = '${option_helper}(it.${c_field})'
				needs_indent = g.fastc_struct_field_str_needs_indent(field.option_value_type)
			}
		}
		if formatted == '' {
			formatted = g.fastc_struct_field_str_expression('it.${c_field}', field.typ) or {
				'_S("<${field.typ_text()}>")'
			}
			needs_indent = g.fastc_struct_field_str_needs_indent(field.typ)
		}
		if needs_indent {
			formatted = '${g.fastc_string_indent_helper()}(${formatted})'
		}
		body += '\tres = builtin__string_plus(res, _S("    ${field.name}: "));\n'
		body += '\tres = builtin__string_plus(res, ${formatted});\n'
		body += '\tres = builtin__string_plus(res, _S("\\n"));\n'
	}
	body += '\tres = builtin__string_plus(res, _S("}"));\n'
	body += '\treturn res;\n}'
	g.spawn_helpers[fn_name] = body
	return fn_name
}

// fastc_default_struct_str_display_name spells a struct the way V's default `str()` does:
// with its module prefix (`mymod.Pt`), except for `main` and `builtin` types.
fn fastc_default_struct_str_display_name(source_name string, c_name string) string {
	if !source_name.contains('.') {
		return c_name.all_after_last('__')
	}
	for prefix in ['main.', 'builtin.'] {
		if source_name.starts_with(prefix) {
			return source_name[prefix.len..]
		}
	}
	return source_name
}

// fastc_string_indent_helper emits, once per program, a helper that indents every line after
// the first by four spaces, as V's default `str()` does to a nested value's text. It is plain C,
// because FastC drops `string.replace` from programs that never call it.
fn (mut g Parser) fastc_string_indent_helper() string {
	name := 'v_fastc_string_indent'
	if name !in g.spawn_helpers {
		g.protos.writeln('string ${name}(string s);')
		g.spawn_helpers[name] = fastc_string_indent_c
	}
	return name
}

// The C for fastc_string_indent_helper. Character code 10 is `\n`, 32 is a space.
const fastc_string_indent_c = r'string v_fastc_string_indent(string s) {
	size_t lines = 0;
	for (size_t i = 0; i < (size_t)s.len; i++) {
		if (s.str[i] == 10) lines++;
	}
	if (lines == 0) return s;
	size_t len = (size_t)s.len + 4 * lines;
	byteptr result = malloc(len + 1);
	if (result == NULL) return s;
	size_t j = 0;
	for (size_t i = 0; i < (size_t)s.len; i++) {
		result[j++] = s.str[i];
		if (s.str[i] == 10) {
			memset(result + j, 32, 4);
			j += 4;
		}
	}
	result[len] = 0;
	return (string){.str = result, .len = len, .is_lit = 0};
}'

// fastc_struct_field_str_needs_indent reports whether a default `str()` field value can span
// several lines (a nested struct, array, map or sum type without its own `str()`), so its
// text is indented one level deeper, as V does. Strings, primitives, enums, fixed arrays and
// custom `str()` results keep their text.
fn (mut g Parser) fastc_struct_field_str_needs_indent(field_c_type u32) bool {
	ft := fastc_normalize_inferred_type(field_c_type)
	if ft == flat.source_type_string {
		return false
	}
	base_id := fastc_trim_pointer_type(ft)
	base := flat.source_type_name(base_id)
	if _ := fastc_fixed_array_element_type(base_id) {
		return false
	}
	if flat.source_type_query(base_id).has(.c_array_prefix) || flat.source_type_query(base_id).has(.c_map_prefix) || base in g.sum_types {
		return true
	}
	key := g.semantic_type_key(ft)
	if '${flat.source_type_name(key)}.str' in g.functions {
		return false
	}
	return flat.source_type_name(key) in g.declared_kinds
		&& g.declared_kinds[flat.source_type_name(key)] in [.struct_, .union_]
}

// fastc_fixed_array_str_name returns a helper that renders a fixed array in V's
// `[item, item]` form. Fixed-array struct fields use raw C array storage, while
// locals use FastC's wrapper; the caller passes either representation as an element
// pointer, so the helper itself is shared by both forms.
fn (mut g Parser) fastc_fixed_array_str_name(c_type u32) ?string {
	array_type_id := fastc_trim_pointer_type(fastc_normalize_inferred_type(c_type))
	array_type := flat.source_type_name(array_type_id)
	element_type := (fastc_fixed_array_element_type(array_type_id) or { return none })
	length_source := fastc_fixed_array_length_id(array_type_id) or { return none }
	length := g.fixed_array_length_value(length_source) or { return none }
	fn_name := 'v_fastc_fixed_array_str_${fastc_composite_type_part(array_type)}'
	if fn_name in g.spawn_helpers {
		return fn_name
	}
	// Reserve the name before formatting elements so nested formatting cannot
	// register the same helper recursively.
	g.spawn_helpers[fn_name] = ''
	g.protos.writeln('string ${fn_name}(${flat.source_type_name(element_type)} *it);')
	mut body := 'string ${fn_name}(${flat.source_type_name(element_type)} *it) {\n'
	body += '\tstring res = _S("[");\n'
	for index in 0 .. length {
		formatted := g.fastc_struct_field_str_expression('it[${index}]', element_type) or {
			g.spawn_helpers.delete(fn_name)
			return none
		}
		if index > 0 {
			body += '\tres = builtin__string_plus(res, _S(", "));\n'
		}
		body += '\tres = builtin__string_plus(res, ${formatted});\n'
	}
	body += '\treturn builtin__string_plus(res, _S("]"));\n}'
	g.spawn_helpers[fn_name] = body
	return fn_name
}

// fastc_array_str_name returns a helper that renders a dynamic array using the
// element type retained in FastC's `Array_T` spelling.
// fastc_sumtype_str_helper generates a `str()` helper for a boxed sum type: it
// switches on the runtime `_typ` tag and formats the concrete variant with the
// same per-type logic as struct-field formatting. Returns none for a non-sum type
// or when any variant cannot be formatted.
fn (mut g Parser) fastc_sumtype_str_helper(c_type u32) ?string {
	base_id := fastc_trim_pointer_type(fastc_normalize_inferred_type(c_type))
	base := flat.source_type_name(base_id)
	if base !in g.sum_types {
		return none
	}
	fn_name := 'v_fastc_sumtype_str_${base}'
	if fn_name in g.spawn_helpers {
		return fn_name
	}
	g.spawn_helpers[fn_name] = ''
	g.protos.writeln('string ${fn_name}(${base} it);')
	mut body := 'string ${fn_name}(${base} it) {\n\tswitch (it._typ) {\n'
	variant_ids := g.sum_type_variant_keys(base_id)
	// V prints a sum value as `Sum(payload)`, with the sum type's short name, as the
	// C backend's transform does: a string payload is quoted, a rune one is in
	// backticks, and any other alias variant keeps its own wrapper (`Res(Ints([1]))`).
	sum_display := flat.source_type_name(flat.source_type_syntax(base_id).last_double_underscore_suffix())
	for variant_id in variant_ids {
		variant := flat.source_type_name(variant_id)
		payload := '(*(${variant} *)it._object)'
		variant_base := g.underlying_alias_type(variant_id)
		mut formatted := g.fastc_struct_field_str_expression(payload, variant_base) or {
			g.spawn_helpers.delete(fn_name)
			return none
		}
		if variant_base == flat.source_type_rune {
			formatted = fastc_wrapped_str_expression('`', formatted, '`')
		} else if variant_base != variant_id && variant_base != flat.source_type_string {
			alias_display := flat.source_type_name(flat.source_type_syntax(variant_id).last_double_underscore_suffix())
			formatted = fastc_wrapped_str_expression('${alias_display}(', formatted, ')')
		}
		formatted = fastc_wrapped_str_expression('${sum_display}(', formatted, ')')
		body += '\t\tcase __v_typeid_${variant}: return ${formatted};\n'
	}
	body += '\t}\n\treturn _S("unknown");\n}'
	g.spawn_helpers[fn_name] = body
	return fn_name
}

// fastc_wrapped_str_expression returns the C string expression `prefix + value + suffix`.
fn fastc_wrapped_str_expression(prefix string, value string, suffix string) string {
	return 'builtin__string_plus(builtin__string_plus(_S("${prefix}"), ${value}), _S("${suffix}"))'
}

// fastc_sumtype_type_name_helper generates a `type_name()` helper for a boxed
// sum type or interface: it switches on the runtime `_typ` tag and returns the
// variant's dotted name (`v.ast.CallExpr`). Returns none for non-boxed types.
fn (mut g Parser) fastc_sumtype_type_name_helper(c_type u32) ?string {
	base_id := fastc_trim_pointer_type(fastc_normalize_inferred_type(c_type))
	base := flat.source_type_name(base_id)
	if base !in g.sum_types && g.declared_kinds[flat.source_type_name(g.semantic_type_key(base_id))] != .interface_ {
		return none
	}
	fn_name := 'v_fastc_typename_${base}'
	if fn_name in g.spawn_helpers {
		return fn_name
	}
	g.spawn_helpers[fn_name] = ''
	g.protos.writeln('string ${fn_name}(${base} it);')
	mut body := 'string ${fn_name}(${base} it) {\n\tswitch (it._typ) {\n'
	variant_ids := g.sum_type_variant_keys(base_id)
	for variant_id in variant_ids {
		variant := flat.source_type_name(variant_id)
		display := flat.source_type_name(flat.source_type_syntax(variant_id).doubleunderscore_dotted())
		body += '\t\tcase __v_typeid_${variant}: return _S("${display}");\n'
	}
	body += '\t}\n\treturn _S("unknown");\n}'
	g.spawn_helpers[fn_name] = body
	return fn_name
}

fn (mut g Parser) fastc_array_str_name(c_type u32) ?string {
	array_type_id := fastc_trim_pointer_type(fastc_normalize_inferred_type(c_type))
	array_type := flat.source_type_name(array_type_id)
	if !flat.source_type_query(array_type_id).has(.c_array_prefix) {
		return none
	}
	element_type := (fastc_array_element_type(array_type_id) or { return none })
	fn_name := 'v_fastc_array_str_${fastc_composite_type_part(array_type)}'
	if fn_name in g.spawn_helpers {
		return fn_name
	}
	g.spawn_helpers[fn_name] = ''
	g.protos.writeln('string ${fn_name}(${array_type} it);')
	index := '__v_fastc_array_str_index'
	element := '(*(${flat.source_type_name(element_type)} *)builtin__array_get(it, ${index}))'
	formatted := g.fastc_struct_field_str_expression(element, element_type) or {
		g.spawn_helpers.delete(fn_name)
		return none
	}
	mut body := 'string ${fn_name}(${array_type} it) {\n'
	body += '\tstring res = _S("[");\n'
	body += '\tfor (int ${index} = 0; ${index} < it.len; ${index}++) {\n'
	body += '\t\tif (${index} > 0) res = builtin__string_plus(res, _S(", "));\n'
	body += '\t\tres = builtin__string_plus(res, ${formatted});\n'
	body += '\t}\n'
	body += '\treturn builtin__string_plus(res, _S("]"));\n}'
	g.spawn_helpers[fn_name] = body
	return fn_name
}

fn (mut g Parser) fastc_map_str_name(c_type u32) ?string {
	map_type_id := fastc_trim_pointer_type(fastc_normalize_inferred_type(c_type))
	map_type := flat.source_type_name(map_type_id)
	key_type_source_id, value_type_source_id := g.map_key_value_types(map_type_id)?
	key_type := key_type_source_id
	value_type := value_type_source_id

	fn_name := 'v_fastc_map_str_${map_type}'
	if fn_name in g.spawn_helpers {
		return fn_name
	}
	g.spawn_helpers[fn_name] = ''
	g.protos.writeln('string ${fn_name}(${map_type} it);')
	key_string := g.fastc_struct_field_str_expression('__vf_key', key_type) or {
		g.spawn_helpers.delete(fn_name)
		return none
	}
	value_string := g.fastc_struct_field_str_expression('(*(${flat.source_type_name(value_type)} *)builtin__map_get_check((map *)&it, &__vf_key))', value_type) or {
		g.spawn_helpers.delete(fn_name)
		return none
	}
	g.spawn_helpers[fn_name] = 'string ${fn_name}(${map_type} it) {\n\tstring res = _S("{");\n\tarray keys = builtin__map_keys((map *)&it);\n\tfor (int i = 0; i < keys.len; i++) {\n\t\t${flat.source_type_name(key_type)} __vf_key = *(${flat.source_type_name(key_type)} *)builtin__array_get(keys, i);\n\t\tif (i) res = builtin__string_plus(res, _S(", "));\n\t\tres = builtin__string_plus(res, ${key_string});\n\t\tres = builtin__string_plus(res, _S(": "));\n\t\tres = builtin__string_plus(res, ${value_string});\n\t}\n\treturn builtin__string_plus(res, _S("}"));\n}'
	return fn_name
}

// fastc_struct_field_str_expression returns a C string expression that renders a struct
// field value `value_c` of C type `field_c_type` for the default `str()`: a string is
// single-quoted, a type with a `str()` method (user or builtin, e.g. `int`) calls it, an
// enum uses its name table, and a nested struct recurses. Returns none if unrenderable.
fn (mut g Parser) fastc_struct_field_str_expression(value_c string, field_c_type u32) ?string {
	ft := fastc_normalize_inferred_type(field_c_type)
	if pointer_helper := g.fastc_pointer_str_name(ft) {
		return '${pointer_helper}(${value_c})'
	}
	if ft == flat.source_type_string {
		// A single-quote C literal, built without `\'`/`\"` escapes (a self-host str_intp
		// hazard) by concatenating literal pieces.
		q := '_S("' + "'" + '")'
		return 'builtin__string_plus(builtin__string_plus(${q}, ${value_c}), ${q})'
	}
	if element_type_source_id := fastc_fixed_array_element_type(fastc_trim_pointer_type(ft)) {
		element_type := element_type_source_id

		if helper := g.fastc_fixed_array_str_name(ft) {
			return '${helper}((${flat.source_type_name(element_type)} *)(${value_c}))'
		}
	}
	if flat.source_type_query(ft).has(.c_array_prefix) {
		if helper := g.fastc_array_str_name(ft) {
			value := if flat.source_type_query(ft).has(.c_pointer_suffix) {
				'*(${value_c})'
			} else {
				value_c
			}
			return '${helper}(${value})'
		}
	}
	if flat.source_type_query(ft).has(.c_map_prefix) {
		if helper := g.fastc_map_str_name(ft) {
			value := if flat.source_type_query(ft).has(.c_pointer_suffix) {
				'*(${value_c})'
			} else {
				value_c
			}
			return '${helper}(${value})'
		}
	}
	if flat.source_type_name(fastc_trim_pointer_type(ft)) in g.sum_types {
		if helper := g.fastc_sumtype_str_helper(ft) {
			value := if flat.source_type_query(ft).has(.c_pointer_suffix) {
				'*(${value_c})'
			} else {
				value_c
			}
			return '${helper}(${value})'
		}
	}
	key := g.semantic_type_key(ft)
	method_key := '${flat.source_type_name(key)}.str'
	if method_signature := g.functions[method_key] {
		receiver := if flat.source_type_query(method_signature.parameter_types.ids[0]).has(.c_pointer_suffix) && !flat.source_type_query(ft).has(.c_pointer_suffix) {
			'&(${value_c})'
		} else if !flat.source_type_query(method_signature.parameter_types.ids[0]).has(.c_pointer_suffix) && flat.source_type_query(ft).has(.c_pointer_suffix) {
			'*(${value_c})'
		} else {
			value_c
		}
		method_c_name := g.c_function_name_or(method_key, fastc_method_c_name(method_signature.module_name, fastc_c_declared_type_name(flat.source_type_name(key)), 'str'))
		return '${method_c_name}(${receiver})'
	}
	if g.declared_kinds[flat.source_type_name(key)] == .enum_ {
		return 'v_fastc_enum_str_${fastc_c_declared_type_name(flat.source_type_name(key))}(${value_c})'
	}
	if nested := g.fastc_default_struct_str_name(ft) {
		return '${nested}(${value_c})'
	}
	return none
}

// fastc_pointer_str_name returns a helper that renders a `&T` value of C type
// `pointer_c_type` the way V's default `str()` does: `&` before the text of the
// pointee, or `&nil` for a null pointer. Returns none for raw pointers (`voidptr`,
// `&u8`, `&char`), pointers to pointers, and pointees that cannot be rendered.
fn (mut g Parser) fastc_pointer_str_name(pointer_c_type_id u32) ?string {
	if !flat.source_type_query(pointer_c_type_id).has(.c_pointer_suffix) {
		return none
	}
	pointee_id := flat.source_type_syntax(flat.source_type_syntax(pointer_c_type_id).c_pointer_base()).trimmed()
	if pointee_id == 0 || flat.source_type_query(pointee_id).has(.c_pointer_suffix) || pointee_id in [
		flat.source_type_id('void'),
		flat.source_type_id('voidptr'),
		flat.source_type_id('u8'),
		flat.source_type_id('char'),
		flat.source_type_id('byte'),
	] {
		return none
	}
	fn_name := 'v_fastc_ptr_str_${fastc_composite_type_part(flat.source_type_name(pointer_c_type_id))}'
	if fn_name in g.spawn_helpers {
		return fn_name
	}
	// Reserve the name first so a self-referential pointee does not recurse forever.
	g.spawn_helpers[fn_name] = ''
	pointee_text := g.fastc_struct_field_str_expression('(*it)', pointee_id) or {
		g.spawn_helpers.delete(fn_name)
		return none
	}
	c_type := fastc_output_c_type(flat.source_type_name(pointee_id)) + '*'
	g.protos.writeln('string ${fn_name}(${c_type} it);')
	mut body := 'string ${fn_name}(${c_type} it) {\n'
	body += '\tif (it == 0) {\n\t\treturn _S("&nil");\n\t}\n'
	body += '\treturn builtin__string_plus(_S("&"), ${pointee_text});\n}'
	g.spawn_helpers[fn_name] = body
	return fn_name
}

// fastc_option_str_name returns a helper that renders an option whose value has
// the type `value_type` the way V does: `Option(value)`, or `Option(none)`.
fn (mut g Parser) fastc_option_str_name(value_type u32) ?string {
	if value_type == 0 {
		return none
	}
	value_c_type := flat.source_type_name(value_type)
	fn_name := 'v_fastc_option_str_${fastc_composite_type_part(fastc_output_c_type(value_c_type))}'
	if fn_name in g.spawn_helpers {
		return fn_name
	}
	g.spawn_helpers[fn_name] = ''
	value_text := g.fastc_struct_field_str_expression(g.option_value_expression(value_type,
		'it'), value_type) or {
		g.spawn_helpers.delete(fn_name)
		return none
	}
	g.protos.writeln('string ${fn_name}(Option it);')
	mut body := 'string ${fn_name}(Option it) {\n'
	body += '\tif (it.state != 0) {\n\t\treturn _S("Option(none)");\n\t}\n'
	body += '\treturn ${fastc_wrapped_str_expression('Option(', value_text, ')')};\n}'
	g.spawn_helpers[fn_name] = body
	return fn_name
}

// fastc_reference_str_expression renders a top-level `&T` value (an interpolated
// or printed pointer) the way V does: `&` before the text of a struct, array, map,
// sum type or custom `str()` pointee, and the plain name for an enum pointee.
// Returns none for `mut` locals and smartcasts (C pointers that V sees as values),
// and for pointers to primitives, which V prints as addresses.
fn (mut g Parser) fastc_reference_str_expression(value string, value_type_id u32, value_tokens []FastcExpressionToken) ?string {
	if !flat.source_type_query(value_type_id).has(.c_pointer_suffix) || flat.source_type_query(value_type_id).has(.c_double_pointer_suffix) {
		return none
	}
	if g.is_reference_local_base(value_tokens, value_type_id)
		|| g.expression_uses_member_smartcast(value_tokens) {
		return none
	}
	pointee_id := flat.source_type_syntax(flat.source_type_syntax(value_type_id).c_pointer_base()).trimmed()
	if fastc_primitive_c_type_id(pointee_id) != none {
		return none
	}
	if g.declared_kinds[flat.source_type_name(g.semantic_type_key(pointee_id))] == .enum_ {
		return g.fastc_struct_field_str_expression('(*(${value}))', pointee_id)
	}
	helper := g.fastc_pointer_str_name(value_type_id) or { return none }
	return '${helper}(${value})'
}

fn (mut g Parser) read_interpolated_string() !string {
	first_literal := g.lit
	mut raw := first_literal
	if raw.len > 0 && raw[0] == `r` {
		raw = raw[1..]
	}
	if raw.len == 0 || raw[0] !in [`'`, `"`] {
		return g.unsupported('interpolated string prefix')
	}
	quote := raw[0]
	mut parts := []string{}
	first_part := fastc_c_interpolation_segment(first_literal, true, quote)!
	if first_part != '_SLIT0' {
		parts << first_part
	}
	g.next()
	for g.tok == .str_dollar {
		g.next()
		g.expect(.lcbr)!
		value := g.read_expression([token.Token.rcbr, token.Token.colon])!
		value_tokens := g.last_expression.clone()
		value_type := fastc_normalize_inferred_type(g.last_expression_type)
		value_key := g.semantic_type_key(value_type)
		alias_has_str_method := g.declared_kinds[flat.source_type_name(value_key)] == .alias_ && '${flat.source_type_name(value_key)}.str' in g.functions
		mut interpolation_type_id := if alias_has_str_method {
			value_type
		} else {
			g.underlying_alias_type(value_type)
		}
		// C pointer differences use the signed pointer-sized integer formatter.
		if interpolation_type_id == fastc_type_ptrdiff_t {
			interpolation_type_id = flat.source_type_isize
		}
		interpolation_type := flat.source_type_name(interpolation_type_id)
		mut format_specifier := ''
		if g.tok == .colon {
			g.next()
			for g.tok != .rcbr && g.tok != .eof {
				if g.tok in [.name, .number] {
					format_specifier += g.lit
				} else if g.tok == .minus {
					format_specifier += '-'
				} else if g.tok == .plus {
					format_specifier += '+'
				} else {
					return g.unsupported('interpolation format token `${g.token_source()}`')
				}
				g.next()
			}
		}
		g.expect(.rcbr)!
		if format_specifier.ends_with('c') && fastc_is_integer_expression_type(interpolation_type_id) && !fastc_integer_interpolation_format_is_supported(format_specifier, fastc_is_unsigned_integer_type(interpolation_type_id)) {
			return g.unsupported('interpolation format `${format_specifier}` for `${flat.source_type_name(value_type)}`')
		}
		if !g.selfhost && format_specifier.ends_with('c') {
			codepoint := fastc_integer_literal_value(value_tokens) or {
				return g.unsupported('nonliteral `:c` interpolation')
			}
			if codepoint == 0 {
				// Ordinary FastC strings use NUL-terminated C storage and cannot retain this byte.
				return g.unsupported('NUL code points in `:c` interpolation')
			}
		}
		mut reference_part := ''
		mut option_part := ''
		if g.selfhost && format_specifier == '' {
			if flat.source_type_query(interpolation_type_id).has(.c_pointer_suffix) {
				reference_part = g.fastc_reference_str_expression(value, interpolation_type_id,
					value_tokens) or { '' }
			} else if interpolation_type_id == fastc_type_option {
				option_helper := g.fastc_option_str_name(g.option_value_type_for_expression(value_tokens)) or {
					return g.unsupported('interpolation of option `${flat.source_type_name(value_type)}`')
				}
				option_part = '${option_helper}(${value})'
			}
		}
		if reference_part != '' {
			parts << reference_part
		} else if option_part != '' {
			parts << option_part
		} else if interpolation_type_id == flat.source_type_string {
			width := fastc_string_interpolation_width(format_specifier) or {
				return g.unsupported('interpolation format `${format_specifier}` for `string`')
			}
			parts << if width.width > 0 {
				'v_fastc_string_pad(${value}, ${width.width}, ${width.left_align})'
			} else {
				value
			}
		} else if element_type_source_id := fastc_fixed_array_element_type(fastc_trim_pointer_type(interpolation_type_id)) {
			element_type := element_type_source_id

			helper := g.fastc_fixed_array_str_name(interpolation_type_id) or {
				return g.unsupported('interpolation of fixed array type `${flat.source_type_name(value_type)}`')
			}
			is_member_array := value_tokens.len >= 3 && value_tokens.last().tok == .name && value_tokens[value_tokens.len - 2].tok == .dot
			is_global_array := value_tokens.len == 1 && value_tokens[0].tok == .name && fastc_global_key(g.module_name, value_tokens[0].lit) in g.globals
			data_source := if is_member_array || is_global_array {
				value
			} else {
				'(${value}).data'
			}
			parts << '${helper}((${flat.source_type_name(element_type)} *)(${data_source}))'
		} else if flat.source_type_query(fastc_trim_pointer_type(interpolation_type_id)).has(.c_array_prefix) {
			helper := g.fastc_array_str_name(interpolation_type_id) or {
				return g.unsupported('interpolation of array type `${flat.source_type_name(value_type)}`')
			}
			array_value := if flat.source_type_query(interpolation_type_id).has(.c_pointer_suffix) {
				'*(${value})'
			} else {
				value
			}
			parts << '${helper}(${array_value})'
		} else if flat.source_type_query(fastc_trim_pointer_type(interpolation_type_id)).has(.c_map_prefix) {
			helper := g.fastc_map_str_name(interpolation_type_id) or {
				return g.unsupported('interpolation of map type `${flat.source_type_name(value_type)}`')
			}
			map_value := if flat.source_type_query(interpolation_type_id).has(.c_pointer_suffix) {
				'*(${value})'
			} else {
				value
			}
			parts << '${helper}(${map_value})'
		} else if format_specifier == '+' && fastc_is_integer_expression_type(interpolation_type_id) {
			// `${n:+}` signs a non-negative integer too.
			integer_str := g.fastc_struct_field_str_expression('__vf_signed', interpolation_type_id) or {
				return g.unsupported('interpolation format `+` for `${flat.source_type_name(value_type)}`')
			}
			parts << '({ ${interpolation_type} __vf_signed = (${value}); string __vf_signed_str = ${integer_str}; __vf_signed >= 0 ? builtin__string_plus(_S("+"), __vf_signed_str) : __vf_signed_str; })'
		} else if g.declared_kinds[flat.source_type_name(g.semantic_type_key(interpolation_type_id))] == .enum_ {
			enum_key := g.semantic_type_key(interpolation_type_id)
			is_unsigned := g.enum_flags[flat.source_type_name(enum_key)]
			enum_value := if flat.source_type_query(interpolation_type_id).has(.c_pointer_suffix) {
				'(*(${value}))'
			} else {
				value
			}
			format_character := if format_specifier.len > 0 {
				format_specifier[format_specifier.len - 1]
			} else {
				u8(0)
			}
			if format_character == `d` || format_character == `u` || format_character == `x` || format_character == `X` || format_character == `o` || format_character == `c` || format_character == `b` {
				if !fastc_integer_interpolation_format_is_supported(format_specifier, is_unsigned) {
					return g.unsupported('interpolation format `${format_specifier}` for enum `${flat.source_type_name(value_type)}`')
				}
				parts << if is_unsigned {
					'v_fastc_unsigned_format((unsigned long long)(${enum_value}), "${format_specifier}")'
				} else {
					'v_fastc_signed_format((long long)(${enum_value}), "${format_specifier}")'
				}
			} else {
				width := fastc_string_interpolation_width(format_specifier) or {
					return g.unsupported('interpolation format `${format_specifier}` for enum `${flat.source_type_name(value_type)}`')
				}
				enum_type := fastc_c_declared_type_name(flat.source_type_name(enum_key))
				method_key := '${flat.source_type_name(enum_key)}.str'
				enum_name := if method_signature := g.functions[method_key] {
					receiver := if flat.source_type_query(method_signature.parameter_types.ids[0]).has(.c_pointer_suffix) && !flat.source_type_query(interpolation_type_id).has(.c_pointer_suffix) {
						'&(${value})'
					} else if !flat.source_type_query(method_signature.parameter_types.ids[0]).has(.c_pointer_suffix) && flat.source_type_query(interpolation_type_id).has(.c_pointer_suffix) {
						'*(${value})'
					} else {
						value
					}
					method_c_name := g.c_function_name_or(method_key, fastc_method_c_name(method_signature.module_name, enum_type, 'str'))
					'${method_c_name}(${receiver})'
				} else {
					'v_fastc_enum_str_${enum_type}(${enum_value})'
				}
				parts << if width.width > 0 {
					'v_fastc_string_pad(${enum_name}, ${width.width}, ${width.left_align})'
				} else {
					enum_name
				}
			}
		} else {
			mut converted_primitive := false
			if g.selfhost && interpolation_type_id in [flat.source_type_f64, flat.source_type_f32] {
				// `${x:.1}`, `${x:8.2}`, `${x:-8.2}`, `${x:.2f}`: the C backend's float formats.
				if float_format := fastc_float_interpolation_format(format_specifier) {
					formatted := '${g.fastc_float_format_helper(float_format.fixed)}((double)(${value}), ${float_format.precision})'
					parts << if float_format.width > 0 {
						'v_fastc_string_pad(${formatted}, ${float_format.width}, ${float_format.left_align})'
					} else {
						formatted
					}
					converted_primitive = true
				}
			}
			if !converted_primitive && (!g.selfhost || format_specifier != '') {
				if primitive_conversion := fastc_primitive_interpolation_expression(interpolation_type_id, value, format_specifier) {
					parts << primitive_conversion
					converted_primitive = true
				} else if !g.selfhost && fastc_is_primitive_interpolation_type(interpolation_type_id) {
					return g.unsupported('interpolation format `${format_specifier}` for `${flat.source_type_name(value_type)}`')
				}
			}
			if !converted_primitive {
				receiver_key := g.semantic_type_key(interpolation_type_id)
				method_key := '${flat.source_type_name(receiver_key)}.str'
				if method_signature := g.functions[method_key] {
					receiver := if flat.source_type_query(method_signature.parameter_types.ids[0]).has(.c_pointer_suffix) && !flat.source_type_query(interpolation_type_id).has(.c_pointer_suffix) {
						'&(${value})'
					} else if !flat.source_type_query(method_signature.parameter_types.ids[0]).has(.c_pointer_suffix) && flat.source_type_query(interpolation_type_id).has(.c_pointer_suffix) {
						'*(${value})'
					} else {
						value
					}
					method_c_name := g.c_function_name_or(method_key, fastc_method_c_name(method_signature.module_name, fastc_c_declared_type_name(flat.source_type_name(receiver_key)), 'str'))
					parts << '${method_c_name}(${receiver})'
				} else if sumtype_helper := g.fastc_sumtype_str_helper(interpolation_type_id) {
					// A boxed sum type dispatches `str()` on its runtime variant tag.
					receiver := if flat.source_type_query(interpolation_type_id).has(.c_pointer_suffix) {
						'*(${value})'
					} else {
						value
					}
					parts << '${sumtype_helper}(${receiver})'
				} else if default_name := g.fastc_default_struct_str_name(interpolation_type_id) {
					// No user `str()`: emit V's default `Type{...}` representation. A pointer
					// value — a member smart-cast narrows a boxed variant to a `T*`, or the
					// receiver itself is `&T` — must be deref'd for the by-value helper argument.
					receiver := if flat.source_type_query(interpolation_type_id).has(.c_pointer_suffix) || (g.selfhost && g.expression_uses_member_smartcast(value_tokens)) {
						'*(${value})'
					} else {
						value
					}
					parts << '${default_name}(${receiver})'
				} else {
					local_type := if g.last_expression.len > 0 && g.last_expression[0].tok == .name {
						local := g.locals[g.last_expression[0].lit] or { FastcLocal{} }
						local.typ_text()
					} else {
						''
					}
					return g.unsupported('interpolation of type `${flat.source_type_name(value_type)}` for `${fastc_expression_tokens_debug(g.last_expression)}` (local `${local_type}`)')
				}
			}
		}
		if g.tok == .string {
			part := fastc_c_interpolation_segment(g.lit, false, quote)!
			if part != '_SLIT0' {
				parts << part
			}
			g.next()
		}
	}
	g.last_expression_type = flat.source_type_string
	g.last_expression = [FastcExpressionToken{
		tok: .string
		lit: first_literal
	}]
	if parts.len == 0 {
		return '_SLIT0'
	}
	if parts.len == 1 {
		return parts[0]
	}
	if g.selfhost {
		values_name := g.temporary_namespace('array') + '_literal'
		return fastc_array_values_call_source('string', parts, values_name,
			'builtin__string_plus_many(${parts.len}, ${values_name})')
	}
	return 'builtin__string_plus_many(${parts.len}, (string[]){${parts.join(', ')}})'
}

fn fastc_is_primitive_interpolation_type(value_type u32) bool {
	return fastc_is_integer_expression_type(value_type) || value_type in [
		flat.source_type_bool,
		flat.source_type_char,
	]
}

fn fastc_string_interpolation_width(format string) ?FastcInterpolationWidth {
	if format == '' || format == 's' {
		return FastcInterpolationWidth{}
	}
	mut end := format.len
	if format[end - 1] == `s` {
		end--
	}
	mut start := 0
	mut left_align := false
	if format[0] == `-` {
		left_align = true
		start = 1
	}
	if start >= end {
		return none
	}
	mut width := 0
	for i in start .. end {
		if format[i] < `0` || format[i] > `9` {
			return none
		}
		width = width * 10 + int(format[i] - `0`)
	}
	return FastcInterpolationWidth{
		width:      width
		left_align: left_align
	}
}

fn fastc_integer_interpolation_format_is_supported(format string, is_unsigned bool) bool {
	if format == '' {
		return true
	}
	specifier := format[format.len - 1]
	supported := specifier == `d` || specifier == `x` || specifier == `X` || specifier == `o` || specifier == `c` || specifier == `b` || (is_unsigned && specifier == `u`)
	if !supported {
		return false
	}
	mut start := 0
	if format[0] == `-` {
		start = 1
	}
	for i in start .. format.len - 1 {
		if format[i] < `0` || format[i] > `9` {
			return false
		}
	}
	return true
}

fn fastc_primitive_interpolation_expression(value_type u32, value string, format string) ?string {
	if fastc_is_integer_expression_type(value_type) {
		is_unsigned := fastc_is_unsigned_integer_type(value_type)
		effective_format := if format.len > 0 && format[format.len - 1].is_digit() {
			format + 'd'
		} else {
			format
		}
		if !fastc_integer_interpolation_format_is_supported(effective_format, is_unsigned) {
			return none
		}
		if effective_format != '' {
			return if is_unsigned {
				'v_fastc_unsigned_format((unsigned long long)(${value}), "${effective_format}")'
			} else {
				'v_fastc_signed_format((long long)(${value}), "${effective_format}")'
			}
		}
	}
	return match value_type {
		flat.source_type_i8, flat.source_type_i16, flat.source_type_i32, flat.source_type_i64, flat.source_type_int,
		flat.source_type_isize, fastc_type_integer_literal, fastc_type_negative_integer_literal {
			'v_fastc_signed_str((long long)(${value}))'
		}
		flat.source_type_u8, flat.source_type_u16, flat.source_type_u32, flat.source_type_u64,
		fastc_type_uint, fastc_type_unsigned_int, flat.source_type_usize {
			'v_fastc_unsigned_str((unsigned long long)(${value}))'
		}
		flat.source_type_bool {
			if format == '' {
				'v_fastc_bool_str(${value})'
			} else {
				none
			}
		}
		flat.source_type_char {
			if format == '' {
				'v_fastc_signed_format((long long)(${value}), "c")'
			} else {
				none
			}
		}
		else {
			none
		}
	}
}

fn fastc_string_literal_is_incomplete(literal string) bool {
	mut raw := literal
	mut is_raw := false
	if raw.len > 0 && raw[0] == `r` {
		is_raw = true
		raw = raw[1..]
	}
	if raw.len < 2 || raw[raw.len - 1] != raw[0] {
		return true
	}
	if is_raw {
		return false
	}
	// The last character matches the opening quote, but when it is an *escaped* quote
	// (`… \'` before an interpolation) it is not a closing quote — the literal continues
	// into a `${…}` fragment, so it is incomplete.
	mut backslashes := 0
	mut i := raw.len - 2
	for i >= 0 && raw[i] == `\\` {
		backslashes++
		i--
	}
	return backslashes % 2 == 1
}

fn fastc_c_interpolation_segment(literal string, is_first bool, quote u8) !string {
	mut content := literal
	if is_first {
		if content.len > 0 && content[0] == `r` {
			content = content[1..]
		}
		if content.len > 0 && content[0] == quote {
			content = content[1..]
		}
	}
	// Only the final segment ends with the closing quote of the whole string; a
	// middle segment that ends with `quote` carries an escaped quote (`\'`)
	// that belongs to the body, so stripping it would drop the quote and leave a
	// dangling backslash that corrupts the emitted C string literal.
	if !is_first && content.len > 0 && content[content.len - 1] == quote && !fastc_trailing_quote_is_escaped(content) {
		content = content[..content.len - 1]
	}
	if content == '' {
		return '_SLIT0'
	}
	wrapper := if quote == `'` { "'" } else { '"' }
	c_literal := fastc_c_string(wrapper + content + wrapper)!
	return '_S(${c_literal})'
}

// fastc_trailing_quote_is_escaped reports whether the last character of `content` is
// preceded by an odd number of backslashes, meaning it is an escaped quote (`\'`) that
// is part of the string body rather than an unescaped closing delimiter.
fn fastc_trailing_quote_is_escaped(content string) bool {
	mut backslashes := 0
	mut i := content.len - 2
	for i >= 0 && content[i] == `\\` {
		backslashes++
		i--
	}
	return backslashes % 2 == 1
}

fn fastc_method_c_name_for_key(receiver_key string, name string) string {
	module_name := if receiver_key.contains('.') {
		receiver_key.all_before_last('.')
	} else {
		'builtin'
	}
	receiver_type := fastc_c_declared_type_name(receiver_key)
	return fastc_method_c_name(module_name, receiver_type, name)
}

fn fastc_line_column(source string, position int) (int, int) {
	limit := if position < source.len { position } else { source.len }
	mut line := 1
	mut column := 1
	for i in 0 .. limit {
		if source[i] == `\n` {
			line++
			column = 1
		} else {
			column++
		}
	}
	return line, column
}

fn fastc_c_string_value(value string) string {
	mut result := strings.new_builder(value.len + 2)
	result.write_u8(`"`)
	for c in value {
		match c {
			`"` { result.write_string('\\"') }
			`\\` { result.write_string('\\\\') }
			`\n` { result.write_string('\\n') }
			`\r` { result.write_string('\\r') }
			`\t` { result.write_string('\\t') }
			else { result.write_u8(c) }
		}
	}
	result.write_u8(`"`)
	return result.str()
}

fn fastc_needs_space(last u8, next string) bool {
	if next.len == 0 {
		return false
	}
	// C's preprocessing number grammar absorbs a sign after e/p, including
	// a hexadecimal integer such as `0xb5e` followed by a separate `+` token.
	return ((last.is_alnum() || last == `_`) && (next[0].is_alnum() || next[0] == `_`))
		|| (last in [`e`, `E`, `p`, `P`] && next in ['+', '-'])
}

fn fastc_c_string(literal string) !string {
	if literal.len < 2 {
		return error('invalid fastc string literal')
	}
	mut raw := literal
	mut is_raw := false
	if raw[0] == `r` && raw.len >= 3 {
		is_raw = true
		raw = raw[1..]
	}
	quote := raw[0]
	if quote !in [`'`, `"`] || raw[raw.len - 1] != quote {
		return error('interpolated or unfinished fastc string literal')
	}
	content := raw[1..raw.len - 1]
	// A `\x00`/`\000` NUL escape renders to the octal escape `\000`, and the `_S` macro's
	// `sizeof`-based length preserves it (V strings are length-prefixed, not
	// NUL-terminated). Other NUL spellings (a raw NUL byte, single `\0`, wrapping octal
	// like `\400`, unicode `\u0000`) cannot be emitted faithfully, so reject just those.
	if fastc_string_has_unrenderable_nul(content, is_raw) {
		return error('fastc parser does not support embedded NUL string literals')
	}
	mut result := strings.new_builder(raw.len + 2)
	result.write_u8(`"`)
	mut i := 1
	for i < raw.len - 1 {
		c := raw[i]
		if c == `\\` && !is_raw && i + 1 < raw.len - 1 {
			if raw[i + 1] == `\n` {
				i += 2
				for i < raw.len - 1 && raw[i] in [` `, `\t`, `\r`] {
					i++
				}
				continue
			}
			if raw[i + 1] == `\r` && i + 2 < raw.len - 1 && raw[i + 2] == `\n` {
				i += 3
				for i < raw.len - 1 && raw[i] in [` `, `\t`] {
					i++
				}
				continue
			}
			if raw[i + 1] == `x` {
				if i + 3 >= raw.len - 1 {
					return error('invalid fastc hex escape')
				}
				high := fastc_hex_digit_value(raw[i + 2])!
				low := fastc_hex_digit_value(raw[i + 3])!
				value := (high << 4) | low
				// V consumes exactly two hexadecimal digits. C consumes every
				// following hex digit, so use a full three-digit octal escape to
				// terminate the encoded byte unambiguously.
				result.write_u8(`\\`)
				result.write_u8(`0` + (value >> 6))
				result.write_u8(`0` + ((value >> 3) & 7))
				result.write_u8(`0` + (value & 7))
				i += 4
				continue
			}
			if raw[i + 1] == `e` {
				result.write_string('\\033')
				i += 2
				continue
			}
			if raw[i + 1] == `$` {
				// V escapes `$` to keep it out of an interpolation; C has no `\$` escape.
				result.write_u8(`$`)
				i += 2
				continue
			}
			if raw[i + 1] >= `0` && raw[i + 1] <= `7` && (i + 3 >= raw.len - 1 || raw[i + 2] < `0` || raw[i + 2] > `7` || raw[i + 3] < `0` || raw[i + 3] > `7`) {
				// V's `\0` is a NUL byte; emit a full three-digit octal so a following
				// digit cannot extend the escape (`_S`'s `sizeof`-based length keeps it).
				if raw[i + 1] == `0` {
					result.write_string('\\000')
					i += 2
					continue
				}
				// V only decodes three-digit octal escapes. Preserve a shorter
				// spelling as a literal backslash and digits instead of letting C
				// consume it as a one- or two-digit octal escape.
				result.write_string('\\\\')
				i++
				continue
			}
			result.write_u8(c)
			result.write_u8(raw[i + 1])
			i += 2
			continue
		} else if c == `"` {
			result.write_string('\\"')
		} else if c == `\\` && is_raw {
			result.write_string('\\\\')
		} else if c == `\n` {
			result.write_string('\\n')
		} else if c == `\r` {
			result.write_string('\\r')
		} else if c == `\t` {
			result.write_string('\\t')
		} else {
			result.write_u8(c)
		}
		i++
	}
	result.write_u8(`"`)
	return result.str()
}

fn fastc_c_rune(literal string) !string {
	if literal.len == 0 {
		return error('invalid fastc rune literal')
	}
	content := if literal.len >= 3 && literal[0] == 96 && literal[literal.len - 1] == 96 {
		literal[1..literal.len - 1]
	} else {
		literal
	}
	if content.len == 1 {
		return '((rune)${content[0]})'
	}
	if content.len >= 2 && content[0] == `\\` {
		if content.len == 2 {
			value := match content[1] {
				`0` { 0 }
				`a` { 7 }
				`b` { 8 }
				`t` { 9 }
				`n` { 10 }
				`v` { 11 }
				`f` { 12 }
				`r` { 13 }
				`e` { 27 }
				`\\` { 92 }
				96 { 96 }
				`'` { 39 }
				`"` { 34 }
				else {
					return error('unsupported fastc rune escape')
				}
			}
			return '((rune)${value})'
		}
		if content.len == 4 && content[1] == `x` {
			high := fastc_hex_digit_value(content[2])!
			low := fastc_hex_digit_value(content[3])!
			value := (high << 4) | low
			return '((rune)${value})'
		}
	}
	first := content[0]
	mut value := u32(0)
	mut needed := 0
	if first & 0xe0 == 0xc0 {
		value = u32(first & 0x1f)
		needed = 1
	} else if first & 0xf0 == 0xe0 {
		value = u32(first & 0x0f)
		needed = 2
	} else if first & 0xf8 == 0xf0 {
		value = u32(first & 0x07)
		needed = 3
	} else {
		return error('invalid fastc UTF-8 rune literal')
	}
	if content.len != needed + 1 {
		return error('invalid fastc rune literal length')
	}
	for i in 1 .. content.len {
		if content[i] & 0xc0 != 0x80 {
			return error('invalid fastc UTF-8 rune literal')
		}
		value = (value << 6) | u32(content[i] & 0x3f)
	}
	return '((rune)${value})'
}

fn fastc_hex_digit_value(c u8) !u8 {
	if c >= `0` && c <= `9` {
		return u8(c - `0`)
	}
	if c >= `a` && c <= `f` {
		return u8(c - `a` + 10)
	}
	if c >= `A` && c <= `F` {
		return u8(c - `A` + 10)
	}
	return error('invalid fastc hex digit `${c.ascii_str()}`')
}

// fastc_string_has_unrenderable_nul reports whether the literal content holds a NUL
// that fastc_c_string cannot faithfully emit: a raw NUL byte, or a NUL escape other
// than `\x00`/`\000` (single `\0`, a wrapping octal like `\400`, or a unicode
// ` `/`\U00000000`). `\x00`/`\000` render to the stable octal escape `\000`.
fn fastc_string_has_unrenderable_nul(content string, is_raw bool) bool {
	for byte_index in 0 .. content.len {
		if content[byte_index] == 0 {
			return true
		}
	}
	if is_raw {
		return false
	}
	mut i := 0
	for i + 1 < content.len {
		if content[i] != `\\` {
			i++
			continue
		}
		escape := content[i + 1]
		if escape == `\\` {
			i += 2
			continue
		}
		if escape >= `0` && escape <= `7` && i + 3 < content.len && content[i + 2] >= `0` && content[i + 2] <= `7` && content[i + 3] >= `0` && content[i + 3] <= `7` {
			high := int(escape - `0`)
			middle := int(content[i + 2] - `0`)
			low := int(content[i + 3] - `0`)
			value := high * 64 + middle * 8 + low
			// `\000` (value 0) renders fine; a wrapping octal like `\400` becomes NUL
			// but cannot be re-spelled as a stable C escape.
			if value != 0 && u8(value) == 0 {
				return true
			}
			i += 4
			continue
		}
		// `\0` renders to the octal escape `\000`; only an all-zero unicode escape
		// (` `/` `) has no faithful C spelling.
		if (escape == `u` && i + 5 < content.len && content[i + 2..i + 6] == '0000') || (escape == `U` && i + 9 < content.len && content[i + 2..i + 10] == '00000000') {
			return true
		}
		i += 2
	}
	return false
}

fn fastc_string_contains_nul(content string, is_raw bool) bool {
	for byte_index in 0 .. content.len {
		if content[byte_index] == 0 {
			return true
		}
	}
	if is_raw {
		return false
	}
	mut i := 0
	for i + 1 < content.len {
		if content[i] != `\\` {
			i++
			continue
		}
		escape := content[i + 1]
		if escape == `\\` {
			i += 2
			continue
		}
		if escape >= `0` && escape <= `7` && i + 3 < content.len && content[i + 2] >= `0` && content[i + 2] <= `7` && content[i + 3] >= `0` && content[i + 3] <= `7` {
			high := int(escape - `0`)
			middle := int(content[i + 2] - `0`)
			low := int(content[i + 3] - `0`)
			value := high * 64 + middle * 8 + low
			// V stores three-digit octal escapes in a byte, including wrapping
			// values such as \400 to NUL.
			if u8(value) == 0 {
				return true
			}
			i += 4
			continue
		}
		if escape == `0` || (escape == `x` && i + 3 < content.len && content[i + 2..i + 4] == '00') || (escape == `u` && i + 5 < content.len && content[i + 2..i + 6] == '0000') || (escape == `U` && i + 9 < content.len && content[i + 2..i + 10] == '00000000') {
			return true
		}
		i += 2
	}
	return false
}
