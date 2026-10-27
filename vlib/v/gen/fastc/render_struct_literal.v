module fastc

import v.flat

struct FastcStructLiteralFieldValue {
	explicit_initializers []string
	rendered_field        string
	field_value           string
	fixed_array_copy      string
	is_fixed_array        bool
}

fn (g &Parser) render_struct_literal_field_value(value_tokens []FastcExpressionToken, expected_type_source_id u32, c_field_name string, temporary_start int, is_shared_pointer bool) ?FastcStructLiteralFieldValue {
	expected_type := flat.source_type_name(expected_type_source_id)
	if value_tokens.len >= 2 && value_tokens[0].tok == .name && value_tokens[0].lit == 'chan' {
		// `jobs: chan Task{cap: N}`: channels are erased to a stub `void*`, so drop the capacity
		// initializer to a zero channel. The struct-literal path would otherwise mistake `chan` for
		// a struct type and emit an invalid `(chan){.cap=…}` designated initializer on a `void*`.
		return FastcStructLiteralFieldValue{
			rendered_field: '.${c_field_name}=((chan){0})'
			field_value:    '(chan){0}'
		}
	}
	if fixed_element_type_source_id := fastc_fixed_array_element_type(expected_type_source_id) {
		fixed_element_type := fixed_element_type_source_id

		array_end := if value_tokens.len > 0 && value_tokens.last().tok == .not {
			value_tokens.len - 1
		} else {
			value_tokens.len
		}
		if array_end >= 2 && value_tokens[0].tok == .lsbr && value_tokens[array_end - 1].tok == .rsbr {
			items := fastc_expression_list_items(value_tokens, 1, array_end - 1) or {
				return none
			}
			mut values := []string{cap: items.len}
			mut initializers := []string{cap: items.len}
			for item in items {
				rendered_item := g.render_call_argument_expression(item, fixed_element_type) or {
					return none
				}
				if g.c_global_static_initializer {
					values << rendered_item
					continue
				}
				temporary := '__vf_sf_${temporary_start + initializers.len}'
				declaration_type := g.declaration_c_type(fixed_element_type, rendered_item)
				initializers << '${declaration_type} ${temporary} = (${rendered_item});'
				values << temporary
			}
			joined_values := values.join(',')
			return FastcStructLiteralFieldValue{
				explicit_initializers: initializers
				rendered_field:        '.${c_field_name}={${joined_values}}'
				field_value:           '{${joined_values}}'
				is_fixed_array:        true
			}
		}
		storage_tokens := fastc_strip_paren_tokens(value_tokens)
		is_raw_fixed_array := g.fixed_array_uses_raw_storage(storage_tokens)
		value := if is_raw_fixed_array {
			g.render_member_receiver(storage_tokens) or { return none }
		} else {
			g.render_call_argument_expression(value_tokens, expected_type_source_id) or { return none }
		}
		copy_source := if is_raw_fixed_array {
			value
		} else if flat.source_type_query(expected_type_source_id).has(.c_pointer_suffix) {
			'(${value})->data'
		} else {
			'(${value}).data'
		}
		return FastcStructLiteralFieldValue{
			field_value:      value
			fixed_array_copy: 'memcpy(__vf_struct_fixed.${c_field_name}, ${copy_source}, sizeof(__vf_struct_fixed.${c_field_name}));'
			is_fixed_array:   true
		}
	}
	value := if value_tokens.len == 1 && value_tokens[0].has_source() {
		// A field value carried as a pre-rendered `({ ... })` (e.g. an `or`-unwrap) is used
		// directly so its internal temporaries stay self-contained.
		value_tokens[0].source_text()
	} else {
		g.render_call_argument_expression(value_tokens, expected_type_source_id) or { return none }
	}
	if g.c_global_static_initializer && !is_shared_pointer {
		return FastcStructLiteralFieldValue{
			rendered_field: '.${c_field_name}=(${value})'
			field_value:    value
		}
	}
	temporary := '__vf_sf_${temporary_start}'
	// Prefer the declared field type. Besides handling statement expressions that
	// TinyCC cannot inspect with `__typeof__`, this avoids spelling a potentially
	// large initializer twice in the generated C.
	field_decl_type := if expected_type_source_id != 0 {
		fastc_output_c_type(flat.source_type_name(fastc_normalize_inferred_type(expected_type_source_id)))
	} else {
		'__typeof__((${value}))'
	}
	stored_value := if is_shared_pointer {
		'(${expected_type}*)v_fastc_interface_box(&${temporary}, sizeof(${expected_type}))'
	} else {
		temporary
	}
	return FastcStructLiteralFieldValue{
		explicit_initializers: if value.contains('({') {
			// TinyCC cannot keep an outer aggregate initializer active while
			// evaluating a statement expression with its own initializers.
			['${field_decl_type} ${temporary}; ${temporary} = (${value});']
		} else {
			['${field_decl_type} ${temporary} = (${value});']
		}
		rendered_field:        '.${c_field_name}=(${stored_value})'
		field_value:           stored_value
	}
}

fn (g &Parser) render_struct_literal_expression(tokens []FastcExpressionToken) ?FastcRenderedExpression {
	mut open := -1
	mut delimiter_depth := 0
	for i, item in tokens {
		if item.tok in [.lpar, .lsbr] {
			delimiter_depth++
		} else if item.tok in [.rpar, .rsbr] {
			delimiter_depth--
		} else if item.tok == .lcbr && delimiter_depth == 0 {
			open = i
			break
		}
	}
	if open <= 0 || tokens.last().tok != .rcbr {
		return none
	}
	close := fastc_matching_delimiter(tokens, open, .lcbr, .rcbr) or { return none }
	if close != tokens.len - 1 {
		return none
	}
	is_c_struct_literal := open == 3 && tokens[0].tok == .name && tokens[0].lit == 'C' && tokens[1].tok == .dot && tokens[2].tok == .name
	mut c_type := if open == 1 && tokens[0].typ != 0 {
		tokens[0].typ
	} else {
		g.type_from_expression_tokens(tokens[..open]) or { u32(0) }
	}
	if c_type == 0 && is_c_struct_literal {
		c_type = flat.source_type_id(if fastc_source_type_key_exists(g.declared_types, '#Cstruct#${tokens[2].lit}') {
			'struct ${tokens[2].lit}'
		} else {
			tokens[2].lit
		})
	}
	mut layout_type := fastc_trim_pointer_type(c_type)
	if flat.source_type_query(layout_type).has(.c_array_prefix) {
		layout_type = flat.source_type_id('array')
	}
	// `Alias{}` where `Alias = u8` (a primitive alias, e.g. ast.EmptyExpr) has no fields, so
	// the "struct literal" is really the primitive's zero value; C rejects the empty `{}`.
	if c_type != 0 && open + 1 == close && fastc_primitive_c_type_id(fastc_normalize_inferred_type(g.underlying_alias_type(c_type))) != none {
		return FastcRenderedExpression{
			source: if fastc_normalize_inferred_type(g.underlying_alias_type(c_type)) in [
				fastc_type_array,
				fastc_type_map,
				fastc_type_option,
				fastc_type_result,
			]
				|| (g.selfhost && g.underlying_alias_type(c_type) == flat.source_type_string) {
				'(${flat.source_type_name(c_type)}){0}'
			} else {
				'((${flat.source_type_name(c_type)})0)'
			}
			typ:    c_type
		}
	}
	if c_type == 0 || (!is_c_struct_literal && flat.source_type_name(layout_type) !in g.struct_fields && g.declared_kinds[g.semantic_type_key(c_type)] !in [
		.struct_,
		.union_,
	]) {
		return none
	}
	// An alias of a map carries the map header's fields, but `Counts{}` is an empty map,
	// which render_map_expression builds.
	if !is_c_struct_literal && !fastc_is_pointer_type(c_type) && g.map_type_of(c_type) != none {
		return none
	}
	if open + 1 < close {
		items := fastc_expression_list_items(tokens, open + 1, close) or { return none }
		mut is_positional := false
		if !fastc_expression_tokens_contain_range(tokens, open + 1, close, .ellipsis) {
			for item in items {
				if item.len == 0 {
					continue
				}
				if !(item.len >= 2 && item[0].tok == .name && item[1].tok == .colon) && !(item.len == 1 && item[0].tok == .name && g.struct_direct_member_type(c_type, item[0].lit) != u32(0)) {
					is_positional = true
					break
				}
			}
		}
		if is_positional {
			mut values := []string{cap: items.len}
			for item_index, item in items {
				field := if !is_c_struct_literal && item_index < g.struct_field_info[flat.source_type_name(layout_type)].len {
					g.struct_field_info[flat.source_type_name(layout_type)][item_index]
				} else {
					FastcStructField{}
				}
				rendered_item := g.render_call_argument_expression(item, field.typ) or { return none }
				values << if field.is_shared_pointer {
					'({ ${field.typ_text()} __vf_shared_field_value = (${rendered_item}); (${field.typ_text()}*)v_fastc_interface_box(&__vf_shared_field_value, sizeof(${field.typ_text()})); })'
				} else {
					rendered_item
				}
			}
			source := if flat.source_type_query(c_type).has(.c_pointer_suffix) {
				'&(${flat.source_type_name(fastc_trim_pointer_type(c_type))}){${values.join(',')}}'
			} else {
				'(${flat.source_type_name(c_type)}){${values.join(',')}}'
			}
			return FastcRenderedExpression{
				source: source
				typ:    c_type
			}
		}
	}
	mut promoted_pointer_fields := map[string]FastcStructLiteralFieldValue{}
	mut rendered_fields := []string{}
	mut rendered_fields_by_name := map[string]string{}
	mut field_values := map[string]string{}
	mut explicit_initializers := []string{}
	mut fixed_array_copies := []string{}
	mut has_applied_defaults := false
	mut update_source := ''
	mut array_init_tokens := []FastcExpressionToken{}
	mut has_array_init := false
	mut index := open + 1
	for index < close {
		for index < close && tokens[index].tok in [.semicolon, .comma] {
			index++
		}
		if index >= close {
			break
		}
		if tokens[index].tok == .ellipsis {
			index++
			value_start := index
			// The base ends at a top-level separator, not one inside its own call.
			mut depth := 0
			for index < close && (depth > 0 || tokens[index].tok !in [.semicolon, .comma]) {
				if tokens[index].tok in [.lpar, .lsbr, .lcbr] {
					depth++
				} else if tokens[index].tok in [.rpar, .rsbr, .rcbr] {
					depth--
				}
				index++
			}
			if value_start == index {
				return none
			}
			update_source = g.render_call_argument_expression(tokens[value_start..index], c_type) or {
				return none
			}
			continue
		}
		if tokens[index].tok != .name {
			return none
		}
		field_name := tokens[index].lit
		index++
		mut value_start := -1
		mut value_end := -1
		if index < close && tokens[index].tok == .colon {
			index++
			value_start = index
			mut parens := 0
			mut brackets := 0
			mut braces := 0
			for index < close {
				// Fields written one-per-line (or trailing named call args) carry no comma
				// between them, so a following `name:` at the top level begins the next
				// field and ends this value.
				if parens == 0 && brackets == 0 && braces == 0 && index > value_start && tokens[index].tok == .name && index + 1 < close && tokens[index + 1].tok == .colon {
					break
				}
				match tokens[index].tok {
					.lpar {
						parens++
					}
					.rpar {
						parens--
					}
					.lsbr {
						brackets++
					}
					.rsbr {
						brackets--
					}
					.lcbr {
						braces++
					}
					.rcbr {
						braces--
					}
					.semicolon, .comma {
						if parens == 0 && brackets == 0 && braces == 0 {
							break
						}
					}
					else {}
				}
				index++
			}
			if value_start == index {
				return none
			}
			value_end = index
		}
		mut c_field_name := if is_c_struct_literal {
			field_name
		} else {
			fastc_c_identifier(field_name)
		}
		// An array `init:` value is a per-element closure (it may read the special `index`
		// element counter), so it must be rendered inside the fill loop, not hoisted into a
		// temp before it. Capture its raw tokens and skip the ordinary field rendering.
		if layout_type == fastc_type_array && field_name == 'init' && value_start >= 0 {
			array_init_tokens = tokens[value_start..value_end].clone()
			has_array_init = true
			continue
		}
		mut promoted_pointer_path := ''
		mut field_is_shared_pointer := false
		mut expected_type := if layout_type == fastc_type_array && field_name == 'init' {
			g.array_element_type(c_type) or { u32(0) }
		} else {
			g.struct_direct_member_type(c_type, field_name)
		}
		if expected_type != 0 {
			if field := g.struct_field_metadata(c_type, field_name) {
				field_is_shared_pointer = field.is_shared_pointer
			}
		}
		if expected_type == 0 && !is_c_struct_literal {
			if field := g.struct_field_metadata(c_type, field_name) {
				field_is_shared_pointer = field.is_shared_pointer
				expected_type = field.typ
				mut storage_path := field.storage_path.clone()
				storage_path << field.name
				mut c_storage_path := []string{}
				for storage_name in storage_path {
					c_storage_path << fastc_c_identifier(storage_name)
				}
				c_field_name = c_storage_path.join('.')
				mut owner := c_type
				for storage_name in field.storage_path {
					owner = g.struct_direct_member_type(owner, storage_name)
					if fastc_is_pointer_type(g.underlying_alias_type(owner)) {
						promoted_pointer_path = c_field_name
						break
					}
				}
			} else {
				// Initializing an embedded field by its type name:
				// `Derived{ Base: Base{...} }` sets the `__embedded_N` field.
				for embed_field in g.struct_field_info[flat.source_type_name(layout_type)] {
					if embed_field.name.starts_with('__embedded_') && (embed_field.typ == flat.source_type_id(field_name) || flat.source_type_syntax(embed_field.typ).last_double_underscore_suffix() == flat.source_type_id(field_name)) {
						c_field_name = embed_field.name
						expected_type = embed_field.typ
						break
					}
				}
			}
		}
		field_value := if value_start >= 0 {
			g.render_struct_literal_field_value(tokens[value_start..value_end], expected_type, c_field_name, explicit_initializers.len, field_is_shared_pointer) or { return none }
		} else {
			g.render_struct_literal_field_value([
				FastcExpressionToken{
					tok: .name
					lit: field_name
				},
			], expected_type, c_field_name, explicit_initializers.len, field_is_shared_pointer) or { return none }
		}
		explicit_initializers << field_value.explicit_initializers
		if promoted_pointer_path != '' {
			promoted_pointer_fields[promoted_pointer_path] = FastcStructLiteralFieldValue{
				...field_value
				fixed_array_copy: field_value.fixed_array_copy.replace('__vf_struct_fixed.${c_field_name}', '__vf_promoted_field')
			}
		} else if field_value.rendered_field != '' {
			rendered_fields << field_value.rendered_field
			rendered_fields_by_name[field_name] = field_value.rendered_field
		}
		field_values[field_name] = field_value.field_value
		if promoted_pointer_path == '' && field_value.fixed_array_copy != '' {
			fixed_array_copies << field_value.fixed_array_copy
		}
		if field_value.is_fixed_array {
			continue
		}
	}
	if promoted_pointer_fields.len > 0 {
		for field in g.struct_field_info[flat.source_type_name(layout_type)] {
			prefix := field.name + '.'
			mut nested := map[string]FastcStructLiteralFieldValue{}
			for path, value in promoted_pointer_fields {
				if path.starts_with(prefix) {
					nested[path[prefix.len..]] = value
				}
			}
			if nested.len == 0 { continue }
			base := if update_source != '' {
				'__vf_struct_update.${fastc_c_identifier(field.name)}'
			} else {
				g.struct_field_initializer_default(field)
			}
			value := g.render_promoted_pointer_literal(field.typ, nested, base, 0) or { return none }
			temporary := '__vf_sf_${explicit_initializers.len}'
			explicit_initializers << '${field.typ_text()} ${temporary}; ${temporary} = (${value});'
			rendered_field := '.${fastc_c_identifier(field.name)}=(${temporary})'
			rendered_fields << rendered_field
			rendered_fields_by_name[field.name] = rendered_field
			field_values[field.name] = temporary
		}
	}
	if update_source == '' {
		// The struct's rendered defaults are consulted; the constants phase
		// re-parses a file that did so after they are ready.
		fastc_note_field_defaults_use(g)
		for field in g.struct_field_info[flat.source_type_name(layout_type)] {
			if field.name in field_values {
				continue
			}
			field_default := g.struct_field_initializer_default(field)
			if field_default == '' {
				continue
			}
			c_field_name := fastc_c_identifier(field.name)
			rendered_field := if g.c_global_static_initializer
				&& fastc_fixed_array_element_type(field.typ) != none {
				'.${c_field_name}=${fastc_static_fixed_array_initializer(field_default)}'
			} else {
				'.${c_field_name}=(${field_default})'
			}
			rendered_fields << rendered_field
			rendered_fields_by_name[field.name] = rendered_field
			field_values[field.name] = field_default
			has_applied_defaults = true
		}
	}
	if flat.source_type_name(layout_type) in g.struct_field_info {
		mut ordered_fields := []string{cap: rendered_fields.len}
		mut ordered_values := map[string]bool{}
		for field in g.struct_field_info[flat.source_type_name(layout_type)] {
			if rendered_field := rendered_fields_by_name[field.name] {
				ordered_fields << rendered_field
				ordered_values[rendered_field] = true
			}
		}
		for rendered_field in rendered_fields {
			if rendered_field !in ordered_values {
				ordered_fields << rendered_field
			}
		}
		rendered_fields = ordered_fields.clone()
	}
	if layout_type == fastc_type_array && update_source == ''
		&& flat.source_type_query(c_type).has(.c_array_prefix) {
		array_type_id := fastc_trim_pointer_type(c_type)
		array_type := flat.source_type_name(array_type_id)
		element_type := g.array_element_type(array_type_id) or { return none }
		storage_type := g.collection_storage_type(element_type)
		length := field_values['len'] or { '0' }
		capacity := field_values['cap'] or { '0' }
		inner_array_element_type := if flat.source_type_query(element_type).has(.c_array_prefix) {
			g.array_element_type(element_type) or { u32(0) }
		} else {
			u32(0)
		}
		base := '((${array_type})builtin____new_array(${length},${capacity},sizeof(${storage_type})))'
		mut value_source := base
		if has_array_init {
			inner := g.render_call_argument_expression(array_init_tokens, element_type) or {
				return none
			}
			// `index` names the current element position inside an `init:` closure; bind it
			// to the loop counter so an expression like `init: index` fills 0, 1, 2, ….
			value_source = '({ ${explicit_initializers.join(' ')} ${array_type} __vf_array_init = ${base}; for (int __vf_array_index = 0; __vf_array_index < __vf_array_init.len; __vf_array_index++) { int index = __vf_array_index; (void)index; ((${storage_type} *)__vf_array_init.data)[__vf_array_index] = (${inner}); } __vf_array_init; })'
		} else if inner_array_element_type != 0 {
			// A zeroed inner dynamic array has element_size == 0, so appending to an
			// element of `[][]T{len: n}` copies no data. Construct a valid empty inner
			// array for every outer element, as the main C backend does.
			inner_default := '((${flat.source_type_name(element_type)})builtin____new_array(0,0,sizeof(${g.collection_storage_type(inner_array_element_type)})))'
			value_source = '({ ${explicit_initializers.join(' ')} ${array_type} __vf_array_init = ${base}; for (int __vf_array_index = 0; __vf_array_index < __vf_array_init.len; __vf_array_index++) { ((${storage_type} *)__vf_array_init.data)[__vf_array_index] = ${inner_default}; } __vf_array_init; })'
		} else if length != '0' && element_type != flat.source_type_string
			&& g.struct_type_has_initializer_defaults(element_type) {
			// A zeroed struct element leaves its defaulted fields invalid: a dynamic array
			// field has element_size == 0, so appending to it copies no data. Give every
			// element the struct's default value, as `Struct{}` builds it. (A zeroed string
			// is already the empty string.)
			element_default := g.render_empty_struct_initializer(element_type)
			value_source = '({ ${explicit_initializers.join(' ')} ${array_type} __vf_array_init = ${base}; for (int __vf_array_index = 0; __vf_array_index < __vf_array_init.len; __vf_array_index++) { ((${storage_type} *)__vf_array_init.data)[__vf_array_index] = ${element_default}; } __vf_array_init; })'
		} else if explicit_initializers.len > 0 {
			value_source = '({ ${explicit_initializers.join(' ')} ${base}; })'
		}
		if flat.source_type_query(c_type).has(.c_pointer_suffix) {
			value_source = '({ ${array_type} __vf_array_pointer_value = (${value_source}); (${flat.source_type_name(c_type)})v_fastc_interface_box(&__vf_array_pointer_value, sizeof(${array_type})); })'
		}
		return FastcRenderedExpression{
			source: value_source
			typ:    c_type
		}
	}
	if update_source != '' {
		mut assignments := []string{cap: rendered_fields.len}
		for field in rendered_fields {
			assignments << '__vf_struct_update${field};'
		}
		if flat.source_type_query(c_type).has(.c_pointer_suffix) {
			base_type := flat.source_type_name(fastc_trim_pointer_type(c_type))
			copy_statements := fixed_array_copies.join(' ')
			return FastcRenderedExpression{
				source: '({ ${base_type} __vf_struct_update = *(${update_source}); ${explicit_initializers.join(' ')} ${assignments.join(' ')} ${copy_statements.replace('__vf_struct_fixed', '__vf_struct_update')} (${flat.source_type_name(c_type)})v_fastc_interface_box(&__vf_struct_update, sizeof(${base_type})); })'
				typ:    c_type
			}
		}
		copy_statements := fixed_array_copies.join(' ')
		return FastcRenderedExpression{
			source: '({ ${flat.source_type_name(c_type)} __vf_struct_update = (${update_source}); ${explicit_initializers.join(' ')} ${assignments.join(' ')} ${copy_statements.replace('__vf_struct_fixed', '__vf_struct_update')} __vf_struct_update; })'
			typ:    c_type
		}
	}
	if has_applied_defaults {
		rendered := g.render_struct_literal_with_defaults(c_type, layout_type, explicit_initializers, rendered_fields, rendered_fields_by_name)
		if fixed_array_copies.len == 0 {
			return rendered
		}
		access := if flat.source_type_query(c_type).has(.c_pointer_suffix) { '->' } else { '.' }
		copies := fixed_array_copies.join(' ').replace('__vf_struct_fixed.', '__vf_struct_with_fixed${access}')
		return FastcRenderedExpression{
			source: '({ ${flat.source_type_name(c_type)} __vf_struct_with_fixed = (${rendered.source}); ${copies} __vf_struct_with_fixed; })'
			typ:    c_type
		}
	}
	literal_source := if flat.source_type_query(c_type).has(.c_pointer_suffix) {
		if g.c_global_static_initializer {
			'&(${flat.source_type_name(fastc_trim_pointer_type(c_type))}){${rendered_fields.join(',')}}'
		} else {
			'(${flat.source_type_name(c_type)})v_fastc_interface_box(&(${flat.source_type_name(fastc_trim_pointer_type(c_type))}){${rendered_fields.join(',')}}, sizeof(${flat.source_type_name(fastc_trim_pointer_type(c_type))}))'
		}
	} else {
		'(${flat.source_type_name(c_type)}){${rendered_fields.join(',')}}'
	}
	if fixed_array_copies.len > 0 {
		base_type := flat.source_type_name(fastc_trim_pointer_type(c_type))
		copies := fixed_array_copies.join(' ')
		result := if flat.source_type_query(c_type).has(.c_pointer_suffix) {
			'(${flat.source_type_name(c_type)})v_fastc_interface_box(&__vf_struct_fixed, sizeof(${base_type}))'
		} else {
			'__vf_struct_fixed'
		}
		return FastcRenderedExpression{
			source: '({ ${explicit_initializers.join(' ')} ${base_type} __vf_struct_fixed = (${base_type}){${rendered_fields.join(',')}}; ${copies} ${result}; })'
			typ:    c_type
		}
	}
	if explicit_initializers.len > 0 {
		return FastcRenderedExpression{
			source: '({ ${explicit_initializers.join(' ')} ${literal_source}; })'
			typ:    c_type
		}
	}
	return FastcRenderedExpression{
		source: literal_source
		typ:    c_type
	}
}

// Copy pointer embeds before overriding promoted fields so shared defaults stay immutable.
fn (g &Parser) render_promoted_pointer_literal(typ u32, values map[string]FastcStructLiteralFieldValue, default_value string, depth int) ?string {
	underlying := g.underlying_alias_type(typ)
	is_pointer := fastc_is_pointer_type(underlying)
	layout_id := fastc_trim_pointer_type(underlying)
	layout := flat.source_type_name(layout_id)
	fields := g.struct_field_info[layout] or { return none }
	variable := '__vf_promoted_${depth}'
	mut assignments := []string{}
	for field in fields {
		name := fastc_c_identifier(field.name)
		if value := values[name] {
			if value.is_fixed_array {
				if value.fixed_array_copy != '' {
					assignments << value.fixed_array_copy.replace('__vf_promoted_field', '${variable}.${name}')
				} else {
					assignments << 'memcpy(${variable}.${name}, ((${field.typ_text()}){.data=${value.field_value}}).data, sizeof(${variable}.${name}));'
				}
			} else {
				assignments << '${variable}.${name} = (${value.field_value});'
			}
			continue
		}
		prefix := name + '.'
		mut nested := map[string]FastcStructLiteralFieldValue{}
		for path, value in values {
			if path.starts_with(prefix) { nested[path[prefix.len..]] = value }
		}
		if nested.len > 0 {
			nested_default := '${variable}.${name}'
			value := g.render_promoted_pointer_literal(field.typ, nested, nested_default, depth + 1) or { return none }
			assignments << '${variable}.${name} = (${value});'
		}
	}
	mut setup := ''
	fallback := g.render_empty_struct_initializer(layout_id)
	initial := if default_value != '' && is_pointer {
		pointer := '__vf_promoted_default_${depth}'
		setup = '${flat.source_type_name(typ)} ${pointer} = (${default_value});'
		'${pointer} ? *${pointer} : (${fallback})'
	} else if default_value != '' {
		default_value
	} else {
		fallback
	}
	result := if is_pointer {
		'(${flat.source_type_name(typ)})v_fastc_interface_box(&${variable}, sizeof(${layout}))'
	} else {
		variable
	}
	return '({ ${setup} ${layout} ${variable} = (${initial}); ${assignments.join(' ')} ${result}; })'
}
