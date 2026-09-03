module fastc

struct FastcStructLiteralFieldValue {
	explicit_initializers []string
	rendered_field        string
	field_value           string
	fixed_array_copy      string
	is_fixed_array        bool
}

fn (g &Parser) render_struct_literal_field_value(value_tokens []FastcExpressionToken, expected_type string, c_field_name string, temporary_start int, is_shared_pointer bool) ?FastcStructLiteralFieldValue {
	if value_tokens.len >= 2 && value_tokens[0].tok == .name && value_tokens[0].lit == 'chan' {
		// `jobs: chan Task{cap: N}`: channels are erased to a stub `void*`, so drop the capacity
		// initializer to a zero channel. The struct-literal path would otherwise mistake `chan` for
		// a struct type and emit an invalid `(chan){.cap=…}` designated initializer on a `void*`.
		return FastcStructLiteralFieldValue{
			rendered_field: '.${c_field_name}=((chan){0})'
			field_value: '(chan){0}'
		}
	}
	if fixed_element_type := fastc_fixed_array_element_type(expected_type) {
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
				temporary := '__v_fastc_struct_field_${temporary_start + initializers.len}'
				initializers << '__typeof__((${rendered_item})) ${temporary} = (${rendered_item});'
				values << temporary
			}
			joined_values := values.join(',')
			return FastcStructLiteralFieldValue{
				explicit_initializers: initializers
				rendered_field: '.${c_field_name}={${joined_values}}'
				field_value: '{${joined_values}}'
				is_fixed_array: true
			}
		}
		value := g.render_call_argument_expression(value_tokens, expected_type) or { return none }
		is_raw_fixed_array := value_tokens.len > 1 || (value_tokens.len == 1 && value_tokens[0].tok == .name && fastc_global_key(g.module_name, value_tokens[0].lit) in g.globals)
		copy_source := if is_raw_fixed_array {
			value
		} else if expected_type.ends_with('*') {
			'(${value})->data'
		} else {
			'(${value}).data'
		}
		return FastcStructLiteralFieldValue{
			field_value: value
			fixed_array_copy: 'memcpy(__v_fastc_struct_fixed.${c_field_name}, ${copy_source}, sizeof(__v_fastc_struct_fixed.${c_field_name}));'
			is_fixed_array: true
		}
	}
	value := if value_tokens.len == 1 && value_tokens[0].source != '' {
		// A field value carried as a pre-rendered `({ ... })` (e.g. an `or`-unwrap) is used
		// directly so its internal temporaries stay self-contained.
		value_tokens[0].source
	} else {
		g.render_call_argument_expression(value_tokens, expected_type) or { return none }
	}
	temporary := '__v_fastc_struct_field_${temporary_start}'
	// TinyCC cannot `__typeof__` a statement-expression that declares locals or ends in a
	// designated compound literal (`chan T{cap: …}`, an `or`-block); the field type is known, so
	// name it directly in that case rather than inferring it back from the value.
	field_decl_type := if expected_type != '' && value.starts_with('({') {
		fastc_normalize_inferred_type(expected_type)
	} else {
		'__typeof__((${value}))'
	}
	stored_value := if is_shared_pointer {
		'(${expected_type}*)v_fastc_interface_box(&${temporary}, sizeof(${expected_type}))'
	} else {
		temporary
	}
	return FastcStructLiteralFieldValue{
		explicit_initializers: ['${field_decl_type} ${temporary} = (${value});']
		rendered_field: '.${c_field_name}=(${stored_value})'
		field_value: stored_value
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
	mut c_type := if open == 1 && tokens[0].typ != '' {
		tokens[0].typ
	} else {
		g.type_from_expression_tokens(tokens[..open]) or { '' }
	}
	if c_type == '' && is_c_struct_literal {
		c_type = if '#Cstruct#${tokens[2].lit}' in g.declared_types {
			'struct ${tokens[2].lit}'
		} else {
			tokens[2].lit
		}
	}
	mut layout_type := c_type.trim_right('*')
	if layout_type.starts_with('Array_') {
		layout_type = 'array'
	}
	// `Alias{}` where `Alias = u8` (a primitive alias, e.g. ast.EmptyExpr) has no fields, so
	// the "struct literal" is really the primitive's zero value; C rejects the empty `{}`.
	if c_type != '' && open + 1 == close && fastc_primitive_c_type(fastc_normalize_inferred_type(g.underlying_alias_type(c_type))) != none {
		return FastcRenderedExpression{
			source: '((${c_type})0)'
			typ: c_type
		}
	}
	if c_type == '' || (!is_c_struct_literal && layout_type !in g.struct_fields && g.declared_kinds[g.semantic_type_key(c_type)] !in [
		.struct_,
		.union_,
	]) {
		return none
	}
	if open + 1 < close {
		items := fastc_expression_list_items(tokens, open + 1, close) or { return none }
		mut is_positional := false
		if !fastc_expression_tokens_contain(tokens[open + 1..close], .ellipsis) {
			for item in items {
				if item.len == 0 {
					continue
				}
				if !(item.len >= 2 && item[0].tok == .name && item[1].tok == .colon) && !(item.len == 1 && item[0].tok == .name && g.struct_direct_member_type(c_type, item[0].lit) != '') {
					is_positional = true
					break
				}
			}
		}
		if is_positional {
			mut values := []string{cap: items.len}
			for item_index, item in items {
				field := if !is_c_struct_literal && item_index < g.struct_field_info[layout_type].len {
					g.struct_field_info[layout_type][item_index]
				} else {
					FastcStructField{}
				}
				rendered_item := g.render_call_argument_expression(item, field.typ) or { return none }
				values << if field.is_shared_pointer {
					'({ ${field.typ} __v_fastc_shared_field_value = (${rendered_item}); (${field.typ}*)v_fastc_interface_box(&__v_fastc_shared_field_value, sizeof(${field.typ})); })'
				} else {
					rendered_item
				}
			}
			source := if c_type.ends_with('*') {
				'&(${c_type.trim_right('*')}){${values.join(',')}}'
			} else {
				'(${c_type}){${values.join(',')}}'
			}
			return FastcRenderedExpression{
				source: source
				typ: c_type
			}
		}
	}
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
			for index < close && tokens[index].tok !in [.semicolon, .comma] {
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
		if layout_type == 'array' && field_name == 'init' && value_start >= 0 {
			array_init_tokens = tokens[value_start..value_end].clone()
			has_array_init = true
			continue
		}
		mut field_metadata := FastcStructField{}
		mut expected_type := if layout_type == 'array' && field_name == 'init' {
			g.array_element_type(c_type) or { '' }
		} else {
			g.struct_direct_member_type(c_type, field_name)
		}
		if expected_type != '' {
			field_metadata = g.struct_field_metadata(c_type, field_name) or { FastcStructField{} }
		}
		if expected_type == '' && !is_c_struct_literal {
			if field := g.struct_field_metadata(c_type, field_name) {
				field_metadata = field
				expected_type = field.typ
				mut storage_path := field.storage_path.clone()
				storage_path << field.name
				mut c_storage_path := []string{}
				for storage_name in storage_path {
					c_storage_path << fastc_c_identifier(storage_name)
				}
				c_field_name = c_storage_path.join('.')
			} else {
				// Initializing an embedded field by its type name:
				// `Derived{ Base: Base{...} }` sets the `__embedded_N` field.
				for embed_field in g.struct_field_info[layout_type] {
					if embed_field.name.starts_with('__embedded_') && (embed_field.typ == field_name || embed_field.typ.all_after_last('__') == field_name) {
						c_field_name = embed_field.name
						expected_type = embed_field.typ
						break
					}
				}
			}
		}
		field_value := if value_start >= 0 {
			g.render_struct_literal_field_value(tokens[value_start..value_end], expected_type, c_field_name, explicit_initializers.len, field_metadata.is_shared_pointer) or { return none }
		} else {
			g.render_struct_literal_field_value([
				FastcExpressionToken{
					tok: .name
					lit: field_name
				},
			], expected_type, c_field_name, explicit_initializers.len, field_metadata.is_shared_pointer) or { return none }
		}
		explicit_initializers << field_value.explicit_initializers
		if field_value.rendered_field != '' {
			rendered_fields << field_value.rendered_field
			rendered_fields_by_name[field_name] = field_value.rendered_field
		}
		field_values[field_name] = field_value.field_value
		if field_value.fixed_array_copy != '' {
			fixed_array_copies << field_value.fixed_array_copy
		}
		if field_value.is_fixed_array {
			continue
		}
	}
	if update_source == '' {
		// The struct's rendered defaults are consulted; the constants phase
		// re-parses a file that did so after they are ready.
		fastc_note_field_defaults_use(g)
		for field in g.struct_field_info[layout_type] {
			if field.name in field_values {
				continue
			}
			field_default := g.struct_field_initializer_default(field)
			if field_default == '' {
				continue
			}
			c_field_name := fastc_c_identifier(field.name)
			rendered_field := '.${c_field_name}=(${field_default})'
			rendered_fields << rendered_field
			rendered_fields_by_name[field.name] = rendered_field
			field_values[field.name] = field_default
			has_applied_defaults = true
		}
	}
	if layout_type in g.struct_field_info {
		mut ordered_fields := []string{cap: rendered_fields.len}
		mut ordered_values := map[string]bool{}
		for field in g.struct_field_info[layout_type] {
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
	if layout_type == 'array' {
		array_type := c_type.trim_right('*')
		element_type := g.array_element_type(array_type) or { return none }
		length := field_values['len'] or { '0' }
		capacity := field_values['cap'] or { '0' }
		inner_array_element_type := if element_type.starts_with('Array_') {
			g.array_element_type(element_type) or { '' }
		} else {
			''
		}
		base := '((${array_type})builtin____new_array(${length},${capacity},sizeof(${element_type})))'
		mut value_source := base
		if has_array_init {
			inner := g.render_call_argument_expression(array_init_tokens, element_type) or {
				return none
			}
			// `index` names the current element position inside an `init:` closure; bind it
			// to the loop counter so an expression like `init: index` fills 0, 1, 2, ….
			value_source = '({ ${explicit_initializers.join(' ')} ${array_type} __v_fastc_array_init = ${base}; for (int __v_fastc_array_index = 0; __v_fastc_array_index < __v_fastc_array_init.len; __v_fastc_array_index++) { int index = __v_fastc_array_index; (void)index; ((${element_type} *)__v_fastc_array_init.data)[__v_fastc_array_index] = (${inner}); } __v_fastc_array_init; })'
		} else if inner_array_element_type != '' {
			// A zeroed inner dynamic array has element_size == 0, so appending to an
			// element of `[][]T{len: n}` copies no data. Construct a valid empty inner
			// array for every outer element, as the main C backend does.
			inner_default := '((${element_type})builtin____new_array(0,0,sizeof(${inner_array_element_type})))'
			value_source = '({ ${explicit_initializers.join(' ')} ${array_type} __v_fastc_array_init = ${base}; for (int __v_fastc_array_index = 0; __v_fastc_array_index < __v_fastc_array_init.len; __v_fastc_array_index++) { ((${element_type} *)__v_fastc_array_init.data)[__v_fastc_array_index] = ${inner_default}; } __v_fastc_array_init; })'
		} else if explicit_initializers.len > 0 {
			value_source = '({ ${explicit_initializers.join(' ')} ${base}; })'
		}
		if c_type.ends_with('*') {
			value_source = '({ ${array_type} __v_fastc_array_pointer_value = (${value_source}); (${c_type})v_fastc_interface_box(&__v_fastc_array_pointer_value, sizeof(${array_type})); })'
		}
		return FastcRenderedExpression{
			source: value_source
			typ: c_type
		}
	}
	if update_source != '' {
		mut assignments := []string{cap: rendered_fields.len}
		for field in rendered_fields {
			assignments << '__v_fastc_struct_update${field};'
		}
		if c_type.ends_with('*') {
			base_type := c_type.trim_right('*')
			copy_statements := fixed_array_copies.join(' ')
			return FastcRenderedExpression{
				source: '({ ${base_type} __v_fastc_struct_update = *(${update_source}); ${explicit_initializers.join(' ')} ${assignments.join(' ')} ${copy_statements.replace('__v_fastc_struct_fixed', '__v_fastc_struct_update')} (${c_type})v_fastc_interface_box(&__v_fastc_struct_update, sizeof(${base_type})); })'
				typ: c_type
			}
		}
		copy_statements := fixed_array_copies.join(' ')
		return FastcRenderedExpression{
			source: '({ ${c_type} __v_fastc_struct_update = (${update_source}); ${explicit_initializers.join(' ')} ${assignments.join(' ')} ${copy_statements.replace('__v_fastc_struct_fixed', '__v_fastc_struct_update')} __v_fastc_struct_update; })'
			typ: c_type
		}
	}
	if has_applied_defaults {
		rendered := g.render_struct_literal_with_defaults(c_type, layout_type, explicit_initializers, rendered_fields, rendered_fields_by_name)
		if fixed_array_copies.len == 0 {
			return rendered
		}
		access := if c_type.ends_with('*') { '->' } else { '.' }
		copies := fixed_array_copies.join(' ').replace('__v_fastc_struct_fixed.', '__v_fastc_struct_with_fixed${access}')
		return FastcRenderedExpression{
			source: '({ ${c_type} __v_fastc_struct_with_fixed = (${rendered.source}); ${copies} __v_fastc_struct_with_fixed; })'
			typ: c_type
		}
	}
	literal_source := if c_type.ends_with('*') {
		'(${c_type})v_fastc_interface_box(&(${c_type.trim_right('*')}){${rendered_fields.join(',')}}, sizeof(${c_type.trim_right('*')}))'
	} else {
		'(${c_type}){${rendered_fields.join(',')}}'
	}
	if fixed_array_copies.len > 0 {
		base_type := c_type.trim_right('*')
		copies := fixed_array_copies.join(' ')
		result := if c_type.ends_with('*') {
			'(${c_type})v_fastc_interface_box(&__v_fastc_struct_fixed, sizeof(${base_type}))'
		} else {
			'__v_fastc_struct_fixed'
		}
		return FastcRenderedExpression{
			source: '({ ${explicit_initializers.join(' ')} ${base_type} __v_fastc_struct_fixed = (${base_type}){${rendered_fields.join(',')}}; ${copies} ${result}; })'
			typ: c_type
		}
	}
	if explicit_initializers.len > 0 {
		return FastcRenderedExpression{
			source: '({ ${explicit_initializers.join(' ')} ${literal_source}; })'
			typ: c_type
		}
	}
	return FastcRenderedExpression{
		source: literal_source
		typ: c_type
	}
}
