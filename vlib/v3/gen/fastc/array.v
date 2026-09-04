module fastc

import strings
import v3.token

// render_higher_order_method_expression lowers a trailing `.map/.filter/.any/.all/.count`
// closure method that appears inside a larger expression (`x in arr.map(it.f)`), rather than as
// the whole statement the streaming reader lowers inline. It renders the receiver and the
// `it`-closure from tokens and emits the same statement-expression the reader produces, so the
// magic array methods work as call arguments, membership collections and struct-field values.
fn (g &Parser) render_higher_order_method_expression(tokens []FastcExpressionToken) ?FastcRenderedExpression {
	if tokens.len < 5 || tokens.last().tok != .rpar {
		return none
	}
	// Find the `(` matching the final `)`.
	mut depth := 0
	mut open := -1
	for i := tokens.len - 1; i >= 0; i-- {
		match tokens[i].tok {
			.rpar { depth++ }
			.lpar {
				depth--
				if depth == 0 {
					open = i
					break
				}
			}
			else {}
		}
	}
	if open < 3 || tokens[open - 1].tok != .name || tokens[open - 2].tok != .dot {
		return none
	}
	method := tokens[open - 1].lit
	if method !in ['map', 'filter', 'any', 'all', 'count'] {
		return none
	}
	receiver_tokens := tokens[..open - 2]
	receiver_type := fastc_normalize_inferred_type(g.underlying_alias_type(g.infer_expression_type(receiver_tokens) or {
		return none
	}))
	if !receiver_type.starts_with('Array_') {
		return none
	}
	element_type := g.array_element_type(receiver_type) or { return none }
	mut closure_tokens := tokens[open + 1..tokens.len - 1].clone()
	if closure_tokens.len == 0 {
		return none
	}
	mut it_name := 'it'
	if closure_tokens[0].tok == .pipe {
		// Explicit closure header `|param|` names the element instead of implicit `it`.
		if closure_tokens.len < 4 || closure_tokens[1].tok != .name
			|| closure_tokens[2].tok != .pipe {
			return none
		}
		it_name = closure_tokens[1].lit
		closure_tokens = closure_tokens[3..].clone()
		if closure_tokens.len == 0 {
			return none
		}
	}
	mut w := unsafe { &Parser(g) }
	collection_type := receiver_type.trim_right('*')
	fastc_register_composite_type(collection_type, mut w.composite_types)
	receiver := g.render_method_receiver_expression(receiver_tokens) or { return none }
	receiver_source := receiver.source
	collection_source := if receiver_type.ends_with('*') {
		'*(${receiver_source})'
	} else {
		receiver_source
	}
	had_it := it_name in g.locals
	saved_it := g.locals[it_name] or { FastcLocal{} }
	it_c_name := fastc_c_identifier(it_name)
	w.locals[it_name] = FastcLocal{
		typ: element_type
		c_name: it_c_name
	}
	// A closure with a flow-sensitive `&&` narrowing (`it.expr is AnonFn && it.expr.decl.…`) must
	// render through the narrowing renderer so the later conjunct reads the smart-cast member;
	// render_call_argument_expression alone would leave `it.expr.decl` on the un-narrowed boxed
	// value. (The reader's inline higher-order path already does this; this is the render-time path
	// used when the call is a sub-expression of a larger condition.)
	mut closure := ''
	if w.boolean_expression_has_narrowing(closure_tokens) {
		if narrowed := w.render_narrowing_boolean_expression(closure_tokens) {
			closure = narrowed
		}
	}
	if closure == '' {
		closure = g.render_call_argument_expression(closure_tokens, '') or {
			if had_it {
				w.locals[it_name] = saved_it
			} else {
				w.locals.delete(it_name)
			}
			return none
		}
	}
	// A closure ending in a top-level `or {}` block (`arr.map(convert(it) or { it })`) yields
	// the option's value type; inferring the whole expression (which trails the `or`) wraps it
	// wrongly, so infer from the option-producing part before the `or`.
	mut closure_or := -1
	mut closure_or_depth := 0
	for j, ct in closure_tokens {
		match ct.tok {
			.lpar, .lsbr, .lcbr { closure_or_depth++ }
			.rpar, .rsbr, .rcbr { closure_or_depth-- }
			.key_or {
				if closure_or_depth == 0 && closure_or < 0 {
					closure_or = j
				}
			}
			else {}
		}
	}
	mut closure_type := if closure_or > 0 {
		vt := g.option_value_type_for_expression(closure_tokens[..closure_or])
		fastc_normalize_inferred_type(if vt != '' {
			vt
		} else {
			g.infer_expression_type(closure_tokens[..closure_or]) or { '' }
		})
	} else {
		fastc_normalize_inferred_type(g.infer_expression_type(closure_tokens) or { '' })
	}
	// A bare function is applied to each element (`items.map(convert)`).
	if closure_tokens.len == 1 && closure_tokens[0].tok == .name
		&& closure_tokens[0].lit != it_name {
		function_key := w.unqualified_function_key(closure_tokens[0].lit)
		if signature := w.functions[function_key] {
			closure = '${closure}(${it_c_name})'
			closure_type = signature.return_type
		}
	}
	if had_it {
		w.locals[it_name] = saved_it
	} else {
		w.locals.delete(it_name)
	}
	if method in ['map'] && closure_type == '' {
		return none
	}
	src := w.temporary_name('collection')
	dst := w.temporary_name('mapped')
	idx := w.temporary_name('index')
	elem := w.temporary_name('element')
	mut lowered := ''
	mut result_type := collection_type
	if method == 'map' {
		result_type = fastc_array_c_type(closure_type)
		fastc_register_composite_type(result_type, mut w.composite_types)
		lowered = '({ ${collection_type} ${src} = (${collection_source}); ${result_type} ${dst} = (${result_type})builtin____new_array(0, ${src}.len, sizeof(${closure_type})); for (int ${idx} = 0; ${idx} < ${src}.len; ${idx}++) { ${element_type} ${it_c_name} = ((${element_type} *)${src}.data)[${idx}]; ${closure_type} ${elem} = (${closure}); builtin__array_push((array *)&${dst}, &${elem}); } ${dst}; })'
	} else if method == 'filter' {
		lowered = '({ ${collection_type} ${src} = (${collection_source}); ${collection_type} ${dst} = (${collection_type})builtin____new_array(0, ${src}.len, sizeof(${element_type})); for (int ${idx} = 0; ${idx} < ${src}.len; ${idx}++) { ${element_type} ${it_c_name} = ((${element_type} *)${src}.data)[${idx}]; if (${closure}) { builtin__array_push((array *)&${dst}, &${it_c_name}); } } ${dst}; })'
	} else if method == 'count' {
		result_type = 'int'
		lowered = '({ ${collection_type} ${src} = (${collection_source}); int ${dst} = 0; for (int ${idx} = 0; ${idx} < ${src}.len; ${idx}++) { ${element_type} ${it_c_name} = ((${element_type} *)${src}.data)[${idx}]; if (${closure}) { ${dst}++; } } ${dst}; })'
	} else {
		result_type = 'bool'
		initial := if method == 'all' { 'true' } else { 'false' }
		condition := if method == 'all' { '!(${closure})' } else { closure }
		matched := if method == 'all' { 'false' } else { 'true' }
		lowered = '({ ${collection_type} ${src} = (${collection_source}); bool ${dst} = ${initial}; for (int ${idx} = 0; ${idx} < ${src}.len; ${idx}++) { ${element_type} ${it_c_name} = ((${element_type} *)${src}.data)[${idx}]; if (${condition}) { ${dst} = ${matched}; break; } } ${dst}; })'
	}
	return FastcRenderedExpression{
		source: lowered
		typ: result_type
	}
}

// fastc_is_empty_fixed_array_literal reports whether the tokens are `[N]T{}` — a fixed-size array
// literal (a size between the brackets) with an empty, zeroing initializer.
fn fastc_is_empty_fixed_array_literal(tokens []FastcExpressionToken) bool {
	if tokens.len < 5 || tokens[0].tok != .lsbr || tokens[1].tok == .rsbr {
		return false
	}
	if tokens[tokens.len - 1].tok != .rcbr || tokens[tokens.len - 2].tok != .lcbr {
		return false
	}
	// A `]` must close the size brackets somewhere before the element type / `{}`.
	for t in tokens {
		if t.tok == .rsbr {
			return true
		}
	}
	return false
}

fn (g &Parser) fixed_array_uses_raw_storage(tokens []FastcExpressionToken) bool {
	if tokens.len == 1 {
		return fastc_global_key(g.module_name, tokens[0].lit) in g.globals
	}
	if tokens.len >= 3 && tokens[tokens.len - 2].tok == .dot && tokens.last().tok == .name {
		// A module-qualified fixed array (`util.name_char_table`) mirrors the bare-name case: a
		// `__global` is raw C storage, but a const is a FixedArray struct indexed through `.data`.
		// Only a genuine struct-field fixed array is always raw C storage.
		if tokens.len == 3 && tokens[0].tok == .name && tokens[0].lit in g.imports {
			return fastc_global_key(g.imports[tokens[0].lit], tokens[2].lit) in g.globals
		}
		return true
	}
	if tokens.len >= 4 && tokens.last().tok == .rsbr {
		mut depth := 0
		for i := tokens.len - 1; i >= 0; i-- {
			if tokens[i].tok == .rsbr {
				depth++
			} else if tokens[i].tok == .lsbr {
				depth--
				if depth == 0 {
					// A fixed array nested inside a raw struct field remains raw C array
					// storage after indexing (`s[0]` in `u32 s[4][256]`).
					return i > 0 && g.fixed_array_uses_raw_storage(tokens[..i])
				}
			}
		}
	}
	return false
}

fn (g &Parser) render_array_access_expression(tokens []FastcExpressionToken) ?FastcRenderedExpression {
	// A pre-rendered synthetic base atom (e.g. a string interpolation `'${x}'[0]`, folded into
	// one token carrying its C text in `.source`) is `.string`, not `.name`; accept it too so
	// the index applies to the rendered string rather than reaching the raw renderer.
	if tokens.len < 4 || (tokens[0].tok !in [.name, .string, .lpar] && tokens[0].source == '') || tokens.last().tok != .rsbr {
		return none
	}
	mut open := -1
	mut depth := 0
	for i := tokens.len - 1; i >= 0; i-- {
		if tokens[i].tok == .rsbr {
			depth++
		} else if tokens[i].tok == .lsbr {
			depth--
			if depth == 0 {
				open = i
				break
			}
		}
	}
	if open <= 0 {
		return none
	}
	mut prefix_depth := 0
	for item in tokens[..open] {
		if item.tok in [.lpar, .lsbr, .lcbr] {
			prefix_depth++
		} else if item.tok in [.rpar, .rsbr, .rcbr] {
			prefix_depth--
		} else if prefix_depth == 0 && item.tok.is_assignment() {
			// `target = [value]` ends in `]`, but its bracket pair is an array literal,
			// not an index applied to the assignment prefix.
			return none
		}
	}
	close := fastc_matching_delimiter(tokens, open, .lsbr, .rsbr) or { return none }
	if close != tokens.len - 1 {
		return none
	}
	// A fully-wrapping paren pair around the base (`(*names)[i]`) is stripped so the deref base
	// logic below sees `*names`; an interior group stays balanced.
	base_tokens := fastc_strip_paren_tokens(tokens[..open])
	// `cond && m[k]` / `a || arr[i]`: a top-level `&&`/`||` in the base means the `[…]` indexes
	// its right operand, not the whole boolean; leave it to the logical/map renderers so the
	// slice/index path does not treat `(cond && m)` as an array.
	if fastc_top_level_boolean_split(base_tokens, .and) != none || fastc_top_level_boolean_split(base_tokens, .logical_or) != none {
		return none
	}
	base_type := g.infer_expression_type(base_tokens) or { return none }
	base_layout_type := g.underlying_alias_type(base_type)
	base_source := if base_tokens.len == 1 && base_tokens[0].source != '' {
		// A pre-rendered synthetic base token (e.g. an `(x as T).field` group the reader
		// already lowered and folded into one token) carries its C text in `.source`, not
		// `.lit`; reading `.lit` here would yield an empty collection (`array_get(, i)`).
		base_tokens[0].source
	} else if base_tokens.len == 1 && base_tokens[0].tok == .string {
		// A string-literal base (`"0123456789abcdef"[i]`): render the C string so the string
		// index below lowers to `string_at`, not a raw C index on the `string` struct.
		literal := fastc_c_string(base_tokens[0].lit) or { return none }
		if g.selfhost { '_S(${literal})' } else { literal }
	} else if base_tokens.len >= 2 && base_tokens[0].tok == .mul {
		// `(*names)[i]` indexes through a pointer-to-array; render the pointee array value.
		inner := g.render_member_receiver(base_tokens[1..]) or {
			g.render_raw_expression_tokens(base_tokens[1..]) or { return none }
		}
		'*(${inner})'
	} else if base_tokens.len == 1 {
		g.resolved_root_expression_name(base_tokens[0].lit)
	} else if nested_base := g.render_array_access_expression(base_tokens) {
		nested_base.source
	} else if map_base := g.render_map_expression(base_tokens) {
		// A map lookup that yields an array (`m[k][i]`, `m[k].field`) is the index base;
		// its read value feeds the outer access.
		map_base.source
	} else if raw_base := g.render_raw_expression_tokens(base_tokens) {
		if method_base := g.render_method_call_expression(base_tokens, raw_base) {
			method_base.source
		} else if call_base := g.render_missing_call_arguments(base_tokens) {
			// A free-function-call receiver (`options_after(a, [x])#[1..]`) must have its
			// arguments lowered (array/map literals, propagation) rather than reused raw.
			call_base.source
		} else if member_base := g.render_member_receiver(base_tokens) {
			member_base
		} else {
			raw_base
		}
	} else if member_base := g.render_member_receiver(base_tokens) {
		member_base
	} else {
		return none
	}
	mut range_index := -1
	for i in open + 1 .. close {
		if tokens[i].tok == .dotdot {
			range_index = i
			break
		}
	}
	if range_index >= 0 {
		start := if range_index == open + 1 {
			'0'
		} else {
			g.render_membership_candidate(tokens[open + 1..range_index], 'int') or { return none }
		}
		omitted_end := range_index + 1 == close
		is_fixed_array := base_layout_type.starts_with('FixedArray_')
		is_raw_fixed_array := is_fixed_array && g.fixed_array_uses_raw_storage(base_tokens)
		needs_receiver_temporary := omitted_end && base_tokens.len > 1 && !is_raw_fixed_array
		receiver_name := '__v_fastc_slice_receiver'
		receiver_source := if needs_receiver_temporary { receiver_name } else { base_source }
		receiver_is_pointer := base_type.ends_with('*') && !needs_receiver_temporary
		access := if receiver_is_pointer { '->' } else { '.' }
		end := if omitted_end {
			if is_fixed_array {
				fastc_fixed_array_length(base_layout_type.trim_right('*')) or { return none }
			} else {
				'${receiver_source}${access}len'
			}
		} else {
			g.render_membership_candidate(tokens[range_index + 1..close], 'int') or { return none }
		}
		mut slice_type := if base_layout_type == 'string' {
			'string'
		} else {
			base_type.trim_right('*')
		}
		mut slice_source := if base_layout_type == 'string' {
			'builtin__string_substr(${if receiver_is_pointer { '*' } else { '' }}(${receiver_source}), ${start}, ${end})'
		} else if is_fixed_array {
			// Slicing a fixed array yields a NEW dynamic array copied from the element
			// range; the raw `u8 x[N]` / wrapped `.data` storage differs by receiver kind.
			element := g.array_element_type(base_layout_type.trim_right('*')) or { return none }
			norm_element := fastc_normalize_inferred_type(element)
			data_expr := if is_raw_fixed_array {
				'(${receiver_source})'
			} else {
				'(${receiver_source})${access}data'
			}
			slice_len := '((${end}) - (${start}))'
			slice_type = fastc_array_c_type(norm_element)
			mut w := unsafe { &Parser(g) }
			fastc_register_composite_type(slice_type, mut w.composite_types)
			'((${slice_type})builtin__new_array_from_c_array(${slice_len}, ${slice_len}, sizeof(${norm_element}), &((${data_expr})[${start}])))'
		} else {
			array_value := if receiver_is_pointer {
				'*(${receiver_source})'
			} else {
				receiver_source
			}
			'builtin__array_slice(${array_value}, ${start}, ${end})'
		}
		if needs_receiver_temporary {
			slice_source = '({ __typeof__((${base_source})) ${receiver_name} = (${base_source}); ${slice_source}; })'
		}
		return FastcRenderedExpression{
			source: slice_source
			typ: slice_type
		}
	}
	is_array_pointer := base_type.ends_with('*') && g.array_element_type(base_type) != none
	element_type := if is_array_pointer {
		g.array_element_type(base_type) or { return none }
	} else if base_type.ends_with('*') {
		base_type[..base_type.len - 1]
	} else if base_layout_type == 'string' {
		'u8'
	} else {
		g.array_element_type(base_type) or { return none }
	}
	index_source := g.render_membership_candidate(tokens[open + 1..close], 'int') or { return none }
	if base_layout_type == 'string' {
		if g.direct_array_access && !base_type.ends_with('*') {
			return FastcRenderedExpression{
				source: '((${base_source}).str[${index_source}])'
				typ: element_type
			}
		}
		return FastcRenderedExpression{
			source: 'builtin__string_at(${base_source}, ${index_source})'
			typ: element_type
		}
	}
	is_raw_fixed_array := base_type.starts_with('FixedArray_') && g.fixed_array_uses_raw_storage(base_tokens)
	if fixed_length := fastc_fixed_array_length(base_type.trim_right('*')) {
		checked_index := 'builtin__v_fixed_index(${index_source}, ${fixed_length})'
		if is_raw_fixed_array {
			return FastcRenderedExpression{
				source: '((${base_source})[${checked_index}])'
				typ: element_type
			}
		}
		access := if base_type.ends_with('*') { '->' } else { '.' }
		return FastcRenderedExpression{
			source: '((${base_source})${access}data[${checked_index}])'
			typ: element_type
		}
	}
	if is_raw_fixed_array {
		return FastcRenderedExpression{
			source: '((${base_source})[${index_source}])'
			typ: element_type
		}
	}
	if base_type.ends_with('*') && !is_array_pointer {
		return FastcRenderedExpression{
			source: '((${base_source})[${index_source}])'
			typ: element_type
		}
	}
	array_value := if base_type.ends_with('*') { '*(${base_source})' } else { base_source }
	if g.direct_array_access {
		return FastcRenderedExpression{
			source: '(((${element_type} *)(${array_value}).data)[${index_source}])'
			typ: element_type
		}
	}
	return FastcRenderedExpression{
		source: '(*(${element_type} *)builtin__array_get(${array_value}, ${index_source}))'
		typ: element_type
	}
}

fn (g &Parser) render_nested_array_access_expression(tokens []FastcExpressionToken, rendered_expression string) ?FastcRenderedExpression {
	if tokens.len < 3 {
		return none
	}
	mut rendered := rendered_expression
	mut changed := false
	for i := tokens.len - 2; i >= 0; i-- {
		if tokens[i].tok != .name || tokens[i + 1].tok != .lsbr {
			continue
		}
		// A name after `.` is a field, not a new root expression. Treating it as
		// a local can replace the field suffix inside its owning expression. Render
		// the complete member-rooted access instead, preserving embedded promotion.
		if i > 0 && tokens[i - 1].tok == .dot {
			close := fastc_matching_delimiter(tokens, i + 1, .lsbr, .rsbr) or { continue }
			start := fastc_method_receiver_start(tokens, i + 1)
			if start >= i {
				continue
			}
			access_tokens := tokens[start..close + 1]
			raw_access := g.render_raw_expression_tokens(access_tokens) or { continue }
			replacement := g.render_array_access_expression(access_tokens) or { continue }
			mut needle := raw_access
			if !rendered.contains(needle) {
				// Pointer-member lowering may already have changed the base from dots to arrows
				// (`g.a.nodes[i]` -> `g->a->nodes[i]`) before the dynamic-array index is
				// rewritten. Reconstruct that spelling from the complete member base.
				if member_base := g.render_member_receiver(tokens[start..i + 1]) {
					index_source := g.render_membership_candidate(tokens[i + 2..close], 'int') or {
						continue
					}
					pointer_needle := '${member_base}[${index_source}]'
					if rendered.contains(pointer_needle) {
						needle = pointer_needle
					}
				}
			}
			if rendered.contains(needle) {
				rendered = rendered.replace(needle, replacement.source)
				changed = true
			}
			continue
		}
		close := fastc_matching_delimiter(tokens, i + 1, .lsbr, .rsbr) or { continue }
		if close <= i + 1 {
			continue
		}
		if fastc_expression_tokens_contain(tokens[i + 2..close], .dotdot) {
			access_tokens := tokens[i..close + 1]
			raw_access := g.render_raw_expression_tokens(access_tokens) or { continue }
			replacement := g.render_array_access_expression(access_tokens) or { continue }
			if rendered.contains(raw_access) {
				rendered = rendered.replace(raw_access, replacement.source)
				changed = true
			}
			continue
		}
		index_source := g.render_membership_candidate(tokens[i + 2..close], 'int') or { continue }
		base_source := g.resolved_root_expression_name(tokens[i].lit)
		needle := '${base_source}[${index_source}]'
		replacement := g.render_array_access_expression(tokens[i..close + 1]) or { continue }
		if rendered.contains(needle) {
			// A method receiver may already contain the lowered direct access
			// `((s).str[i])`; the raw `s[i]` suffix there is a struct member, not
			// another root access to lower.
			replaced := fastc_replace_c_root_identifier(rendered, needle, replacement.source)
			if replaced != rendered {
				rendered = replaced
				changed = true
			}
		}
	}
	for open, item in tokens {
		if item.tok != .lsbr || open == 0 || tokens[open - 1].tok != .rpar {
			continue
		}
		close := fastc_matching_delimiter(tokens, open, .lsbr, .rsbr) or { continue }
		if close <= open + 1 || fastc_expression_tokens_contain(tokens[open + 1..close], .dotdot) {
			continue
		}
		start := fastc_method_receiver_start(tokens, open)
		if start >= open || tokens[start].tok != .name {
			continue
		}
		access_tokens := tokens[start..close + 1]
		replacement := g.render_array_access_expression(access_tokens) or { continue }
		raw_access := g.render_raw_expression_tokens(access_tokens) or { '' }
		if raw_access != '' && rendered.contains(raw_access) {
			rendered = rendered.replace(raw_access, replacement.source)
			changed = true
			continue
		}
		base_tokens := tokens[start..open]
		raw_base := g.render_raw_expression_tokens(base_tokens) or { continue }
		method_base := g.render_method_call_expression(base_tokens, raw_base) or { continue }
		index_source := g.render_membership_candidate(tokens[open + 1..close], 'int') or {
			continue
		}
		needle := '${method_base.source}[${index_source}]'
		if rendered.contains(needle) {
			rendered = rendered.replace(needle, replacement.source)
			changed = true
		}
	}
	if !changed {
		return none
	}
	// Index lowering can expose a pointer-valued element or pointer field later in
	// the same chain (`items[i].scope.parent`). Finish that promotion here so callers
	// that return this rewrite directly do not leave a C `.` on a pointer value.
	if pointer_members := g.render_pointer_member_access_expression(tokens, rendered) {
		rendered = pointer_members.source
	}
	inferred_type := g.infer_expression_type(tokens) or { '' }
	return FastcRenderedExpression{
		source: rendered
		typ: inferred_type
	}
}

// render_indexed_member_receiver renders an index and the field chain that follows it while
// preserving the exact pointer type of each field (`branches[i].scope` where `scope` is `&Scope`).
fn (g &Parser) render_indexed_member_receiver(tokens []FastcExpressionToken) ?FastcRenderedExpression {
	mut close := -1
	for i := tokens.len - 1; i >= 0; i-- {
		if tokens[i].tok == .rsbr {
			close = i
			break
		}
	}
	if close < 0 {
		return none
	}
	indexed := g.render_array_access_expression(tokens[..close + 1]) or { return none }
	mut source := indexed.source
	mut current_type := indexed.typ
	mut i := close + 1
	for i < tokens.len {
		if i + 1 >= tokens.len || tokens[i].tok != .dot || tokens[i + 1].tok != .name {
			return none
		}
		field := g.struct_field_metadata(current_type, tokens[i + 1].lit) or { return none }
		for storage_name in field.storage_path {
			separator := if current_type.ends_with('*') { '->' } else { '.' }
			source += separator + fastc_c_identifier(storage_name)
			current_type = g.struct_direct_member_type(current_type, storage_name)
			if current_type == '' {
				return none
			}
		}
		separator := if current_type.ends_with('*') { '->' } else { '.' }
		field_source := source + separator + fastc_c_identifier(field.name)
		source = if field.is_shared_pointer { '*(${field_source})' } else { field_source }
		current_type = field.typ
		i += 2
	}
	return FastcRenderedExpression{
		source: source
		typ: current_type
	}
}

fn (g &Parser) resolved_root_expression_name(name string) string {
	if global_name := g.globals[fastc_global_key(g.module_name, name)] {
		return global_name
	}
	if constant_name := g.constants[fastc_constant_key(g.module_name, name)] {
		return constant_name
	}
	if constant_name := g.constants[name] {
		return constant_name
	}
	if local := g.locals[name] {
		if local.c_name != '' {
			return local.c_name
		}
	}
	// A local/parameter whose name is a C keyword (`short`, `default`) is emitted as
	// `__v_fastc_keyword_<name>`; return that so an index base matches the render_raw buffer
	// spelling and its lowered `string_at`/`array_get` receiver is the real variable.
	return fastc_c_identifier(name)
}

// fastc_inline_array_element_equality compares two dynamic-array values element-wise as a
// statement-expression, used for `x in [][]T` membership where the array elements themselves
// cannot be compared with C `==`. The inner element type must be a scalar or string.
fn (g &Parser) fastc_inline_array_element_equality(array_type string, left string, right string) !string {
	inner_element := g.array_element_type(array_type) or { return error('no element type') }
	if fastc_trim_pointer_suffix(g.underlying_alias_type(inner_element)).starts_with('Array_') {
		// A further-nested array element would need another loop level; not supported yet.
		return error('nested array element')
	}
	inner_comparison := if g.underlying_alias_type(inner_element).trim_right('*') == 'string' {
		'builtin__string_eq(((${inner_element} *)__v_fastc_meq_l.data)[__v_fastc_meq_k], ((${inner_element} *)__v_fastc_meq_r.data)[__v_fastc_meq_k])'
	} else {
		'(((${inner_element} *)__v_fastc_meq_l.data)[__v_fastc_meq_k] == ((${inner_element} *)__v_fastc_meq_r.data)[__v_fastc_meq_k])'
	}
	return '({ ${array_type} __v_fastc_meq_l = (${left}); ${array_type} __v_fastc_meq_r = (${right}); bool __v_fastc_meq = (__v_fastc_meq_l.len == __v_fastc_meq_r.len); for (int __v_fastc_meq_k = 0; __v_fastc_meq && __v_fastc_meq_k < __v_fastc_meq_l.len; __v_fastc_meq_k++) { if (!${inner_comparison}) { __v_fastc_meq = false; break; } } __v_fastc_meq; })'
}

fn (g &Parser) render_membership_candidate(tokens []FastcExpressionToken, expected_type string) ?string {
	if tokens.len == 1 && tokens[0].source != '' {
		// A synthetic atom (an `or`-unwrap, interpolation, etc.) carries its complete C
		// spelling in `source`; use it verbatim rather than re-rendering its bare `lit`.
		return tokens[0].source
	}
	if tokens.len == 2 && tokens[0].tok == .dot && tokens[1].tok == .name && g.declared_kinds[g.semantic_type_key(expected_type)] == .enum_ {
		return '${expected_type.trim_right('*')}__${tokens[1].lit}'
	}
	if array_access := g.render_array_access_expression(tokens) {
		return array_access.source
	}
	raw := g.render_raw_expression_tokens(tokens) or { return none }
	if special := g.render_special_expression(tokens, raw) {
		return special.source
	}
	if map_expression := g.render_map_expression(tokens) {
		return map_expression.source
	}
	if method_expression := g.render_method_call_expression(tokens, raw) {
		return method_expression.source
	}
	if call_expression := g.render_missing_call_arguments(tokens) {
		return call_expression.source
	}
	if pointer_members := g.render_pointer_member_access_expression(tokens, raw) {
		return pointer_members.source
	}
	if member_source := g.render_member_receiver(tokens) {
		return member_source
	}
	if tokens.len >= 4 && tokens[0].tok == .name && tokens[1].tok == .lpar && tokens.last().tok == .rpar {
		if cast_type := fastc_primitive_c_type(tokens[0].lit) {
			close := fastc_matching_rpar(tokens, 1) or { return none }
			if close == tokens.len - 1 {
				inner := g.render_membership_candidate(tokens[2..close], '') or { return none }
				return '((${fastc_output_c_type(cast_type)})(${inner}))'
			}
		}
	}
	if promoted := g.render_leading_member_chain_promotion(tokens, expected_type) {
		return promoted
	}
	return raw
}

// render_leading_member_chain_promotion handles a compound expression that begins
// with a member chain reaching an EMBEDDED field (`ss.pos + 2`), which the pure
// member-chain renderer rejects because of the trailing operator. It promotes the
// leading chain and renders the rest recursively. Returns none unless the leading
// chain is actually an embedded access, so every other expression keeps its
// byte-for-byte raw form.
fn (g &Parser) render_leading_member_chain_promotion(tokens []FastcExpressionToken, expected_type string) ?string {
	if tokens.len < 4 || tokens[0].tok != .name {
		return none
	}
	mut chain_end := 1
	for chain_end + 1 < tokens.len && tokens[chain_end].tok == .dot && tokens[chain_end + 1].tok == .name {
		chain_end += 2
	}
	// Need `root.field` (chain_end >= 3) followed by an arithmetic operator; a pure
	// member chain (chain_end == tokens.len) has no trailing token and is handled
	// upstream, so bail before indexing past the end.
	if chain_end < 3 || chain_end >= tokens.len {
		return none
	}
	operator := tokens[chain_end]
	if operator.tok !in [.plus, .minus, .mul, .div, .mod] {
		return none
	}
	chain_source := g.render_member_receiver(tokens[..chain_end]) or { return none }
	if !chain_source.contains('__embedded_') {
		return none
	}
	rest_source := g.render_membership_candidate(tokens[chain_end + 1..], expected_type) or {
		return none
	}
	return '${chain_source} ${operator.tok.str()} ${rest_source}'
}

fn (g &Parser) render_raw_expression_tokens(tokens []FastcExpressionToken) ?string {
	if tokens.len == 1 {
		item := tokens[0]
		if item.source != '' {
			return item.source
		}
		return match item.tok {
			.name { g.resolved_expression_name(item.lit, .unknown) }
			.number {
				if g.selfhost {
					fastc_c_selfhost_number(item.lit)
				} else {
					fastc_c_number(item.lit) or { return none }
				}
			}
			.string {
				literal := fastc_c_string(item.lit) or { return none }
				if g.selfhost { '_S(${literal})' } else { literal }
			}
			.char {
				if item.lit.starts_with('c:') {
					fastc_c_string("'" + item.lit['c:'.len..] + "'") or { return none }
				} else {
					fastc_c_rune(item.lit) or { return none }
				}
			}
			.key_true { '((bool)true)' }
			.key_false { '((bool)false)' }
			.key_nil { 'NULL' }
			.key_none { '(Option){.state=2}' }
			else {
				if item.lit == '' { item.tok.str() } else { item.lit }
			}
		}
	}
	mut result := strings.new_builder(32)
	mut cast_closes := []int{}
	mut cast_open := -1
	mut previous_module_separator := false
	for i, item in tokens {
		mut piece := item.lit
		if item.tok == .right_shift_unsigned {
			// V's logical right shift `>>>` has no C spelling; on the unsigned operand it is
			// always applied to (V code casts to an unsigned type first, e.g. `u64(x) >>> n`)
			// a plain C `>>` is the logical shift.
			piece = '>>'
		}
		module_separator := item.tok == .dot && g.expression_dot_is_module_separator(tokens, i)
		is_direct_pointer_cast := item.tok in [.amp, .and] && fastc_token_is_prefix_operator(tokens, i) && i + 2 < tokens.len && tokens[i + 1].tok == .name && tokens[i + 2].tok == .lpar && (fastc_primitive_c_type(tokens[i + 1].lit) != none || g.resolve_declared_type_key(tokens[i + 1].lit) != none)
		is_c_pointer_cast := item.tok in [.amp, .and] && fastc_token_is_prefix_operator(tokens, i) && i + 4 < tokens.len && tokens[i + 1].tok == .name && tokens[i + 1].lit == 'C' && tokens[i + 2].tok == .dot && tokens[i + 3].tok == .name && tokens[i + 4].tok == .lpar
		if item.source != '' {
			// Synthetic expression atoms (an `or` unwrap, interpolation, anonymous
			// function, etc.) carry their complete C spelling in `source`. Preserve it
			// when the atom is nested inside a larger binary or call expression.
			piece = item.source
		} else if is_direct_pointer_cast || is_c_pointer_cast {
			piece = ''
		} else if item.tok == .name && i + 1 < tokens.len && tokens[i + 1].tok == .lpar {
			is_member_call := i > 0 && tokens[i - 1].tok == .dot
			mut cast_type := if is_member_call {
				''
			} else {
				fastc_primitive_c_type(item.lit) or { '' }
			}
			is_c_cast := i >= 2 && tokens[i - 2].tok == .name && tokens[i - 2].lit == 'C' && tokens[i - 1].tok == .dot && item.lit.len > 0 && 'C.${item.lit}' !in g.functions && fastc_call_has_one_argument(tokens, i + 1) && (item.lit[0].is_capital() || '#Cstruct#${item.lit}' in g.declared_types)
			if is_c_cast {
				cast_type = if '#Cstruct#${item.lit}' in g.declared_types {
					'struct ${item.lit}'
				} else {
					item.lit
				}
			}
			if cast_type == '' && !is_member_call {
				if type_key := g.resolve_declared_type_key(item.lit) {
					cast_type = fastc_c_declared_type_name(type_key)
				}
			}
			if cast_type != '' {
				pointer_token := if i > 0 && tokens[i - 1].tok in [.amp, .and] && fastc_token_is_prefix_operator(tokens, i - 1) {
					tokens[i - 1].tok
				} else if is_c_cast && i >= 3 && tokens[i - 3].tok in [.amp, .and] && fastc_token_is_prefix_operator(tokens, i - 3) {
					tokens[i - 3].tok
				} else {
					token.Token.unknown
				}
				pointer_suffix := '*'.repeat(if pointer_token == .and {
					2
				} else if pointer_token == .amp {
					1
				} else {
					0
				})
				piece = '((${cast_type}${pointer_suffix})('
				close := fastc_matching_rpar(tokens, i + 1) or { return none }
				cast_open = i + 1
				cast_closes << close
			} else {
				previous := if i == 0 { token.Token.unknown } else { tokens[i - 1].tok }
				piece = if previous == .dot {
					item.lit
				} else {
					g.resolved_expression_name(item.lit, previous)
				}
			}
		} else if item.tok == .lpar && i == cast_open {
			piece = ''
		} else if item.tok == .rpar && i in cast_closes {
			piece = '))'
		} else if item.tok == .number {
			piece = if g.selfhost {
				fastc_c_selfhost_number(item.lit)
			} else {
				fastc_c_number(item.lit) or { return none }
			}
		} else if item.tok == .string {
			if item.source != '' {
				piece = item.source
			} else {
				literal := fastc_c_string(item.lit) or { return none }
				piece = if g.selfhost { '_S(${literal})' } else { literal }
			}
		} else if item.tok == .char {
			piece = if item.lit.starts_with('c:') {
				fastc_c_string("'" + item.lit['c:'.len..] + "'") or { return none }
			} else {
				fastc_c_rune(item.lit) or { return none }
			}
		} else if item.tok == .key_true {
			piece = '((bool)true)'
		} else if item.tok == .key_false {
			piece = '((bool)false)'
		} else if item.tok == .key_nil {
			piece = 'NULL'
		} else if item.tok == .key_none {
			piece = '(Option){.state=2}'
		} else if item.tok == .name {
			previous := if i == 0 { token.Token.unknown } else { tokens[i - 1].tok }
			piece = if previous == .dot && i + 1 < tokens.len && tokens[i + 1].tok == .lpar {
				item.lit
			} else if previous == .dot && previous_module_separator {
				// The module prefix already makes a qualified keyword-named constant safe
				// (`orm.float` -> `orm__float`), so do not sanitize the member by itself.
				item.lit
			} else if i >= 2 && previous == .dot && tokens[i - 2].tok == .name && g.is_enum_type_name(tokens[i - 2].lit) {
				// An enum type prefix likewise makes keyword-named fields safe
				// (`TokenKind.float` -> `TokenKind__float`).
				item.lit
			} else if i >= 2 && previous == .dot && tokens[i - 2].tok == .name && tokens[i - 2].lit == 'C' {
				item.lit
			} else if previous == .dot {
				fastc_c_identifier(item.lit)
			} else {
				g.resolved_expression_name(item.lit, previous)
			}
		} else if item.tok == .dot && i > 0 && tokens[i - 1].tok == .name && tokens[i - 1].lit in g.imports && tokens[i - 1].lit !in g.locals && (i < 2 || tokens[i - 2].tok != .dot) {
			// An imported module name qualifies only at the start of a chain; `v.pref.os.str()`
			// accesses the field `os`, not the `os` module, even when that module is imported. A
			// LOCAL of the same name (`for token in … { token.starts_with(…) }`) shadows the module.
			piece = '__'
		} else if item.tok == .dot && i > 0 && tokens[i - 1].tok == .name && tokens[i - 1].lit == 'C' {
			piece = ''
		} else if item.tok == .dot && i > 0 && tokens[i - 1].tok == .name && g.local_is_pointer(tokens[i - 1].lit) {
			piece = '->'
		} else if item.tok == .dot && i > 0 && tokens[i - 1].tok == .name && tokens[i - 1].lit !in g.locals && g.is_enum_type_name(tokens[i - 1].lit) {
			piece = '__'
		} else if item.tok == .dot && module_separator {
			piece = '__'
		} else if piece == '' {
			piece = item.tok.str()
		}
		if result.len > 0 && fastc_needs_space(result.last(), piece) && !module_separator && !previous_module_separator {
			result.write_u8(` `)
		}
		result.write_string(piece)
		previous_module_separator = module_separator
	}
	return g.render_enum_alias_member_references(tokens, fastc_take_string(mut result))
}

fn (g &Parser) expression_dot_is_module_separator(tokens []FastcExpressionToken, index int) bool {
	if index <= 0 || index >= tokens.len || tokens[index].tok != .dot || tokens[index - 1].tok != .name {
		return false
	}
	previous_name := tokens[index - 1].lit
	// An imported module name is only a qualifier at the start of a member chain.
	// In `app.config.value`, `config` is a field even when the file imports `config`.
	// A LOCAL of the same name shadows the module (`for token in … { token.starts_with(…) }` with
	// the `token` module imported), so `token.method(…)` is a method call, not a module function.
	if (index < 2 || tokens[index - 2].tok != .dot) && previous_name !in g.locals && (previous_name in g.imports || previous_name == 'C' || g.is_enum_type_name(previous_name)) {
		return true
	}
	if index < 3 || tokens[index - 2].tok != .dot || tokens[index - 3].tok != .name {
		return false
	}
	imported_module := g.imports[tokens[index - 3].lit] or { return false }
	type_key := fastc_type_key(imported_module, previous_name)
	return g.underlying_enum_type_key(type_key) != none
}

fn fastc_token_is_prefix_operator(tokens []FastcExpressionToken, index int) bool {
	if index == 0 {
		return true
	}
	return tokens[index - 1].tok !in [.name, .number, .string, .char, .key_true, .key_false, .key_nil,
		.key_none, .rpar, .rsbr, .rcbr, .inc, .dec]
}

fn (g &Parser) array_initializer_type(tokens []FastcExpressionToken) ?string {
	if tokens.len < 3 {
		return none
	}
	mut index := 0
	mut dimensions := 0
	mut fixed_length := ''
	if tokens.len >= 4 && tokens[0].tok == .lsbr && tokens[1].tok in [.name, .number] && tokens[2].tok == .rsbr {
		fixed_length = if tokens[1].tok == .name {
			constant_key := fastc_constant_key(g.module_name, tokens[1].lit)
			g.constants[constant_key] or { fastc_c_constant_name(g.module_name, tokens[1].lit) }
		} else {
			fastc_c_selfhost_number(tokens[1].lit)
		}
		dimensions = 1
		index = 3
	}
	for index + 1 < tokens.len && tokens[index].tok == .lsbr && tokens[index + 1].tok == .rsbr {
		dimensions++
		index += 2
	}
	if dimensions == 0 || index >= tokens.len {
		return none
	}
	mut element_type := g.type_from_expression_tokens(tokens[index..]) or { '' }
	if element_type == '' && tokens[index].tok == .name && tokens[index].lit == 'thread' {
		// `[]thread` / `[]thread T` is an array of spawned-thread handles. All handles
		// share one C layout; the void handle for a bare `thread`, or one keyed by the
		// thread's value type `T` so `.wait()` recovers it.
		if index + 1 == tokens.len {
			element_type = fastc_thread_type_name('')
		} else if value_type := g.type_from_expression_tokens(tokens[index + 1..]) {
			element_type = fastc_thread_type_name(fastc_normalize_inferred_type(value_type))
		}
	}
	if element_type == '' {
		return none
	}
	if fixed_length != '' {
		return fastc_fixed_array_type(fixed_length, element_type)
	}
	mut result := element_type
	for _ in 0 .. dimensions {
		result = fastc_array_c_type(result)
	}
	return result
}

fn fastc_initializer_type_start(tokens []FastcExpressionToken) int {
	mut depth := 0
	mut start := 0
	for i, item in tokens {
		if item.tok in [.lpar, .lsbr, .lcbr] {
			depth++
		} else if item.tok in [.rpar, .rsbr, .rcbr] {
			depth--
		} else if depth == 0 && (item.tok.is_assignment() || item.tok in [.left_shift, .right_shift,
			.right_shift_unsigned, .plus, .minus, .div, .mod, .eq, .ne, .lt, .gt, .le, .ge, .and,
			.logical_or, .comma, .key_return, .key_in, .not_in]) {
			// The literal's type spelling ends at the `{`; anything before an assignment,
			// binary operator, or separator (`c.cache << []i8{…}`) belongs to a preceding
			// operand and must not be folded into the type.
			start = i + 1
		}
	}
	return start
}

fn (g &Parser) map_initializer_type(tokens []FastcExpressionToken) ?string {
	map_type := g.type_from_expression_tokens(tokens) or { return none }
	return if map_type.starts_with('Map_') { map_type } else { none }
}

fn (g &Parser) type_from_expression_tokens(tokens []FastcExpressionToken) ?string {
	if tokens.len == 0 {
		return none
	}
	mut pointers := 0
	mut start := 0
	for start < tokens.len && tokens[start].tok in [.amp, .and, .mul] {
		pointers += if tokens[start].tok == .and { 2 } else { 1 }
		start++
	}
	if start >= tokens.len {
		return none
	}
	remaining := tokens[start..]
	if remaining.len >= 3 && remaining[0].tok == .lsbr && remaining[1].tok == .rsbr {
		element_type := g.type_from_expression_tokens(remaining[2..]) or { return none }
		return fastc_array_c_type(element_type) + '*'.repeat(pointers)
	}
	if remaining.len >= 5 && remaining[0].tok == .name && remaining[0].lit == 'map' && remaining[1].tok == .lsbr {
		close := fastc_matching_delimiter(remaining, 1, .lsbr, .rsbr) or { return none }
		if close <= 2 || close + 1 >= remaining.len {
			return none
		}
		key_type := g.type_from_expression_tokens(remaining[2..close]) or { return none }
		value_type := g.type_from_expression_tokens(remaining[close + 1..]) or { return none }
		return fastc_map_c_type(key_type, value_type) + '*'.repeat(pointers)
	}
	if remaining.len >= 2 && remaining[0].tok == .name && remaining[0].lit == 'chan' {
		// Channels use one erased runtime representation. Still validate the element
		// spelling so an arbitrary trailing expression is not accepted as a type.
		_ := g.type_from_expression_tokens(remaining[1..]) or { return none }
		return 'chan' + '*'.repeat(pointers)
	}
	if remaining.len == 1 && remaining[0].tok == .name {
		mut base := fastc_primitive_c_type(remaining[0].lit) or { '' }
		if base == '' {
			type_key := g.resolve_declared_type_key(remaining[0].lit) or { return none }
			base = fastc_c_declared_type_name(type_key)
		}
		return base + '*'.repeat(pointers)
	}
	if remaining.len == 3 && remaining[0].tok == .name && remaining[1].tok == .dot && remaining[2].tok == .name {
		if remaining[0].lit == 'C' {
			raw_type := remaining[2].lit
			if '#Cstruct#${raw_type}' in g.declared_types {
				return 'struct ${raw_type}' + '*'.repeat(pointers)
			}
			if 'C.${raw_type}' in g.functions {
				return none
			}
			if raw_type.len == 0 || !raw_type[0].is_capital() {
				return none
			}
			return raw_type + '*'.repeat(pointers)
		}
		module_name := g.imports[remaining[0].lit] or { return none }
		type_key := fastc_type_key(module_name, remaining[2].lit)
		if type_key !in g.declared_types {
			return none
		}
		return fastc_c_declared_type_name(type_key) + '*'.repeat(pointers)
	}
	return none
}

fn fastc_array_initializer_c_type(array_type string) string {
	length := fastc_fixed_array_length(array_type) or { return array_type }
	element_type := fastc_fixed_array_element_type(array_type) or { return array_type }
	return 'FixedArray_${fastc_composite_type_part(length)}_${fastc_composite_type_part(element_type)}'
}

// fastc_collect_referenced_fixed_array_types finds every `FixedArray_<len>_FASTC_ARRAY_OF_<elem>`
// type name that appears in already-emitted C and registers a typedef for it. Fixed arrays used
// only as a function return type, parameter, or struct field are spelled with this raw marker name
// by the signature scanner but — unlike array literals — never pass through the expression renderer
// that would register them, so their typedef would otherwise be missing.
fn fastc_collect_referenced_fixed_array_types(source string, mut fixed_array_types map[string]string) {
	mut i := 0
	for i < source.len {
		c := source[i]
		if !(c.is_letter() || c == `_`) {
			i++
			continue
		}
		if i > 0 {
			p := source[i - 1]
			if p.is_letter() || p.is_digit() || p == `_` {
				i++
				continue
			}
		}
		mut end := i
		for end < source.len && (source[end].is_letter() || source[end].is_digit() || source[end] == `_`) {
			end++
		}
		name := source[i..end]
		if name.starts_with('FixedArray_') && name.contains('_FASTC_ARRAY_OF_') {
			if _ := fastc_fixed_array_length(name) {
				if _ := fastc_fixed_array_element_type(name) {
					// Key on the same sanitized name the expression renderer uses, so a
					// type reached both ways is registered once, not typedef'd twice.
					key := fastc_array_initializer_c_type(name)
					if key !in fixed_array_types {
						fixed_array_types[key] = name
					}
				}
			}
		}
		i = end
	}
}

fn fastc_generate_fixed_array_declarations(fixed_array_types map[string]string) string {
	mut names := fixed_array_types.keys()
	names.sort()
	mut out := strings.new_builder(256)
	for name in names {
		array_type := fixed_array_types[name]
		length := fastc_fixed_array_length(array_type) or { continue }
		element_type := fastc_fixed_array_element_type(array_type) or { continue }
		declaration_name := if fastc_composite_type_part(array_type) == array_type {
			array_type
		} else {
			name
		}
		out.writeln('typedef struct { ${element_type} data[${length}]; } ${declaration_name};')
		if name != declaration_name {
			out.writeln('typedef ${declaration_name} ${name};')
		}
	}
	if out.len > 0 {
		out.writeln('')
	}
	return fastc_take_string(mut out)
}

fn fastc_expression_list_items(tokens []FastcExpressionToken, start int, end int) ![][]FastcExpressionToken {
	if start == end {
		return [][]FastcExpressionToken{}
	}
	mut result := [][]FastcExpressionToken{}
	mut item_start := start
	mut parens := 0
	mut brackets := 0
	mut braces := 0
	for i in start .. end {
		match tokens[i].tok {
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
			.comma {
				if parens == 0 && brackets == 0 && braces == 0 {
					if item_start == i {
						return error('empty expression-list item')
					}
					// Items are consumed while `tokens` is alive and never mutated, so views are safe.
					item := unsafe { tokens[item_start..i] }
					result << item
					item_start = i + 1
				}
			}
			else {}
		}
	}
	if item_start == end {
		return result
	}
	item := unsafe { tokens[item_start..end] }
	result << item
	return result
}
