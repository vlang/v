module flat

// SourceTypeQuery records immutable syntax queries separately from declaration resolution.
@[flag]
pub enum SourceTypeQuery {
	complete
	channel
	shared_modifier
	thread_prefix
	generic_token
	generic_word
	generic_name
	generic_fn_name
	struct_candidate
	fixed_array_prefix
	fixed_array_postfix
	c_pointer_suffix
	unknown_fragment
	c_prefix
	main_prefix
	fn_ptr_prefix
	c_array_prefix
	c_fixed_array_prefix
	c_map_prefix
	thread_suffix
	ctx_suffix
	c_double_pointer_suffix
	c_pointer_infix
	map_arc_key
	inferred_witness
	c_thread_prefix
	js_prefix
	pointer_option_ref
	shared_array
	anon_struct_prefix
	anon_union_prefix
	bracket_suffix
}

@[direct_array_access]
fn source_type_spelling_query(text string, shape SourceTypeShape) SourceTypeQuery {
	mut query := unsafe { SourceTypeQuery(0) } // No query has matched yet.
	mut parens := 0
	mut brackets := 0
	mut complete := true
	for i, c in text {
		match c {
			`(` { parens++ }
			`)` {
				if parens == 0 { complete = false } else { parens-- }
			}
			`[` { brackets++ }
			`]` {
				if brackets == 0 { complete = false } else { brackets-- }
			}
			else {}
		}
		if c >= `A` && c <= `Z`
			&& (i == 0 || !source_type_query_ident(text[i - 1]))
			&& (i + 1 == text.len || !source_type_query_ident(text[i + 1])) {
			query.set(.generic_token)
		}
	}
	// These counters deliberately allow crossing delimiters, matching the original query.
	if complete && parens == 0 && brackets == 0 { query.set(.complete) }
	if text.contains('generic') { query.set(.generic_word) }
	if text.ends_with('*') { query.set(.c_pointer_suffix) }
	// The declaration screen already excludes every spelling containing unknown.
	if shape.has(.unusable_decl) && text.contains('unknown') { query.set(.unknown_fragment) }
	if text.starts_with('thread') { query.set(.thread_prefix) }
	if source_type_spelling_channel(text) { query.set(.channel) }
	if source_type_spelling_shared_modifier(text) { query.set(.shared_modifier) }
	if source_type_spelling_struct_candidate(text, shape) { query.set(.struct_candidate) }
	if shape.has(.leading_bracket) {
		if text.len >= 2 && text[1] != `]` && text.contains(']') {
			query.set(.fixed_array_prefix)
		}
	} else if shape.has(.bracketed) && text.ends_with(']') && !shape.has(.map_type) {
		query.set(.fixed_array_postfix)
	}
	if (text.len == 1 || (text.len > 1 && text[text.len - 2] == `.`))
		&& text[text.len - 1] in [`T`, `U`, `V`, `K`] {
		query.set(.generic_name)
	}
	if source_type_spelling_fn_placeholder(text) { query.set(.generic_fn_name) }
	if text.starts_with('C.') { query.set(.c_prefix) }
	if text.starts_with('main.') { query.set(.main_prefix) }
	if text.starts_with('fn_ptr:') { query.set(.fn_ptr_prefix) }
	if text.starts_with('Array_') { query.set(.c_array_prefix) }
	if text.starts_with('FixedArray_') { query.set(.c_fixed_array_prefix) }
	if text.starts_with('Map_') { query.set(.c_map_prefix) }
	if text.ends_with('.thread') { query.set(.thread_suffix) }
	if text.ends_with('_Ctx') { query.set(.ctx_suffix) }
	if text.ends_with('**') { query.set(.c_double_pointer_suffix) }
	if text.contains('*') { query.set(.c_pointer_infix) }
	if text.contains('Arc[') || text.contains('Arc_') { query.set(.map_arc_key) }
	if text.contains('typeof(__expr_') { query.set(.inferred_witness) }
	if text.starts_with('__vf_thread_') { query.set(.c_thread_prefix) }
	if text.starts_with('JS.') { query.set(.js_prefix) }
	if text.starts_with('&?&') { query.set(.pointer_option_ref) }
	if text.starts_with('[]shared ') { query.set(.shared_array) }
	if text.starts_with('AnonStruct_') { query.set(.anon_struct_prefix) }
	if text.starts_with('AnonUnion_') { query.set(.anon_union_prefix) }
	if text.ends_with(']') { query.set(.bracket_suffix) }
	return query
}

@[inline]
fn source_type_query_ident(c u8) bool {
	return (c >= `a` && c <= `z`) || (c >= `A` && c <= `Z`)
		|| (c >= `0` && c <= `9`) || c == `_`
}

fn source_type_spelling_fn_placeholder(text string) bool {
	if text.len == 0 { return false }
	if text.len == 1 { return text[0] >= `A` && text[0] <= `Z` }
	// Preserve the unpadded query's exact suffix grammar, including interior spaces.
	if text[0] > ` ` && text[text.len - 1] > ` ` {
		return text[text.len - 1] >= `A` && text[text.len - 1] <= `Z`
			&& text[text.len - 2] == `.`
	}
	clean := source_type_query_trim(text)
	if clean.contains('.') {
		start := clean.last_index_u8(`.`) + 1
		// The suffix is read only while the original spelling is alive.
		suffix := unsafe { clean.substr_unsafe(start, clean.len) }
		return source_type_spelling_fn_placeholder(suffix)
	}
	return clean.len == 1 && clean[0] >= `A` && clean[0] <= `Z`
}

fn source_type_spelling_channel(text string) bool {
	mut clean := source_type_query_trim(text)
	for {
		if clean.starts_with('&') {
			// Borrowed views are consumed before source_type_spelling_query returns.
			clean = source_type_query_trim(unsafe { clean.substr_unsafe(1, clean.len) })
		} else if clean.starts_with('mut ') {
			clean = source_type_query_trim(unsafe { clean.substr_unsafe(4, clean.len) })
		} else {
			return clean.starts_with('chan ') || clean == 'chan'
		}
	}
	return false
}

fn source_type_query_trim(text string) string {
	mut start := 0
	mut end := text.len
	for start < end && source_type_space(text[start]) { start++ }
	for end > start && source_type_space(text[end - 1]) { end-- }
	// Metadata construction never retains a view of the caller's bytes.
	return unsafe { text.substr_unsafe(start, end) }
}

fn source_type_spelling_shared_modifier(text string) bool {
	mut start := 0
	for {
		idx := text.index_after('shared ', start) or { return false }
		if idx == 0 || !(text[idx - 1].is_letter() || text[idx - 1].is_digit()
			|| text[idx - 1] == `_` || text[idx - 1] == `.`) {
			return true
		}
		start = idx + 'shared '.len
	}
	return false
}

fn source_type_spelling_struct_candidate(text string, shape SourceTypeShape) bool {
	if shape.has(.builtin_alias) || shape.has(.array_type) || shape.has(.map_type)
		|| shape.has(.leading_bracket) || shape.has(.function_compact) || shape.has(.function_spaced)
		|| shape.has(.channel_type) || shape.has(.optional) {
		return false
	}
	if !shape.has(.pointer) { return true }
	mut start := 0
	for start < text.len && text[start] == `&` { start++ }
	if start == text.len { return false }
	// This view is only used for immediate syntax comparisons.
	clean := unsafe { text.substr_unsafe(start, text.len) }
	// Pointer bases retain the original builtin list, which deliberately lacks usize.
	if clean in ['bool', 'string', 'void', 'int', 'i8', 'i16', 'i32', 'i64', 'u8', 'u16', 'u32',
		'u64', 'f32', 'f64', 'rune', 'isize', 'uint', 'voidptr', 'byteptr', 'charptr'] {
		return false
	}
	return !clean.starts_with('[]') && !clean.starts_with('map[') && !clean.starts_with('[')
		&& !clean.starts_with('fn(') && !clean.starts_with('fn (') && !clean.starts_with('chan ')
		&& !clean.starts_with('?') && !clean.starts_with('!')
}
