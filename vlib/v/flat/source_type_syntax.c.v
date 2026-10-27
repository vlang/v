@[has_globals]
module flat

import encoding.hex

// SourceTypeSyntaxKind describes the outer constructor of an interned spelling.
// It does not resolve names or distinguish generic arguments from suffix array sizes.
pub enum SourceTypeSyntaxKind {
	empty
	name
	pointer
	array
	fixed_array
	option
	result
	map_type
	function_type
	tuple
	channel
	thread
	mutable_type
	shared_type
	atomic_type
	variadic
	generic
	invalid
}

struct SourceTypeSyntaxSpan {
	offset u32
	len    u32
}

// SourceTypeSyntax exposes immutable numeric components of a source spelling.
// Wrappers have a base; maps have a value base and one key argument; fixed arrays
// have an element base and their exact length spelling as the argument. Functions
// have a result base and parameter arguments, tuples have component arguments,
// and generics have a head base and argument spellings. Children are trimmed,
// except fixed array lengths, which retain their exact interior bytes.
// unqualified and generic_base preserve the raw all_after_last('.') and
// all_before('[') queries. words use ASCII identifier boundaries; symbols also
// retain dots. Array accessors return immutable views into process-owned storage;
// lexical views are created independently only when requested.
// These components describe spelling syntax, not semantic TypeIds.
pub struct SourceTypeSyntax {
	payload                             voidptr
	args_span                           SourceTypeSyntaxSpan
	application_args_span               SourceTypeSyntaxSpan
	generic_app_args_span               SourceTypeSyntaxSpan
	postfix_dims_span                   SourceTypeSyntaxSpan
	function_params_span                SourceTypeSyntaxSpan
	tuple_components_span               SourceTypeSyntaxSpan
	qualifier_count                     u32
	cgen_app_args_span                  SourceTypeSyntaxSpan
	cgen_function_params_span           SourceTypeSyntaxSpan
	checker_function_params_span        SourceTypeSyntaxSpan
	checker_tuple_components_span       SourceTypeSyntaxSpan
	cgen_raw_app_args_span              SourceTypeSyntaxSpan
	cgen_parts_span                     SourceTypeSyntaxSpan
	checker_raw_tuple_components_span   SourceTypeSyntaxSpan
	bracket_raw_parts_span              SourceTypeSyntaxSpan
	bracket_generic_parts_span          SourceTypeSyntaxSpan
	tuple_function_components_span      SourceTypeSyntaxSpan
	plain_comma_parts_span              SourceTypeSyntaxSpan
	generic_mentions_span               SourceTypeSyntaxSpan
	receiver_app_args_span              SourceTypeSyntaxSpan
	checker_bracket_components_span     SourceTypeSyntaxSpan
	voidptr_parts_span                  SourceTypeSyntaxSpan
	inferred_parts_span                 SourceTypeSyntaxSpan
	inferred_annotations_span           SourceTypeSyntaxSpan
	inferred_expr_ids_span              SourceTypeSyntaxSpan
	substitution_function_params_span   SourceTypeSyntaxSpan
	written_applications_span           SourceTypeSyntaxSpan
	written_application_offsets_span    SourceTypeSyntaxSpan
	checker_raw_bracket_components_span SourceTypeSyntaxSpan
	cgen_placeholder_candidates_span    SourceTypeSyntaxSpan
	reflection_parts_span               SourceTypeSyntaxSpan
	reflection_tuple_parts_span         SourceTypeSyntaxSpan
	receiver_raw_app_args_span          SourceTypeSyntaxSpan
	vls_generic_words_span              SourceTypeSyntaxSpan
	eval_tuple_parts_span               SourceTypeSyntaxSpan
	implicit_tuple_components_span      SourceTypeSyntaxSpan
	generic_application_heads_span      SourceTypeSyntaxSpan
	markused_application_heads_span     SourceTypeSyntaxSpan
	markused_generic_components_span    SourceTypeSyntaxSpan
	raw_tuple_parse_components_span     SourceTypeSyntaxSpan
	union_parts_span                    SourceTypeSyntaxSpan
pub:
	kind                               SourceTypeSyntaxKind
	trimmed                            u32
	base                               u32
	unqualified                        u32
	qualifier                          u32
	generic_base                       u32
	pointer_base                       u32
	pointer_depth                      int
	function_has_result                bool
	raw_base                           u32
	main_base                          u32
	bare_generic                       bool
	application_base                   u32
	application_valid                  bool
	generic_app_base                   u32
	generic_app_valid                  bool
	postfix_base                       u32
	parameter_type                     u32
	parameter_mut_type                 u32
	bracket_closed                     bool
	bracket_base                       u32
	bracket_inner                      u32
	bracket_raw_inner                  u32
	bracket_prefix                     u32
	function_closed                    bool
	function_result                    u32
	tuple_like                         bool
	function_space_prefix              bool
	spaced                             bool
	first_bracket_closed               bool
	first_bracket_base                 u32
	last_space_word                    u32
	named_type                         bool
	c_qualified                        bool
	first_dot_prefix                   u32
	first_dot_suffix                   u32
	builtin_prefix                     bool
	main_fragment                      bool
	function_prefix                    bool
	fn_prefix                          bool
	shared_fragment                    bool
	pointer_raw_base                   u32
	builtin_base                       u32
	cgen_app_base                      u32
	cgen_app_valid                     bool
	tuple_contains_comma               bool
	top_space_head                     u32
	top_space_tail                     u32
	top_space_valid                    bool
	typeof_expr_prefix                 bool
	bracket_suffix                     u32
	bracket_from                       u32
	bracket_inner_fixed_length         bool
	first_bracket_prefix               u32
	void_pointer_postfix               bool
	canonical_dotted_name              bool
	decimal                            bool
	contains_comma                     bool
	function_fragment                  bool
	dots_base                          u32
	cgen_raw_app_base                  u32
	cgen_raw_app_valid                 bool
	generic_parameter_payload          u32
	legacy_parameter_type              u32
	legacy_parameter_mut_type          u32
	first_bracket_length               u32
	c_pointer_base                     u32
	c_ptr_name_suffix                  bool
	c_fn_type_fragment                 bool
	underscored                        bool
	source_wrapper_base                u32
	declaration_head                   u32
	fn_fragment                        bool
	compact_fn_spelling                u32
	first_bracket_raw_inner            u32
	cgen_generic_placeholder           bool
	fixed_array_type                   bool
	typeof_valid                       bool
	typeof_inner                       u32
	postfix_index_base                 u32
	last_bracket_prefix                u32
	last_bracket_length                u32
	trailing_bracket_prefix            u32
	trailing_bracket_closed            bool
	array_chars_base                   u32
	lowercase_value_ident              bool
	typeof_prefix                      bool
	first_capital                      bool
	named_variant                      bool
	named_variant_valid                bool
	named_variant_owner                u32
	named_variant_name                 u32
	main_unlocked                      u32
	encoded_fixed_length               u32
	encoded_fixed_base                 u32
	map_name_prefix                    bool
	suffix_bracket_length              u32
	suffix_bracket_valid               bool
	map_function_fragment              bool
	bracket_source_inner               u32
	c_fixed_typedef_element            u32
	c_fixed_typedef_length             u32
	c_fixed_typedef_valid              bool
	fixed_array_query_length           u32
	last_bracket_span_valid            bool
	last_bracket_span_inner            u32
	plain_type_name                    bool
	generic_app_legacy_suffix          u32
	wrapper_prefix                     u32
	wrapper_base                       u32
	checker_fixed_array_may_be         bool
	dollar_prefix                      bool
	define_prefix                      bool
	define_fragment                    bool
	paren_prefix                       bool
	fixed_length                       bool
	qualified_modifiers                u32
	plain_alias_target                 bool
	c_array_payload                    u32
	c_fixed_array_payload              u32
	fastc_array_marker                 bool
	fastc_array_marker_payload         u32
	c_map_payload                      u32
	c_ptr_name_base                    u32
	c_tagged_type                      u32
	backward_bracket_prefix            u32
	backward_bracket_closed            bool
	opening_paren                      bool
	byte_len                           int
	receiver_app_valid                 bool
	receiver_app_base                  u32
	declaration_parameter_payload      u32
	declaration_parameter_mut          bool
	prefix_dimension_tail              u32
	prefix_dimensions_closed           bool
	backward_bracket_after_prefix_dims bool
	backward_bracket_inner             u32
	option_result_void_prefix          bool
	nested_option_result_marker        int
	c_pointer_depth                    int
	c_pointer_count                    int
	c_pointer_raw_base                 u32
	thread_value_prefix                bool
	thread_value_payload               u32
	thread_value_type_valid            bool
	thread_value_type                  u32
	contains_closing_bracket           bool
	atomic_val_fragment                bool
	// inferred_expr_ids stores AST NodeId bit patterns, not source spelling IDs.
	expr_prefix                       bool
	expr_id                           int
	parameter_name                    u32
	parameter_named                   bool
	parameter_is_mut                  bool
	transform_fixed_length            bool
	receiver_collection_id            u32
	ampersand_trimmed                 u32
	contains_doubleunderscore         bool
	parallel_preseed_fn               bool
	parallel_preseed_short            u32
	closing_bracket                   bool
	constraint_receiver_base          u32
	result_suffix                     bool
	cgen_decl_unusable                bool
	cgen_decl_authority               bool
	generic_encoded_fragment          bool
	fn_ptr_encoded_prefix             bool
	c_result_prefix                   bool
	c_option_prefix                   bool
	c_multi_return_prefix             bool
	c_lower_map_prefix                bool
	open_generic_receiver_marker      bool
	space_removed                     u32
	qualified_name_chars              bool
	field_owner_module                u32
	const_name_prefix                 bool
	pthread_prefix                    bool
	thread_bracket_prefix             bool
	fixed_typedef_bracket             bool
	question_trimmed                  u32
	c_array_fixed_prefix              bool
	function_space_result             u32
	decimal_value                     int
	integer_value                     int
	encoded_alnum_dotted              bool
	internal_symbol_prefix            bool
	main_at_base                      u32
	incremental_generic_application   bool
	system_error_suffix               bool
	receiver_app_inner                u32
	fastc_fixed_array_length_valid    bool
	fastc_fixed_array_length          u32
	contains_capital                  bool
	first_bracket_source_inner        u32
	c_function_pointer_hole           bool
	channel_fragment                  bool
	array_receiver_fragment           bool
	mangled_open_generic_marker       bool
	array_suffix                      bool
	empty_function_prefix             bool
	c_shared_fragment                 bool
	typ_suffix                        bool
	typ_suffix_base                   u32
	checker_parse_function_result     u32
	checker_parse_function_has_params bool
	top_level_dot_valid               bool
	top_level_dot_prefix              u32
	top_level_dot_suffix              u32
	parser_fn_decl_valid              bool
	parser_fn_decl_params             u32
	parser_fn_decl_result             u32
	contains_question                 bool
	contains_union                    bool
	named_variant_dot_valid           bool
	named_variant_dot_prefix          u32
	named_variant_dot_suffix          u32
}

@[inline]
fn source_type_syntax_empty_ids() []u32 {
	return unsafe { array{ element_size: sizeof(u32), flags: .managed } }
}

@[inline]
fn source_type_syntax_empty_bools() []bool {
	return unsafe { array{ element_size: sizeof(bool), flags: .managed } }
}

// Cold constructor singleton views borrow only immutable process-owned backing.
// The syntax payload copies these values before publishing its own array views.
__global g_source_type_syntax_zero_offset u32

@[inline]
fn source_type_syntax_singleton_ids(value &u32) []u32 {
	return unsafe {
		array{
			data:         voidptr(value)
			len:          1
			cap:          1
			element_size: sizeof(u32)
			flags:        .nogrow | .nofree
		}
	}
}

// Singleton backing remains process-owned even when the syntax receiver is copied.
@[inline]
fn source_type_syntax_scalar_ids(id u32) []u32 {
	if id == 0 { return source_type_syntax_singleton_ids(&g_source_type_syntax_zero_offset) }
	entry := source_type_read_entry(id)
	return source_type_syntax_singleton_ids(&entry.id)
}

@[inline]
fn (syntax &SourceTypeSyntax) ids(span SourceTypeSyntaxSpan) []u32 {
	return unsafe {
		array{
			data:         if span.len == 0 {
				nil
			} else {
				voidptr(&u8(syntax.payload) + span.offset)
			}
			len:          int(span.len)
			cap:          int(span.len)
			element_size: sizeof(u32)
			flags:        .nogrow | .nofree
		}
	}
}

// args returns a process-owned immutable array view.
@[inline]
pub fn (syntax &SourceTypeSyntax) args() []u32 {
	return syntax.ids(syntax.args_span)
}

// symbols returns a lazily cached process-owned immutable array view.
@[inline]
pub fn (syntax &SourceTypeSyntax) symbols() []u32 {
	if isnil(syntax.payload) { return syntax.ids(SourceTypeSyntaxSpan{}) }
	if syntax.named_type { return syntax.ids(SourceTypeSyntaxSpan{ len: 1 }) }
	id := unsafe { *&u32(syntax.payload) }
	return source_type_syntax_lexical_ids(source_type_syntax_lexical(id, .symbols), false)
}

// words returns a lazily cached process-owned immutable array view.
@[inline]
pub fn (syntax &SourceTypeSyntax) words() []u32 {
	if isnil(syntax.payload) { return syntax.ids(SourceTypeSyntaxSpan{}) }
	id := unsafe { *&u32(syntax.payload) }
	if syntax.named_type && syntax.first_dot_prefix == id {
		return syntax.ids(SourceTypeSyntaxSpan{ len: 1 })
	}
	return source_type_syntax_lexical_ids(source_type_syntax_lexical(id, .words), false)
}

// application_args returns a process-owned immutable array view.
@[inline]
pub fn (syntax &SourceTypeSyntax) application_args() []u32 {
	return syntax.ids(syntax.application_args_span)
}

// generic_app_args returns a process-owned immutable array view.
@[inline]
pub fn (syntax &SourceTypeSyntax) generic_app_args() []u32 {
	return syntax.ids(syntax.generic_app_args_span)
}

// postfix_dims returns a process-owned immutable array view.
@[inline]
pub fn (syntax &SourceTypeSyntax) postfix_dims() []u32 {
	return syntax.ids(syntax.postfix_dims_span)
}

// function_params returns a process-owned immutable array view.
@[inline]
pub fn (syntax &SourceTypeSyntax) function_params() []u32 {
	return syntax.ids(syntax.function_params_span)
}

// tuple_components returns a process-owned immutable array view.
@[inline]
pub fn (syntax &SourceTypeSyntax) tuple_components() []u32 {
	return syntax.ids(syntax.tuple_components_span)
}

// qualifier_prefixes returns raw prefixes before each dot in process-owned storage.
// A single prefix borrows its immutable source entry; longer views are cached on request.
@[inline]
pub fn (syntax &SourceTypeSyntax) qualifier_prefixes() []u32 {
	if syntax.qualifier_count == 0 { return syntax.ids(SourceTypeSyntaxSpan{}) }
	if syntax.qualifier_count == 1 { return source_type_syntax_scalar_ids(syntax.first_dot_prefix) }
	id := unsafe { *&u32(syntax.payload) }
	return source_type_syntax_lexical_ids(source_type_syntax_lexical(id, .qualifiers), false)
}

// qualifier_suffixes returns raw suffixes after each dot in process-owned storage.
// A single suffix borrows its immutable source entry; longer views are cached on request.
@[inline]
pub fn (syntax &SourceTypeSyntax) qualifier_suffixes() []u32 {
	if syntax.qualifier_count == 0 { return syntax.ids(SourceTypeSyntaxSpan{}) }
	if syntax.qualifier_count == 1 { return source_type_syntax_scalar_ids(syntax.first_dot_suffix) }
	id := unsafe { *&u32(syntax.payload) }
	return source_type_syntax_lexical_ids(source_type_syntax_lexical(id, .qualifiers), true)
}

// cgen_app_args returns a process-owned immutable array view.
@[inline]
pub fn (syntax &SourceTypeSyntax) cgen_app_args() []u32 {
	return syntax.ids(syntax.cgen_app_args_span)
}

// cgen_function_params returns a process-owned immutable array view.
@[inline]
pub fn (syntax &SourceTypeSyntax) cgen_function_params() []u32 {
	return syntax.ids(syntax.cgen_function_params_span)
}

// checker_function_params returns a process-owned immutable array view.
@[inline]
pub fn (syntax &SourceTypeSyntax) checker_function_params() []u32 {
	return syntax.ids(syntax.checker_function_params_span)
}

// checker_tuple_components returns a process-owned immutable array view.
@[inline]
pub fn (syntax &SourceTypeSyntax) checker_tuple_components() []u32 {
	return syntax.ids(syntax.checker_tuple_components_span)
}

// cgen_raw_app_args returns a process-owned immutable array view.
@[inline]
pub fn (syntax &SourceTypeSyntax) cgen_raw_app_args() []u32 {
	return syntax.ids(syntax.cgen_raw_app_args_span)
}

// cgen_parts returns a process-owned immutable array view.
@[inline]
pub fn (syntax &SourceTypeSyntax) cgen_parts() []u32 {
	return syntax.ids(syntax.cgen_parts_span)
}

// checker_raw_tuple_components returns a process-owned immutable array view.
@[inline]
pub fn (syntax &SourceTypeSyntax) checker_raw_tuple_components() []u32 {
	return syntax.ids(syntax.checker_raw_tuple_components_span)
}

// bracket_raw_parts returns a process-owned immutable array view.
@[inline]
pub fn (syntax &SourceTypeSyntax) bracket_raw_parts() []u32 {
	return syntax.ids(syntax.bracket_raw_parts_span)
}

// bracket_generic_parts returns a process-owned immutable array view.
@[inline]
pub fn (syntax &SourceTypeSyntax) bracket_generic_parts() []u32 {
	return syntax.ids(syntax.bracket_generic_parts_span)
}

// tuple_function_components returns a process-owned immutable array view.
@[inline]
pub fn (syntax &SourceTypeSyntax) tuple_function_components() []u32 {
	return syntax.ids(syntax.tuple_function_components_span)
}

// plain_comma_parts returns a process-owned immutable array view.
@[inline]
pub fn (syntax &SourceTypeSyntax) plain_comma_parts() []u32 {
	return syntax.ids(syntax.plain_comma_parts_span)
}

// underscore_prefixes returns a lazily cached process-owned immutable array view.
@[inline]
pub fn (syntax &SourceTypeSyntax) underscore_prefixes() []u32 {
	if isnil(syntax.payload) { return syntax.ids(SourceTypeSyntaxSpan{}) }
	id := unsafe { *&u32(syntax.payload) }
	return source_type_syntax_underscore_ids(id, false)
}

// symbol_parts returns a lazily cached process-owned immutable array view.
@[inline]
pub fn (syntax &SourceTypeSyntax) symbol_parts() []u32 {
	if isnil(syntax.payload) { return syntax.ids(SourceTypeSyntaxSpan{}) }
	if syntax.plain_type_name || syntax.canonical_dotted_name {
		return syntax.ids(SourceTypeSyntaxSpan{ len: 1 })
	}
	id := unsafe { *&u32(syntax.payload) }
	return source_type_syntax_lexical_ids(source_type_syntax_lexical(id, .parts), false)
}

// symbol_is_name returns a lazily cached process-owned immutable array view.
@[inline]
pub fn (syntax &SourceTypeSyntax) symbol_is_name() []bool {
	if isnil(syntax.payload) { return source_type_syntax_empty_bools() }
	id := unsafe { *&u32(syntax.payload) }
	return source_type_syntax_lexical_names(source_type_syntax_lexical(id, .parts))
}

// dotted_parts returns a lazily cached process-owned immutable array view.
@[inline]
pub fn (syntax &SourceTypeSyntax) dotted_parts() []u32 {
	if isnil(syntax.payload) { return syntax.ids(SourceTypeSyntaxSpan{}) }
	id := unsafe { *&u32(syntax.payload) }
	if syntax.first_dot_prefix == id { return syntax.ids(SourceTypeSyntaxSpan{ len: 1 }) }
	return source_type_syntax_lexical_ids(source_type_syntax_lexical(id, .dotted), false)
}

// underscore_suffixes returns a lazily cached process-owned immutable array view.
@[inline]
pub fn (syntax &SourceTypeSyntax) underscore_suffixes() []u32 {
	if isnil(syntax.payload) { return syntax.ids(SourceTypeSyntaxSpan{}) }
	id := unsafe { *&u32(syntax.payload) }
	return source_type_syntax_underscore_ids(id, true)
}

// last_underscore_prefix returns the exact raw component after querying its cached view.
// When no underscore occurs, it returns the original spelling ID.
@[inline]
pub fn (syntax &SourceTypeSyntax) last_underscore_prefix() u32 {
	parts := syntax.underscore_prefixes()
	if parts.len > 0 { return parts[parts.len - 1] }
	return if isnil(syntax.payload) { u32(0) } else { unsafe { *&u32(syntax.payload) } }
}

// last_underscore_suffix returns the exact raw component after querying its cached view.
// When no underscore occurs, it returns the original spelling ID.
@[inline]
pub fn (syntax &SourceTypeSyntax) last_underscore_suffix() u32 {
	parts := syntax.underscore_suffixes()
	if parts.len > 0 { return parts[parts.len - 1] }
	return if isnil(syntax.payload) { u32(0) } else { unsafe { *&u32(syntax.payload) } }
}

// generic_mentions returns a process-owned immutable array view.
@[inline]
pub fn (syntax &SourceTypeSyntax) generic_mentions() []u32 {
	return syntax.ids(syntax.generic_mentions_span)
}

// receiver_app_args returns a process-owned immutable array view.
@[inline]
pub fn (syntax &SourceTypeSyntax) receiver_app_args() []u32 {
	return syntax.ids(syntax.receiver_app_args_span)
}

// checker_bracket_components returns a process-owned immutable array view.
@[inline]
pub fn (syntax &SourceTypeSyntax) checker_bracket_components() []u32 {
	return syntax.ids(syntax.checker_bracket_components_span)
}

// legacy_generic_words returns a lazily cached process-owned immutable array view.
@[inline]
pub fn (syntax &SourceTypeSyntax) legacy_generic_words() []u32 {
	if isnil(syntax.payload) { return syntax.ids(SourceTypeSyntaxSpan{}) }
	if syntax.plain_type_name { return syntax.ids(SourceTypeSyntaxSpan{ len: 1 }) }
	id := unsafe { *&u32(syntax.payload) }
	return source_type_syntax_lexical_ids(source_type_syntax_lexical(id, .legacy_words), false)
}

// symbols_first_offsets returns a lazily cached process-owned immutable array view.
@[inline]
pub fn (syntax &SourceTypeSyntax) symbols_first_offsets() []u32 {
	if isnil(syntax.payload) { return syntax.ids(SourceTypeSyntaxSpan{}) }
	if syntax.named_type {
		return source_type_syntax_singleton_ids(&g_source_type_syntax_zero_offset)
	}
	id := unsafe { *&u32(syntax.payload) }
	return source_type_syntax_lexical_ids(source_type_syntax_lexical(id, .symbols), true)
}

// voidptr_parts returns a process-owned immutable array view.
@[inline]
pub fn (syntax &SourceTypeSyntax) voidptr_parts() []u32 {
	return syntax.ids(syntax.voidptr_parts_span)
}

// inferred_parts returns a process-owned immutable array view.
@[inline]
pub fn (syntax &SourceTypeSyntax) inferred_parts() []u32 {
	return syntax.ids(syntax.inferred_parts_span)
}

// inferred_annotations returns a process-owned immutable array view.
@[inline]
pub fn (syntax &SourceTypeSyntax) inferred_annotations() []u32 {
	return syntax.ids(syntax.inferred_annotations_span)
}

// inferred_expr_ids returns a process-owned immutable array view.
@[inline]
pub fn (syntax &SourceTypeSyntax) inferred_expr_ids() []u32 {
	return syntax.ids(syntax.inferred_expr_ids_span)
}

// substitution_function_params returns a process-owned immutable array view.
@[inline]
pub fn (syntax &SourceTypeSyntax) substitution_function_params() []u32 {
	return syntax.ids(syntax.substitution_function_params_span)
}

// flattened_receiver_parts returns a lazily cached process-owned immutable array view.
@[inline]
pub fn (syntax &SourceTypeSyntax) flattened_receiver_parts() []u32 {
	if isnil(syntax.payload) { return syntax.ids(SourceTypeSyntaxSpan{}) }
	if !syntax.underscored { return syntax.ids(SourceTypeSyntaxSpan{ len: 1 }) }
	id := unsafe { *&u32(syntax.payload) }
	return source_type_syntax_flattened_receiver_ids(id)
}

// mut_chars_base applies the legacy raw trim_left('mut ') character-set query lazily.
// The numeric composition cache decodes the spelling only on its first query.
@[inline]
pub fn (syntax &SourceTypeSyntax) mut_chars_base() u32 {
	if isnil(syntax.payload) {
		return 0
	}
	id := unsafe { *&u32(syntax.payload) }
	if id == 0 {
		return 0
	}
	return source_type_composition_one(.legacy_mut_chars, id)
}

// last_double_underscore_prefix returns the exact raw spelling before its final double underscore.
// The numeric composition cache decodes a spelling only on its first query.
@[inline]
pub fn (syntax &SourceTypeSyntax) last_double_underscore_prefix() u32 {
	if isnil(syntax.payload) {
		return 0
	}
	id := unsafe { *&u32(syntax.payload) }
	if !syntax.contains_doubleunderscore {
		return id
	}
	return source_type_composition_one(.last_double_underscore_prefix, id)
}

// last_double_underscore_suffix returns the exact raw spelling after its final double underscore.
// The numeric composition cache decodes a spelling only on its first query.
@[inline]
pub fn (syntax &SourceTypeSyntax) last_double_underscore_suffix() u32 {
	if isnil(syntax.payload) {
		return 0
	}
	id := unsafe { *&u32(syntax.payload) }
	if !syntax.contains_doubleunderscore {
		return id
	}
	return source_type_composition_one(.last_double_underscore_suffix, id)
}

// doubleunderscore_dotted replaces raw double underscores with dots only when requested.
// The numeric composition cache decodes a spelling only on its first query.
@[inline]
pub fn (syntax &SourceTypeSyntax) doubleunderscore_dotted() u32 {
	if isnil(syntax.payload) {
		return 0
	}
	id := unsafe { *&u32(syntax.payload) }
	if !syntax.contains_doubleunderscore {
		return id
	}
	return source_type_composition_one(.doubleunderscore_dotted, id)
}

// ptr_name_base returns the raw suffix after a nonempty ptr prefix, or zero.
// The numeric composition cache decodes a spelling only on its first query.
@[inline]
pub fn (syntax &SourceTypeSyntax) ptr_name_base() u32 {
	if isnil(syntax.payload) {
		return 0
	}
	id := unsafe { *&u32(syntax.payload) }
	if syntax.byte_len <= 3 {
		return 0
	}
	return source_type_composition_one(.ptr_name_base, id)
}

// main_removed removes every raw main. occurrence only when requested.
// The numeric composition cache decodes a spelling only on its first query.
@[inline]
pub fn (syntax &SourceTypeSyntax) main_removed() u32 {
	if isnil(syntax.payload) {
		return 0
	}
	id := unsafe { *&u32(syntax.payload) }
	if !syntax.main_fragment {
		return id
	}
	return source_type_composition_one(.main_removed, id)
}

// written_applications returns a process-owned immutable array view.
@[inline]
pub fn (syntax &SourceTypeSyntax) written_applications() []u32 {
	return syntax.ids(syntax.written_applications_span)
}

// written_application_offsets returns a process-owned immutable array view.
@[inline]
pub fn (syntax &SourceTypeSyntax) written_application_offsets() []u32 {
	return syntax.ids(syntax.written_application_offsets_span)
}

// checker_raw_bracket_components returns a process-owned immutable array view.
@[inline]
pub fn (syntax &SourceTypeSyntax) checker_raw_bracket_components() []u32 {
	return syntax.ids(syntax.checker_raw_bracket_components_span)
}

// cgen_placeholder_candidates returns a process-owned immutable array view.
@[inline]
pub fn (syntax &SourceTypeSyntax) cgen_placeholder_candidates() []u32 {
	return syntax.ids(syntax.cgen_placeholder_candidates_span)
}

// reflection_parts returns a process-owned immutable array view.
@[inline]
pub fn (syntax &SourceTypeSyntax) reflection_parts() []u32 {
	return syntax.ids(syntax.reflection_parts_span)
}

// reflection_tuple_parts returns a process-owned immutable array view.
@[inline]
pub fn (syntax &SourceTypeSyntax) reflection_tuple_parts() []u32 {
	return syntax.ids(syntax.reflection_tuple_parts_span)
}

// receiver_raw_app_args returns a process-owned immutable array view.
@[inline]
pub fn (syntax &SourceTypeSyntax) receiver_raw_app_args() []u32 {
	return syntax.ids(syntax.receiver_raw_app_args_span)
}

// vls_generic_words returns a process-owned immutable array view.
@[inline]
pub fn (syntax &SourceTypeSyntax) vls_generic_words() []u32 {
	return syntax.ids(syntax.vls_generic_words_span)
}

// eval_tuple_parts returns a process-owned immutable array view.
@[inline]
pub fn (syntax &SourceTypeSyntax) eval_tuple_parts() []u32 {
	return syntax.ids(syntax.eval_tuple_parts_span)
}

// implicit_tuple_components returns a process-owned immutable array view.
@[inline]
pub fn (syntax &SourceTypeSyntax) implicit_tuple_components() []u32 {
	return syntax.ids(syntax.implicit_tuple_components_span)
}

// generic_application_heads returns a process-owned immutable array view.
@[inline]
pub fn (syntax &SourceTypeSyntax) generic_application_heads() []u32 {
	return syntax.ids(syntax.generic_application_heads_span)
}

// markused_application_heads returns a process-owned immutable array view.
@[inline]
pub fn (syntax &SourceTypeSyntax) markused_application_heads() []u32 {
	return syntax.ids(syntax.markused_application_heads_span)
}

// markused_generic_components returns a process-owned immutable array view.
@[inline]
pub fn (syntax &SourceTypeSyntax) markused_generic_components() []u32 {
	return syntax.ids(syntax.markused_generic_components_span)
}

// raw_tuple_parse_components returns a process-owned immutable array view.
@[inline]
pub fn (syntax &SourceTypeSyntax) raw_tuple_parse_components() []u32 {
	return syntax.ids(syntax.raw_tuple_parse_components_span)
}

// union_parts returns a process-owned immutable array view.
@[inline]
pub fn (syntax &SourceTypeSyntax) union_parts() []u32 {
	return syntax.ids(syntax.union_parts_span)
}

struct SourceTypeSyntaxParsed {
	kind                               SourceTypeSyntaxKind
	trimmed                            u32
	base                               u32
	args                               []u32 = source_type_syntax_empty_ids()
	unqualified                        u32
	qualifier                          u32
	generic_base                       u32
	pointer_base                       u32
	pointer_depth                      int
	function_has_result                bool
	name_packing                       bool
	raw_base                           u32
	main_base                          u32
	bare_generic                       bool
	application_base                   u32
	application_args                   []u32 = source_type_syntax_empty_ids()
	application_valid                  bool
	generic_app_base                   u32
	generic_app_args                   []u32 = source_type_syntax_empty_ids()
	generic_app_valid                  bool
	postfix_base                       u32
	postfix_dims                       []u32 = source_type_syntax_empty_ids()
	parameter_type                     u32
	parameter_mut_type                 u32
	bracket_closed                     bool
	bracket_base                       u32
	bracket_inner                      u32
	bracket_raw_inner                  u32
	bracket_prefix                     u32
	function_closed                    bool
	function_params                    []u32 = source_type_syntax_empty_ids()
	function_result                    u32
	tuple_components                   []u32 = source_type_syntax_empty_ids()
	tuple_like                         bool
	function_space_prefix              bool
	spaced                             bool
	first_bracket_closed               bool
	first_bracket_base                 u32
	last_space_word                    u32
	named_type                         bool
	c_qualified                        bool
	first_dot_prefix                   u32
	first_dot_suffix                   u32
	builtin_prefix                     bool
	main_fragment                      bool
	function_prefix                    bool
	fn_prefix                          bool
	shared_fragment                    bool
	pointer_raw_base                   u32
	qualifier_count                    u32
	builtin_base                       u32
	cgen_app_base                      u32
	cgen_app_args                      []u32 = source_type_syntax_empty_ids()
	cgen_app_valid                     bool
	cgen_function_params               []u32 = source_type_syntax_empty_ids()
	checker_function_params            []u32 = source_type_syntax_empty_ids()
	checker_tuple_components           []u32 = source_type_syntax_empty_ids()
	tuple_contains_comma               bool
	top_space_head                     u32
	top_space_tail                     u32
	top_space_valid                    bool
	typeof_expr_prefix                 bool
	bracket_suffix                     u32
	bracket_from                       u32
	bracket_inner_fixed_length         bool
	first_bracket_prefix               u32
	void_pointer_postfix               bool
	canonical_dotted_name              bool
	decimal                            bool
	contains_comma                     bool
	function_fragment                  bool
	dots_base                          u32
	cgen_raw_app_base                  u32
	cgen_raw_app_args                  []u32 = source_type_syntax_empty_ids()
	cgen_raw_app_valid                 bool
	cgen_parts                         []u32 = source_type_syntax_empty_ids()
	generic_parameter_payload          u32
	legacy_parameter_type              u32
	legacy_parameter_mut_type          u32
	first_bracket_length               u32
	checker_raw_tuple_components       []u32 = source_type_syntax_empty_ids()
	bracket_raw_parts                  []u32 = source_type_syntax_empty_ids()
	bracket_generic_parts              []u32 = source_type_syntax_empty_ids()
	tuple_function_components          []u32 = source_type_syntax_empty_ids()
	c_pointer_base                     u32
	c_ptr_name_suffix                  bool
	c_fn_type_fragment                 bool
	underscored                        bool
	plain_comma_parts                  []u32 = source_type_syntax_empty_ids()
	source_wrapper_base                u32
	declaration_head                   u32
	fn_fragment                        bool
	compact_fn_spelling                u32
	first_bracket_raw_inner            u32
	cgen_generic_placeholder           bool
	fixed_array_type                   bool
	typeof_valid                       bool
	typeof_inner                       u32
	postfix_index_base                 u32
	last_bracket_prefix                u32
	last_bracket_length                u32
	trailing_bracket_prefix            u32
	trailing_bracket_closed            bool
	array_chars_base                   u32
	lowercase_value_ident              bool
	typeof_prefix                      bool
	first_capital                      bool
	named_variant                      bool
	named_variant_valid                bool
	named_variant_owner                u32
	named_variant_name                 u32
	main_unlocked                      u32
	encoded_fixed_length               u32
	encoded_fixed_base                 u32
	map_name_prefix                    bool
	suffix_bracket_length              u32
	suffix_bracket_valid               bool
	map_function_fragment              bool
	bracket_source_inner               u32
	c_fixed_typedef_element            u32
	c_fixed_typedef_length             u32
	c_fixed_typedef_valid              bool
	fixed_array_query_length           u32
	last_bracket_span_valid            bool
	last_bracket_span_inner            u32
	plain_type_name                    bool
	generic_app_legacy_suffix          u32
	wrapper_prefix                     u32
	wrapper_base                       u32
	checker_fixed_array_may_be         bool
	dollar_prefix                      bool
	define_prefix                      bool
	define_fragment                    bool
	paren_prefix                       bool
	fixed_length                       bool
	qualified_modifiers                u32
	plain_alias_target                 bool
	c_array_payload                    u32
	c_fixed_array_payload              u32
	fastc_array_marker                 bool
	fastc_array_marker_payload         u32
	c_map_payload                      u32
	c_ptr_name_base                    u32
	c_tagged_type                      u32
	backward_bracket_prefix            u32
	backward_bracket_closed            bool
	generic_mentions                   []u32 = source_type_syntax_empty_ids()
	opening_paren                      bool
	byte_len                           int
	receiver_app_valid                 bool
	receiver_app_base                  u32
	receiver_app_args                  []u32 = source_type_syntax_empty_ids()
	checker_bracket_components         []u32 = source_type_syntax_empty_ids()
	declaration_parameter_payload      u32
	declaration_parameter_mut          bool
	prefix_dimension_tail              u32
	prefix_dimensions_closed           bool
	backward_bracket_after_prefix_dims bool
	backward_bracket_inner             u32
	option_result_void_prefix          bool
	nested_option_result_marker        int
	c_pointer_depth                    int
	c_pointer_count                    int
	c_pointer_raw_base                 u32
	voidptr_parts                      []u32 = source_type_syntax_empty_ids()
	thread_value_prefix                bool
	thread_value_payload               u32
	thread_value_type_valid            bool
	thread_value_type                  u32
	contains_closing_bracket           bool
	atomic_val_fragment                bool
	inferred_parts                     []u32 = source_type_syntax_empty_ids()
	inferred_annotations               []u32 = source_type_syntax_empty_ids()
	// inferred_expr_ids stores AST NodeId bit patterns, not source spelling IDs.
	inferred_expr_ids                 []u32 = source_type_syntax_empty_ids()
	expr_prefix                       bool
	expr_id                           int
	substitution_function_params      []u32 = source_type_syntax_empty_ids()
	parameter_name                    u32
	parameter_named                   bool
	parameter_is_mut                  bool
	transform_fixed_length            bool
	receiver_collection_id            u32
	ampersand_trimmed                 u32
	contains_doubleunderscore         bool
	parallel_preseed_fn               bool
	parallel_preseed_short            u32
	closing_bracket                   bool
	constraint_receiver_base          u32
	written_applications              []u32 = source_type_syntax_empty_ids()
	written_application_offsets       []u32 = source_type_syntax_empty_ids()
	checker_raw_bracket_components    []u32 = source_type_syntax_empty_ids()
	cgen_placeholder_candidates       []u32 = source_type_syntax_empty_ids()
	result_suffix                     bool
	cgen_decl_unusable                bool
	cgen_decl_authority               bool
	generic_encoded_fragment          bool
	fn_ptr_encoded_prefix             bool
	c_result_prefix                   bool
	c_option_prefix                   bool
	c_multi_return_prefix             bool
	c_lower_map_prefix                bool
	open_generic_receiver_marker      bool
	space_removed                     u32
	qualified_name_chars              bool
	field_owner_module                u32
	const_name_prefix                 bool
	reflection_parts                  []u32 = source_type_syntax_empty_ids()
	reflection_tuple_parts            []u32 = source_type_syntax_empty_ids()
	pthread_prefix                    bool
	thread_bracket_prefix             bool
	fixed_typedef_bracket             bool
	receiver_raw_app_args             []u32 = source_type_syntax_empty_ids()
	question_trimmed                  u32
	c_array_fixed_prefix              bool
	vls_generic_words                 []u32 = source_type_syntax_empty_ids()
	function_space_result             u32
	decimal_value                     int
	integer_value                     int
	encoded_alnum_dotted              bool
	internal_symbol_prefix            bool
	main_at_base                      u32
	eval_tuple_parts                  []u32 = source_type_syntax_empty_ids()
	implicit_tuple_components         []u32 = source_type_syntax_empty_ids()
	generic_application_heads         []u32 = source_type_syntax_empty_ids()
	incremental_generic_application   bool
	system_error_suffix               bool
	receiver_app_inner                u32
	fastc_fixed_array_length_valid    bool
	fastc_fixed_array_length          u32
	markused_application_heads        []u32 = source_type_syntax_empty_ids()
	contains_capital                  bool
	markused_generic_components       []u32 = source_type_syntax_empty_ids()
	first_bracket_source_inner        u32
	c_function_pointer_hole           bool
	channel_fragment                  bool
	array_receiver_fragment           bool
	mangled_open_generic_marker       bool
	array_suffix                      bool
	empty_function_prefix             bool
	c_shared_fragment                 bool
	typ_suffix                        bool
	typ_suffix_base                   u32
	raw_tuple_parse_components        []u32 = source_type_syntax_empty_ids()
	checker_parse_function_result     u32
	checker_parse_function_has_params bool
	top_level_dot_valid               bool
	top_level_dot_prefix              u32
	top_level_dot_suffix              u32
	parser_fn_decl_valid              bool
	parser_fn_decl_params             u32
	parser_fn_decl_result             u32
	contains_question                 bool
	contains_union                    bool
	union_parts                       []u32 = source_type_syntax_empty_ids()
	named_variant_dot_valid           bool
	named_variant_dot_prefix          u32
	named_variant_dot_suffix          u32
}

struct SourceTypeSyntaxEntry {
	syntax SourceTypeSyntax
}

// Positive IDs use the same bounded page indices as the spelling interner.
// Zero is a valid empty spelling and has its own slot outside those pages.
__global g_source_type_syntax_pages [source_type_chunk_count]voidptr
__global g_source_type_syntax_empty voidptr

// Prefixes and suffixes have independent cache entries: requesting either view
// never interns the other. Empty views are cached too. The spelling ID is stored
// in slot zero of the owning syntax payload, even for the empty spelling.
struct SourceTypeUnderscoreEntry {
	id     u32
	suffix bool
	len    int
	next   &SourceTypeUnderscoreEntry = unsafe { nil }
}

__global g_source_type_underscore_parts [4096]voidptr

@[inline]
fn source_type_syntax_underscore_slot(id u32, suffix bool) int {
	return int((id ^ (if suffix { u32(2166136261) } else { u32(0) })) & 4095)
}

fn source_type_syntax_underscore_ids(id u32, suffix bool) []u32 {
	slot := source_type_syntax_underscore_slot(id, suffix)
	cached := source_type_syntax_underscore_find(slot, id, suffix)
	if !isnil(cached) { return source_type_syntax_underscore_view(cached) }
	return source_type_syntax_underscore_miss(slot, id, suffix)
}

fn source_type_syntax_underscore_miss(slot int, id u32, suffix bool) []u32 {
	// Interning can acquire the source spelling lock. Complete every spelling
	// operation before taking that lock to publish the immutable result.
	raw := source_type_name(id)
	mut parts := source_type_syntax_empty_ids()
	for i, c in raw {
		if c == `_` {
			parts << if suffix {
				source_type_syntax_child_range_id(id, raw, i + 1, raw.len)
			} else {
				source_type_syntax_child_range_id(id, raw, 0, i)
			}
		}
	}
	source_type_lock()
	defer { C.atomic_store_u32(&g_source_types_lock, 0) }
	existing := source_type_syntax_underscore_find(slot, id, suffix)
	if !isnil(existing) { return source_type_syntax_underscore_view(existing) }
	payload_bytes := u64(sizeof(SourceTypeUnderscoreEntry)) + u64(parts.len) * u64(sizeof(u32))
	if payload_bytes > u64(0x7fffffff) {
		panic('source type underscore components exceed the allocation limit')
	}
	mut entry := unsafe { &SourceTypeUnderscoreEntry(node_payload_alloc_zeroed(uint(payload_bytes))) }
	if isnil(entry) { panic('could not allocate source type underscore components') }
	unsafe {
		*entry = SourceTypeUnderscoreEntry{
			id:     id
			suffix: suffix
			len:    parts.len
			next:   source_type_syntax_underscore_head(slot)
		}
		if parts.len > 0 {
			vmemcpy(voidptr(&u8(entry) + sizeof(SourceTypeUnderscoreEntry)), parts.data,
				parts.len * int(sizeof(u32)))
		}
	}
	$if x32 {
		C.atomic_store_u32(voidptr(&g_source_type_underscore_parts[slot]), u32(voidptr(entry)))
	} $else {
		C.atomic_store_u64(voidptr(&g_source_type_underscore_parts[slot]), u64(voidptr(entry)))
	}
	return source_type_syntax_underscore_view(entry)
}

@[inline]
fn source_type_syntax_underscore_view(entry &SourceTypeUnderscoreEntry) []u32 {
	return unsafe {
		array{
			data:         if entry.len == 0 {
				nil
			} else {
				voidptr(&u8(entry) + sizeof(SourceTypeUnderscoreEntry))
			}
			len:          entry.len
			cap:          entry.len
			element_size: sizeof(u32)
			flags:        .nogrow | .nofree
		}
	}
}

@[direct_array_access; inline]
fn source_type_syntax_underscore_head(slot int) &SourceTypeUnderscoreEntry {
	$if x32 {
		return unsafe { &SourceTypeUnderscoreEntry(voidptr(C.atomic_load_u32(voidptr(&g_source_type_underscore_parts[slot])))) }
	} $else {
		return unsafe { &SourceTypeUnderscoreEntry(voidptr(C.atomic_load_u64(voidptr(&g_source_type_underscore_parts[slot])))) }
	}
}

fn source_type_syntax_underscore_find(slot int, id u32, suffix bool) &SourceTypeUnderscoreEntry {
	mut entry := source_type_syntax_underscore_head(slot)
	for !isnil(entry) {
		if entry.id == id && entry.suffix == suffix { return entry }
		entry = entry.next
	}
	return unsafe { nil }
}

// Lexical grammars are independent views: build only the requested one. A
// published entry owns every ID and tag, even when its result is empty.
enum SourceTypeLexicalKind {
	symbols
	words
	legacy_words
	dotted
	parts
	qualifiers
}

struct SourceTypeLexicalEntry {
	id            u32
	kind          SourceTypeLexicalKind
	ids_len       int
	secondary_len int
	names_len     int
	next          &SourceTypeLexicalEntry = unsafe { nil }
}

__global g_source_type_lexical_views [4096]voidptr

@[inline]
fn source_type_syntax_lexical_slot(id u32, kind SourceTypeLexicalKind) int {
	return int((id ^ (u32(kind) * 16777619)) & 4095)
}

@[inline]
fn source_type_syntax_lexical_head(slot int) &SourceTypeLexicalEntry {
	return unsafe { &SourceTypeLexicalEntry(source_type_syntax_pointer_load(voidptr(&g_source_type_lexical_views[slot]))) }
}

fn source_type_syntax_lexical_find(slot int, id u32, kind SourceTypeLexicalKind) &SourceTypeLexicalEntry {
	mut entry := source_type_syntax_lexical_head(slot)
	for !isnil(entry) {
		if entry.id == id && entry.kind == kind { return entry }
		entry = entry.next
	}
	return unsafe { nil }
}

fn source_type_syntax_lexical(id u32, kind SourceTypeLexicalKind) &SourceTypeLexicalEntry {
	slot := source_type_syntax_lexical_slot(id, kind)
	cached := source_type_syntax_lexical_find(slot, id, kind)
	if !isnil(cached) { return cached }
	return source_type_syntax_lexical_miss(slot, id, kind)
}

fn source_type_syntax_lexical_miss(slot int, id u32, kind SourceTypeLexicalKind) &SourceTypeLexicalEntry {
	// Lexing and interning may acquire the spelling lock: finish them before
	// entering the publication critical section. No worker-backed slice escapes.
	raw := source_type_name(id)
	mut ids := source_type_syntax_empty_ids()
	mut secondary := source_type_syntax_empty_ids()
	mut names := source_type_syntax_empty_bools()
	match kind {
		.symbols { ids, secondary = source_type_syntax_symbols(raw) }
		.words { ids = source_type_syntax_tokens(raw, false) }
		.legacy_words { ids = source_type_syntax_legacy_words(raw) }
		.dotted {
			for part in raw.split('.') { ids << source_type_syntax_child_id(id, raw, part) }
		}
		.parts { ids, names = source_type_syntax_symbol_parts(raw) }
		.qualifiers {
			for i, c in raw {
				if c == `.` {
					ids << source_type_syntax_child_range_id(id, raw, 0, i)
					secondary << source_type_syntax_child_range_id(id, raw, i + 1, raw.len)
				}
			}
		}
	}
	source_type_lock()
	defer { C.atomic_store_u32(&g_source_types_lock, 0) }
	existing := source_type_syntax_lexical_find(slot, id, kind)
	if !isnil(existing) { return existing }
	payload_bytes := u64(sizeof(SourceTypeLexicalEntry)) +
		(u64(ids.len) + u64(secondary.len)) * u64(sizeof(u32)) + u64(names.len) * u64(sizeof(bool))
	if payload_bytes > u64(0x7fffffff) {
		panic('source type lexical view exceeds the allocation limit')
	}
	mut entry := unsafe { &SourceTypeLexicalEntry(node_payload_alloc_zeroed(uint(payload_bytes))) }
	if isnil(entry) { panic('could not allocate source type lexical view') }
	unsafe {
		*entry = SourceTypeLexicalEntry{
			id:            id
			kind:          kind
			ids_len:       ids.len
			secondary_len: secondary.len
			names_len:     names.len
			next:          source_type_syntax_lexical_head(slot)
		}
		data := &u8(entry) + sizeof(SourceTypeLexicalEntry)
		if ids.len > 0 { vmemcpy(data, ids.data, ids.len * int(sizeof(u32))) }
		if secondary.len > 0 {
			vmemcpy(data + ids.len * int(sizeof(u32)), secondary.data, secondary.len * int(sizeof(u32)))
		}
		if names.len > 0 {
			vmemcpy(data + (ids.len + secondary.len) * int(sizeof(u32)), names.data, names.len * int(sizeof(bool)))
		}
	}
	source_type_syntax_pointer_store(voidptr(&g_source_type_lexical_views[slot]), voidptr(entry))
	return entry
}

@[inline]
fn source_type_syntax_lexical_ids(entry &SourceTypeLexicalEntry, secondary bool) []u32 {
	len := if secondary { entry.secondary_len } else { entry.ids_len }
	offset := if secondary { entry.ids_len * int(sizeof(u32)) } else { 0 }
	return unsafe {
		array{
			data:         if len == 0 {
				nil
			} else {
				voidptr(&u8(entry) + sizeof(SourceTypeLexicalEntry) + offset)
			}
			len:          len
			cap:          len
			element_size: sizeof(u32)
			flags:        .nogrow | .nofree
		}
	}
}

@[inline]
fn source_type_syntax_lexical_names(entry &SourceTypeLexicalEntry) []bool {
	return unsafe {
		array{
			data:         if entry.names_len == 0 {
				nil
			} else {
				voidptr(&u8(entry) + sizeof(SourceTypeLexicalEntry) + (entry.ids_len + entry.secondary_len) * int(sizeof(u32)))
			}
			len:          entry.names_len
			cap:          entry.names_len
			element_size: sizeof(bool)
			flags:        .nogrow | .nofree
		}
	}
}

// Flattened receiver components split single underscores while preserving pairs.
// The cache is independent of ordinary underscore prefix and suffix views.
struct SourceTypeFlattenedReceiverEntry {
	id   u32
	len  int
	next &SourceTypeFlattenedReceiverEntry = unsafe { nil }
}

__global g_source_type_flattened_receivers [4096]voidptr

fn source_type_syntax_flattened_receiver_ids(id u32) []u32 {
	slot := int(id & 4095)
	cached := source_type_syntax_flattened_receiver_find(slot, id)
	if !isnil(cached) { return source_type_syntax_flattened_receiver_view(cached) }
	return source_type_syntax_flattened_receiver_miss(slot, id)
}

fn source_type_syntax_flattened_receiver_miss(slot int, id u32) []u32 {
	// Derived spellings must be interned before taking the spelling writer lock.
	parts := source_type_syntax_flattened_receiver(id, source_type_name(id))
	source_type_lock()
	defer { C.atomic_store_u32(&g_source_types_lock, 0) }
	existing := source_type_syntax_flattened_receiver_find(slot, id)
	if !isnil(existing) { return source_type_syntax_flattened_receiver_view(existing) }
	payload_bytes := u64(sizeof(SourceTypeFlattenedReceiverEntry)) + u64(parts.len) * u64(sizeof(u32))
	if payload_bytes > u64(0x7fffffff) {
		panic('source type flattened receiver components exceed the allocation limit')
	}
	mut entry := unsafe { &SourceTypeFlattenedReceiverEntry(node_payload_alloc_zeroed(uint(payload_bytes))) }
	if isnil(entry) { panic('could not allocate source type flattened receiver components') }
	unsafe {
		*entry = SourceTypeFlattenedReceiverEntry{
			id:   id
			len:  parts.len
			next: source_type_syntax_flattened_receiver_head(slot)
		}
		if parts.len > 0 {
			vmemcpy(voidptr(&u8(entry) + sizeof(SourceTypeFlattenedReceiverEntry)), parts.data,
				parts.len * int(sizeof(u32)))
		}
	}
	source_type_syntax_pointer_store(voidptr(&g_source_type_flattened_receivers[slot]), voidptr(entry))
	return source_type_syntax_flattened_receiver_view(entry)
}

@[inline]
fn source_type_syntax_flattened_receiver_view(entry &SourceTypeFlattenedReceiverEntry) []u32 {
	return unsafe {
		array{
			data:         if entry.len == 0 {
				nil
			} else {
				voidptr(&u8(entry) + sizeof(SourceTypeFlattenedReceiverEntry))
			}
			len:          entry.len
			cap:          entry.len
			element_size: sizeof(u32)
			flags:        .nogrow | .nofree
		}
	}
}

@[direct_array_access; inline]
fn source_type_syntax_flattened_receiver_head(slot int) &SourceTypeFlattenedReceiverEntry {
	return unsafe { &SourceTypeFlattenedReceiverEntry(source_type_syntax_pointer_load(voidptr(&g_source_type_flattened_receivers[slot]))) }
}

fn source_type_syntax_flattened_receiver_find(slot int, id u32) &SourceTypeFlattenedReceiverEntry {
	mut entry := source_type_syntax_flattened_receiver_head(slot)
	for !isnil(entry) {
		if entry.id == id { return entry }
		entry = entry.next
	}
	return unsafe { nil }
}

struct SourceTypeCaptureEntry {
	actual  u32
	prefix  u32
	suffix  u32
	result  u32
	matched bool
	next    &SourceTypeCaptureEntry = unsafe { nil }
}

__global g_source_type_captures [4096]voidptr

// source_type_capture captures the exact spelling between an interned prefix
// and suffix. Repeated queries compare IDs without reading spelling bytes.
pub fn source_type_capture(actual u32, prefix u32, suffix u32) ?u32 {
	slot := int((actual ^ (prefix * u32(16777619)) ^ (suffix * u32(2166136261))) & 4095)
	first := source_type_capture_find(slot, actual, prefix, suffix)
	if !isnil(first) {
		if first.matched { return first.result }
		return none
	}
	text := source_type_name(actual)
	head := source_type_name(prefix)
	tail := source_type_name(suffix)
	matched := text.len >= head.len + tail.len && text.starts_with(head) && text.ends_with(tail)
	result := if matched { source_type_id(text[head.len..text.len - tail.len]) } else { u32(0) }
	source_type_lock()
	defer { C.atomic_store_u32(&g_source_types_lock, 0) }
	existing := source_type_capture_find(slot, actual, prefix, suffix)
	if !isnil(existing) {
		if existing.matched { return existing.result }
		return none
	}
	mut entry := unsafe { &SourceTypeCaptureEntry(node_payload_alloc_zeroed(sizeof(SourceTypeCaptureEntry))) }
	if isnil(entry) { panic('could not allocate source type capture') }
	unsafe {
		*entry = SourceTypeCaptureEntry{
			actual:  actual
			prefix:  prefix
			suffix:  suffix
			result:  result
			matched: matched
			next:    source_type_capture_head(slot)
		}
	}
	$if x32 {
		C.atomic_store_u32(voidptr(&g_source_type_captures[slot]), u32(voidptr(entry)))
	} $else {
		C.atomic_store_u64(voidptr(&g_source_type_captures[slot]), u64(voidptr(entry)))
	}
	if matched { return result }
	return none
}

@[direct_array_access; inline]
fn source_type_capture_head(slot int) &SourceTypeCaptureEntry {
	$if x32 {
		return unsafe { &SourceTypeCaptureEntry(voidptr(C.atomic_load_u32(voidptr(&g_source_type_captures[slot])))) }
	} $else {
		return unsafe { &SourceTypeCaptureEntry(voidptr(C.atomic_load_u64(voidptr(&g_source_type_captures[slot])))) }
	}
}

fn source_type_capture_find(slot int, actual u32, prefix u32, suffix u32) &SourceTypeCaptureEntry {
	mut entry := source_type_capture_head(slot)
	for !isnil(entry) {
		if entry.actual == actual && entry.prefix == prefix && entry.suffix == suffix {
			return entry
		}
		entry = entry.next
	}
	return unsafe { nil }
}

struct SourceTypeEncodedPrefixEntry {
	actual  u32
	base    u32
	matched bool
	next    &SourceTypeEncodedPrefixEntry = unsafe { nil }
}

__global g_source_type_encoded_prefixes [4096]voidptr

// source_type_c_encoded_prefix_matches compares a source name with a C-encoded
// prefix. Dots accept either one dot or two underscores, consumed greedily.
// Numeric pair cache hits do not read either spelling.
pub fn source_type_c_encoded_prefix_matches(actual u32, base u32) bool {
	slot := int((actual ^ (base * u32(16777619))) & 4095)
	cached := source_type_encoded_prefix_find(slot, actual, base)
	if !isnil(cached) { return cached.matched }
	matched := source_type_c_encoded_prefix_bytes(source_type_name(actual), source_type_name(base))
	source_type_lock()
	defer { C.atomic_store_u32(&g_source_types_lock, 0) }
	existing := source_type_encoded_prefix_find(slot, actual, base)
	if !isnil(existing) { return existing.matched }
	mut entry := unsafe { &SourceTypeEncodedPrefixEntry(node_payload_alloc_zeroed(sizeof(SourceTypeEncodedPrefixEntry))) }
	if isnil(entry) { panic('could not allocate source type encoded prefix') }
	unsafe {
		*entry = SourceTypeEncodedPrefixEntry{
			actual:  actual
			base:    base
			matched: matched
			next:    source_type_encoded_prefix_head(slot)
		}
	}
	$if x32 {
		C.atomic_store_u32(voidptr(&g_source_type_encoded_prefixes[slot]), u32(voidptr(entry)))
	} $else {
		C.atomic_store_u64(voidptr(&g_source_type_encoded_prefixes[slot]), u64(voidptr(entry)))
	}
	return matched
}

fn source_type_c_encoded_prefix_bytes(actual string, base string) bool {
	mut offset := 0
	for c in base {
		if offset >= actual.len { return false }
		if c == `.` && actual[offset..].starts_with('__') {
			offset += 2
		} else {
			if actual[offset] != c { return false }
			offset++
		}
	}
	return true
}

@[direct_array_access; inline]
fn source_type_encoded_prefix_head(slot int) &SourceTypeEncodedPrefixEntry {
	$if x32 {
		return unsafe { &SourceTypeEncodedPrefixEntry(voidptr(C.atomic_load_u32(voidptr(&g_source_type_encoded_prefixes[slot])))) }
	} $else {
		return unsafe { &SourceTypeEncodedPrefixEntry(voidptr(C.atomic_load_u64(voidptr(&g_source_type_encoded_prefixes[slot])))) }
	}
}

fn source_type_encoded_prefix_find(slot int, actual u32, base u32) &SourceTypeEncodedPrefixEntry {
	mut entry := source_type_encoded_prefix_head(slot)
	for !isnil(entry) {
		if entry.actual == actual && entry.base == base { return entry }
		entry = entry.next
	}
	return unsafe { nil }
}

enum SourceTypePredicate {
	encoded_receiver
	legacy_generic_param
	word
}

struct SourceTypePredicateEntry {
	actual    u32
	parameter u32
	kind      SourceTypePredicate
	matched   bool
	next      &SourceTypePredicateEntry = unsafe { nil }
}

__global g_source_type_predicates [4096]voidptr

// source_type_encoded_receiver_matches tests the exact encoded receiver fragment
// '__' + base + '_'. Numeric pair hits read no spelling bytes.
pub fn source_type_encoded_receiver_matches(actual u32, base u32) bool {
	return source_type_predicate(actual, base, .encoded_receiver)
}

// source_type_legacy_generic_param_matches preserves the legacy generic
// substring query without eagerly interning every possible substring.
pub fn source_type_legacy_generic_param_matches(actual u32, parameter u32) bool {
	return source_type_predicate(actual, parameter, .legacy_generic_param)
}

// source_type_contains_word tests a spelling substring with ASCII letter,
// digit and underscore boundaries, preserving dots inside a requested word.
pub fn source_type_contains_word(actual u32, word u32) bool {
	return source_type_predicate(actual, word, .word)
}

fn source_type_predicate(actual u32, parameter u32, kind SourceTypePredicate) bool {
	slot := int((actual ^ (parameter * u32(16777619)) ^ (u32(kind) * u32(2166136261))) & 4095)
	cached := source_type_predicate_find(slot, actual, parameter, kind)
	if !isnil(cached) { return cached.matched }
	text := source_type_name(actual)
	part := source_type_name(parameter)
	matched := match kind {
		.encoded_receiver { text.contains('__' + part + '_') }
		.legacy_generic_param {
			clean := source_type_query_trim(text)
			clean == part || clean.starts_with('&' + part) || clean.ends_with('[' + part + ']')
				|| clean.contains('[' + part + ',') || clean.contains(', ' + part + ']')
		}
		.word { source_type_word_bytes(text, part) }
	}
	source_type_lock()
	defer { C.atomic_store_u32(&g_source_types_lock, 0) }
	existing := source_type_predicate_find(slot, actual, parameter, kind)
	if !isnil(existing) { return existing.matched }
	mut entry := unsafe { &SourceTypePredicateEntry(node_payload_alloc_zeroed(sizeof(SourceTypePredicateEntry))) }
	if isnil(entry) { panic('could not allocate source type predicate') }
	unsafe {
		*entry = SourceTypePredicateEntry{
			actual:    actual
			parameter: parameter
			kind:      kind
			matched:   matched
			next:      source_type_predicate_head(slot)
		}
	}
	$if x32 {
		C.atomic_store_u32(voidptr(&g_source_type_predicates[slot]), u32(voidptr(entry)))
	} $else {
		C.atomic_store_u64(voidptr(&g_source_type_predicates[slot]), u64(voidptr(entry)))
	}
	return matched
}

fn source_type_word_bytes(text string, word string) bool {
	if word.len == 0 || text.len < word.len { return false }
	for i := 0; i + word.len <= text.len; i++ {
		if text[i..i + word.len] != word { continue }
		if (i == 0 || !source_type_query_ident(text[i - 1]))
			&& (i + word.len == text.len || !source_type_query_ident(text[i + word.len])) {
			return true
		}
	}
	return false
}

@[direct_array_access; inline]
fn source_type_predicate_head(slot int) &SourceTypePredicateEntry {
	$if x32 {
		return unsafe { &SourceTypePredicateEntry(voidptr(C.atomic_load_u32(voidptr(&g_source_type_predicates[slot])))) }
	} $else {
		return unsafe { &SourceTypePredicateEntry(voidptr(C.atomic_load_u64(voidptr(&g_source_type_predicates[slot])))) }
	}
}

fn source_type_predicate_find(slot int, actual u32, parameter u32, kind SourceTypePredicate) &SourceTypePredicateEntry {
	mut entry := source_type_predicate_head(slot)
	for !isnil(entry) {
		if entry.actual == actual && entry.parameter == parameter && entry.kind == kind {
			return entry
		}
		entry = entry.next
	}
	return unsafe { nil }
}

// source_type_syntax returns process-owned numeric spelling components. A cache
// hit addresses immutable components by ID without reading spelling bytes.
@[direct_array_access; inline]
pub fn source_type_syntax(id u32) &SourceTypeSyntax {
	cached := source_type_syntax_find(id)
	if !isnil(cached) {
		return &cached.syntax
	}
	return source_type_syntax_miss(id)
}

@[inline]
fn source_type_syntax_pointer_load(slot voidptr) voidptr {
	$if x32 {
		return unsafe { voidptr(C.atomic_load_u32(slot)) }
	} $else {
		return unsafe { voidptr(C.atomic_load_u64(slot)) }
	}
}

@[inline]
fn source_type_syntax_pointer_store(slot voidptr, value voidptr) {
	$if x32 {
		C.atomic_store_u32(slot, u32(value))
	} $else {
		C.atomic_store_u64(slot, u64(value))
	}
}

@[direct_array_access; inline]
fn source_type_syntax_find(id u32) &SourceTypeSyntaxEntry {
	if id == 0 {
		return unsafe { &SourceTypeSyntaxEntry(source_type_syntax_pointer_load(voidptr(&g_source_type_syntax_empty))) }
	}
	idx := id - 1
	page_index := idx >> source_type_chunk_bits
	if page_index >= source_type_chunk_count {
		// The miss path preserves source_type_name's invalid-ID diagnostic.
		return unsafe { nil }
	}
	page := source_type_syntax_pointer_load(voidptr(&g_source_type_syntax_pages[page_index]))
	if isnil(page) {
		return unsafe { nil }
	}
	slot := unsafe { voidptr(&(&voidptr(page))[idx & (source_type_chunk_size - 1)]) }
	return unsafe { &SourceTypeSyntaxEntry(source_type_syntax_pointer_load(slot)) }
}

@[direct_array_access]
fn source_type_syntax_publish(id u32, entry &SourceTypeSyntaxEntry) {
	// The caller owns the spelling writer lock and has rechecked this ID.
	if id == 0 {
		source_type_syntax_pointer_store(voidptr(&g_source_type_syntax_empty), voidptr(entry))
		return
	}
	idx := id - 1
	page_index := idx >> source_type_chunk_bits
	// Parsing the spelling before publication already checked the ID bounds.
	mut page := source_type_syntax_pointer_load(voidptr(&g_source_type_syntax_pages[page_index]))
	new_page := isnil(page)
	if new_page {
		page = node_payload_alloc_zeroed(source_type_chunk_size * sizeof(voidptr))
		if isnil(page) {
			panic('could not allocate a source type syntax page')
		}
	}
	slot := unsafe { voidptr(&(&voidptr(page))[idx & (source_type_chunk_size - 1)]) }
	source_type_syntax_pointer_store(slot, voidptr(entry))
	if new_page {
		// Publish a new page only after its first complete entry is visible.
		source_type_syntax_pointer_store(voidptr(&g_source_type_syntax_pages[page_index]), page)
	}
}

fn source_type_syntax_miss(id u32) &SourceTypeSyntax {
	// Interning components can acquire the source spelling writer lock itself.
	// Finish every spelling operation before taking that lock for publication.
	parsed := source_type_syntax_parse(id, source_type_name(id))
	source_type_lock()
	defer { C.atomic_store_u32(&g_source_types_lock, 0) }
	cached := source_type_syntax_find(id)
	if !isnil(cached) {
		return &cached.syntax
	}
	// Pack eagerly required immutable ID components into one aligned allocation.
	// Lexical views publish their own storage only when first requested.
	id_count := if parsed.name_packing {
		1 + source_type_syntax_packed_ids_len(parsed.voidptr_parts, id) +
			source_type_syntax_packed_ids_len(parsed.vls_generic_words, id) +
			source_type_syntax_packed_ids_len(parsed.markused_generic_components, id)
	} else {
		1 + source_type_syntax_packed_ids_len(parsed.args, id) +
			source_type_syntax_packed_ids_len(parsed.application_args, id) +
			source_type_syntax_packed_ids_len(parsed.generic_app_args, id) +
			source_type_syntax_packed_ids_len(parsed.postfix_dims, id) +
			source_type_syntax_packed_ids_len(parsed.function_params, id) +
			source_type_syntax_packed_ids_len(parsed.tuple_components, id) +
			source_type_syntax_packed_ids_len(parsed.cgen_app_args, id) +
			source_type_syntax_packed_ids_len(parsed.cgen_function_params, id) +
			source_type_syntax_packed_ids_len(parsed.checker_function_params, id) +
			source_type_syntax_packed_ids_len(parsed.checker_tuple_components, id) +
			source_type_syntax_packed_ids_len(parsed.cgen_raw_app_args, id) +
			source_type_syntax_packed_ids_len(parsed.cgen_parts, id) +
			source_type_syntax_packed_ids_len(parsed.checker_raw_tuple_components, id) +
			source_type_syntax_packed_ids_len(parsed.bracket_raw_parts, id) +
			source_type_syntax_packed_ids_len(parsed.bracket_generic_parts, id) +
			source_type_syntax_packed_ids_len(parsed.tuple_function_components, id) +
			source_type_syntax_packed_ids_len(parsed.plain_comma_parts, id) +
			source_type_syntax_packed_ids_len(parsed.generic_mentions, id) +
			source_type_syntax_packed_ids_len(parsed.receiver_app_args, id) +
			source_type_syntax_packed_ids_len(parsed.checker_bracket_components, id) +
			source_type_syntax_packed_ids_len(parsed.voidptr_parts, id) +
			source_type_syntax_packed_ids_len(parsed.inferred_parts, id) +
			source_type_syntax_packed_ids_len(parsed.inferred_annotations, id) +
			source_type_syntax_packed_ids_len(parsed.inferred_expr_ids, id) +
			source_type_syntax_packed_ids_len(parsed.substitution_function_params, id) +
			source_type_syntax_packed_ids_len(parsed.written_applications, id) +
			source_type_syntax_packed_ids_len(parsed.written_application_offsets, id) +
			source_type_syntax_packed_ids_len(parsed.checker_raw_bracket_components, id) +
			source_type_syntax_packed_ids_len(parsed.cgen_placeholder_candidates, id) +
			source_type_syntax_packed_ids_len(parsed.reflection_parts, id) +
			source_type_syntax_packed_ids_len(parsed.reflection_tuple_parts, id) +
			source_type_syntax_packed_ids_len(parsed.receiver_raw_app_args, id) +
			source_type_syntax_packed_ids_len(parsed.vls_generic_words, id) +
			source_type_syntax_packed_ids_len(parsed.eval_tuple_parts, id) +
			source_type_syntax_packed_ids_len(parsed.implicit_tuple_components, id) +
			source_type_syntax_packed_ids_len(parsed.generic_application_heads, id) +
			source_type_syntax_packed_ids_len(parsed.markused_application_heads, id) +
			source_type_syntax_packed_ids_len(parsed.markused_generic_components, id) +
			source_type_syntax_packed_ids_len(parsed.raw_tuple_parse_components, id) +
			source_type_syntax_packed_ids_len(parsed.union_parts, id)
	}
	payload_bytes := u64(sizeof(SourceTypeSyntaxEntry)) + u64(id_count) * u64(sizeof(u32))
	if payload_bytes > u64(0x7fffffff) {
		panic('source type syntax components exceed the allocation limit')
	}
	payload_size := uint(payload_bytes)
	mut entry := unsafe { &SourceTypeSyntaxEntry(node_payload_alloc_zeroed(payload_size)) }
	if isnil(entry) {
		panic('could not allocate source type syntax')
	}
	mut payload := SourceTypeSyntaxPayload{
		identity_id: id
		id_offset:   1
		ids:         unsafe { voidptr(&u8(entry) + sizeof(SourceTypeSyntaxEntry)) }
	}
	unsafe { *&u32(payload.ids) = id }
	// Records and slices belong to the process, independently of parser workers
	// and preallocated arenas. The release store publishes them only when complete.
	unsafe {
		*entry = SourceTypeSyntaxEntry{
			syntax: SourceTypeSyntax{
				payload:                            payload.ids
				kind:                               parsed.kind
				trimmed:                            parsed.trimmed
				base:                               parsed.base
				unqualified:                        parsed.unqualified
				qualifier:                          parsed.qualifier
				generic_base:                       parsed.generic_base
				pointer_base:                       parsed.pointer_base
				pointer_depth:                      parsed.pointer_depth
				function_has_result:                parsed.function_has_result
				raw_base:                           parsed.raw_base
				main_base:                          parsed.main_base
				bare_generic:                       parsed.bare_generic
				application_base:                   parsed.application_base
				application_valid:                  parsed.application_valid
				generic_app_base:                   parsed.generic_app_base
				generic_app_valid:                  parsed.generic_app_valid
				postfix_base:                       parsed.postfix_base
				parameter_type:                     parsed.parameter_type
				parameter_mut_type:                 parsed.parameter_mut_type
				bracket_closed:                     parsed.bracket_closed
				bracket_base:                       parsed.bracket_base
				bracket_inner:                      parsed.bracket_inner
				bracket_raw_inner:                  parsed.bracket_raw_inner
				bracket_prefix:                     parsed.bracket_prefix
				function_closed:                    parsed.function_closed
				function_result:                    parsed.function_result
				tuple_like:                         parsed.tuple_like
				function_space_prefix:              parsed.function_space_prefix
				spaced:                             parsed.spaced
				first_bracket_closed:               parsed.first_bracket_closed
				first_bracket_base:                 parsed.first_bracket_base
				last_space_word:                    parsed.last_space_word
				named_type:                         parsed.named_type
				c_qualified:                        parsed.c_qualified
				first_dot_prefix:                   parsed.first_dot_prefix
				first_dot_suffix:                   parsed.first_dot_suffix
				builtin_prefix:                     parsed.builtin_prefix
				main_fragment:                      parsed.main_fragment
				function_prefix:                    parsed.function_prefix
				fn_prefix:                          parsed.fn_prefix
				shared_fragment:                    parsed.shared_fragment
				pointer_raw_base:                   parsed.pointer_raw_base
				qualifier_count:                    parsed.qualifier_count
				builtin_base:                       parsed.builtin_base
				cgen_app_base:                      parsed.cgen_app_base
				cgen_app_valid:                     parsed.cgen_app_valid
				tuple_contains_comma:               parsed.tuple_contains_comma
				top_space_head:                     parsed.top_space_head
				top_space_tail:                     parsed.top_space_tail
				top_space_valid:                    parsed.top_space_valid
				typeof_expr_prefix:                 parsed.typeof_expr_prefix
				bracket_suffix:                     parsed.bracket_suffix
				bracket_from:                       parsed.bracket_from
				bracket_inner_fixed_length:         parsed.bracket_inner_fixed_length
				first_bracket_prefix:               parsed.first_bracket_prefix
				void_pointer_postfix:               parsed.void_pointer_postfix
				canonical_dotted_name:              parsed.canonical_dotted_name
				decimal:                            parsed.decimal
				contains_comma:                     parsed.contains_comma
				function_fragment:                  parsed.function_fragment
				dots_base:                          parsed.dots_base
				cgen_raw_app_base:                  parsed.cgen_raw_app_base
				cgen_raw_app_valid:                 parsed.cgen_raw_app_valid
				generic_parameter_payload:          parsed.generic_parameter_payload
				legacy_parameter_type:              parsed.legacy_parameter_type
				legacy_parameter_mut_type:          parsed.legacy_parameter_mut_type
				first_bracket_length:               parsed.first_bracket_length
				c_pointer_base:                     parsed.c_pointer_base
				c_ptr_name_suffix:                  parsed.c_ptr_name_suffix
				c_fn_type_fragment:                 parsed.c_fn_type_fragment
				underscored:                        parsed.underscored
				source_wrapper_base:                parsed.source_wrapper_base
				declaration_head:                   parsed.declaration_head
				fn_fragment:                        parsed.fn_fragment
				compact_fn_spelling:                parsed.compact_fn_spelling
				first_bracket_raw_inner:            parsed.first_bracket_raw_inner
				cgen_generic_placeholder:           parsed.cgen_generic_placeholder
				fixed_array_type:                   parsed.fixed_array_type
				typeof_valid:                       parsed.typeof_valid
				typeof_inner:                       parsed.typeof_inner
				postfix_index_base:                 parsed.postfix_index_base
				last_bracket_prefix:                parsed.last_bracket_prefix
				last_bracket_length:                parsed.last_bracket_length
				trailing_bracket_prefix:            parsed.trailing_bracket_prefix
				trailing_bracket_closed:            parsed.trailing_bracket_closed
				array_chars_base:                   parsed.array_chars_base
				lowercase_value_ident:              parsed.lowercase_value_ident
				typeof_prefix:                      parsed.typeof_prefix
				first_capital:                      parsed.first_capital
				named_variant:                      parsed.named_variant
				named_variant_valid:                parsed.named_variant_valid
				named_variant_owner:                parsed.named_variant_owner
				named_variant_name:                 parsed.named_variant_name
				main_unlocked:                      parsed.main_unlocked
				encoded_fixed_length:               parsed.encoded_fixed_length
				encoded_fixed_base:                 parsed.encoded_fixed_base
				map_name_prefix:                    parsed.map_name_prefix
				suffix_bracket_length:              parsed.suffix_bracket_length
				suffix_bracket_valid:               parsed.suffix_bracket_valid
				map_function_fragment:              parsed.map_function_fragment
				bracket_source_inner:               parsed.bracket_source_inner
				c_fixed_typedef_element:            parsed.c_fixed_typedef_element
				c_fixed_typedef_length:             parsed.c_fixed_typedef_length
				c_fixed_typedef_valid:              parsed.c_fixed_typedef_valid
				fixed_array_query_length:           parsed.fixed_array_query_length
				last_bracket_span_valid:            parsed.last_bracket_span_valid
				last_bracket_span_inner:            parsed.last_bracket_span_inner
				plain_type_name:                    parsed.plain_type_name
				generic_app_legacy_suffix:          parsed.generic_app_legacy_suffix
				wrapper_prefix:                     parsed.wrapper_prefix
				wrapper_base:                       parsed.wrapper_base
				checker_fixed_array_may_be:         parsed.checker_fixed_array_may_be
				dollar_prefix:                      parsed.dollar_prefix
				define_prefix:                      parsed.define_prefix
				define_fragment:                    parsed.define_fragment
				paren_prefix:                       parsed.paren_prefix
				fixed_length:                       parsed.fixed_length
				qualified_modifiers:                parsed.qualified_modifiers
				plain_alias_target:                 parsed.plain_alias_target
				c_array_payload:                    parsed.c_array_payload
				c_fixed_array_payload:              parsed.c_fixed_array_payload
				fastc_array_marker:                 parsed.fastc_array_marker
				fastc_array_marker_payload:         parsed.fastc_array_marker_payload
				c_map_payload:                      parsed.c_map_payload
				c_ptr_name_base:                    parsed.c_ptr_name_base
				c_tagged_type:                      parsed.c_tagged_type
				backward_bracket_prefix:            parsed.backward_bracket_prefix
				backward_bracket_closed:            parsed.backward_bracket_closed
				opening_paren:                      parsed.opening_paren
				byte_len:                           parsed.byte_len
				receiver_app_valid:                 parsed.receiver_app_valid
				receiver_app_base:                  parsed.receiver_app_base
				declaration_parameter_payload:      parsed.declaration_parameter_payload
				declaration_parameter_mut:          parsed.declaration_parameter_mut
				prefix_dimension_tail:              parsed.prefix_dimension_tail
				prefix_dimensions_closed:           parsed.prefix_dimensions_closed
				backward_bracket_after_prefix_dims: parsed.backward_bracket_after_prefix_dims
				backward_bracket_inner:             parsed.backward_bracket_inner
				option_result_void_prefix:          parsed.option_result_void_prefix
				nested_option_result_marker:        parsed.nested_option_result_marker
				c_pointer_depth:                    parsed.c_pointer_depth
				c_pointer_count:                    parsed.c_pointer_count
				c_pointer_raw_base:                 parsed.c_pointer_raw_base
				thread_value_prefix:                parsed.thread_value_prefix
				thread_value_payload:               parsed.thread_value_payload
				thread_value_type_valid:            parsed.thread_value_type_valid
				thread_value_type:                  parsed.thread_value_type
				contains_closing_bracket:           parsed.contains_closing_bracket
				atomic_val_fragment:                parsed.atomic_val_fragment
				expr_prefix:                        parsed.expr_prefix
				expr_id:                            parsed.expr_id
				parameter_name:                     parsed.parameter_name
				parameter_named:                    parsed.parameter_named
				parameter_is_mut:                   parsed.parameter_is_mut
				transform_fixed_length:             parsed.transform_fixed_length
				receiver_collection_id:             parsed.receiver_collection_id
				ampersand_trimmed:                  parsed.ampersand_trimmed
				contains_doubleunderscore:          parsed.contains_doubleunderscore
				parallel_preseed_fn:                parsed.parallel_preseed_fn
				parallel_preseed_short:             parsed.parallel_preseed_short
				closing_bracket:                    parsed.closing_bracket
				constraint_receiver_base:           parsed.constraint_receiver_base
				result_suffix:                      parsed.result_suffix
				cgen_decl_unusable:                 parsed.cgen_decl_unusable
				cgen_decl_authority:                parsed.cgen_decl_authority
				generic_encoded_fragment:           parsed.generic_encoded_fragment
				fn_ptr_encoded_prefix:              parsed.fn_ptr_encoded_prefix
				c_result_prefix:                    parsed.c_result_prefix
				c_option_prefix:                    parsed.c_option_prefix
				c_multi_return_prefix:              parsed.c_multi_return_prefix
				c_lower_map_prefix:                 parsed.c_lower_map_prefix
				open_generic_receiver_marker:       parsed.open_generic_receiver_marker
				space_removed:                      parsed.space_removed
				qualified_name_chars:               parsed.qualified_name_chars
				field_owner_module:                 parsed.field_owner_module
				const_name_prefix:                  parsed.const_name_prefix
				pthread_prefix:                     parsed.pthread_prefix
				thread_bracket_prefix:              parsed.thread_bracket_prefix
				fixed_typedef_bracket:              parsed.fixed_typedef_bracket
				question_trimmed:                   parsed.question_trimmed
				c_array_fixed_prefix:               parsed.c_array_fixed_prefix
				function_space_result:              parsed.function_space_result
				decimal_value:                      parsed.decimal_value
				integer_value:                      parsed.integer_value
				encoded_alnum_dotted:               parsed.encoded_alnum_dotted
				internal_symbol_prefix:             parsed.internal_symbol_prefix
				main_at_base:                       parsed.main_at_base
				incremental_generic_application:    parsed.incremental_generic_application
				system_error_suffix:                parsed.system_error_suffix
				receiver_app_inner:                 parsed.receiver_app_inner
				fastc_fixed_array_length_valid:     parsed.fastc_fixed_array_length_valid
				fastc_fixed_array_length:           parsed.fastc_fixed_array_length
				contains_capital:                   parsed.contains_capital
				first_bracket_source_inner:         parsed.first_bracket_source_inner
				c_function_pointer_hole:            parsed.c_function_pointer_hole
				channel_fragment:                   parsed.channel_fragment
				array_receiver_fragment:            parsed.array_receiver_fragment
				mangled_open_generic_marker:        parsed.mangled_open_generic_marker
				array_suffix:                       parsed.array_suffix
				empty_function_prefix:              parsed.empty_function_prefix
				c_shared_fragment:                  parsed.c_shared_fragment
				typ_suffix:                         parsed.typ_suffix
				typ_suffix_base:                    parsed.typ_suffix_base
				checker_parse_function_result:      parsed.checker_parse_function_result
				checker_parse_function_has_params:  parsed.checker_parse_function_has_params
				top_level_dot_valid:                parsed.top_level_dot_valid
				top_level_dot_prefix:               parsed.top_level_dot_prefix
				top_level_dot_suffix:               parsed.top_level_dot_suffix
				parser_fn_decl_valid:               parsed.parser_fn_decl_valid
				parser_fn_decl_params:              parsed.parser_fn_decl_params
				parser_fn_decl_result:              parsed.parser_fn_decl_result
				contains_question:                  parsed.contains_question
				contains_union:                     parsed.contains_union
				named_variant_dot_valid:            parsed.named_variant_dot_valid
				named_variant_dot_prefix:           parsed.named_variant_dot_prefix
				named_variant_dot_suffix:           parsed.named_variant_dot_suffix
			}
		}
	}
	// The strict name constructor proves that thirty views are empty. Their
	// zeroed span fields need no generic length/copy work; identity views use slot 0.
	if parsed.name_packing {
		identity_span := SourceTypeSyntaxSpan{ len: 1 }
		source_type_syntax_store_span(&entry.syntax.cgen_parts_span, identity_span)
		source_type_syntax_store_span(&entry.syntax.plain_comma_parts_span, identity_span)
		source_type_syntax_store_span(&entry.syntax.inferred_parts_span, identity_span)
		source_type_syntax_store_span(&entry.syntax.reflection_parts_span, identity_span)
		source_type_syntax_store_span(&entry.syntax.union_parts_span, identity_span)
		if parsed.generic_mentions.len != 0 {
			source_type_syntax_store_span(&entry.syntax.generic_mentions_span, identity_span)
		}
		if parsed.cgen_placeholder_candidates.len != 0 {
			source_type_syntax_store_span(&entry.syntax.cgen_placeholder_candidates_span, identity_span)
		}
		source_type_syntax_store_span(&entry.syntax.voidptr_parts_span,
			source_type_syntax_owned_ids(parsed.voidptr_parts, mut payload))
		source_type_syntax_store_span(&entry.syntax.vls_generic_words_span,
			source_type_syntax_owned_ids(parsed.vls_generic_words, mut payload))
		source_type_syntax_store_span(&entry.syntax.markused_generic_components_span,
			source_type_syntax_owned_ids(parsed.markused_generic_components, mut payload))
	} else {
		source_type_syntax_store_span(&entry.syntax.args_span,
			source_type_syntax_owned_ids(parsed.args, mut payload))
		source_type_syntax_store_span(&entry.syntax.application_args_span,
			source_type_syntax_owned_ids(parsed.application_args, mut payload))
		source_type_syntax_store_span(&entry.syntax.generic_app_args_span,
			source_type_syntax_owned_ids(parsed.generic_app_args, mut payload))
		source_type_syntax_store_span(&entry.syntax.postfix_dims_span,
			source_type_syntax_owned_ids(parsed.postfix_dims, mut payload))
		source_type_syntax_store_span(&entry.syntax.function_params_span,
			source_type_syntax_owned_ids(parsed.function_params, mut payload))
		source_type_syntax_store_span(&entry.syntax.tuple_components_span,
			source_type_syntax_owned_ids(parsed.tuple_components, mut payload))
		source_type_syntax_store_span(&entry.syntax.cgen_app_args_span,
			source_type_syntax_owned_ids(parsed.cgen_app_args, mut payload))
		source_type_syntax_store_span(&entry.syntax.cgen_function_params_span,
			source_type_syntax_owned_ids(parsed.cgen_function_params, mut payload))
		source_type_syntax_store_span(&entry.syntax.checker_function_params_span,
			source_type_syntax_owned_ids(parsed.checker_function_params, mut payload))
		source_type_syntax_store_span(&entry.syntax.checker_tuple_components_span,
			source_type_syntax_owned_ids(parsed.checker_tuple_components, mut payload))
		source_type_syntax_store_span(&entry.syntax.cgen_raw_app_args_span,
			source_type_syntax_owned_ids(parsed.cgen_raw_app_args, mut payload))
		source_type_syntax_store_span(&entry.syntax.cgen_parts_span,
			source_type_syntax_owned_ids(parsed.cgen_parts, mut payload))
		source_type_syntax_store_span(&entry.syntax.checker_raw_tuple_components_span,
			source_type_syntax_owned_ids(parsed.checker_raw_tuple_components, mut payload))
		source_type_syntax_store_span(&entry.syntax.bracket_raw_parts_span,
			source_type_syntax_owned_ids(parsed.bracket_raw_parts, mut payload))
		source_type_syntax_store_span(&entry.syntax.bracket_generic_parts_span,
			source_type_syntax_owned_ids(parsed.bracket_generic_parts, mut payload))
		source_type_syntax_store_span(&entry.syntax.tuple_function_components_span,
			source_type_syntax_owned_ids(parsed.tuple_function_components, mut payload))
		source_type_syntax_store_span(&entry.syntax.plain_comma_parts_span,
			source_type_syntax_owned_ids(parsed.plain_comma_parts, mut payload))
		source_type_syntax_store_span(&entry.syntax.generic_mentions_span,
			source_type_syntax_owned_ids(parsed.generic_mentions, mut payload))
		source_type_syntax_store_span(&entry.syntax.receiver_app_args_span,
			source_type_syntax_owned_ids(parsed.receiver_app_args, mut payload))
		source_type_syntax_store_span(&entry.syntax.checker_bracket_components_span,
			source_type_syntax_owned_ids(parsed.checker_bracket_components, mut payload))
		source_type_syntax_store_span(&entry.syntax.voidptr_parts_span,
			source_type_syntax_owned_ids(parsed.voidptr_parts, mut payload))
		source_type_syntax_store_span(&entry.syntax.inferred_parts_span,
			source_type_syntax_owned_ids(parsed.inferred_parts, mut payload))
		source_type_syntax_store_span(&entry.syntax.inferred_annotations_span,
			source_type_syntax_owned_ids(parsed.inferred_annotations, mut payload))
		source_type_syntax_store_span(&entry.syntax.inferred_expr_ids_span,
			source_type_syntax_owned_ids(parsed.inferred_expr_ids, mut payload))
		source_type_syntax_store_span(&entry.syntax.substitution_function_params_span,
			source_type_syntax_owned_ids(parsed.substitution_function_params, mut payload))
		source_type_syntax_store_span(&entry.syntax.written_applications_span,
			source_type_syntax_owned_ids(parsed.written_applications, mut payload))
		source_type_syntax_store_span(&entry.syntax.written_application_offsets_span,
			source_type_syntax_owned_ids(parsed.written_application_offsets, mut payload))
		source_type_syntax_store_span(&entry.syntax.checker_raw_bracket_components_span,
			source_type_syntax_owned_ids(parsed.checker_raw_bracket_components, mut payload))
		source_type_syntax_store_span(&entry.syntax.cgen_placeholder_candidates_span,
			source_type_syntax_owned_ids(parsed.cgen_placeholder_candidates, mut payload))
		source_type_syntax_store_span(&entry.syntax.reflection_parts_span,
			source_type_syntax_owned_ids(parsed.reflection_parts, mut payload))
		source_type_syntax_store_span(&entry.syntax.reflection_tuple_parts_span,
			source_type_syntax_owned_ids(parsed.reflection_tuple_parts, mut payload))
		source_type_syntax_store_span(&entry.syntax.receiver_raw_app_args_span,
			source_type_syntax_owned_ids(parsed.receiver_raw_app_args, mut payload))
		source_type_syntax_store_span(&entry.syntax.vls_generic_words_span,
			source_type_syntax_owned_ids(parsed.vls_generic_words, mut payload))
		source_type_syntax_store_span(&entry.syntax.eval_tuple_parts_span,
			source_type_syntax_owned_ids(parsed.eval_tuple_parts, mut payload))
		source_type_syntax_store_span(&entry.syntax.implicit_tuple_components_span,
			source_type_syntax_owned_ids(parsed.implicit_tuple_components, mut payload))
		source_type_syntax_store_span(&entry.syntax.generic_application_heads_span,
			source_type_syntax_owned_ids(parsed.generic_application_heads, mut payload))
		source_type_syntax_store_span(&entry.syntax.markused_application_heads_span,
			source_type_syntax_owned_ids(parsed.markused_application_heads, mut payload))
		source_type_syntax_store_span(&entry.syntax.markused_generic_components_span,
			source_type_syntax_owned_ids(parsed.markused_generic_components, mut payload))
		source_type_syntax_store_span(&entry.syntax.raw_tuple_parse_components_span,
			source_type_syntax_owned_ids(parsed.raw_tuple_parse_components, mut payload))
		source_type_syntax_store_span(&entry.syntax.union_parts_span,
			source_type_syntax_owned_ids(parsed.union_parts, mut payload))
	}
	source_type_syntax_publish(id, entry)
	return &entry.syntax
}

struct SourceTypeSyntaxPayload {
	identity_id u32
	ids         voidptr
mut:
	id_offset int
}

// Only the owning writer calls this while constructing an unpublished entry.
@[inline]
fn source_type_syntax_store_span(target voidptr, span SourceTypeSyntaxSpan) {
	// These process-owned fields are initialized before the release publication.
	unsafe { *&SourceTypeSyntaxSpan(target) = span }
}

@[inline]
fn source_type_syntax_packed_ids_len(ids []u32, identity_id u32) int {
	return if ids.len == 1 && ids[0] == identity_id { 0 } else { ids.len }
}

fn source_type_syntax_owned_ids(ids []u32, mut payload SourceTypeSyntaxPayload) SourceTypeSyntaxSpan {
	if ids.len == 0 {
		return SourceTypeSyntaxSpan{}
	}
	if ids.len == 1 && ids[0] == payload.identity_id {
		return SourceTypeSyntaxSpan{ len: 1 }
	}
	offset := u32(payload.id_offset) * u32(sizeof(u32))
	data := unsafe { voidptr(&u8(payload.ids) + offset) }
	if ids.len == 1 {
		// Packed ID slots are u32-aligned; copy this value before publication.
		unsafe { *&u32(data) = ids[0] }
		payload.id_offset++
		return SourceTypeSyntaxSpan{ offset: offset, len: 1 }
	}
	payload.id_offset += ids.len
	unsafe { vmemcpy(data, ids.data, ids.len * int(sizeof(u32))) }
	return SourceTypeSyntaxSpan{ offset: offset, len: u32(ids.len) }
}

fn source_type_syntax_mangled_generic_marker(raw string) bool {
	for i := 0; i + 3 < raw.len; i++ {
		if raw[i] == `_` && raw[i + 1] >= `A` && raw[i + 1] <= `Z`
			&& raw[i + 2] == `_` && raw[i + 3] == `_` {
			return true
		}
	}
	return false
}

fn source_type_syntax_symbols(raw string) ([]u32, []u32) {
	mut symbols := source_type_syntax_empty_ids()
	mut offsets := source_type_syntax_empty_ids()
	mut first_offsets := map[u32]u32{}
	mut prior_words := []string{}
	mut prior_offsets := source_type_syntax_empty_ids()
	mut i := 0
	for i < raw.len {
		if !source_type_query_ident(raw[i]) && raw[i] != `.` {
			i++
			continue
		}
		start := i
		for i < raw.len && (source_type_query_ident(raw[i]) || raw[i] == `.`) { i++ }
		word := raw[start..i]
		id := source_type_id(word)
		symbols << id
		if offset := first_offsets[id] {
			offsets << offset
			continue
		}
		mut first := u32(start)
		// The legacy offset is the first substring occurrence, which may lie
		// inside an earlier larger token. Repeated whole tokens reuse this map.
		for index, earlier in prior_words {
			if earlier.len <= word.len { continue }
			if inner := earlier.index(word) {
				candidate := prior_offsets[index] + u32(inner)
				if candidate < first { first = candidate }
			}
		}
		first_offsets[id] = first
		offsets << first
		prior_words << word
		prior_offsets << u32(start)
	}
	return symbols, offsets
}

fn source_type_syntax_union_parts(raw string) []u32 {
	mut parts := source_type_syntax_empty_ids()
	mut depth := 0
	mut start := 0
	for i, c in raw {
		if c in [`[`, `(`, `{`] {
			depth++
		} else if c in [`]`, `)`, `}`] {
			if depth > 0 { depth-- }
		} else if c == `|` && depth == 0 {
			part := source_type_id(source_type_query_trim(raw[start..i]))
			if part != 0 { parts << part }
			start = i + 1
		}
	}
	part := source_type_id(source_type_query_trim(raw[start..]))
	if part != 0 { parts << part }
	return parts
}

fn source_type_syntax_symbol_parts(raw string) ([]u32, []bool) {
	mut parts := source_type_syntax_empty_ids()
	mut names := source_type_syntax_empty_bools()
	mut start := 0
	mut i := 0
	for i < raw.len {
		if raw[i].is_letter() || raw[i] == `_` {
			if i > start {
				parts << source_type_id(raw[start..i])
				names << false
			}
			start = i
			i++
			for i < raw.len && (raw[i].is_alnum() || raw[i] in [`_`, `.`]) { i++ }
			parts << source_type_id(raw[start..i])
			names << true
			start = i
		} else {
			i++
		}
	}
	if start < raw.len {
		parts << source_type_id(raw[start..])
		names << false
	}
	return parts, names
}

fn source_type_syntax_main_unlocked(id u32, raw string) u32 {
	if !raw.contains('main.') { return id }
	mut out := []u8{cap: raw.len}
	mut i := 0
	for i < raw.len {
		if i + 5 <= raw.len && raw[i..].starts_with('main.')
			&& (i == 0 || !(source_type_query_ident(raw[i - 1]) || raw[i - 1] == `.`)) {
			i += 5
		} else {
			out << raw[i]
			i++
		}
	}
	return source_type_id(out.bytestr())
}

fn source_type_syntax_encoded_fixed(raw string) (u32, u32) {
	if !raw.starts_with('_') { return 0, 0 }
	mut end := 1
	for end < raw.len && raw[end].is_digit() { end++ }
	if end <= 1 || end >= raw.len { return 0, 0 }
	return source_type_id(raw[1..end]), source_type_id(raw[end..])
}

fn source_type_syntax_c_tagged_type(id u32, raw string) u32 {
	if raw.starts_with('struct_') { return source_type_id('struct ' + raw['struct_'.len..]) }
	if raw.starts_with('union_') { return source_type_id('union ' + raw['union_'.len..]) }
	if raw.starts_with('enum_') { return source_type_id('enum ' + raw['enum_'.len..]) }
	if raw == 'unsigned_int' { return source_type_id('unsigned int') }
	return id
}

fn source_type_syntax_c_fixed_typedef(raw string) (u32, u32, bool) {
	if !raw.starts_with('Array_fixed_') { return 0, 0, false }
	payload := raw['Array_fixed_'.len..]
	if !payload.contains('_') { return 0, 0, false }
	elem := payload.all_before_last('_')
	length := payload.all_after_last('_')
	if elem.len == 0 || length.len == 0 { return 0, 0, false }
	return source_type_id(elem), source_type_id(length), true
}

fn source_type_syntax_plain_parts(raw string, separator string) []u32 {
	mut parts := source_type_syntax_empty_ids()
	for part in raw.split(separator) { parts << source_type_id(part) }
	return parts
}

fn source_type_syntax_inferred(raw string) ([]u32, []u32, []u32) {
	mut parts := source_type_syntax_empty_ids()
	mut annotations := source_type_syntax_empty_ids()
	mut expressions := source_type_syntax_empty_ids()
	mut offset := 0
	marker := 'typeof(__expr_'
	for offset < raw.len {
		start := raw[offset..].index(marker) or { break } + offset
		end := raw[start..].index(')') or { break } + start + 1
		parts << source_type_id(raw[offset..start])
		annotations << source_type_id(raw[start..end])
		expressions << u32(raw[start + marker.len..end - 1].int())
		offset = end
	}
	parts << source_type_id(raw[offset..])
	return parts, annotations, expressions
}

fn source_type_syntax_receiver_collection(id u32, raw string) u32 {
	if raw.starts_with('[]') || raw.starts_with('map[') { return id }
	if start := raw.index('.[]') { return source_type_id(raw[start + 1..]) }
	if start := raw.index('.map[') { return source_type_id(raw[start + 1..]) }
	return 0
}

fn source_type_syntax_flattened_receiver(id u32, raw string) []u32 {
	mut parts := source_type_syntax_empty_ids()
	mut start := 0
	mut i := 0
	for i < raw.len {
		if raw[i] == `_` {
			if i + 1 < raw.len && raw[i + 1] == `_` {
				i += 2
				continue
			}
			parts << source_type_syntax_child_range_id(id, raw, start, i)
			i++
			start = i
			continue
		}
		i++
	}
	parts << source_type_syntax_child_range_id(id, raw, start, raw.len)
	return parts
}

fn source_type_syntax_parallel_preseed(raw string) (bool, u32) {
	mut start := 0
	for start < raw.len {
		for start < raw.len && raw[start] in [` `, `\t`, `\n`, `\r`] { start++ }
		if raw[start..].starts_with('shared ') {
			start += 7
			continue
		}
		if raw[start..].starts_with('...') {
			start += 3
			continue
		}
		if raw[start..].starts_with('[]') {
			start += 2
			continue
		}
		if start < raw.len && raw[start] in [`&`, `?`, `!`] {
			start++
			continue
		}
		break
	}
	if start >= raw.len { return false, 0 }
	if raw[start..].starts_with('fn') { return true, 0 }
	mut name_start := start
	for i := start; i < raw.len; i++ { if raw[i] == `.` { name_start = i + 1 } }
	if name_start >= raw.len || !raw[name_start].is_capital() { return false, 0 }
	mut end := raw.len
	for end > name_start && raw[end - 1] in [` `, `\t`, `\n`, `\r`] { end-- }
	return false, source_type_id(raw[name_start..end])
}

fn source_type_syntax_vls_generic_words(raw string) []u32 {
	mut words := source_type_syntax_empty_ids()
	mut i := 0
	for i < raw.len {
		if !raw[i].is_letter() && raw[i] != `_` {
			i++
			continue
		}
		start := i
		i++
		for i < raw.len && (raw[i].is_alnum() || raw[i] == `_`) { i++ }
		if i == start + 1 && raw[start] >= `A` && raw[start] <= `Z` && (start == 0 || raw[start - 1] != `.`) {
			words << source_type_id(raw[start..i])
		}
	}
	return words
}

fn source_type_syntax_contains_capital(raw string) bool {
	for c in raw { if c >= `A` && c <= `Z` { return true } }
	return false
}

fn source_type_syntax_markused_components(raw string) []u32 {
	mut parts := source_type_syntax_empty_ids()
	mut start := 0
	for start < raw.len {
		for start < raw.len && !(raw[start].is_letter() || raw[start] == `_`) { start++ }
		if start >= raw.len { break }
		mut end := start + 1
		mut capital := raw[start] >= `A` && raw[start] <= `Z`
		for end < raw.len && (raw[end].is_alnum() || raw[end] in [`_`, `.`, `@`]) {
			if raw[end] >= `A` && raw[end] <= `Z` { capital = true }
			end++
		}
		if capital { parts << source_type_id(raw[start..end]) }
		start = end
	}
	return parts
}

fn source_type_syntax_eval_parts(raw string) []u32 {
	mut parts := source_type_syntax_empty_ids()
	mut brackets := 0
	mut parens := 0
	mut start := 0
	for i, c in raw {
		match c {
			`[` { brackets++ }
			`]` {
				if brackets > 0 { brackets-- }
			}
			`(` { parens++ }
			`)` {
				if parens > 0 { parens-- }
			}
			`,` {
				if brackets == 0 && parens == 0 {
					parts << source_type_id(source_type_query_trim(raw[start..i]))
					start = i + 1
				}
			}
			else {}
		}
	}
	last := source_type_query_trim(raw[start..])
	if last.len > 0 { parts << source_type_id(last) }
	return parts
}

fn source_type_syntax_implicit_tuple(raw string) []u32 {
	if raw.len < 2 || raw[0] != `(` || raw[raw.len - 1] != `)` {
		return source_type_syntax_empty_ids()
	}
	mut parts := source_type_syntax_empty_ids()
	mut depth := 0
	mut start := 1
	for i := 1; i < raw.len - 1; i++ {
		match raw[i] {
			`(`, `[` { depth++ }
			`)`, `]` { depth-- }
			`,` {
				if depth == 0 {
					parts << source_type_id(source_type_query_trim(raw[start..i]))
					start = i + 1
				}
			}
			else {}
		}
	}
	parts << source_type_id(source_type_query_trim(raw[start..raw.len - 1]))
	if depth != 0 || parts.any(it == 0) { return source_type_syntax_empty_ids() }
	return parts
}

fn source_type_syntax_application_heads(raw string, allow_at bool) []u32 {
	mut heads := source_type_syntax_empty_ids()
	for i, c in raw {
		if c != `[` { continue }
		mut start := i
		for start > 0 && (raw[start - 1].is_alnum() || raw[start - 1] in [`_`, `.`]
			|| (allow_at && raw[start - 1] == `@`)) {
			start--
		}
		if start < i { heads << source_type_id(raw[start..i]) }
	}
	return heads
}

fn source_type_syntax_incremental_generic_application(raw string) bool {
	for i := 1; i < raw.len; i++ {
		if raw[i] != `[` || !source_type_query_ident(raw[i - 1]) { continue }
		mut start := i - 1
		for start > 0 && source_type_query_ident(raw[start - 1]) { start-- }
		if !(i - start == 3 && raw[start] == `m` && raw[start + 1] == `a`
			&& raw[start + 2] == `p`) {
			return true
		}
	}
	return false
}

fn source_type_syntax_thread_value(raw string) (bool, u32) {
	if raw == '__vf_thread_void' { return true, 0 }
	prefix := '__vf_thread_k'
	if !raw.starts_with(prefix) { return false, 0 }
	mut decoded := []u8{cap: raw.len - prefix.len}
	mut i := prefix.len
	for i < raw.len {
		if raw[i] != `_` {
			decoded << raw[i]
			i++
			continue
		}
		if i + 2 >= raw.len { return false, 0 }
		high := source_type_syntax_lower_hex(raw[i + 1])
		low := source_type_syntax_lower_hex(raw[i + 2])
		if high < 0 || low < 0 { return false, 0 }
		decoded << u8((high << 4) | low)
		i += 3
	}
	return true, source_type_id(decoded.bytestr())
}

fn source_type_syntax_lower_hex(value u8) int {
	if value >= `0` && value <= `9` { return int(value - `0`) }
	if value >= `a` && value <= `f` { return int(value - `a` + 10) }
	return -1
}

fn source_type_syntax_fastc_fixed_length(raw string) (bool, u32) {
	if !raw.starts_with('FixedArray_') || !raw.contains('_FASTC_ARRAY_OF_') { return false, 0 }
	part := raw['FixedArray_'.len..].all_before('_FASTC_ARRAY_OF_')
	if part.starts_with('EXPRHEX') {
		decoded := hex.decode(part['EXPRHEX'.len..]) or { return false, 0 }
		return true, source_type_id(decoded.bytestr())
	}
	return true, source_type_id(part)
}

fn source_type_syntax_reflection_parts(raw string) []u32 {
	mut parts := source_type_syntax_empty_ids()
	mut brackets := 0
	mut parens := 0
	mut start := 0
	for i, c in raw {
		match c {
			`[` { brackets++ }
			`]` { brackets-- }
			`(` { parens++ }
			`)` { parens-- }
			`,` {
				if brackets == 0 && parens == 0 {
					parts << source_type_id(source_type_query_trim(raw[start..i]))
					start = i + 1
				}
			}
			else {}
		}
	}
	if start < raw.len { parts << source_type_id(source_type_query_trim(raw[start..])) }
	return parts
}

fn source_type_syntax_written_applications(raw string) ([]u32, []u32) {
	mut applications := source_type_syntax_empty_ids()
	mut offsets := source_type_syntax_empty_ids()
	mut i := 0
	for i < raw.len {
		c := raw[i]
		if !(c.is_letter() || c == `_`) || (i > 0 && (raw[i - 1].is_alnum() || raw[i - 1] in [
			`_`,
			`.`,
		])) {
			i++
			continue
		}
		mut end := i
		for end < raw.len && (raw[end].is_alnum() || raw[end] in [`_`, `.`]) { end++ }
		if end >= raw.len || raw[end] != `[` || raw[i..end] == 'map' {
			i = end
			continue
		}
		close := source_type_syntax_matching_bracket(raw, end)
		if close >= raw.len { break }
		applications << source_type_id(raw[i..close + 1])
		offsets << u32(i)
		i = end
	}
	return applications, offsets
}

fn source_type_syntax_cgen_placeholder_candidates(raw string) []u32 {
	mut candidates := source_type_syntax_empty_ids()
	source_type_syntax_cgen_collect_placeholders(raw, mut candidates)
	return candidates
}

fn source_type_syntax_cgen_collect_placeholders(raw string, mut candidates []u32) {
	clean := source_type_query_trim(raw)
	if clean.len == 0 { return }
	if !clean.contains('[') {
		if clean.len == 1 && clean[0] >= `A` && clean[0] <= `Z` {
			candidates << source_type_id(clean)
		}
		return
	}
	mut depth := 0
	mut start := 0
	for i, c in clean {
		if c == `[` {
			if depth == 0 { start = i + 1 }
			depth++
		} else if c == `]` {
			depth--
			if depth == 0 { source_type_syntax_cgen_collect_args(clean[start..i], mut candidates) }
		}
	}
}

fn source_type_syntax_cgen_collect_args(raw string, mut candidates []u32) {
	mut depth := 0
	mut start := 0
	for i, c in raw {
		if c in [`[`, `(`] {
			depth++
		} else if c in [`]`, `)`] {
			if depth > 0 { depth-- }
		} else if c == `,` && depth == 0 {
			source_type_syntax_cgen_collect_placeholders(raw[start..i], mut candidates)
			start = i + 1
		}
	}
	source_type_syntax_cgen_collect_placeholders(raw[start..], mut candidates)
}

fn source_type_syntax_cgen_declaration(raw string) (bool, bool) {
	clean := if raw.contains(' ') { raw.replace(' ', '') } else { raw }
	authority := clean in ['Option', '__v_option', '__v_result'] || clean.starts_with('Option_')
		|| clean.starts_with('__v_option_') || clean.starts_with('__v_result_')
	unusable := authority || clean == 'void' || clean.starts_with('void[')
		|| clean.ends_with(']void') || clean.contains(']void[')
	return unusable, authority
}

fn source_type_syntax_open_generic_receiver_marker(raw string) bool {
	mut has_bracket := false
	mut bracket_marker := false
	for i, c in raw {
		if c == `[` { has_bracket = true }
		if !source_type_syntax_receiver_marker_byte(c) { continue }
		if (i == 0 || raw[i - 1] == `_`)
			&& (i + 1 == raw.len || raw[i + 1] == `_`) {
			return true
		}
		if i + 1 >= raw.len { continue }
		if i > 0 && raw[i - 1] == `[` && raw[i + 1] in [`]`, `,`] {
			bracket_marker = true
		} else if raw[i + 1] == `]` && ((i > 0 && raw[i - 1] == `,`)
			|| (i > 1 && raw[i - 1] == ` ` && raw[i - 2] == `,`)) {
			bracket_marker = true
		}
	}
	// The legacy comma patterns require an opening bracket somewhere in the raw spelling.
	return has_bracket && bracket_marker
}

@[inline]
fn source_type_syntax_receiver_marker_byte(c u8) bool {
	return match c {
		`T`, `U`, `K`, `V`, `A`, `B`, `C`, `X`, `Y`, `Z` { true }
		else { false }
	}
}

fn source_type_syntax_encoded_alnum_dotted(raw string) bool {
	for c in raw { if !c.is_alnum() && c != `.` { return false } }
	return true
}

// Reuse the parent spelling when a cold query leaves its original slice intact.
// Pointer identity is sufficient here; other slices still use exact interning.
@[inline]
fn source_type_syntax_child_id(parent u32, raw string, part string) u32 {
	if raw.str == part.str && raw.len == part.len { return parent }
	return source_type_id(part)
}

// Every caller establishes these bounds before extracting an exact raw view.
// Interning copies a new spelling before this temporary view can escape.
@[inline]
fn source_type_syntax_child_range_id(parent u32, raw string, start int, end int) u32 {
	if start == 0 && end == raw.len { return parent }
	if start == end { return 0 }
	return source_type_id(unsafe { raw.substr_unsafe(start, end) })
}

// Compute exact raw edge views once; intermediate qualifiers remain lazy.
fn source_type_syntax_dot_edges(id u32, raw string, first_dot int, last_dot int) (u32, u32, u32, u32) {
	if first_dot < 0 { return id, id, id, id }
	first_prefix := source_type_syntax_child_range_id(id, raw, 0, first_dot)
	first_suffix := source_type_syntax_child_range_id(id, raw, first_dot + 1, raw.len)
	if first_dot == last_dot { return first_prefix, first_suffix, first_prefix, first_suffix }
	return first_prefix, first_suffix, source_type_syntax_child_range_id(id, raw, 0, last_dot), source_type_syntax_child_range_id(id, raw, last_dot + 1, raw.len)
}

fn source_type_syntax_parse(id u32, raw string) SourceTypeSyntaxParsed {
	mut simple := raw.len > 0
	mut qualified := raw.len > 0
	mut segment_start := true
	mut first_dot := -1
	mut last_dot := -1
	for i, c in raw {
		if source_type_query_ident(c) {
			if segment_start && !c.is_letter() && c != `_` { qualified = false }
			segment_start = false
		} else if c == `.` {
			simple = false
			if segment_start { qualified = false }
			if first_dot < 0 { first_dot = i }
			last_dot = i
			segment_start = true
		} else {
			simple = false
			qualified = false
			break
		}
	}
	if simple { return source_type_syntax_name(id, raw, -1, -1) }
	if qualified && !segment_start && first_dot >= 0 {
		return source_type_syntax_name(id, raw, first_dot, last_dot)
	}
	clean := source_type_query_trim(raw)
	clean_id := source_type_syntax_child_id(id, raw, clean)
	// No-split views borrow the immutable spelling entry until payload publication.
	identity_ids := source_type_syntax_scalar_ids(id)
	has_comma := raw.contains(',')
	contains_union := raw.contains('|')
	unpadded_nonempty := raw.len > 0 && clean_id == id
	spaced := raw.contains(' ')
	main_fragment := raw.contains('main.')
	compact_fn_fragment := raw.contains('fn (')
	contains_doubleunderscore := raw.contains('__')
	cgen_decl_unusable, cgen_decl_authority := source_type_syntax_cgen_declaration(raw)
	last_star := raw.last_index_u8(`*`)
	named_variant := raw.contains(named_variant_marker)
	mut named_variant_valid := false
	mut named_variant_owner := u32(0)
	mut named_variant_name := u32(0)
	if named_variant {
		if owner, name := decode_named_variant_type_name(raw) {
			named_variant_valid = true
			named_variant_owner = source_type_syntax_child_id(id, raw, owner)
			named_variant_name = source_type_syntax_child_id(id, raw, name)
		}
	}
	thread_value_type_valid, thread_value_type := source_type_syntax_thread_value(raw)
	kind, base, args, has_result := source_type_syntax_components(clean)
	written_applications, written_application_offsets := source_type_syntax_written_applications(raw)
	mut pointer_text := clean
	mut pointer_depth := 0
	for pointer_text.starts_with('&') {
		pointer_depth++
		pointer_text = source_type_query_trim(pointer_text[1..])
	}
	application_base, application_args, application_valid := source_type_syntax_application(raw, false)
	generic_app_base, generic_app_args, generic_app_valid := source_type_syntax_application(raw, true)
	postfix_base, postfix_dims := source_type_syntax_postfix(clean)
	legacy_parameter_type, parameter_name, parameter_named, parameter_is_mut, parameter_text := source_type_syntax_parameter_details(raw)
	parameter_type, parameter_mut_type := source_type_syntax_parameter(parameter_text, legacy_parameter_type, parameter_is_mut)
	legacy_parameter_mut_type := if parameter_is_mut {
		source_type_composition_one(.mutable_parameter, legacy_parameter_type)
	} else {
		legacy_parameter_type
	}
	declaration_parameter_payload, declaration_parameter_mut := source_type_syntax_declaration_parameter(legacy_parameter_type, parameter_is_mut)
	raw_prefix := source_type_syntax_wrapper_len(raw, true)
	bracket := raw.index_u8(`[`)
	bracket_end := if bracket >= 0 {
		source_type_syntax_matching_bracket(raw, bracket)
	} else {
		raw.len
	}
	bracket_closed := bracket >= 0 && bracket_end < raw.len
	function_closed, function_params, function_result, checker_function_params, cgen_function_params, substitution_function_params, function_space_result := source_type_syntax_function(raw)
	checker_parse_function_result, checker_parse_function_has_params := source_type_syntax_checker_function_result(raw)
	cgen_app_base, cgen_app_args, cgen_app_valid := source_type_syntax_cgen_application_raw(clean)
	mut cgen_raw_app_base := cgen_app_base
	mut cgen_raw_app_args := cgen_app_args
	mut cgen_raw_app_valid := cgen_app_valid
	if clean != raw {
		cgen_raw_app_base, cgen_raw_app_args, cgen_raw_app_valid = source_type_syntax_cgen_application_raw(raw)
	}
	top_space_head, top_space_tail, top_space_valid := source_type_syntax_top_space(raw)
	tuple_like := raw.starts_with('(') && raw.ends_with(')')
	first_close := raw.index_u8(`]`)
	mut top_level_dot := -1
	mut top_dot_depth := 0
	for i, c in raw {
		if c in [`[`, `(`] {
			top_dot_depth++
		} else if c in [`]`, `)`] {
			if top_dot_depth > 0 { top_dot_depth-- }
		} else if c == `.` && top_dot_depth == 0 {
			top_level_dot = i
			break
		}
	}
	mut named_variant_dot := -1
	mut variant_depth := 0
	for i, c in raw {
		if c == `[` {
			variant_depth++
		} else if c == `]` {
			variant_depth--
		} else if c == `.` && variant_depth == 0 {
			named_variant_dot = i
		}
	}
	parser_fn_close := raw.index_u8(`)`)
	parser_fn_decl_valid := raw.starts_with('fn(') && parser_fn_close > 3
	mut named_type := raw.len > 0
	integer_value := raw.int()
	mut decimal := raw.len > 0
	for c in raw {
		if c < `0` || c > `9` {
			decimal = false
			break
		}
	}
	mut dot_count := 0
	for dot_count < raw.len && raw[dot_count] == `.` { dot_count++ }
	mut raw_pointer_depth := 0
	for raw_pointer_depth < raw.len && raw[raw_pointer_depth] == `&` { raw_pointer_depth++ }
	mut qualifier_count := u32(0)
	first_dot = -1
	last_dot = -1
	for i, c in raw {
		if c == `.` {
			if first_dot < 0 { first_dot = i }
			last_dot = i
			qualifier_count++
		}
	}
	first_dot_prefix, first_dot_suffix, qualifier, unqualified := source_type_syntax_dot_edges(id, raw, first_dot, last_dot)
	mut nested_option_result_marker := -1
	for i := 0; i + 1 < raw.len; i++ {
		if raw[i] in [`?`, `!`] && raw[i + 1] in [`?`, `!`] {
			nested_option_result_marker = i
			break
		}
	}
	mut c_pointer_end := raw.len
	for c_pointer_end > 0 && raw[c_pointer_end - 1] == `*` { c_pointer_end-- }
	mut voidptr_parts := identity_ids
	if raw.len == 0 || raw.contains('voidptr') {
		voidptr_parts = source_type_syntax_empty_ids()
		for part in raw.split('voidptr') {
			voidptr_parts << source_type_syntax_child_id(id, raw, part)
		}
	}
	mut inferred_parts := identity_ids
	mut inferred_annotations := source_type_syntax_empty_ids()
	mut inferred_expr_ids := source_type_syntax_empty_ids()
	if raw.len == 0 || raw.contains('typeof(__expr_') {
		inferred_parts, inferred_annotations, inferred_expr_ids = source_type_syntax_inferred(raw)
	}
	fastc_fixed_array_length_valid, fastc_fixed_array_length := source_type_syntax_fastc_fixed_length(raw)
	parallel_preseed_fn, parallel_preseed_short := source_type_syntax_parallel_preseed(raw)
	mut plain_comma_parts := identity_ids
	if raw.len == 0 || has_comma {
		plain_comma_parts = source_type_syntax_empty_ids()
		for part in raw.split(',') {
			plain_comma_parts << source_type_syntax_child_id(id, raw, part)
		}
	}
	mut source_wrapper_len := 0
	for source_wrapper_len < raw.len && raw[source_wrapper_len] in [`&`, `?`, `!`] {
		source_wrapper_len++
	}
	mut cgen_generic_placeholder := clean.len > 0 && !clean.contains('.') && !clean.contains('[')
		&& !clean.contains(']') && !clean.contains(' ')
	if cgen_generic_placeholder {
		cgen_generic_placeholder = clean[0] >= `A` && clean[0] <= `Z`
			&& (clean.len == 1 || (clean[1] >= `0` && clean[1] <= `9`))
	}
	mut fixed_array_type := false
	if !raw.starts_with('[]') && !raw.starts_with('[?') {
		if raw.starts_with('[') {
			fixed_array_type = first_close > 1
		} else if bracket >= 0 && raw.ends_with(']') {
			length := if postfix_dims.len > 0 {
				source_type_name(postfix_dims[0])
			} else {
				source_type_query_trim(raw.all_after('[').all_before(']'))
			}
			fixed_array_type = source_type_syntax_decimal(length)
		}
	}
	for c in raw {
		if !source_type_query_ident(c) && c != `.` {
			named_type = false
			break
		}
	}
	last_bracket := raw.last_index_u8(`[`)
	last_right_bracket := raw.last_index_u8(`]`)
	last_bracket_close := if last_bracket >= 0 { raw[last_bracket + 1..].index_u8(`]`) } else { -1 }
	mut trailing_bracket := -1
	if raw.ends_with(']') {
		mut depth := 0
		for i := raw.len - 1; i >= 0; i-- {
			if raw[i] == `]` {
				depth++
			} else if raw[i] == `[` {
				depth--
				if depth == 0 {
					trailing_bracket = i
					break
				}
			}
		}
	}
	mut array_chars := 0
	for array_chars < raw.len && raw[array_chars] in [`[`, `]`] { array_chars++ }
	typeof_valid := clean.starts_with('typeof(') && clean.ends_with(')')
	mut lowercase_value_ident := raw.len > 0 && raw[0] >= `a` && raw[0] <= `z`
	if lowercase_value_ident {
		for c in raw {
			if !source_type_query_ident(c) {
				lowercase_value_ident = false
				break
			}
		}
	}
	mut raw_base := u32(0)
	if raw_prefix > 0 {
		raw_base = source_type_syntax_child_range_id(id, raw, raw_prefix, raw.len)
	} else if bracket_closed && (raw.starts_with('map[') || raw.starts_with('[')) {
		raw_base = source_type_syntax_child_range_id(id, raw, bracket_end + 1, raw.len)
	}
	mut backward_bracket := -1
	mut backward_depth := 0
	for i := raw.len - 1; i >= 0; i-- {
		if raw[i] == `]` {
			backward_depth++
		} else if raw[i] == `[` {
			backward_depth--
			if backward_depth == 0 {
				backward_bracket = i
				break
			}
		}
	}
	mut prefix_dimension_end := 0
	mut prefix_dimensions_closed := true
	for prefix_dimension_end < raw.len && raw[prefix_dimension_end] == `[` {
		close := raw[prefix_dimension_end + 1..].index_u8(`]`)
		if close < 0 {
			prefix_dimensions_closed = false
			break
		}
		prefix_dimension_end += close + 2
	}
	mut generic_mentions := source_type_syntax_empty_ids()
	for i, c in raw {
		if c >= `A` && c <= `Z` && (i == 0 || !(source_type_query_ident(raw[i - 1]) || raw[i - 1] == `.`))
			&& (i + 1 == raw.len || !(source_type_query_ident(raw[i + 1]) || raw[i + 1] == `.`)) {
			generic_mentions << source_type_syntax_child_range_id(id, raw, i, i + 1)
		}
	}
	mut modifier_start := raw.len
	for modifier_start > 0 {
		c := raw[modifier_start - 1]
		if !(c.is_letter() || c.is_digit() || c in [`_`, `.`]) { break }
		modifier_start--
	}
	mut plain_alias_target := true
	for c in raw {
		if c in [`[`, `]`, `?`, `&`, `!`, ` `] {
			plain_alias_target = false
			break
		}
	}
	wrapper_len := source_type_syntax_wrapper_len(raw, false)
	mut checker_fixed_array_may_be := false
	if clean.len > 0 && !clean.starts_with('[]') && !clean.starts_with('map[') {
		if clean.starts_with('[') {
			end := source_type_syntax_matching_bracket(clean, 0)
			checker_fixed_array_may_be = end > 0 && end + 1 < clean.len
		} else if clean.ends_with(']') {
			left := clean.last_index_u8(`[`)
			right := clean.last_index_u8(`]`)
			checker_fixed_array_may_be = left > 0 && right > left && source_type_syntax_checker_length(clean[left + 1..right])
		}
	}
	encoded_fixed_length, encoded_fixed_base := source_type_syntax_encoded_fixed(raw)
	c_fixed_typedef_element, c_fixed_typedef_length, c_fixed_typedef_valid := source_type_syntax_c_fixed_typedef(raw)
	suffix_bracket_valid := raw.ends_with(']') && last_bracket > 0 && last_bracket + 1 < raw.len - 1
	plain_type_name := raw.len > 0 && ((raw[0] >= `a` && raw[0] <= `z`) || (raw[0] >= `A` && raw[0] <= `Z`) || raw[0] == `_`)
		&& source_type_syntax_plain_name(raw)
	return SourceTypeSyntaxParsed{
		kind:                               kind
		trimmed:                            clean_id
		base:                               base
		args:                               args
		unqualified:                        unqualified
		qualifier:                          qualifier
		generic_base:                       if bracket < 0 {
			id
		} else {
			source_type_syntax_child_range_id(id, raw, 0, bracket)
		}
		pointer_base:                       if pointer_depth == 0 {
			clean_id
		} else {
			source_type_syntax_child_id(id, raw, pointer_text)
		}
		pointer_depth:                      pointer_depth
		function_has_result:                has_result
		raw_base:                           raw_base
		main_base:                          if raw.starts_with('main.') {
			source_type_syntax_child_range_id(id, raw, 5, raw.len)
		} else {
			u32(0)
		}
		bare_generic:                       clean.len == 1 && clean[0] >= `A` && clean[0] <= `Z`
		application_base:                   application_base
		application_args:                   application_args
		application_valid:                  application_valid
		generic_app_base:                   generic_app_base
		generic_app_args:                   generic_app_args
		generic_app_valid:                  generic_app_valid
		postfix_base:                       postfix_base
		postfix_dims:                       postfix_dims
		parameter_type:                     parameter_type
		parameter_mut_type:                 parameter_mut_type
		bracket_closed:                     bracket_closed
		bracket_base:                       if bracket_closed {
			source_type_syntax_child_id(id, raw, source_type_query_trim(raw[bracket_end + 1..]))
		} else {
			u32(0)
		}
		bracket_inner:                      if bracket_closed {
			source_type_syntax_child_id(id, raw, source_type_query_trim(raw[bracket + 1..bracket_end]))
		} else {
			u32(0)
		}
		bracket_raw_inner:                  if bracket_closed {
			source_type_syntax_child_range_id(id, raw, bracket + 1, bracket_end)
		} else {
			u32(0)
		}
		bracket_prefix:                     if bracket_closed {
			source_type_syntax_child_range_id(id, raw, 0, bracket_end + 1)
		} else {
			u32(0)
		}
		function_closed:                    function_closed
		function_params:                    function_params
		function_result:                    function_result
		tuple_components:                   if tuple_like {
			source_type_syntax_legacy_parts(raw[1..raw.len - 1])
		} else {
			source_type_syntax_empty_ids()
		}
		tuple_like:                         tuple_like
		function_space_prefix:              raw.starts_with('fn ')
		spaced:                             spaced
		first_bracket_closed:               first_close > 0
		first_bracket_base:                 if first_close > 0 {
			source_type_syntax_child_range_id(id, raw, first_close + 1, raw.len)
		} else {
			u32(0)
		}
		last_space_word:                    if !spaced {
			id
		} else {
			source_type_syntax_child_id(id, raw, raw.all_after_last(' '))
		}
		named_type:                         named_type
		c_qualified:                        raw.starts_with('C.')
		first_dot_prefix:                   first_dot_prefix
		first_dot_suffix:                   first_dot_suffix
		builtin_prefix:                     raw.starts_with('builtin.')
		main_fragment:                      main_fragment
		function_prefix:                    raw.starts_with('fn')
		fn_prefix:                          raw.starts_with('fn')
		shared_fragment:                    raw.contains('shared ')
		pointer_raw_base:                   source_type_syntax_child_range_id(id, raw, raw_pointer_depth, raw.len)
		qualifier_count:                    qualifier_count
		builtin_base:                       if raw.starts_with('builtin.') {
			source_type_syntax_child_range_id(id, raw, 8, raw.len)
		} else {
			u32(0)
		}
		cgen_app_base:                      cgen_app_base
		cgen_app_args:                      cgen_app_args
		cgen_app_valid:                     cgen_app_valid
		cgen_function_params:               cgen_function_params
		checker_function_params:            checker_function_params
		checker_tuple_components:           if tuple_like {
			source_type_syntax_checker_parts(raw[1..raw.len - 1])
		} else {
			source_type_syntax_empty_ids()
		}
		tuple_contains_comma:               raw.contains(',')
		top_space_head:                     top_space_head
		top_space_tail:                     top_space_tail
		top_space_valid:                    top_space_valid
		typeof_expr_prefix:                 raw.starts_with('typeof(__expr_')
		bracket_suffix:                     if bracket_closed {
			source_type_syntax_child_range_id(id, raw, bracket_end + 1, raw.len)
		} else {
			u32(0)
		}
		bracket_from:                       if bracket >= 0 {
			source_type_syntax_child_range_id(id, raw, bracket, raw.len)
		} else {
			u32(0)
		}
		bracket_inner_fixed_length:         if bracket_closed {
			source_type_syntax_checker_length(raw[bracket + 1..bracket_end])
		} else {
			false
		}
		first_bracket_prefix:               if first_close > 0 {
			source_type_syntax_child_range_id(id, raw, 0, first_close + 1)
		} else {
			u32(0)
		}
		void_pointer_postfix:               raw.starts_with('&void[') && raw.ends_with(']')
		canonical_dotted_name:              named_type && raw.contains('.') && ((raw[0] >= `a` && raw[0] <= `z`) || (raw[0] >= `A` && raw[0] <= `Z`) || raw[0] == `_`)
		decimal:                            decimal
		contains_comma:                     raw.contains(',')
		function_fragment:                  raw.contains('fn(') || compact_fn_fragment
		dots_base:                          source_type_syntax_child_range_id(id, raw, dot_count, raw.len)
		cgen_raw_app_base:                  cgen_raw_app_base
		cgen_raw_app_args:                  cgen_raw_app_args
		cgen_raw_app_valid:                 cgen_raw_app_valid
		cgen_parts:                         if unpadded_nonempty && !has_comma {
			identity_ids
		} else {
			source_type_syntax_legacy_parts(raw)
		}
		generic_parameter_payload:          legacy_parameter_type
		legacy_parameter_type:              legacy_parameter_type
		legacy_parameter_mut_type:          legacy_parameter_mut_type
		first_bracket_length:               if bracket < 0 && first_close < 0 {
			clean_id
		} else {
			source_type_syntax_child_id(id, raw, source_type_query_trim(raw.all_after('[').all_before(']')))
		}
		checker_raw_tuple_components:       if tuple_like {
			source_type_syntax_checker_parts_mode(raw[1..raw.len - 1], false)
		} else {
			source_type_syntax_empty_ids()
		}
		bracket_raw_parts:                  if bracket >= 0 {
			source_type_syntax_checker_parts_mode(source_type_query_trim(raw[bracket + 1..bracket_end]), false)
		} else {
			source_type_syntax_empty_ids()
		}
		bracket_generic_parts:              if bracket >= 0 {
			source_type_syntax_legacy_parts(raw[bracket + 1..bracket_end])
		} else {
			source_type_syntax_empty_ids()
		}
		tuple_function_components:          source_type_syntax_tuple_function(raw)
		c_pointer_base:                     if last_star < 0 {
			id
		} else {
			source_type_syntax_child_range_id(id, raw, 0, last_star)
		}
		c_ptr_name_suffix:                  raw.ends_with('_ptr')
		c_fn_type_fragment:                 raw.contains('__vf_fn_type_')
		underscored:                        raw.contains('_')
		plain_comma_parts:                  plain_comma_parts
		source_wrapper_base:                source_type_syntax_child_range_id(id, raw, source_wrapper_len, raw.len)
		declaration_head:                   if !clean.contains(' ') && !clean.contains('\t') && !clean.contains('(') {
			clean_id
		} else {
			source_type_syntax_child_id(id, raw, clean.all_before(' ').all_before('\t').all_before('('))
		}
		fn_fragment:                        raw.contains('fn')
		compact_fn_spelling:                if !compact_fn_fragment {
			id
		} else {
			source_type_syntax_child_id(id, raw, raw.replace('fn (', 'fn('))
		}
		first_bracket_raw_inner:            if bracket >= 0 && first_close > bracket {
			source_type_syntax_child_range_id(id, raw, bracket + 1, first_close)
		} else {
			u32(0)
		}
		cgen_generic_placeholder:           cgen_generic_placeholder
		fixed_array_type:                   fixed_array_type
		typeof_valid:                       typeof_valid
		typeof_inner:                       if typeof_valid {
			source_type_syntax_child_id(id, raw, source_type_query_trim(clean[7..clean.len - 1]))
		} else {
			u32(0)
		}
		postfix_index_base:                 if raw.ends_with(']') && last_bracket > 0 {
			source_type_syntax_child_id(id, raw, source_type_query_trim(raw[..last_bracket]))
		} else {
			u32(0)
		}
		last_bracket_prefix:                if last_bracket >= 0 {
			source_type_syntax_child_range_id(id, raw, 0, last_bracket)
		} else {
			u32(0)
		}
		last_bracket_length:                if last_bracket_close >= 0 {
			source_type_syntax_child_id(id, raw, source_type_query_trim(raw[last_bracket + 1..last_bracket + 1 + last_bracket_close]))
		} else {
			u32(0)
		}
		trailing_bracket_prefix:            if trailing_bracket >= 0 {
			source_type_syntax_child_range_id(id, raw, 0, trailing_bracket)
		} else {
			u32(0)
		}
		trailing_bracket_closed:            trailing_bracket >= 0
		array_chars_base:                   source_type_syntax_child_range_id(id, raw, array_chars, raw.len)
		lowercase_value_ident:              lowercase_value_ident
		typeof_prefix:                      raw.starts_with('typeof(')
		first_capital:                      raw.len > 0 && raw[0].is_capital()
		named_variant:                      named_variant
		named_variant_valid:                named_variant_valid
		named_variant_owner:                named_variant_owner
		named_variant_name:                 named_variant_name
		main_unlocked:                      source_type_syntax_main_unlocked(id, raw)
		encoded_fixed_length:               encoded_fixed_length
		encoded_fixed_base:                 encoded_fixed_base
		map_name_prefix:                    raw.starts_with('map_')
		suffix_bracket_length:              if suffix_bracket_valid {
			source_type_syntax_child_id(id, raw, source_type_query_trim(raw[last_bracket + 1..raw.len - 1]))
		} else {
			u32(0)
		}
		suffix_bracket_valid:               suffix_bracket_valid
		map_function_fragment:              raw.contains(']fn (') || raw.contains(']fn(')
		bracket_source_inner:               if bracket >= 0 {
			source_type_syntax_child_range_id(id, raw, bracket + 1, bracket_end)
		} else {
			u32(0)
		}
		c_fixed_typedef_element:            c_fixed_typedef_element
		c_fixed_typedef_length:             c_fixed_typedef_length
		c_fixed_typedef_valid:              c_fixed_typedef_valid
		fixed_array_query_length:           if raw.starts_with('[') {
			if first_close > 1 {
				source_type_syntax_child_range_id(id, raw, 1, first_close)
			} else {
				u32(0)
			}
		} else if bracket >= 0 && raw.ends_with(']') {
			source_type_syntax_child_range_id(id, raw, bracket + 1, raw.len - 1)
		} else {
			u32(0)
		}
		last_bracket_span_valid:            last_bracket >= 0 && last_right_bracket > last_bracket
		last_bracket_span_inner:            if last_bracket >= 0 && last_right_bracket > last_bracket {
			source_type_syntax_child_id(id, raw, source_type_query_trim(raw[last_bracket + 1..last_right_bracket]))
		} else {
			u32(0)
		}
		plain_type_name:                    plain_type_name
		generic_app_legacy_suffix:          if generic_app_valid {
			source_type_syntax_child_range_id(id, raw, source_type_name(generic_app_base).len, raw.len)
		} else {
			u32(0)
		}
		wrapper_prefix:                     if wrapper_len > 0 {
			source_type_syntax_child_range_id(id, raw, 0, wrapper_len)
		} else {
			u32(0)
		}
		wrapper_base:                       if wrapper_len > 0 {
			source_type_syntax_child_range_id(id, raw, wrapper_len, raw.len)
		} else {
			u32(0)
		}
		checker_fixed_array_may_be:         checker_fixed_array_may_be
		dollar_prefix:                      raw.starts_with('$')
		define_prefix:                      raw.starts_with('$d(')
		define_fragment:                    raw.contains('$d(')
		paren_prefix:                       raw.starts_with('(')
		fixed_length:                       source_type_syntax_checker_length(raw)
		qualified_modifiers:                if modifier_start < raw.len {
			source_type_syntax_child_range_id(id, raw, 0, modifier_start)
		} else {
			u32(0)
		}
		plain_alias_target:                 plain_alias_target
		c_array_payload:                    if raw.starts_with('Array_') {
			source_type_syntax_child_range_id(id, raw, 'Array_'.len, raw.len)
		} else {
			u32(0)
		}
		c_fixed_array_payload:              if raw.starts_with('FixedArray_') {
			source_type_syntax_child_range_id(id, raw, 'FixedArray_'.len, raw.len)
		} else {
			u32(0)
		}
		fastc_array_marker:                 raw.contains('_FASTC_ARRAY_OF_')
		fastc_array_marker_payload:         if raw.contains('_FASTC_ARRAY_OF_') {
			source_type_syntax_child_id(id, raw, raw.all_after('_FASTC_ARRAY_OF_'))
		} else {
			u32(0)
		}
		c_map_payload:                      if raw.starts_with('Map_') {
			source_type_syntax_child_range_id(id, raw, 'Map_'.len, raw.len)
		} else {
			u32(0)
		}
		c_ptr_name_base:                    if raw.ends_with('_ptr') {
			source_type_syntax_child_range_id(id, raw, 0, raw.len - 4)
		} else {
			u32(0)
		}
		c_tagged_type:                      source_type_syntax_c_tagged_type(id, raw)
		backward_bracket_prefix:            if backward_bracket >= 0 {
			source_type_syntax_child_range_id(id, raw, 0, backward_bracket)
		} else {
			u32(0)
		}
		backward_bracket_closed:            backward_bracket >= 0
		generic_mentions:                   generic_mentions
		opening_paren:                      raw.contains('(')
		byte_len:                           raw.len
		receiver_app_valid:                 bracket > 0 && raw.ends_with(']')
		receiver_app_base:                  if bracket > 0 && raw.ends_with(']') {
			source_type_syntax_child_range_id(id, raw, 0, bracket)
		} else {
			u32(0)
		}
		receiver_app_args:                  if bracket > 0 && raw.ends_with(']') {
			source_type_syntax_receiver_parts(raw[bracket + 1..raw.len - 1])
		} else {
			source_type_syntax_empty_ids()
		}
		checker_bracket_components:         if bracket >= 0 {
			source_type_syntax_checker_parts(raw[bracket + 1..bracket_end])
		} else {
			source_type_syntax_empty_ids()
		}
		declaration_parameter_payload:      declaration_parameter_payload
		declaration_parameter_mut:          declaration_parameter_mut
		prefix_dimension_tail:              if prefix_dimensions_closed {
			source_type_syntax_child_range_id(id, raw, prefix_dimension_end, raw.len)
		} else {
			u32(0)
		}
		prefix_dimensions_closed:           prefix_dimensions_closed
		backward_bracket_after_prefix_dims: backward_bracket >= prefix_dimension_end
		backward_bracket_inner:             if backward_bracket >= 0 && backward_bracket + 1 <= raw.len - 1 {
			source_type_syntax_child_id(id, raw, source_type_query_trim(raw[backward_bracket + 1..raw.len - 1]))
		} else {
			u32(0)
		}
		option_result_void_prefix:          raw.starts_with('?void') || raw.starts_with('!void')
		nested_option_result_marker:        nested_option_result_marker
		c_pointer_depth:                    raw.len - c_pointer_end
		c_pointer_count:                    raw.count('*')
		c_pointer_raw_base:                 source_type_syntax_child_range_id(id, raw, 0, c_pointer_end)
		voidptr_parts:                      voidptr_parts
		thread_value_type_valid:            thread_value_type_valid
		thread_value_type:                  thread_value_type
		thread_value_prefix:                raw.starts_with('__vf_thread_k')
		thread_value_payload:               if raw.starts_with('__vf_thread_k') {
			source_type_syntax_child_range_id(id, raw, '__vf_thread_k'.len, raw.len)
		} else {
			u32(0)
		}
		contains_closing_bracket:           raw.contains(']')
		atomic_val_fragment:                raw.contains('AtomicVal')
		inferred_parts:                     inferred_parts
		inferred_annotations:               inferred_annotations
		inferred_expr_ids:                  inferred_expr_ids
		expr_prefix:                        raw.starts_with('__expr_')
		expr_id:                            if raw.starts_with('__expr_') {
			raw['__expr_'.len..].int()
		} else {
			0
		}
		substitution_function_params:       substitution_function_params
		parameter_name:                     parameter_name
		parameter_named:                    parameter_named
		parameter_is_mut:                   parameter_is_mut
		transform_fixed_length:             source_type_syntax_transform_length(raw)
		receiver_collection_id:             source_type_syntax_receiver_collection(id, raw)
		ampersand_trimmed:                  if raw.len == 0 || (raw[0] != `&` && raw[raw.len - 1] != `&`) {
			id
		} else {
			source_type_syntax_child_id(id, raw, raw.trim('&'))
		}
		contains_doubleunderscore:          contains_doubleunderscore
		parallel_preseed_fn:                parallel_preseed_fn
		parallel_preseed_short:             parallel_preseed_short
		closing_bracket:                    raw.ends_with(']')
		constraint_receiver_base:           source_type_syntax_child_id(id, raw, raw.trim_left('&').all_after('mut ').trim_space())
		written_applications:               written_applications
		written_application_offsets:        written_application_offsets
		checker_raw_bracket_components:     if bracket >= 0 {
			source_type_syntax_checker_parts_mode(raw[bracket + 1..bracket_end], false)
		} else {
			source_type_syntax_empty_ids()
		}
		cgen_placeholder_candidates:        source_type_syntax_cgen_placeholder_candidates(raw)
		result_suffix:                      raw.ends_with('Result')
		cgen_decl_unusable:                 cgen_decl_unusable
		cgen_decl_authority:                cgen_decl_authority
		generic_encoded_fragment:           raw.contains('_T_')
		fn_ptr_encoded_prefix:              raw.starts_with('_fn_ptr_')
		c_result_prefix:                    raw.starts_with('__v_result')
		c_option_prefix:                    raw.starts_with('__v_option_')
		c_multi_return_prefix:              raw.starts_with('multi_return_')
		c_lower_map_prefix:                 raw.starts_with('map_') && !raw.starts_with('map__')
		open_generic_receiver_marker:       source_type_syntax_open_generic_receiver_marker(raw)
		space_removed:                      if !spaced {
			id
		} else {
			source_type_syntax_child_id(id, raw, raw.replace(' ', ''))
		}
		qualified_name_chars:               named_type && raw.contains('.')
		field_owner_module:                 if qualifier_count == 0 {
			id
		} else if raw.contains('.C.') {
			source_type_syntax_child_id(id, raw, raw.all_before('.C.'))
		} else if raw.contains('.JS.') {
			source_type_syntax_child_id(id, raw, raw.all_before('.JS.'))
		} else {
			qualifier
		}
		const_name_prefix:                  raw.starts_with('const_')
		reflection_parts:                   if unpadded_nonempty && !has_comma {
			identity_ids
		} else {
			source_type_syntax_reflection_parts(raw)
		}
		reflection_tuple_parts:             if tuple_like {
			source_type_syntax_reflection_parts(raw[1..raw.len - 1])
		} else {
			source_type_syntax_empty_ids()
		}
		pthread_prefix:                     raw.starts_with('pthread_')
		thread_bracket_prefix:              raw.starts_with('thread[')
		fixed_typedef_bracket:              raw.len > 0 && raw[..raw.len - 1].contains('[')
		receiver_raw_app_args:              if bracket >= 0 && raw.ends_with(']') {
			source_type_syntax_checker_parts_mode(raw[bracket + 1..raw.len - 1], false)
		} else {
			source_type_syntax_empty_ids()
		}
		question_trimmed:                   if !raw.ends_with('?') {
			id
		} else {
			source_type_syntax_child_id(id, raw, raw.trim_right('?'))
		}
		c_array_fixed_prefix:               raw.starts_with('Array_fixed_')
		vls_generic_words:                  source_type_syntax_vls_generic_words(raw)
		function_space_result:              function_space_result
		decimal_value:                      if decimal { integer_value } else { 0 }
		integer_value:                      integer_value
		encoded_alnum_dotted:               source_type_syntax_encoded_alnum_dotted(raw)
		internal_symbol_prefix:             raw.starts_with('__v3_internal_symbol_')
		main_at_base:                       if raw.starts_with('main.@') {
			source_type_syntax_child_range_id(id, raw, 6, raw.len)
		} else {
			id
		}
		eval_tuple_parts:                   if tuple_like {
			source_type_syntax_eval_parts(raw[1..raw.len - 1])
		} else {
			source_type_syntax_empty_ids()
		}
		implicit_tuple_components:          source_type_syntax_implicit_tuple(raw)
		generic_application_heads:          source_type_syntax_application_heads(raw, false)
		incremental_generic_application:    source_type_syntax_incremental_generic_application(raw)
		system_error_suffix:                raw.ends_with('SystemError')
		receiver_app_inner:                 if bracket > 0 && raw.ends_with(']') {
			source_type_syntax_child_range_id(id, raw, bracket + 1, raw.len - 1)
		} else {
			u32(0)
		}
		fastc_fixed_array_length_valid:     fastc_fixed_array_length_valid
		fastc_fixed_array_length:           fastc_fixed_array_length
		markused_application_heads:         source_type_syntax_application_heads(raw, true)
		contains_capital:                   source_type_syntax_contains_capital(raw)
		markused_generic_components:        source_type_syntax_markused_components(raw)
		first_bracket_source_inner:         if bracket < 0 && first_close < 0 {
			id
		} else {
			source_type_syntax_child_id(id, raw, raw.all_after('[').all_before(']'))
		}
		c_function_pointer_hole:            raw.contains('(*)')
		channel_fragment:                   raw.contains('chan ')
		array_receiver_fragment:            raw.contains('.[]')
		mangled_open_generic_marker:        source_type_syntax_mangled_generic_marker(raw)
		array_suffix:                       raw.ends_with('[]')
		empty_function_prefix:              raw.starts_with('fn()') || raw.starts_with('fn ()')
		c_shared_fragment:                  raw.contains('__shared__')
		typ_suffix:                         raw.ends_with('.typ')
		typ_suffix_base:                    if raw.ends_with('.typ') {
			source_type_syntax_child_range_id(id, raw, 0, raw.len - 4)
		} else {
			u32(0)
		}
		raw_tuple_parse_components:         if raw.starts_with('(') && raw.contains(',') {
			source_type_syntax_checker_parts_mode(raw[1..raw.len - 1], false)
		} else {
			source_type_syntax_empty_ids()
		}
		checker_parse_function_result:      checker_parse_function_result
		checker_parse_function_has_params:  checker_parse_function_has_params
		contains_question:                  raw.contains('?')
		contains_union:                     contains_union
		union_parts:                        if unpadded_nonempty && !contains_union {
			identity_ids
		} else {
			source_type_syntax_union_parts(raw)
		}
		named_variant_dot_valid:            named_variant_dot >= 0
		named_variant_dot_prefix:           if named_variant_dot >= 0 {
			source_type_syntax_child_range_id(id, raw, 0, named_variant_dot)
		} else {
			u32(0)
		}
		named_variant_dot_suffix:           if named_variant_dot >= 0 {
			source_type_syntax_child_range_id(id, raw, named_variant_dot + 1, raw.len)
		} else {
			u32(0)
		}
		top_level_dot_valid:                top_level_dot >= 0
		top_level_dot_prefix:               if top_level_dot >= 0 {
			source_type_syntax_child_range_id(id, raw, 0, top_level_dot)
		} else {
			u32(0)
		}
		top_level_dot_suffix:               if top_level_dot >= 0 {
			source_type_syntax_child_range_id(id, raw, top_level_dot + 1, raw.len)
		} else {
			u32(0)
		}
		parser_fn_decl_valid:               parser_fn_decl_valid
		parser_fn_decl_params:              if parser_fn_decl_valid {
			source_type_syntax_child_range_id(id, raw, 3, parser_fn_close)
		} else {
			u32(0)
		}
		parser_fn_decl_result:              if parser_fn_decl_valid {
			source_type_syntax_child_id(id, raw, source_type_query_trim(raw[parser_fn_close + 1..]).trim_left('?!&'))
		} else {
			u32(0)
		}
	}
}

fn source_type_syntax_plain_name(raw string) bool {
	for c in raw { if !source_type_query_ident(c) { return false } }
	return true
}

fn source_type_syntax_decimal(text string) bool {
	if text.len == 0 { return false }
	for c in text { if c < `0` || c > `9` { return false } }
	return true
}

fn source_type_syntax_name(id u32, raw string, first_dot int, last_dot int) SourceTypeSyntaxParsed {
	// Plain and qualified identifiers need no composite parser or parameter
	// normalizer. Compute their dot views together without copying a second record.
	qualified := first_dot >= 0
	identity_entry := source_type_read_entry(id)
	identity_ids := source_type_syntax_singleton_ids(&identity_entry.id)
	integer_value := raw.int()
	decimal := source_type_syntax_decimal(raw)
	contains_capital := source_type_syntax_contains_capital(raw)
	contains_doubleunderscore := raw.contains('__')
	cgen_decl_unusable, cgen_decl_authority := source_type_syntax_cgen_declaration(raw)
	thread_value_type_valid, thread_value_type := source_type_syntax_thread_value(raw)
	mut last_underscore := -1
	mut qualifier_count := u32(0)
	mut field_owner_c := -1
	mut field_owner_js := -1
	for i, c in raw {
		if c == `_` {
			last_underscore = i
		} else if c == `.` {
			qualifier_count++
			if field_owner_c < 0 && raw[i..].starts_with('.C.') { field_owner_c = i }
			if field_owner_js < 0 && raw[i..].starts_with('.JS.') { field_owner_js = i }
		}
	}
	first_dot_prefix, first_dot_suffix, qualifier, unqualified := source_type_syntax_dot_edges(id, raw, first_dot, last_dot)
	mut main_base := u32(0)
	mut builtin_base := u32(0)
	mut vls_generic_words := source_type_syntax_empty_ids()
	if qualified {
		if first_dot == 4 && raw.starts_with('main.') { main_base = first_dot_suffix }
		if first_dot == 7 && raw.starts_with('builtin.') { builtin_base = first_dot_suffix }
		if first_dot == 1 && raw[0].is_capital() { vls_generic_words << first_dot_prefix }
	} else {
		if raw[0].is_digit() {
			vls_generic_words = source_type_syntax_vls_generic_words(raw)
		} else if raw.len == 1 && raw[0].is_capital() {
			vls_generic_words = unsafe { identity_ids }
		}
	}
	main_fragment := qualified && raw.contains('main.')
	field_owner_dot := if field_owner_c >= 0 { field_owner_c } else { field_owner_js }
	field_owner_module := if field_owner_dot < 0 || field_owner_dot == last_dot {
		qualifier
	} else if field_owner_dot == first_dot {
		first_dot_prefix
	} else {
		source_type_syntax_child_range_id(id, raw, 0, field_owner_dot)
	}
	typ_suffix := qualified && raw.ends_with('.typ')
	fn_prefix := raw.starts_with('fn')
	encoded_fixed_length, encoded_fixed_base := source_type_syntax_encoded_fixed(raw)
	c_fixed_typedef_element, c_fixed_typedef_length, c_fixed_typedef_valid := source_type_syntax_c_fixed_typedef(raw)
	fastc_fixed_array_length_valid, fastc_fixed_array_length := source_type_syntax_fastc_fixed_length(raw)
	return SourceTypeSyntaxParsed{
		name_packing:                   true
		kind:                           .name
		trimmed:                        id
		unqualified:                    unqualified
		qualifier:                      qualifier
		generic_base:                   id
		pointer_base:                   id
		postfix_base:                   id
		parameter_type:                 id
		parameter_mut_type:             id
		bare_generic:                   raw.len == 1 && raw[0] >= `A` && raw[0] <= `Z`
		last_space_word:                id
		named_type:                     true
		first_dot_prefix:               first_dot_prefix
		first_dot_suffix:               first_dot_suffix
		main_base:                      main_base
		builtin_base:                   builtin_base
		c_qualified:                    qualified && first_dot == 1 && raw[0] == `C`
		builtin_prefix:                 builtin_base != 0
		main_fragment:                  main_fragment
		qualifier_count:                qualifier_count
		canonical_dotted_name:          qualified
		function_prefix:                fn_prefix
		fn_prefix:                      fn_prefix
		pointer_raw_base:               id
		decimal:                        decimal
		dots_base:                      id
		cgen_parts:                     identity_ids
		generic_parameter_payload:      id
		legacy_parameter_type:          id
		legacy_parameter_mut_type:      id
		first_bracket_length:           id
		c_pointer_base:                 id
		c_ptr_name_suffix:              raw.ends_with('_ptr')
		c_fn_type_fragment:             raw.contains('__vf_fn_type_')
		underscored:                    last_underscore >= 0
		plain_comma_parts:              identity_ids
		source_wrapper_base:            id
		declaration_head:               id
		fn_fragment:                    raw.contains('fn')
		compact_fn_spelling:            id
		array_chars_base:               id
		lowercase_value_ident:          !qualified && raw[0] >= `a` && raw[0] <= `z`
		first_capital:                  raw[0].is_capital()
		main_unlocked:                  if main_base != 0 { main_base } else { id }
		encoded_fixed_length:           encoded_fixed_length
		encoded_fixed_base:             encoded_fixed_base
		map_name_prefix:                raw.starts_with('map_')
		c_fixed_typedef_element:        c_fixed_typedef_element
		c_fixed_typedef_length:         c_fixed_typedef_length
		c_fixed_typedef_valid:          c_fixed_typedef_valid
		plain_type_name:                !qualified && (raw[0].is_letter() || raw[0] == `_`)
		fixed_length:                   (raw[0].is_digit() || raw[0] == `_`) && source_type_syntax_integer(raw)
		qualified_modifiers:            u32(0)
		plain_alias_target:             true
		c_array_payload:                if raw.starts_with('Array_') {
			source_type_id(raw['Array_'.len..])
		} else {
			u32(0)
		}
		c_fixed_array_payload:          if raw.starts_with('FixedArray_') {
			source_type_id(raw['FixedArray_'.len..])
		} else {
			u32(0)
		}
		fastc_array_marker:             raw.contains('_FASTC_ARRAY_OF_')
		fastc_array_marker_payload:     if raw.contains('_FASTC_ARRAY_OF_') {
			source_type_id(raw.all_after('_FASTC_ARRAY_OF_'))
		} else {
			u32(0)
		}
		c_map_payload:                  if raw.starts_with('Map_') {
			source_type_id(raw['Map_'.len..])
		} else {
			u32(0)
		}
		c_ptr_name_base:                if raw.ends_with('_ptr') {
			source_type_id(raw[..raw.len - 4])
		} else {
			u32(0)
		}
		c_tagged_type:                  source_type_syntax_c_tagged_type(id, raw)
		generic_mentions:               if raw.len == 1 && raw[0].is_capital() {
			identity_ids
		} else {
			source_type_syntax_empty_ids()
		}
		cgen_generic_placeholder:       !qualified && raw[0] >= `A` && raw[0] <= `Z`
			&& (raw.len == 1 || (raw[1] >= `0` && raw[1] <= `9`))
		byte_len:                       raw.len
		declaration_parameter_payload:  id
		prefix_dimension_tail:          id
		prefix_dimensions_closed:       true
		nested_option_result_marker:    -1
		c_pointer_raw_base:             id
		voidptr_parts:                  if raw.contains('voidptr') {
			source_type_syntax_plain_parts(raw, 'voidptr')
		} else {
			identity_ids
		}
		thread_value_type_valid:        thread_value_type_valid
		thread_value_type:              thread_value_type
		thread_value_prefix:            raw.starts_with('__vf_thread_k')
		thread_value_payload:           if raw.starts_with('__vf_thread_k') {
			source_type_id(raw['__vf_thread_k'.len..])
		} else {
			u32(0)
		}
		atomic_val_fragment:            raw.contains('AtomicVal')
		inferred_parts:                 identity_ids
		expr_prefix:                    raw.starts_with('__expr_')
		expr_id:                        if raw.starts_with('__expr_') {
			raw['__expr_'.len..].int()
		} else {
			0
		}
		transform_fixed_length:         raw[0].is_digit()
			|| (raw[last_dot + 1] >= `a` && raw[last_dot + 1] <= `z`
				&& (qualified || !source_type_syntax_builtin(raw)))
		ampersand_trimmed:              id
		contains_doubleunderscore:      contains_doubleunderscore
		parallel_preseed_fn:            fn_prefix
		parallel_preseed_short:         if !fn_prefix && raw[last_dot + 1].is_capital() {
			unqualified
		} else {
			u32(0)
		}
		constraint_receiver_base:       id
		cgen_placeholder_candidates:    if raw.len == 1 && raw[0] >= `A` && raw[0] <= `Z` {
			identity_ids
		} else {
			source_type_syntax_empty_ids()
		}
		result_suffix:                  raw.ends_with('Result')
		cgen_decl_unusable:             cgen_decl_unusable
		cgen_decl_authority:            cgen_decl_authority
		generic_encoded_fragment:       raw.contains('_T_')
		fn_ptr_encoded_prefix:          raw.starts_with('_fn_ptr_')
		c_result_prefix:                raw.starts_with('__v_result')
		c_option_prefix:                raw.starts_with('__v_option_')
		c_multi_return_prefix:          raw.starts_with('multi_return_')
		c_lower_map_prefix:             raw.starts_with('map_') && !raw.starts_with('map__')
		open_generic_receiver_marker:   source_type_syntax_open_generic_receiver_marker(raw)
		space_removed:                  id
		qualified_name_chars:           qualified
		field_owner_module:             field_owner_module
		const_name_prefix:              raw.starts_with('const_')
		reflection_parts:               identity_ids
		reflection_tuple_parts:         source_type_syntax_empty_ids()
		pthread_prefix:                 raw.starts_with('pthread_')
		thread_bracket_prefix:          false
		fixed_typedef_bracket:          false
		receiver_raw_app_args:          source_type_syntax_empty_ids()
		question_trimmed:               id
		c_array_fixed_prefix:           raw.starts_with('Array_fixed_')
		vls_generic_words:              vls_generic_words
		decimal_value:                  if decimal {
			integer_value
		} else {
			0
		}
		integer_value:                  integer_value
		encoded_alnum_dotted:           !raw.contains('_')
		internal_symbol_prefix:         raw.starts_with('__v3_internal_symbol_')
		main_at_base:                   id
		mangled_open_generic_marker:    source_type_syntax_mangled_generic_marker(raw)
		c_shared_fragment:              raw.contains('__shared__')
		typ_suffix:                     typ_suffix
		typ_suffix_base:                if typ_suffix { qualifier } else { u32(0) }
		named_variant_dot_valid:        qualified
		named_variant_dot_prefix:       if qualified { qualifier } else { u32(0) }
		named_variant_dot_suffix:       if qualified { unqualified } else { u32(0) }
		top_level_dot_valid:            qualified
		top_level_dot_prefix:           if qualified { first_dot_prefix } else { u32(0) }
		top_level_dot_suffix:           if qualified { first_dot_suffix } else { u32(0) }
		union_parts:                    identity_ids
		system_error_suffix:            raw.ends_with('SystemError')
		fastc_fixed_array_length_valid: fastc_fixed_array_length_valid
		fastc_fixed_array_length:       fastc_fixed_array_length
		contains_capital:               contains_capital
		markused_generic_components:    if contains_capital {
			if raw[0].is_letter() || raw[0] == `_` {
				identity_ids
			} else {
				source_type_syntax_markused_components(raw)
			}
		} else {
			source_type_syntax_empty_ids()
		}
		first_bracket_source_inner:     id
	}
}

fn source_type_syntax_checker_function_result(raw string) (u32, bool) {
	open := raw.index_u8(`(`)
	if open < 0 { return 0, false }
	mut depth := 1
	for i := open + 1; i < raw.len; i++ {
		if raw[i] == `(` {
			depth++
		} else if raw[i] == `)` {
			depth--
			if depth == 0 {
				return source_type_id(raw[i + 1..].trim_left(' ')), source_type_query_trim(raw[open + 1..i]).len > 0
			}
		}
	}
	return 0, false
}

fn source_type_syntax_function(raw string) (bool, []u32, u32, []u32, []u32, []u32, u32) {
	text := source_type_query_trim(raw)
	open := text.index_u8(`(`)
	if open < 0 {
		return false, source_type_syntax_empty_ids(), 0, source_type_syntax_empty_ids(), source_type_syntax_empty_ids(), source_type_syntax_empty_ids(), 0
	}
	mut depth := 1
	mut end := -1
	for i := open + 1; i < text.len; i++ {
		if text[i] == `(` {
			depth++
		} else if text[i] == `)` {
			depth--
			if depth == 0 {
				end = i
				break
			}
		}
	}
	if end < 0 {
		return false, source_type_syntax_empty_ids(), 0, source_type_syntax_empty_ids(), source_type_syntax_empty_ids(), source_type_syntax_empty_ids(), 0
	}
	checker_params := source_type_syntax_checker_parts(text[open + 1..end])
	mut params := source_type_syntax_empty_ids()
	for param in checker_params {
		if param != 0 { params << param }
	}
	return true, params, source_type_id(source_type_query_trim(text[end + 1..])), checker_params, source_type_syntax_legacy_parts(text[open + 1..end]), if source_type_query_trim(text[open + 1..end]).len > 0 {
		source_type_syntax_receiver_parts(text[open + 1..end])
	} else {
		source_type_syntax_empty_ids()
	}, source_type_id(text[end + 1..].trim_left(' '))
}

fn source_type_syntax_tuple_function(raw string) []u32 {
	mut components := source_type_syntax_empty_ids()
	if raw.starts_with('(') && raw.ends_with(')') {
		for part in source_type_syntax_checker_parts(raw[1..raw.len - 1]) {
			if part != 0 { components << part }
		}
	}
	return components
}

fn source_type_syntax_cgen_application_raw(text string) (u32, []u32, bool) {
	if text.starts_with('fn(') || text.starts_with('fn (') {
		return 0, source_type_syntax_empty_ids(), false
	}
	bracket := text.index_u8(`[`)
	if bracket <= 0 { return 0, source_type_syntax_empty_ids(), false }
	end := source_type_syntax_matching_bracket(text, bracket)
	if end >= text.len { return 0, source_type_syntax_empty_ids(), false }
	return source_type_id(text[..bracket]), source_type_syntax_legacy_parts(text[bracket + 1..end]), true
}

fn source_type_syntax_top_space(text string) (u32, u32, bool) {
	mut brackets := 0
	mut parens := 0
	for i, c in text {
		match c {
			`[` { brackets++ }
			`]` { brackets-- }
			`(` { parens++ }
			`)` { parens-- }
			` ` {
				if brackets == 0 && parens == 0 {
					if i <= 0 { return 0, 0, false }
					return source_type_id(source_type_query_trim(text[..i])), source_type_id(source_type_query_trim(text[i + 1..])), true
				}
			}
			else {}
		}
	}
	return 0, 0, false
}

// source_type_has_qualified_prefix compares a module prefix using only IDs on hits.
pub fn source_type_has_qualified_prefix(id u32, prefix u32) bool {
	syntax := source_type_syntax(id)
	if syntax.qualifier_count == 0 { return false }
	if prefix == syntax.first_dot_prefix || prefix == syntax.qualifier { return true }
	for candidate in syntax.qualifier_prefixes() {
		if candidate == prefix { return true }
	}
	return false
}

// source_type_qualified_suffix returns the raw suffix following prefix and a dot.
// Both matching and the returned spelling use numeric IDs after the first query.
pub fn source_type_qualified_suffix(id u32, prefix u32) ?u32 {
	syntax := source_type_syntax(id)
	if syntax.qualifier_count == 0 { return none }
	if prefix == syntax.first_dot_prefix { return syntax.first_dot_suffix }
	if prefix == syntax.qualifier { return syntax.unqualified }
	for i, candidate in syntax.qualifier_prefixes() {
		if candidate == prefix { return syntax.qualifier_suffixes()[i] }
	}
	return none
}

fn source_type_syntax_wrapper_len(text string, extended bool) int {
	if text.len == 0 { return 0 }
	if text[0] in [`&`, `?`, `!`] { return 1 }
	if text.starts_with('[]') { return 2 }
	if text.starts_with('...') { return 3 }
	if text.starts_with('mut ') { return 4 }
	if text.starts_with('shared ') { return 7 }
	if text.starts_with('chan ') { return 5 }
	if extended && (text.starts_with('atomic ') || text.starts_with('thread ')) { return 7 }
	return 0
}

fn source_type_syntax_matching_bracket(text string, start int) int {
	mut depth := 0
	for i in start .. text.len {
		if text[i] == `[` {
			depth++
		} else if text[i] == `]` {
			depth--
			if depth == 0 { return i }
		}
	}
	return text.len
}

// These independent views preserve the permissive legacy query grammars. They
// must not become stricter merely because the complete spelling is malformed.
fn source_type_syntax_application(text string, transform bool) (u32, []u32, bool) {
	bracket := text.index_u8(`[`)
	if bracket <= 0 { return 0, source_type_syntax_empty_ids(), false }
	if transform {
		if text.starts_with('fn(') || text.starts_with('fn (') {
			return 0, source_type_syntax_empty_ids(), false
		}
		mut parens := 0
		for c in text[..bracket] {
			if c == `(` {
				parens++
			} else if c == `)` {
				parens--
			}
		}
		if parens != 0 { return 0, source_type_syntax_empty_ids(), false }
	}
	end := source_type_syntax_matching_bracket(text, bracket)
	if end <= bracket || (transform && end >= text.len) {
		return 0, source_type_syntax_empty_ids(), false
	}
	inner := source_type_query_trim(text[bracket + 1..end])
	if transform {
		if source_type_syntax_transform_length(inner) && !inner.starts_with('C.') && !inner.starts_with('JS.') {
			return 0, source_type_syntax_empty_ids(), false
		}
	} else if source_type_syntax_checker_length(inner) {
		return 0, source_type_syntax_empty_ids(), false
	}
	mut args := if transform {
		source_type_syntax_legacy_parts(inner)
	} else {
		source_type_syntax_checker_parts(inner)
	}
	base := text[..bracket]
	if !transform {
		mut map_base := source_type_query_trim(base)
		for {
			prefix := source_type_syntax_wrapper_len(map_base, false)
			if prefix == 0 { break }
			map_base = source_type_query_trim(map_base[prefix..])
		}
		if map_base == 'map' {
			if end >= text.len { return 0, source_type_syntax_empty_ids(), false }
			value := source_type_id(source_type_query_trim(text[end + 1..]))
			if value == 0 { return 0, source_type_syntax_empty_ids(), false }
			args << value
		}
	}
	return source_type_id(base), args, true
}

fn source_type_syntax_legacy_parts(text string) []u32 {
	mut parts := source_type_syntax_empty_ids()
	mut brackets := 0
	mut parens := 0
	mut start := 0
	for i, c in text {
		match c {
			`[` { brackets++ }
			`]` { brackets-- }
			`(` { parens++ }
			`)` { parens-- }
			`,` {
				if brackets == 0 && parens == 0 {
					parts << source_type_id(source_type_query_trim(text[start..i]))
					start = i + 1
				}
			}
			else {}
		}
	}
	parts << source_type_id(source_type_query_trim(text[start..]))
	return parts
}

fn source_type_syntax_checker_parts(text string) []u32 {
	return source_type_syntax_checker_parts_mode(text, true)
}

fn source_type_syntax_checker_parts_mode(text string, trim_parts bool) []u32 {
	mut parts := source_type_syntax_empty_ids()
	mut depth := 0
	mut start := 0
	for i, c in text {
		match c {
			`(`, `[` { depth++ }
			`)`, `]` { depth-- }
			`,` {
				if depth == 0 {
					part := text[start..i]
					parts << source_type_id(if trim_parts {
						source_type_query_trim(part)
					} else {
						part
					})
					start = i + 1
				}
			}
			else {}
		}
	}
	if start < text.len {
		part := text[start..]
		parts << source_type_id(if trim_parts { source_type_query_trim(part) } else { part })
	}
	return parts
}

fn source_type_syntax_receiver_parts(text string) []u32 {
	mut parts := source_type_syntax_empty_ids()
	mut depth := 0
	mut start := 0
	for i, c in text {
		if c == `[` || c == `(` {
			depth++
		} else if c == `]` || c == `)` {
			depth--
		} else if c == `,` && depth == 0 {
			parts << source_type_id(text[start..i])
			start = i + 1
		}
	}
	parts << source_type_id(text[start..])
	return parts
}

fn source_type_syntax_legacy_words(text string) []u32 {
	mut words := source_type_syntax_empty_ids()
	mut start := -1
	for i, c in text {
		letter := (c >= `a` && c <= `z`) || (c >= `A` && c <= `Z`) || c == `_`
		if start < 0 {
			if letter { start = i }
		} else if !letter && !(c >= `0` && c <= `9`) {
			words << source_type_id(text[start..i])
			start = -1
		}
	}
	if start >= 0 { words << source_type_id(text[start..]) }
	return words
}

fn source_type_syntax_checker_length(text string) bool {
	clean := source_type_query_trim(text)
	if clean.len == 0 || clean.contains(',') { return false }
	if source_type_syntax_integer(clean) { return true }
	if clean[0] in [`&`, `?`, `!`, `[`] || clean.starts_with('...') || clean.starts_with('map[')
		|| clean.starts_with('fn(') || clean.starts_with('fn ') || clean.starts_with('chan ')
		|| clean.starts_with('thread ') || clean.starts_with('shared ') || clean.starts_with('atomic ')
		|| clean.starts_with('mut ') {
		return false
	}
	mut depth := 0
	for i, c in clean {
		if c == `[` {
			depth++
		} else if c == `]` {
			depth--
		}
		if c in [`+`, `*`, `/`, `%`, `|`, `^`, `<`, `>`] || ((c == `-` || c == `&`) && i > 0 && depth == 0) {
			return true
		}
	}
	return false
}

fn source_type_syntax_integer(text string) bool {
	t := text.replace('_', '')
	mut base := 10
	mut start := 0
	if t.len >= 2 && t[0] == `0` {
		match t[1] {
			`x`, `X` { base, start = 16, 2 }
			`o`, `O` { base, start = 8, 2 }
			`b`, `B` { base, start = 2, 2 }
			else {}
		}
	}
	if start == t.len { return false }
	for c in t[start..] {
		digit := if c >= `0` && c <= `9` {
			int(c - `0`)
		} else if c >= `a` && c <= `f` {
			int(c - `a`) + 10
		} else if c >= `A` && c <= `F` {
			int(c - `A`) + 10
		} else {
			16
		}
		if digit >= base { return false }
	}
	return true
}

fn source_type_syntax_builtin(text string) bool {
	return text in ['i8', 'u8', 'int', 'i16', 'i32', 'i64', 'u16', 'u32', 'u64', 'f32', 'f64',
		'map', 'nil', 'bool', 'char', 'i128', 'rune', 'u128', 'uint', 'void', 'none', 'isize',
		'usize', 'array', 'string', 'voidptr', 'charptr', 'byteptr']
}

fn source_type_syntax_transform_length(text string) bool {
	clean := source_type_query_trim(text)
	if clean.len == 0 || clean.contains(',') || clean.contains('[') || clean.contains(']') {
		return false
	}
	if clean[0] in [`&`, `?`, `!`] || clean.starts_with('fn(') || clean.starts_with('fn (')
		|| clean.starts_with('chan ') || clean.starts_with('shared ') || clean.starts_with('atomic ')
		|| clean.starts_with('mut ') || clean.starts_with('thread ') {
		return false
	}
	if clean[0] >= `0` && clean[0] <= `9` { return true }
	if clean[0] == `(` && clean.ends_with(')') {
		return source_type_syntax_transform_length(clean[1..clean.len - 1])
	}
	name := clean.all_after_last('.')
	if source_type_syntax_builtin(clean) || (name.len == 1 && name[0] >= `A` && name[0] <= `Z`) {
		return false
	}
	for i, c in clean {
		if c in [`+`, `*`, `/`, `%`, `|`, `^`, `<`, `>`] || ((c == `-` || c == `&`) && i > 0) {
			return true
		}
	}
	return name.len > 0 && name[0] >= `a` && name[0] <= `z`
}

fn source_type_syntax_postfix(text string) (u32, []u32) {
	mut end := text.len
	mut dims := source_type_syntax_empty_ids()
	for end > 0 && text[end - 1] == `]` {
		mut depth := 0
		mut start := -1
		for i := end - 1; i >= 0; i-- {
			if text[i] == `]` {
				depth++
			} else if text[i] == `[` {
				depth--
				if depth == 0 {
					start = i
					break
				}
			}
		}
		if start < 0 { break }
		dims << source_type_id(source_type_query_trim(text[start + 1..end - 1]))
		end = start
	}
	return source_type_id(text[..end]), dims
}

fn source_type_syntax_parameter_name(head string, tail string) bool {
	if head.len == 0 || tail.len == 0 || head.starts_with('fn') || head.starts_with('&') || head.starts_with('[') {
		return false
	}
	if head in ['shared', 'atomic', 'chan', 'thread', 'map'] || head.contains('.') || source_type_syntax_builtin(head) {
		return false
	}
	return (head[0] >= `a` && head[0] <= `z`) || head[0] == `_`
}

fn source_type_syntax_parameter_details(raw string) (u32, u32, bool, bool, string) {
	trimmed := source_type_query_trim(raw)
	mut text := trimmed
	is_mut := text.starts_with('mut ')
	if is_mut { text = source_type_query_trim(text[4..]) }
	mut name := u32(0)
	mut named := false
	mut depth := 0
	mut space := -1
	for i, c in text {
		if c == `(` || c == `[` {
			depth++
		} else if c == `)` || c == `]` {
			depth--
		} else if c == ` ` && depth == 0 {
			space = i
			break
		}
	}
	if space > 0 {
		head := source_type_query_trim(text[..space])
		tail := source_type_query_trim(text[space + 1..])
		if source_type_syntax_parameter_name(head, tail) {
			name = source_type_id(head)
			named = true
			text = tail
		}
	}
	payload := source_type_id(text)
	return payload, name, named, is_mut, text
}

fn source_type_syntax_declaration_parameter(payload u32, is_mut bool) (u32, bool) {
	original := source_type_name(payload)
	mut text := original
	for marker in ['[]', '&', 'map[', 'fn(', 'fn ('] {
		index := text.index(marker) or { continue }
		if index <= 0 { continue }
		head := source_type_query_trim(text[..index])
		tail := source_type_query_trim(text[index..])
		if source_type_syntax_parameter_name(head, tail) { text = tail }
		break
	}
	return source_type_syntax_child_id(payload, original, text), is_mut
}

// The shared details pass has already trimmed and peeled the first mut/name.
// This view alone also handles a second mut and embedded type markers.
fn source_type_syntax_parameter(param string, payload u32, initial_mut bool) (u32, u32) {
	mut text := param
	mut is_mut := initial_mut
	if text.starts_with('mut ') {
		is_mut = true
		text = source_type_query_trim(text[4..])
	}
	if text.len > 0 {
		for marker in ['[]', '&', 'map[', 'fn(', 'fn ('] {
			index := text.index(marker) or { continue }
			if index <= 0 { continue }
			head := source_type_query_trim(text[..index])
			tail := source_type_query_trim(text[index..])
			if source_type_syntax_parameter_name(head, tail) { text = tail }
			break
		}
	}
	id := source_type_syntax_child_id(payload, param, text)
	if is_mut && id != 0 {
		return source_type_composition_one(.pointer, id), source_type_composition_one(.mutable_parameter, id)
	}
	return id, id
}

fn source_type_syntax_tokens(text string, dotted bool) []u32 {
	mut ids := source_type_syntax_empty_ids()
	mut start := -1
	for i, c in text {
		if source_type_query_ident(c) || (dotted && c == `.`) {
			if start < 0 {
				start = i
			}
		} else if start >= 0 {
			ids << source_type_id(text[start..i])
			start = -1
		}
	}
	if start >= 0 {
		ids << source_type_id(text[start..])
	}
	return ids
}

// A delimiter scan is confined to first construction of a syntax record. Quotes
// can occur in fixed array length expressions and must hide embedded delimiters.
fn source_type_syntax_close(text string, start int) int {
	mut stack := []u8{}
	mut quote := u8(0)
	mut escaped := false
	for i := start; i < text.len; i++ {
		c := text[i]
		if quote != 0 {
			if escaped {
				escaped = false
			} else if c == 92 {
				escaped = true
			} else if c == quote {
				quote = 0
			}
			continue
		}
		if c == 39 || c == 34 {
			quote = c
			continue
		}
		if c == `(` || c == `[` || c == `{` {
			stack << c
		} else if c == `)` || c == `]` || c == `}` {
			if stack.len == 0 {
				return -1
			}
			open := stack.pop()
			if (c == `)` && open != `(`) || (c == `]` && open != `[`) || (c == `}` && open != `{`) {
				return -1
			}
			if stack.len == 0 {
				return i
			}
		}
	}
	return -1
}

fn source_type_syntax_balanced(text string) bool {
	mut i := 0
	for i < text.len {
		c := text[i]
		if c == `(` || c == `[` || c == `{` {
			end := source_type_syntax_close(text, i)
			if end < 0 {
				return false
			}
			i = end + 1
		} else if c == `)` || c == `]` || c == `}` {
			return false
		} else {
			i++
		}
	}
	return true
}

fn source_type_syntax_arguments(text string) (bool, []u32) {
	mut ids := source_type_syntax_empty_ids()
	if source_type_query_trim(text).len == 0 {
		return true, ids
	}
	mut start := 0
	mut i := 0
	for i < text.len {
		c := text[i]
		if c == `(` || c == `[` || c == `{` {
			end := source_type_syntax_close(text, i)
			if end < 0 {
				return false, ids
			}
			i = end + 1
			continue
		}
		if c == `,` {
			part := source_type_query_trim(text[start..i])
			if part.len == 0 {
				return false, ids
			}
			ids << source_type_id(part)
			start = i + 1
		}
		i++
	}
	last := source_type_query_trim(text[start..])
	if last.len == 0 {
		return false, ids
	}
	ids << source_type_id(last)
	return true, ids
}

fn source_type_syntax_components(text string) (SourceTypeSyntaxKind, u32, []u32, bool) {
	if text.len == 0 {
		return .empty, 0, source_type_syntax_empty_ids(), false
	}
	if !source_type_syntax_balanced(text) {
		return .invalid, 0, source_type_syntax_empty_ids(), false
	}
	mut prefix_len := 0
	mut prefix_kind := SourceTypeSyntaxKind.name
	if text[0] == `&` {
		prefix_len, prefix_kind = 1, .pointer
	} else if text[0] == `?` {
		prefix_len, prefix_kind = 1, .option
	} else if text[0] == `!` {
		prefix_len, prefix_kind = 1, .result
	} else if text.starts_with('...') {
		prefix_len, prefix_kind = 3, .variadic
	} else if text.starts_with('mut ') {
		prefix_len, prefix_kind = 4, .mutable_type
	} else if text.starts_with('shared ') {
		prefix_len, prefix_kind = 7, .shared_type
	} else if text.starts_with('atomic ') {
		prefix_len, prefix_kind = 7, .atomic_type
	} else if text.starts_with('chan ') {
		prefix_len, prefix_kind = 5, .channel
	} else if text.starts_with('thread ') {
		prefix_len, prefix_kind = 7, .thread
	}
	if prefix_len > 0 {
		child := source_type_query_trim(text[prefix_len..])
		if child.len == 0 {
			return .invalid, 0, source_type_syntax_empty_ids(), false
		}
		return prefix_kind, source_type_id(child), source_type_syntax_empty_ids(), false
	}
	if text.starts_with('map[') {
		end := source_type_syntax_close(text, 3)
		if end < 0 || end + 1 == text.len {
			return .invalid, 0, source_type_syntax_empty_ids(), false
		}
		key := source_type_id(source_type_query_trim(text[4..end]))
		value := source_type_id(source_type_query_trim(text[end + 1..]))
		if key == 0 || value == 0 {
			return .invalid, 0, source_type_syntax_empty_ids(), false
		}
		return .map_type, value, [key], false
	}
	if text[0] == `[` {
		end := source_type_syntax_close(text, 0)
		if end < 0 || end + 1 == text.len {
			return .invalid, 0, source_type_syntax_empty_ids(), false
		}
		child := source_type_id(source_type_query_trim(text[end + 1..]))
		if child == 0 {
			return .invalid, 0, source_type_syntax_empty_ids(), false
		}
		if end == 1 {
			return .array, child, source_type_syntax_empty_ids(), false
		}
		return .fixed_array, child, [source_type_id(text[1..end])], false
	}
	if text.starts_with('fn') {
		tail := source_type_query_trim(text[2..])
		if tail.starts_with('(') {
			end := source_type_syntax_close(tail, 0)
			if end < 0 {
				return .invalid, 0, source_type_syntax_empty_ids(), false
			}
			valid, params := source_type_syntax_arguments(tail[1..end])
			if !valid {
				return .invalid, 0, source_type_syntax_empty_ids(), false
			}
			result := source_type_id(source_type_query_trim(tail[end + 1..]))
			return .function_type, if result == 0 { source_type_void } else { result }, params, result != 0
		}
	}
	if text[0] == `(` && source_type_syntax_close(text, 0) == text.len - 1 {
		valid, components := source_type_syntax_arguments(text[1..text.len - 1])
		if !valid {
			return .invalid, 0, source_type_syntax_empty_ids(), false
		}
		return .tuple, 0, components, false
	}
	open := text.index_u8(`[`)
	if open > 0 && source_type_syntax_close(text, open) == text.len - 1 {
		head := source_type_id(source_type_query_trim(text[..open]))
		valid, components := source_type_syntax_arguments(text[open + 1..text.len - 1])
		if !valid || head == 0 || components.len == 0 {
			return .invalid, 0, source_type_syntax_empty_ids(), false
		}
		return .generic, head, components, false
	}
	return .name, 0, source_type_syntax_empty_ids(), false
}
