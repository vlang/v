@[has_globals]
module flat

import encoding.hex
import math.bits

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
// These components describe spelling syntax, not semantic TypeIds. Flags use two
// bitsets; absent or identity scalars and array views need no payload slots.
pub struct SourceTypeSyntax {
	payload    voidptr
	layout     &SourceTypeSyntaxLayout = source_type_syntax_zero_layout
	bool_flags [2]u64
}

// Layouts describe payload positions, independently of owner IDs and values.
// Narrow scalar identity bits mean the owner ID; the eight int fields use them
// for -1 instead. Integer presence bits have their own dense ordinal namespace.
// Boolean flags stay in each syntax header so their hot accessors need no indirection.
struct SourceTypeSyntaxLayout {
	scalar_present     [2]u64
	scalar_identity    [2]u64
	span_present       u64
	span_identity      u64
	scalar_count       u8
	scalar_first_count u8
	integer_present    u8
	span_offset        u8
}

const source_type_syntax_empty_layout = SourceTypeSyntaxLayout{}
const source_type_syntax_zero_layout = &source_type_syntax_empty_layout

struct SourceTypeSyntaxLayoutEntry {
	layout SourceTypeSyntaxLayout
	hash   u64
	next   &SourceTypeSyntaxLayoutEntry = unsafe { nil }
}

// This index is only read and changed while the spelling writer lock is held.
// Published syntax headers point directly to immutable process-owned layouts.
__global g_source_type_syntax_layouts [4096]voidptr

// source_type_syntax_layout interns metadata while the caller holds the spelling writer lock.
@[direct_array_access]
fn source_type_syntax_layout(layout SourceTypeSyntaxLayout) &SourceTypeSyntaxLayout {
	mut hash := u64(14_695_981_039_346_656_037)
	for value in layout.scalar_present {
		hash = (hash ^ value) * u64(1_099_511_628_211)
	}
	for value in layout.scalar_identity {
		hash = (hash ^ value) * u64(1_099_511_628_211)
	}
	hash = (hash ^ layout.span_present) * u64(1_099_511_628_211)
	hash = (hash ^ layout.span_identity) * u64(1_099_511_628_211)
	hash = (hash ^ (u64(layout.scalar_count) | (u64(layout.scalar_first_count) << 8) |
		(u64(layout.integer_present) << 16) | (u64(layout.span_offset) << 24))) * u64(1_099_511_628_211)
	hash ^= hash >> 32
	slot := int(hash & 4095)
	mut existing := unsafe { &SourceTypeSyntaxLayoutEntry(g_source_type_syntax_layouts[slot]) }
	for !isnil(existing) {
		candidate := &existing.layout
		if existing.hash == hash
			&& candidate.scalar_present[0] == layout.scalar_present[0]
			&& candidate.scalar_present[1] == layout.scalar_present[1]
			&& candidate.scalar_identity[0] == layout.scalar_identity[0]
			&& candidate.scalar_identity[1] == layout.scalar_identity[1]
			&& candidate.span_present == layout.span_present
			&& candidate.span_identity == layout.span_identity
			&& candidate.scalar_count == layout.scalar_count
			&& candidate.scalar_first_count == layout.scalar_first_count
			&& candidate.integer_present == layout.integer_present
			&& candidate.span_offset == layout.span_offset {
			return &existing.layout
		}
		existing = existing.next
	}
	mut entry := unsafe { &SourceTypeSyntaxLayoutEntry(node_payload_alloc_zeroed(sizeof(SourceTypeSyntaxLayoutEntry))) }
	if isnil(entry) { panic('could not allocate source type syntax layout') }
	unsafe {
		*entry = SourceTypeSyntaxLayoutEntry{
			layout: layout
			hash:   hash
			next:   &SourceTypeSyntaxLayoutEntry(g_source_type_syntax_layouts[slot])
		}
		g_source_type_syntax_layouts[slot] = voidptr(entry)
	}
	return &entry.layout
}

// Narrow scalars store only nonzero values that differ from the owner ID.
// Constant field indices let inlining fold the word and bit selection.
@[direct_array_access; inline]
fn (syntax &SourceTypeSyntax) scalar(index int) u32 {
	word := index >> 6
	bit := u64(1) << u32(index & 63)
	if syntax.layout.scalar_identity[word] & bit != 0 {
		return unsafe { *&u32(syntax.payload) }
	}
	present := syntax.layout.scalar_present[word]
	if present & bit == 0 { return 0 }
	offset := bits.ones_count_64(present & (bit - 1)) + if word == 0 {
		0
	} else {
		int(syntax.layout.scalar_first_count)
	}
	return unsafe { (&u32(syntax.payload))[1 + offset] }
}

// Integer scalars retain all signed int bits. Zero and -1 need no payload slot;
// owner-equal integers stay explicit so ID identity never changes their meaning.
@[direct_array_access; inline]
fn (syntax &SourceTypeSyntax) integer(index int, ordinal int) int {
	word := index >> 6
	bit := u64(1) << u32(index & 63)
	if syntax.layout.scalar_identity[word] & bit != 0 { return -1 }
	integer_bit := u8(1) << u32(ordinal)
	present := syntax.layout.integer_present
	if present & integer_bit == 0 { return 0 }
	offset := ((int(syntax.layout.scalar_count) + 2) >> 1) +
		bits.ones_count_8(present & (integer_bit - 1))
	return unsafe { int((&u64(syntax.payload))[offset]) }
}

@[inline]
fn (syntax &SourceTypeSyntax) span(index int) SourceTypeSyntaxSpan {
	bit := u64(1) << u32(index)
	if syntax.layout.span_identity & bit != 0 { return SourceTypeSyntaxSpan{ len: 1 } }
	if syntax.layout.span_present & bit == 0 { return SourceTypeSyntaxSpan{} }
	offset := int(syntax.layout.span_offset) + 2 * bits.ones_count_64(syntax.layout.span_present & (bit - 1))
	// The writer stores these two-u32 records at u32-aligned payload offsets.
	return unsafe { *&SourceTypeSyntaxSpan(&(&u32(syntax.payload))[offset]) }
}

// qualifier_count returns the cached qualifier count query.
@[inline]
fn (syntax &SourceTypeSyntax) qualifier_count() u32 {
	return u32(syntax.scalar(0))
}

// kind returns the cached kind query.
@[inline]
pub fn (syntax &SourceTypeSyntax) kind() SourceTypeSyntaxKind {
	return unsafe { SourceTypeSyntaxKind(u32(syntax.scalar(1))) }
}

// trimmed returns the cached trimmed query.
@[inline]
pub fn (syntax &SourceTypeSyntax) trimmed() u32 {
	return u32(syntax.scalar(2))
}

// base returns the cached base query.
@[inline]
pub fn (syntax &SourceTypeSyntax) base() u32 {
	return u32(syntax.scalar(3))
}

// unqualified returns the cached unqualified query.
@[inline]
pub fn (syntax &SourceTypeSyntax) unqualified() u32 {
	return u32(syntax.scalar(4))
}

// qualifier returns the cached qualifier query.
@[inline]
pub fn (syntax &SourceTypeSyntax) qualifier() u32 {
	return u32(syntax.scalar(5))
}

// generic_base returns the cached generic base query.
@[inline]
pub fn (syntax &SourceTypeSyntax) generic_base() u32 {
	return u32(syntax.scalar(6))
}

// pointer_base returns the cached pointer base query.
@[inline]
pub fn (syntax &SourceTypeSyntax) pointer_base() u32 {
	return u32(syntax.scalar(7))
}

// pointer_depth returns the cached pointer depth query.
@[inline]
pub fn (syntax &SourceTypeSyntax) pointer_depth() int {
	return syntax.integer(8, 0)
}

// raw_base returns the cached raw base query.
@[inline]
pub fn (syntax &SourceTypeSyntax) raw_base() u32 {
	return u32(syntax.scalar(9))
}

// main_base returns the cached main base query.
@[inline]
pub fn (syntax &SourceTypeSyntax) main_base() u32 {
	return u32(syntax.scalar(10))
}

// application_base returns the cached application base query.
@[inline]
pub fn (syntax &SourceTypeSyntax) application_base() u32 {
	return u32(syntax.scalar(11))
}

// generic_app_base returns the cached generic app base query.
@[inline]
pub fn (syntax &SourceTypeSyntax) generic_app_base() u32 {
	return u32(syntax.scalar(12))
}

// postfix_base returns the cached postfix base query.
@[inline]
pub fn (syntax &SourceTypeSyntax) postfix_base() u32 {
	return u32(syntax.scalar(13))
}

// parameter_type returns the cached parameter type query.
@[inline]
pub fn (syntax &SourceTypeSyntax) parameter_type() u32 {
	return u32(syntax.scalar(14))
}

// parameter_mut_type returns the cached parameter mut type query.
@[inline]
pub fn (syntax &SourceTypeSyntax) parameter_mut_type() u32 {
	return u32(syntax.scalar(15))
}

// bracket_base returns the cached bracket base query.
@[inline]
pub fn (syntax &SourceTypeSyntax) bracket_base() u32 {
	return u32(syntax.scalar(16))
}

// bracket_inner returns the cached bracket inner query.
@[inline]
pub fn (syntax &SourceTypeSyntax) bracket_inner() u32 {
	return u32(syntax.scalar(17))
}

// bracket_raw_inner returns the cached bracket raw inner query.
@[inline]
pub fn (syntax &SourceTypeSyntax) bracket_raw_inner() u32 {
	return u32(syntax.scalar(18))
}

// bracket_prefix returns the cached bracket prefix query.
@[inline]
pub fn (syntax &SourceTypeSyntax) bracket_prefix() u32 {
	return u32(syntax.scalar(19))
}

// function_result returns the cached function result query.
@[inline]
pub fn (syntax &SourceTypeSyntax) function_result() u32 {
	return u32(syntax.scalar(20))
}

// first_bracket_base returns the cached first bracket base query.
@[inline]
pub fn (syntax &SourceTypeSyntax) first_bracket_base() u32 {
	return u32(syntax.scalar(21))
}

// last_space_word returns the cached last space word query.
@[inline]
pub fn (syntax &SourceTypeSyntax) last_space_word() u32 {
	return u32(syntax.scalar(22))
}

// first_dot_prefix returns the cached first dot prefix query.
@[inline]
pub fn (syntax &SourceTypeSyntax) first_dot_prefix() u32 {
	return u32(syntax.scalar(23))
}

// first_dot_suffix returns the cached first dot suffix query.
@[inline]
pub fn (syntax &SourceTypeSyntax) first_dot_suffix() u32 {
	return u32(syntax.scalar(24))
}

// pointer_raw_base returns the cached pointer raw base query.
@[inline]
pub fn (syntax &SourceTypeSyntax) pointer_raw_base() u32 {
	return u32(syntax.scalar(25))
}

// builtin_base returns the cached builtin base query.
@[inline]
pub fn (syntax &SourceTypeSyntax) builtin_base() u32 {
	return u32(syntax.scalar(26))
}

// cgen_app_base returns the cached cgen app base query.
@[inline]
pub fn (syntax &SourceTypeSyntax) cgen_app_base() u32 {
	return u32(syntax.scalar(27))
}

// top_space_head returns the cached top space head query.
@[inline]
pub fn (syntax &SourceTypeSyntax) top_space_head() u32 {
	return u32(syntax.scalar(28))
}

// top_space_tail returns the cached top space tail query.
@[inline]
pub fn (syntax &SourceTypeSyntax) top_space_tail() u32 {
	return u32(syntax.scalar(29))
}

// bracket_suffix returns the cached bracket suffix query.
@[inline]
pub fn (syntax &SourceTypeSyntax) bracket_suffix() u32 {
	return u32(syntax.scalar(30))
}

// bracket_from returns the cached bracket from query.
@[inline]
pub fn (syntax &SourceTypeSyntax) bracket_from() u32 {
	return u32(syntax.scalar(31))
}

// first_bracket_prefix returns the cached first bracket prefix query.
@[inline]
pub fn (syntax &SourceTypeSyntax) first_bracket_prefix() u32 {
	return u32(syntax.scalar(32))
}

// dots_base returns the cached dots base query.
@[inline]
pub fn (syntax &SourceTypeSyntax) dots_base() u32 {
	return u32(syntax.scalar(33))
}

// cgen_raw_app_base returns the cached cgen raw app base query.
@[inline]
pub fn (syntax &SourceTypeSyntax) cgen_raw_app_base() u32 {
	return u32(syntax.scalar(34))
}

// generic_parameter_payload returns the cached generic parameter payload query.
@[inline]
pub fn (syntax &SourceTypeSyntax) generic_parameter_payload() u32 {
	return u32(syntax.scalar(35))
}

// legacy_parameter_type returns the cached legacy parameter type query.
@[inline]
pub fn (syntax &SourceTypeSyntax) legacy_parameter_type() u32 {
	return u32(syntax.scalar(36))
}

// legacy_parameter_mut_type returns the cached legacy parameter mut type query.
@[inline]
pub fn (syntax &SourceTypeSyntax) legacy_parameter_mut_type() u32 {
	return u32(syntax.scalar(37))
}

// first_bracket_length returns the cached first bracket length query.
@[inline]
pub fn (syntax &SourceTypeSyntax) first_bracket_length() u32 {
	return u32(syntax.scalar(38))
}

// c_pointer_base returns the cached c pointer base query.
@[inline]
pub fn (syntax &SourceTypeSyntax) c_pointer_base() u32 {
	return u32(syntax.scalar(39))
}

// source_wrapper_base returns the cached source wrapper base query.
@[inline]
pub fn (syntax &SourceTypeSyntax) source_wrapper_base() u32 {
	return u32(syntax.scalar(40))
}

// declaration_head returns the cached declaration head query.
@[inline]
pub fn (syntax &SourceTypeSyntax) declaration_head() u32 {
	return u32(syntax.scalar(41))
}

// compact_fn_spelling returns the cached compact fn spelling query.
@[inline]
pub fn (syntax &SourceTypeSyntax) compact_fn_spelling() u32 {
	return u32(syntax.scalar(42))
}

// first_bracket_raw_inner returns the cached first bracket raw inner query.
@[inline]
pub fn (syntax &SourceTypeSyntax) first_bracket_raw_inner() u32 {
	return u32(syntax.scalar(43))
}

// typeof_inner returns the cached typeof inner query.
@[inline]
pub fn (syntax &SourceTypeSyntax) typeof_inner() u32 {
	return u32(syntax.scalar(44))
}

// postfix_index_base returns the cached postfix index base query.
@[inline]
pub fn (syntax &SourceTypeSyntax) postfix_index_base() u32 {
	return u32(syntax.scalar(45))
}

// last_bracket_prefix returns the cached last bracket prefix query.
@[inline]
pub fn (syntax &SourceTypeSyntax) last_bracket_prefix() u32 {
	return u32(syntax.scalar(46))
}

// last_bracket_length returns the cached last bracket length query.
@[inline]
pub fn (syntax &SourceTypeSyntax) last_bracket_length() u32 {
	return u32(syntax.scalar(47))
}

// trailing_bracket_prefix returns the cached trailing bracket prefix query.
@[inline]
pub fn (syntax &SourceTypeSyntax) trailing_bracket_prefix() u32 {
	return u32(syntax.scalar(48))
}

// array_chars_base returns the cached array chars base query.
@[inline]
pub fn (syntax &SourceTypeSyntax) array_chars_base() u32 {
	return u32(syntax.scalar(49))
}

// named_variant_owner returns the cached named variant owner query.
@[inline]
pub fn (syntax &SourceTypeSyntax) named_variant_owner() u32 {
	return u32(syntax.scalar(50))
}

// named_variant_name returns the cached named variant name query.
@[inline]
pub fn (syntax &SourceTypeSyntax) named_variant_name() u32 {
	return u32(syntax.scalar(51))
}

// main_unlocked returns the cached main unlocked query.
@[inline]
pub fn (syntax &SourceTypeSyntax) main_unlocked() u32 {
	return u32(syntax.scalar(52))
}

// encoded_fixed_length returns the cached encoded fixed length query.
@[inline]
pub fn (syntax &SourceTypeSyntax) encoded_fixed_length() u32 {
	return u32(syntax.scalar(53))
}

// encoded_fixed_base returns the cached encoded fixed base query.
@[inline]
pub fn (syntax &SourceTypeSyntax) encoded_fixed_base() u32 {
	return u32(syntax.scalar(54))
}

// suffix_bracket_length returns the cached suffix bracket length query.
@[inline]
pub fn (syntax &SourceTypeSyntax) suffix_bracket_length() u32 {
	return u32(syntax.scalar(55))
}

// bracket_source_inner returns the cached bracket source inner query.
@[inline]
pub fn (syntax &SourceTypeSyntax) bracket_source_inner() u32 {
	return u32(syntax.scalar(56))
}

// c_fixed_typedef_element returns the cached c fixed typedef element query.
@[inline]
pub fn (syntax &SourceTypeSyntax) c_fixed_typedef_element() u32 {
	return u32(syntax.scalar(57))
}

// c_fixed_typedef_length returns the cached c fixed typedef length query.
@[inline]
pub fn (syntax &SourceTypeSyntax) c_fixed_typedef_length() u32 {
	return u32(syntax.scalar(58))
}

// fixed_array_query_length returns the cached fixed array query length query.
@[inline]
pub fn (syntax &SourceTypeSyntax) fixed_array_query_length() u32 {
	return u32(syntax.scalar(59))
}

// last_bracket_span_inner returns the cached last bracket span inner query.
@[inline]
pub fn (syntax &SourceTypeSyntax) last_bracket_span_inner() u32 {
	return u32(syntax.scalar(60))
}

// generic_app_legacy_suffix returns the cached generic app legacy suffix query.
@[inline]
pub fn (syntax &SourceTypeSyntax) generic_app_legacy_suffix() u32 {
	return u32(syntax.scalar(61))
}

// wrapper_prefix returns the cached wrapper prefix query.
@[inline]
pub fn (syntax &SourceTypeSyntax) wrapper_prefix() u32 {
	return u32(syntax.scalar(62))
}

// wrapper_base returns the cached wrapper base query.
@[inline]
pub fn (syntax &SourceTypeSyntax) wrapper_base() u32 {
	return u32(syntax.scalar(63))
}

// qualified_modifiers returns the cached qualified modifiers query.
@[inline]
pub fn (syntax &SourceTypeSyntax) qualified_modifiers() u32 {
	return u32(syntax.scalar(64))
}

// c_array_payload returns the cached c array payload query.
@[inline]
pub fn (syntax &SourceTypeSyntax) c_array_payload() u32 {
	return u32(syntax.scalar(65))
}

// c_fixed_array_payload returns the cached c fixed array payload query.
@[inline]
pub fn (syntax &SourceTypeSyntax) c_fixed_array_payload() u32 {
	return u32(syntax.scalar(66))
}

// fastc_array_marker_payload returns the cached fastc array marker payload query.
@[inline]
pub fn (syntax &SourceTypeSyntax) fastc_array_marker_payload() u32 {
	return u32(syntax.scalar(67))
}

// c_map_payload returns the cached c map payload query.
@[inline]
pub fn (syntax &SourceTypeSyntax) c_map_payload() u32 {
	return u32(syntax.scalar(68))
}

// c_ptr_name_base returns the cached c ptr name base query.
@[inline]
pub fn (syntax &SourceTypeSyntax) c_ptr_name_base() u32 {
	return u32(syntax.scalar(69))
}

// c_tagged_type returns the cached c tagged type query.
@[inline]
pub fn (syntax &SourceTypeSyntax) c_tagged_type() u32 {
	return u32(syntax.scalar(70))
}

// backward_bracket_prefix returns the cached backward bracket prefix query.
@[inline]
pub fn (syntax &SourceTypeSyntax) backward_bracket_prefix() u32 {
	return u32(syntax.scalar(71))
}

// byte_len returns the cached byte len query.
@[inline]
pub fn (syntax &SourceTypeSyntax) byte_len() int {
	return syntax.integer(72, 1)
}

// receiver_app_base returns the cached receiver app base query.
@[inline]
pub fn (syntax &SourceTypeSyntax) receiver_app_base() u32 {
	return u32(syntax.scalar(73))
}

// declaration_parameter_payload returns the cached declaration parameter payload query.
@[inline]
pub fn (syntax &SourceTypeSyntax) declaration_parameter_payload() u32 {
	return u32(syntax.scalar(74))
}

// prefix_dimension_tail returns the cached prefix dimension tail query.
@[inline]
pub fn (syntax &SourceTypeSyntax) prefix_dimension_tail() u32 {
	return u32(syntax.scalar(75))
}

// backward_bracket_inner returns the cached backward bracket inner query.
@[inline]
pub fn (syntax &SourceTypeSyntax) backward_bracket_inner() u32 {
	return u32(syntax.scalar(76))
}

// nested_option_result_marker returns the cached nested option result marker query.
@[inline]
pub fn (syntax &SourceTypeSyntax) nested_option_result_marker() int {
	return syntax.integer(77, 2)
}

// c_pointer_depth returns the cached c pointer depth query.
@[inline]
pub fn (syntax &SourceTypeSyntax) c_pointer_depth() int {
	return syntax.integer(78, 3)
}

// c_pointer_count returns the cached c pointer count query.
@[inline]
pub fn (syntax &SourceTypeSyntax) c_pointer_count() int {
	return syntax.integer(79, 4)
}

// c_pointer_raw_base returns the cached c pointer raw base query.
@[inline]
pub fn (syntax &SourceTypeSyntax) c_pointer_raw_base() u32 {
	return u32(syntax.scalar(80))
}

// thread_value_payload returns the cached thread value payload query.
@[inline]
pub fn (syntax &SourceTypeSyntax) thread_value_payload() u32 {
	return u32(syntax.scalar(81))
}

// thread_value_type returns the cached thread value type query.
@[inline]
pub fn (syntax &SourceTypeSyntax) thread_value_type() u32 {
	return u32(syntax.scalar(82))
}

// expr_id returns the cached expr id query.
@[inline]
pub fn (syntax &SourceTypeSyntax) expr_id() int {
	return syntax.integer(83, 5)
}

// parameter_name returns the cached parameter name query.
@[inline]
pub fn (syntax &SourceTypeSyntax) parameter_name() u32 {
	return u32(syntax.scalar(84))
}

// receiver_collection_id returns the cached receiver collection id query.
@[inline]
pub fn (syntax &SourceTypeSyntax) receiver_collection_id() u32 {
	return u32(syntax.scalar(85))
}

// ampersand_trimmed returns the cached ampersand trimmed query.
@[inline]
pub fn (syntax &SourceTypeSyntax) ampersand_trimmed() u32 {
	return u32(syntax.scalar(86))
}

// parallel_preseed_short returns the cached parallel preseed short query.
@[inline]
pub fn (syntax &SourceTypeSyntax) parallel_preseed_short() u32 {
	return u32(syntax.scalar(87))
}

// constraint_receiver_base returns the cached constraint receiver base query.
@[inline]
pub fn (syntax &SourceTypeSyntax) constraint_receiver_base() u32 {
	return u32(syntax.scalar(88))
}

// space_removed returns the cached space removed query.
@[inline]
pub fn (syntax &SourceTypeSyntax) space_removed() u32 {
	return u32(syntax.scalar(89))
}

// field_owner_module returns the cached field owner module query.
@[inline]
pub fn (syntax &SourceTypeSyntax) field_owner_module() u32 {
	return u32(syntax.scalar(90))
}

// question_trimmed returns the cached question trimmed query.
@[inline]
pub fn (syntax &SourceTypeSyntax) question_trimmed() u32 {
	return u32(syntax.scalar(91))
}

// function_space_result returns the cached function space result query.
@[inline]
pub fn (syntax &SourceTypeSyntax) function_space_result() u32 {
	return u32(syntax.scalar(92))
}

// decimal_value returns the cached decimal value query.
@[inline]
pub fn (syntax &SourceTypeSyntax) decimal_value() int {
	return syntax.integer(93, 6)
}

// integer_value returns the cached integer value query.
@[inline]
pub fn (syntax &SourceTypeSyntax) integer_value() int {
	return syntax.integer(94, 7)
}

// main_at_base returns the cached main at base query.
@[inline]
pub fn (syntax &SourceTypeSyntax) main_at_base() u32 {
	return u32(syntax.scalar(95))
}

// receiver_app_inner returns the cached receiver app inner query.
@[inline]
pub fn (syntax &SourceTypeSyntax) receiver_app_inner() u32 {
	return u32(syntax.scalar(96))
}

// fastc_fixed_array_length returns the cached fastc fixed array length query.
@[inline]
pub fn (syntax &SourceTypeSyntax) fastc_fixed_array_length() u32 {
	return u32(syntax.scalar(97))
}

// first_bracket_source_inner returns the cached first bracket source inner query.
@[inline]
pub fn (syntax &SourceTypeSyntax) first_bracket_source_inner() u32 {
	return u32(syntax.scalar(98))
}

// typ_suffix_base returns the cached typ suffix base query.
@[inline]
pub fn (syntax &SourceTypeSyntax) typ_suffix_base() u32 {
	return u32(syntax.scalar(99))
}

// checker_parse_function_result returns the cached checker parse function result query.
@[inline]
pub fn (syntax &SourceTypeSyntax) checker_parse_function_result() u32 {
	return u32(syntax.scalar(100))
}

// top_level_dot_prefix returns the cached top level dot prefix query.
@[inline]
pub fn (syntax &SourceTypeSyntax) top_level_dot_prefix() u32 {
	return u32(syntax.scalar(101))
}

// top_level_dot_suffix returns the cached top level dot suffix query.
@[inline]
pub fn (syntax &SourceTypeSyntax) top_level_dot_suffix() u32 {
	return u32(syntax.scalar(102))
}

// parser_fn_decl_params returns the cached parser fn decl params query.
@[inline]
pub fn (syntax &SourceTypeSyntax) parser_fn_decl_params() u32 {
	return u32(syntax.scalar(103))
}

// parser_fn_decl_result returns the cached parser fn decl result query.
@[inline]
pub fn (syntax &SourceTypeSyntax) parser_fn_decl_result() u32 {
	return u32(syntax.scalar(104))
}

// named_variant_dot_prefix returns the cached named variant dot prefix query.
@[inline]
pub fn (syntax &SourceTypeSyntax) named_variant_dot_prefix() u32 {
	return u32(syntax.scalar(105))
}

// named_variant_dot_suffix returns the cached named variant dot suffix query.
@[inline]
pub fn (syntax &SourceTypeSyntax) named_variant_dot_suffix() u32 {
	return u32(syntax.scalar(106))
}

// function_has_result returns the cached function has result query.
@[inline]
pub fn (syntax &SourceTypeSyntax) function_has_result() bool {
	return syntax.bool_flags[0] & (u64(1) << 0) != 0
}

// bare_generic returns the cached bare generic query.
@[inline]
pub fn (syntax &SourceTypeSyntax) bare_generic() bool {
	return syntax.bool_flags[0] & (u64(1) << 1) != 0
}

// application_valid returns the cached application valid query.
@[inline]
pub fn (syntax &SourceTypeSyntax) application_valid() bool {
	return syntax.bool_flags[0] & (u64(1) << 2) != 0
}

// generic_app_valid returns the cached generic app valid query.
@[inline]
pub fn (syntax &SourceTypeSyntax) generic_app_valid() bool {
	return syntax.bool_flags[0] & (u64(1) << 3) != 0
}

// bracket_closed returns the cached bracket closed query.
@[inline]
pub fn (syntax &SourceTypeSyntax) bracket_closed() bool {
	return syntax.bool_flags[0] & (u64(1) << 4) != 0
}

// function_closed returns the cached function closed query.
@[inline]
pub fn (syntax &SourceTypeSyntax) function_closed() bool {
	return syntax.bool_flags[0] & (u64(1) << 5) != 0
}

// tuple_like returns the cached tuple like query.
@[inline]
pub fn (syntax &SourceTypeSyntax) tuple_like() bool {
	return syntax.bool_flags[0] & (u64(1) << 6) != 0
}

// function_space_prefix returns the cached function space prefix query.
@[inline]
pub fn (syntax &SourceTypeSyntax) function_space_prefix() bool {
	return syntax.bool_flags[0] & (u64(1) << 7) != 0
}

// spaced returns the cached spaced query.
@[inline]
pub fn (syntax &SourceTypeSyntax) spaced() bool {
	return syntax.bool_flags[0] & (u64(1) << 8) != 0
}

// first_bracket_closed returns the cached first bracket closed query.
@[inline]
pub fn (syntax &SourceTypeSyntax) first_bracket_closed() bool {
	return syntax.bool_flags[0] & (u64(1) << 9) != 0
}

// named_type returns the cached named type query.
@[inline]
pub fn (syntax &SourceTypeSyntax) named_type() bool {
	return syntax.bool_flags[0] & (u64(1) << 10) != 0
}

// c_qualified returns the cached c qualified query.
@[inline]
pub fn (syntax &SourceTypeSyntax) c_qualified() bool {
	return syntax.bool_flags[0] & (u64(1) << 11) != 0
}

// builtin_prefix returns the cached builtin prefix query.
@[inline]
pub fn (syntax &SourceTypeSyntax) builtin_prefix() bool {
	return syntax.bool_flags[0] & (u64(1) << 12) != 0
}

// main_fragment returns the cached main fragment query.
@[inline]
pub fn (syntax &SourceTypeSyntax) main_fragment() bool {
	return syntax.bool_flags[0] & (u64(1) << 13) != 0
}

// function_prefix returns the cached function prefix query.
@[inline]
pub fn (syntax &SourceTypeSyntax) function_prefix() bool {
	return syntax.bool_flags[0] & (u64(1) << 14) != 0
}

// fn_prefix returns the cached fn prefix query.
@[inline]
pub fn (syntax &SourceTypeSyntax) fn_prefix() bool {
	return syntax.bool_flags[0] & (u64(1) << 15) != 0
}

// shared_fragment returns the cached shared fragment query.
@[inline]
pub fn (syntax &SourceTypeSyntax) shared_fragment() bool {
	return syntax.bool_flags[0] & (u64(1) << 16) != 0
}

// cgen_app_valid returns the cached cgen app valid query.
@[inline]
pub fn (syntax &SourceTypeSyntax) cgen_app_valid() bool {
	return syntax.bool_flags[0] & (u64(1) << 17) != 0
}

// tuple_contains_comma returns the cached tuple contains comma query.
@[inline]
pub fn (syntax &SourceTypeSyntax) tuple_contains_comma() bool {
	return syntax.bool_flags[0] & (u64(1) << 18) != 0
}

// top_space_valid returns the cached top space valid query.
@[inline]
pub fn (syntax &SourceTypeSyntax) top_space_valid() bool {
	return syntax.bool_flags[0] & (u64(1) << 19) != 0
}

// typeof_expr_prefix returns the cached typeof expr prefix query.
@[inline]
pub fn (syntax &SourceTypeSyntax) typeof_expr_prefix() bool {
	return syntax.bool_flags[0] & (u64(1) << 20) != 0
}

// bracket_inner_fixed_length returns the cached bracket inner fixed length query.
@[inline]
pub fn (syntax &SourceTypeSyntax) bracket_inner_fixed_length() bool {
	return syntax.bool_flags[0] & (u64(1) << 21) != 0
}

// void_pointer_postfix returns the cached void pointer postfix query.
@[inline]
pub fn (syntax &SourceTypeSyntax) void_pointer_postfix() bool {
	return syntax.bool_flags[0] & (u64(1) << 22) != 0
}

// canonical_dotted_name returns the cached canonical dotted name query.
@[inline]
pub fn (syntax &SourceTypeSyntax) canonical_dotted_name() bool {
	return syntax.bool_flags[0] & (u64(1) << 23) != 0
}

// decimal returns the cached decimal query.
@[inline]
pub fn (syntax &SourceTypeSyntax) decimal() bool {
	return syntax.bool_flags[0] & (u64(1) << 24) != 0
}

// contains_comma returns the cached contains comma query.
@[inline]
pub fn (syntax &SourceTypeSyntax) contains_comma() bool {
	return syntax.bool_flags[0] & (u64(1) << 25) != 0
}

// function_fragment returns the cached function fragment query.
@[inline]
pub fn (syntax &SourceTypeSyntax) function_fragment() bool {
	return syntax.bool_flags[0] & (u64(1) << 26) != 0
}

// cgen_raw_app_valid returns the cached cgen raw app valid query.
@[inline]
pub fn (syntax &SourceTypeSyntax) cgen_raw_app_valid() bool {
	return syntax.bool_flags[0] & (u64(1) << 27) != 0
}

// c_ptr_name_suffix returns the cached c ptr name suffix query.
@[inline]
pub fn (syntax &SourceTypeSyntax) c_ptr_name_suffix() bool {
	return syntax.bool_flags[0] & (u64(1) << 28) != 0
}

// c_fn_type_fragment returns the cached c fn type fragment query.
@[inline]
pub fn (syntax &SourceTypeSyntax) c_fn_type_fragment() bool {
	return syntax.bool_flags[0] & (u64(1) << 29) != 0
}

// underscored returns the cached underscored query.
@[inline]
pub fn (syntax &SourceTypeSyntax) underscored() bool {
	return syntax.bool_flags[0] & (u64(1) << 30) != 0
}

// fn_fragment returns the cached fn fragment query.
@[inline]
pub fn (syntax &SourceTypeSyntax) fn_fragment() bool {
	return syntax.bool_flags[0] & (u64(1) << 31) != 0
}

// cgen_generic_placeholder returns the cached cgen generic placeholder query.
@[inline]
pub fn (syntax &SourceTypeSyntax) cgen_generic_placeholder() bool {
	return syntax.bool_flags[0] & (u64(1) << 32) != 0
}

// fixed_array_type returns the cached fixed array type query.
@[inline]
pub fn (syntax &SourceTypeSyntax) fixed_array_type() bool {
	return syntax.bool_flags[0] & (u64(1) << 33) != 0
}

// typeof_valid returns the cached typeof valid query.
@[inline]
pub fn (syntax &SourceTypeSyntax) typeof_valid() bool {
	return syntax.bool_flags[0] & (u64(1) << 34) != 0
}

// trailing_bracket_closed returns the cached trailing bracket closed query.
@[inline]
pub fn (syntax &SourceTypeSyntax) trailing_bracket_closed() bool {
	return syntax.bool_flags[0] & (u64(1) << 35) != 0
}

// lowercase_value_ident returns the cached lowercase value ident query.
@[inline]
pub fn (syntax &SourceTypeSyntax) lowercase_value_ident() bool {
	return syntax.bool_flags[0] & (u64(1) << 36) != 0
}

// typeof_prefix returns the cached typeof prefix query.
@[inline]
pub fn (syntax &SourceTypeSyntax) typeof_prefix() bool {
	return syntax.bool_flags[0] & (u64(1) << 37) != 0
}

// first_capital returns the cached first capital query.
@[inline]
pub fn (syntax &SourceTypeSyntax) first_capital() bool {
	return syntax.bool_flags[0] & (u64(1) << 38) != 0
}

// named_variant returns the cached named variant query.
@[inline]
pub fn (syntax &SourceTypeSyntax) named_variant() bool {
	return syntax.bool_flags[0] & (u64(1) << 39) != 0
}

// named_variant_valid returns the cached named variant valid query.
@[inline]
pub fn (syntax &SourceTypeSyntax) named_variant_valid() bool {
	return syntax.bool_flags[0] & (u64(1) << 40) != 0
}

// map_name_prefix returns the cached map name prefix query.
@[inline]
pub fn (syntax &SourceTypeSyntax) map_name_prefix() bool {
	return syntax.bool_flags[0] & (u64(1) << 41) != 0
}

// suffix_bracket_valid returns the cached suffix bracket valid query.
@[inline]
pub fn (syntax &SourceTypeSyntax) suffix_bracket_valid() bool {
	return syntax.bool_flags[0] & (u64(1) << 42) != 0
}

// map_function_fragment returns the cached map function fragment query.
@[inline]
pub fn (syntax &SourceTypeSyntax) map_function_fragment() bool {
	return syntax.bool_flags[0] & (u64(1) << 43) != 0
}

// c_fixed_typedef_valid returns the cached c fixed typedef valid query.
@[inline]
pub fn (syntax &SourceTypeSyntax) c_fixed_typedef_valid() bool {
	return syntax.bool_flags[0] & (u64(1) << 44) != 0
}

// last_bracket_span_valid returns the cached last bracket span valid query.
@[inline]
pub fn (syntax &SourceTypeSyntax) last_bracket_span_valid() bool {
	return syntax.bool_flags[0] & (u64(1) << 45) != 0
}

// plain_type_name returns the cached plain type name query.
@[inline]
pub fn (syntax &SourceTypeSyntax) plain_type_name() bool {
	return syntax.bool_flags[0] & (u64(1) << 46) != 0
}

// checker_fixed_array_may_be returns the cached checker fixed array may be query.
@[inline]
pub fn (syntax &SourceTypeSyntax) checker_fixed_array_may_be() bool {
	return syntax.bool_flags[0] & (u64(1) << 47) != 0
}

// dollar_prefix returns the cached dollar prefix query.
@[inline]
pub fn (syntax &SourceTypeSyntax) dollar_prefix() bool {
	return syntax.bool_flags[0] & (u64(1) << 48) != 0
}

// define_prefix returns the cached define prefix query.
@[inline]
pub fn (syntax &SourceTypeSyntax) define_prefix() bool {
	return syntax.bool_flags[0] & (u64(1) << 49) != 0
}

// define_fragment returns the cached define fragment query.
@[inline]
pub fn (syntax &SourceTypeSyntax) define_fragment() bool {
	return syntax.bool_flags[0] & (u64(1) << 50) != 0
}

// paren_prefix returns the cached paren prefix query.
@[inline]
pub fn (syntax &SourceTypeSyntax) paren_prefix() bool {
	return syntax.bool_flags[0] & (u64(1) << 51) != 0
}

// fixed_length returns the cached fixed length query.
@[inline]
pub fn (syntax &SourceTypeSyntax) fixed_length() bool {
	return syntax.bool_flags[0] & (u64(1) << 52) != 0
}

// plain_alias_target returns the cached plain alias target query.
@[inline]
pub fn (syntax &SourceTypeSyntax) plain_alias_target() bool {
	return syntax.bool_flags[0] & (u64(1) << 53) != 0
}

// fastc_array_marker returns the cached fastc array marker query.
@[inline]
pub fn (syntax &SourceTypeSyntax) fastc_array_marker() bool {
	return syntax.bool_flags[0] & (u64(1) << 54) != 0
}

// backward_bracket_closed returns the cached backward bracket closed query.
@[inline]
pub fn (syntax &SourceTypeSyntax) backward_bracket_closed() bool {
	return syntax.bool_flags[0] & (u64(1) << 55) != 0
}

// opening_paren returns the cached opening paren query.
@[inline]
pub fn (syntax &SourceTypeSyntax) opening_paren() bool {
	return syntax.bool_flags[0] & (u64(1) << 56) != 0
}

// receiver_app_valid returns the cached receiver app valid query.
@[inline]
pub fn (syntax &SourceTypeSyntax) receiver_app_valid() bool {
	return syntax.bool_flags[0] & (u64(1) << 57) != 0
}

// declaration_parameter_mut returns the cached declaration parameter mut query.
@[inline]
pub fn (syntax &SourceTypeSyntax) declaration_parameter_mut() bool {
	return syntax.bool_flags[0] & (u64(1) << 58) != 0
}

// prefix_dimensions_closed returns the cached prefix dimensions closed query.
@[inline]
pub fn (syntax &SourceTypeSyntax) prefix_dimensions_closed() bool {
	return syntax.bool_flags[0] & (u64(1) << 59) != 0
}

// backward_bracket_after_prefix_dims returns the cached backward bracket after prefix dims query.
@[inline]
pub fn (syntax &SourceTypeSyntax) backward_bracket_after_prefix_dims() bool {
	return syntax.bool_flags[0] & (u64(1) << 60) != 0
}

// option_result_void_prefix returns the cached option result void prefix query.
@[inline]
pub fn (syntax &SourceTypeSyntax) option_result_void_prefix() bool {
	return syntax.bool_flags[0] & (u64(1) << 61) != 0
}

// thread_value_prefix returns the cached thread value prefix query.
@[inline]
pub fn (syntax &SourceTypeSyntax) thread_value_prefix() bool {
	return syntax.bool_flags[0] & (u64(1) << 62) != 0
}

// thread_value_type_valid returns the cached thread value type valid query.
@[inline]
pub fn (syntax &SourceTypeSyntax) thread_value_type_valid() bool {
	return syntax.bool_flags[0] & (u64(1) << 63) != 0
}

// contains_closing_bracket returns the cached contains closing bracket query.
@[inline]
pub fn (syntax &SourceTypeSyntax) contains_closing_bracket() bool {
	return syntax.bool_flags[1] & (u64(1) << 0) != 0
}

// atomic_val_fragment returns the cached atomic val fragment query.
@[inline]
pub fn (syntax &SourceTypeSyntax) atomic_val_fragment() bool {
	return syntax.bool_flags[1] & (u64(1) << 1) != 0
}

// expr_prefix returns the cached expr prefix query.
@[inline]
pub fn (syntax &SourceTypeSyntax) expr_prefix() bool {
	return syntax.bool_flags[1] & (u64(1) << 2) != 0
}

// parameter_named returns the cached parameter named query.
@[inline]
pub fn (syntax &SourceTypeSyntax) parameter_named() bool {
	return syntax.bool_flags[1] & (u64(1) << 3) != 0
}

// parameter_is_mut returns the cached parameter is mut query.
@[inline]
pub fn (syntax &SourceTypeSyntax) parameter_is_mut() bool {
	return syntax.bool_flags[1] & (u64(1) << 4) != 0
}

// transform_fixed_length returns the cached transform fixed length query.
@[inline]
pub fn (syntax &SourceTypeSyntax) transform_fixed_length() bool {
	return syntax.bool_flags[1] & (u64(1) << 5) != 0
}

// contains_doubleunderscore returns the cached contains doubleunderscore query.
@[inline]
pub fn (syntax &SourceTypeSyntax) contains_doubleunderscore() bool {
	return syntax.bool_flags[1] & (u64(1) << 6) != 0
}

// parallel_preseed_fn returns the cached parallel preseed fn query.
@[inline]
pub fn (syntax &SourceTypeSyntax) parallel_preseed_fn() bool {
	return syntax.bool_flags[1] & (u64(1) << 7) != 0
}

// closing_bracket returns the cached closing bracket query.
@[inline]
pub fn (syntax &SourceTypeSyntax) closing_bracket() bool {
	return syntax.bool_flags[1] & (u64(1) << 8) != 0
}

// result_suffix returns the cached result suffix query.
@[inline]
pub fn (syntax &SourceTypeSyntax) result_suffix() bool {
	return syntax.bool_flags[1] & (u64(1) << 9) != 0
}

// cgen_decl_unusable returns the cached cgen decl unusable query.
@[inline]
pub fn (syntax &SourceTypeSyntax) cgen_decl_unusable() bool {
	return syntax.bool_flags[1] & (u64(1) << 10) != 0
}

// cgen_decl_authority returns the cached cgen decl authority query.
@[inline]
pub fn (syntax &SourceTypeSyntax) cgen_decl_authority() bool {
	return syntax.bool_flags[1] & (u64(1) << 11) != 0
}

// generic_encoded_fragment returns the cached generic encoded fragment query.
@[inline]
pub fn (syntax &SourceTypeSyntax) generic_encoded_fragment() bool {
	return syntax.bool_flags[1] & (u64(1) << 12) != 0
}

// fn_ptr_encoded_prefix returns the cached fn ptr encoded prefix query.
@[inline]
pub fn (syntax &SourceTypeSyntax) fn_ptr_encoded_prefix() bool {
	return syntax.bool_flags[1] & (u64(1) << 13) != 0
}

// c_result_prefix returns the cached c result prefix query.
@[inline]
pub fn (syntax &SourceTypeSyntax) c_result_prefix() bool {
	return syntax.bool_flags[1] & (u64(1) << 14) != 0
}

// c_option_prefix returns the cached c option prefix query.
@[inline]
pub fn (syntax &SourceTypeSyntax) c_option_prefix() bool {
	return syntax.bool_flags[1] & (u64(1) << 15) != 0
}

// c_multi_return_prefix returns the cached c multi return prefix query.
@[inline]
pub fn (syntax &SourceTypeSyntax) c_multi_return_prefix() bool {
	return syntax.bool_flags[1] & (u64(1) << 16) != 0
}

// c_lower_map_prefix returns the cached c lower map prefix query.
@[inline]
pub fn (syntax &SourceTypeSyntax) c_lower_map_prefix() bool {
	return syntax.bool_flags[1] & (u64(1) << 17) != 0
}

// open_generic_receiver_marker returns the cached open generic receiver marker query.
@[inline]
pub fn (syntax &SourceTypeSyntax) open_generic_receiver_marker() bool {
	return syntax.bool_flags[1] & (u64(1) << 18) != 0
}

// qualified_name_chars returns the cached qualified name chars query.
@[inline]
pub fn (syntax &SourceTypeSyntax) qualified_name_chars() bool {
	return syntax.bool_flags[1] & (u64(1) << 19) != 0
}

// const_name_prefix returns the cached const name prefix query.
@[inline]
pub fn (syntax &SourceTypeSyntax) const_name_prefix() bool {
	return syntax.bool_flags[1] & (u64(1) << 20) != 0
}

// pthread_prefix returns the cached pthread prefix query.
@[inline]
pub fn (syntax &SourceTypeSyntax) pthread_prefix() bool {
	return syntax.bool_flags[1] & (u64(1) << 21) != 0
}

// thread_bracket_prefix returns the cached thread bracket prefix query.
@[inline]
pub fn (syntax &SourceTypeSyntax) thread_bracket_prefix() bool {
	return syntax.bool_flags[1] & (u64(1) << 22) != 0
}

// fixed_typedef_bracket returns the cached fixed typedef bracket query.
@[inline]
pub fn (syntax &SourceTypeSyntax) fixed_typedef_bracket() bool {
	return syntax.bool_flags[1] & (u64(1) << 23) != 0
}

// c_array_fixed_prefix returns the cached c array fixed prefix query.
@[inline]
pub fn (syntax &SourceTypeSyntax) c_array_fixed_prefix() bool {
	return syntax.bool_flags[1] & (u64(1) << 24) != 0
}

// encoded_alnum_dotted returns the cached encoded alnum dotted query.
@[inline]
pub fn (syntax &SourceTypeSyntax) encoded_alnum_dotted() bool {
	return syntax.bool_flags[1] & (u64(1) << 25) != 0
}

// internal_symbol_prefix returns the cached internal symbol prefix query.
@[inline]
pub fn (syntax &SourceTypeSyntax) internal_symbol_prefix() bool {
	return syntax.bool_flags[1] & (u64(1) << 26) != 0
}

// incremental_generic_application returns the cached incremental generic application query.
@[inline]
pub fn (syntax &SourceTypeSyntax) incremental_generic_application() bool {
	return syntax.bool_flags[1] & (u64(1) << 27) != 0
}

// system_error_suffix returns the cached system error suffix query.
@[inline]
pub fn (syntax &SourceTypeSyntax) system_error_suffix() bool {
	return syntax.bool_flags[1] & (u64(1) << 28) != 0
}

// fastc_fixed_array_length_valid returns the cached fastc fixed array length valid query.
@[inline]
pub fn (syntax &SourceTypeSyntax) fastc_fixed_array_length_valid() bool {
	return syntax.bool_flags[1] & (u64(1) << 29) != 0
}

// contains_capital returns the cached contains capital query.
@[inline]
pub fn (syntax &SourceTypeSyntax) contains_capital() bool {
	return syntax.bool_flags[1] & (u64(1) << 30) != 0
}

// c_function_pointer_hole returns the cached c function pointer hole query.
@[inline]
pub fn (syntax &SourceTypeSyntax) c_function_pointer_hole() bool {
	return syntax.bool_flags[1] & (u64(1) << 31) != 0
}

// channel_fragment returns the cached channel fragment query.
@[inline]
pub fn (syntax &SourceTypeSyntax) channel_fragment() bool {
	return syntax.bool_flags[1] & (u64(1) << 32) != 0
}

// array_receiver_fragment returns the cached array receiver fragment query.
@[inline]
pub fn (syntax &SourceTypeSyntax) array_receiver_fragment() bool {
	return syntax.bool_flags[1] & (u64(1) << 33) != 0
}

// mangled_open_generic_marker returns the cached mangled open generic marker query.
@[inline]
pub fn (syntax &SourceTypeSyntax) mangled_open_generic_marker() bool {
	return syntax.bool_flags[1] & (u64(1) << 34) != 0
}

// array_suffix returns the cached array suffix query.
@[inline]
pub fn (syntax &SourceTypeSyntax) array_suffix() bool {
	return syntax.bool_flags[1] & (u64(1) << 35) != 0
}

// empty_function_prefix returns the cached empty function prefix query.
@[inline]
pub fn (syntax &SourceTypeSyntax) empty_function_prefix() bool {
	return syntax.bool_flags[1] & (u64(1) << 36) != 0
}

// c_shared_fragment returns the cached c shared fragment query.
@[inline]
pub fn (syntax &SourceTypeSyntax) c_shared_fragment() bool {
	return syntax.bool_flags[1] & (u64(1) << 37) != 0
}

// typ_suffix returns the cached typ suffix query.
@[inline]
pub fn (syntax &SourceTypeSyntax) typ_suffix() bool {
	return syntax.bool_flags[1] & (u64(1) << 38) != 0
}

// checker_parse_function_has_params returns the cached checker parse function has params query.
@[inline]
pub fn (syntax &SourceTypeSyntax) checker_parse_function_has_params() bool {
	return syntax.bool_flags[1] & (u64(1) << 39) != 0
}

// top_level_dot_valid returns the cached top level dot valid query.
@[inline]
pub fn (syntax &SourceTypeSyntax) top_level_dot_valid() bool {
	return syntax.bool_flags[1] & (u64(1) << 40) != 0
}

// parser_fn_decl_valid returns the cached parser fn decl valid query.
@[inline]
pub fn (syntax &SourceTypeSyntax) parser_fn_decl_valid() bool {
	return syntax.bool_flags[1] & (u64(1) << 41) != 0
}

// contains_question returns the cached contains question query.
@[inline]
pub fn (syntax &SourceTypeSyntax) contains_question() bool {
	return syntax.bool_flags[1] & (u64(1) << 42) != 0
}

// contains_union returns the cached contains union query.
@[inline]
pub fn (syntax &SourceTypeSyntax) contains_union() bool {
	return syntax.bool_flags[1] & (u64(1) << 43) != 0
}

// named_variant_dot_valid returns the cached named variant dot valid query.
@[inline]
pub fn (syntax &SourceTypeSyntax) named_variant_dot_valid() bool {
	return syntax.bool_flags[1] & (u64(1) << 44) != 0
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
	return syntax.ids(syntax.span(0))
}

// symbols returns a lazily cached process-owned immutable array view.
@[inline]
pub fn (syntax &SourceTypeSyntax) symbols() []u32 {
	if isnil(syntax.payload) { return syntax.ids(SourceTypeSyntaxSpan{}) }
	if syntax.named_type() { return syntax.ids(SourceTypeSyntaxSpan{ len: 1 }) }
	id := unsafe { *&u32(syntax.payload) }
	return source_type_syntax_lexical_ids(source_type_syntax_lexical(id, .symbols), false)
}

// words returns a lazily cached process-owned immutable array view.
@[inline]
pub fn (syntax &SourceTypeSyntax) words() []u32 {
	if isnil(syntax.payload) { return syntax.ids(SourceTypeSyntaxSpan{}) }
	id := unsafe { *&u32(syntax.payload) }
	if syntax.named_type() && syntax.first_dot_prefix() == id {
		return syntax.ids(SourceTypeSyntaxSpan{ len: 1 })
	}
	return source_type_syntax_lexical_ids(source_type_syntax_lexical(id, .words), false)
}

// application_args returns a process-owned immutable array view.
@[inline]
pub fn (syntax &SourceTypeSyntax) application_args() []u32 {
	return syntax.ids(syntax.span(1))
}

// generic_app_args returns a process-owned immutable array view.
@[inline]
pub fn (syntax &SourceTypeSyntax) generic_app_args() []u32 {
	return syntax.ids(syntax.span(2))
}

// postfix_dims returns a process-owned immutable array view.
@[inline]
pub fn (syntax &SourceTypeSyntax) postfix_dims() []u32 {
	return syntax.ids(syntax.span(3))
}

// function_params returns a process-owned immutable array view.
@[inline]
pub fn (syntax &SourceTypeSyntax) function_params() []u32 {
	return syntax.ids(syntax.span(4))
}

// tuple_components returns a process-owned immutable array view.
@[inline]
pub fn (syntax &SourceTypeSyntax) tuple_components() []u32 {
	return syntax.ids(syntax.span(5))
}

// qualifier_prefixes returns raw prefixes before each dot in process-owned storage.
// A single prefix borrows its immutable source entry; longer views are cached on request.
@[inline]
pub fn (syntax &SourceTypeSyntax) qualifier_prefixes() []u32 {
	if syntax.qualifier_count() == 0 { return syntax.ids(SourceTypeSyntaxSpan{}) }
	if syntax.qualifier_count() == 1 {
		return source_type_syntax_scalar_ids(syntax.first_dot_prefix())
	}
	id := unsafe { *&u32(syntax.payload) }
	return source_type_syntax_lexical_ids(source_type_syntax_lexical(id, .qualifiers), false)
}

// qualifier_suffixes returns raw suffixes after each dot in process-owned storage.
// A single suffix borrows its immutable source entry; longer views are cached on request.
@[inline]
pub fn (syntax &SourceTypeSyntax) qualifier_suffixes() []u32 {
	if syntax.qualifier_count() == 0 { return syntax.ids(SourceTypeSyntaxSpan{}) }
	if syntax.qualifier_count() == 1 {
		return source_type_syntax_scalar_ids(syntax.first_dot_suffix())
	}
	id := unsafe { *&u32(syntax.payload) }
	return source_type_syntax_lexical_ids(source_type_syntax_lexical(id, .qualifiers), true)
}

// cgen_app_args returns a process-owned immutable array view.
@[inline]
pub fn (syntax &SourceTypeSyntax) cgen_app_args() []u32 {
	return syntax.ids(syntax.span(6))
}

// cgen_function_params returns a process-owned immutable array view.
@[inline]
pub fn (syntax &SourceTypeSyntax) cgen_function_params() []u32 {
	return syntax.ids(syntax.span(7))
}

// checker_function_params returns a process-owned immutable array view.
@[inline]
pub fn (syntax &SourceTypeSyntax) checker_function_params() []u32 {
	return syntax.ids(syntax.span(8))
}

// checker_tuple_components returns a process-owned immutable array view.
@[inline]
pub fn (syntax &SourceTypeSyntax) checker_tuple_components() []u32 {
	return syntax.ids(syntax.span(9))
}

// cgen_raw_app_args returns a process-owned immutable array view.
@[inline]
pub fn (syntax &SourceTypeSyntax) cgen_raw_app_args() []u32 {
	return syntax.ids(syntax.span(10))
}

// cgen_parts returns a process-owned immutable array view.
@[inline]
pub fn (syntax &SourceTypeSyntax) cgen_parts() []u32 {
	return syntax.ids(syntax.span(11))
}

// checker_raw_tuple_components returns a process-owned immutable array view.
@[inline]
pub fn (syntax &SourceTypeSyntax) checker_raw_tuple_components() []u32 {
	return syntax.ids(syntax.span(12))
}

// bracket_raw_parts returns a process-owned immutable array view.
@[inline]
pub fn (syntax &SourceTypeSyntax) bracket_raw_parts() []u32 {
	return syntax.ids(syntax.span(13))
}

// bracket_generic_parts returns a process-owned immutable array view.
@[inline]
pub fn (syntax &SourceTypeSyntax) bracket_generic_parts() []u32 {
	return syntax.ids(syntax.span(14))
}

// tuple_function_components returns a process-owned immutable array view.
@[inline]
pub fn (syntax &SourceTypeSyntax) tuple_function_components() []u32 {
	return syntax.ids(syntax.span(15))
}

// plain_comma_parts returns a process-owned immutable array view.
@[inline]
pub fn (syntax &SourceTypeSyntax) plain_comma_parts() []u32 {
	return syntax.ids(syntax.span(16))
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
	if syntax.plain_type_name() || syntax.canonical_dotted_name() {
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
	if syntax.first_dot_prefix() == id { return syntax.ids(SourceTypeSyntaxSpan{ len: 1 }) }
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
	return syntax.ids(syntax.span(17))
}

// receiver_app_args returns a process-owned immutable array view.
@[inline]
pub fn (syntax &SourceTypeSyntax) receiver_app_args() []u32 {
	return syntax.ids(syntax.span(18))
}

// checker_bracket_components returns a process-owned immutable array view.
@[inline]
pub fn (syntax &SourceTypeSyntax) checker_bracket_components() []u32 {
	return syntax.ids(syntax.span(19))
}

// legacy_generic_words returns a lazily cached process-owned immutable array view.
@[inline]
pub fn (syntax &SourceTypeSyntax) legacy_generic_words() []u32 {
	if isnil(syntax.payload) { return syntax.ids(SourceTypeSyntaxSpan{}) }
	if syntax.plain_type_name() { return syntax.ids(SourceTypeSyntaxSpan{ len: 1 }) }
	id := unsafe { *&u32(syntax.payload) }
	return source_type_syntax_lexical_ids(source_type_syntax_lexical(id, .legacy_words), false)
}

// symbols_first_offsets returns a lazily cached process-owned immutable array view.
@[inline]
pub fn (syntax &SourceTypeSyntax) symbols_first_offsets() []u32 {
	if isnil(syntax.payload) { return syntax.ids(SourceTypeSyntaxSpan{}) }
	if syntax.named_type() {
		return source_type_syntax_singleton_ids(&g_source_type_syntax_zero_offset)
	}
	id := unsafe { *&u32(syntax.payload) }
	return source_type_syntax_lexical_ids(source_type_syntax_lexical(id, .symbols), true)
}

// voidptr_parts returns a process-owned immutable array view.
@[inline]
pub fn (syntax &SourceTypeSyntax) voidptr_parts() []u32 {
	return syntax.ids(syntax.span(20))
}

// inferred_parts returns a process-owned immutable array view.
@[inline]
pub fn (syntax &SourceTypeSyntax) inferred_parts() []u32 {
	return syntax.ids(syntax.span(21))
}

// inferred_annotations returns a process-owned immutable array view.
@[inline]
pub fn (syntax &SourceTypeSyntax) inferred_annotations() []u32 {
	return syntax.ids(syntax.span(22))
}

// inferred_expr_ids returns a process-owned immutable array view.
@[inline]
pub fn (syntax &SourceTypeSyntax) inferred_expr_ids() []u32 {
	return syntax.ids(syntax.span(23))
}

// substitution_function_params returns a process-owned immutable array view.
@[inline]
pub fn (syntax &SourceTypeSyntax) substitution_function_params() []u32 {
	return syntax.ids(syntax.span(24))
}

// flattened_receiver_parts returns a lazily cached process-owned immutable array view.
@[inline]
pub fn (syntax &SourceTypeSyntax) flattened_receiver_parts() []u32 {
	if isnil(syntax.payload) { return syntax.ids(SourceTypeSyntaxSpan{}) }
	if !syntax.underscored() { return syntax.ids(SourceTypeSyntaxSpan{ len: 1 }) }
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
	if !syntax.contains_doubleunderscore() {
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
	if !syntax.contains_doubleunderscore() {
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
	if !syntax.contains_doubleunderscore() {
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
	if syntax.byte_len() <= 3 {
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
	if !syntax.main_fragment() {
		return id
	}
	return source_type_composition_one(.main_removed, id)
}

// written_applications returns a process-owned immutable array view.
@[inline]
pub fn (syntax &SourceTypeSyntax) written_applications() []u32 {
	return syntax.ids(syntax.span(25))
}

// written_application_offsets returns a process-owned immutable array view.
@[inline]
pub fn (syntax &SourceTypeSyntax) written_application_offsets() []u32 {
	return syntax.ids(syntax.span(26))
}

// checker_raw_bracket_components returns a process-owned immutable array view.
@[inline]
pub fn (syntax &SourceTypeSyntax) checker_raw_bracket_components() []u32 {
	return syntax.ids(syntax.span(27))
}

// cgen_placeholder_candidates returns a process-owned immutable array view.
@[inline]
pub fn (syntax &SourceTypeSyntax) cgen_placeholder_candidates() []u32 {
	return syntax.ids(syntax.span(28))
}

// reflection_parts returns a process-owned immutable array view.
@[inline]
pub fn (syntax &SourceTypeSyntax) reflection_parts() []u32 {
	return syntax.ids(syntax.span(29))
}

// reflection_tuple_parts returns a process-owned immutable array view.
@[inline]
pub fn (syntax &SourceTypeSyntax) reflection_tuple_parts() []u32 {
	return syntax.ids(syntax.span(30))
}

// receiver_raw_app_args returns a process-owned immutable array view.
@[inline]
pub fn (syntax &SourceTypeSyntax) receiver_raw_app_args() []u32 {
	return syntax.ids(syntax.span(31))
}

// vls_generic_words returns a process-owned immutable array view.
@[inline]
pub fn (syntax &SourceTypeSyntax) vls_generic_words() []u32 {
	return syntax.ids(syntax.span(32))
}

// eval_tuple_parts returns a process-owned immutable array view.
@[inline]
pub fn (syntax &SourceTypeSyntax) eval_tuple_parts() []u32 {
	return syntax.ids(syntax.span(33))
}

// implicit_tuple_components returns a process-owned immutable array view.
@[inline]
pub fn (syntax &SourceTypeSyntax) implicit_tuple_components() []u32 {
	return syntax.ids(syntax.span(34))
}

// generic_application_heads returns a process-owned immutable array view.
@[inline]
pub fn (syntax &SourceTypeSyntax) generic_application_heads() []u32 {
	return syntax.ids(syntax.span(35))
}

// markused_application_heads returns a process-owned immutable array view.
@[inline]
pub fn (syntax &SourceTypeSyntax) markused_application_heads() []u32 {
	return syntax.ids(syntax.span(36))
}

// markused_generic_components returns a process-owned immutable array view.
@[inline]
pub fn (syntax &SourceTypeSyntax) markused_generic_components() []u32 {
	return syntax.ids(syntax.span(37))
}

// raw_tuple_parse_components returns a process-owned immutable array view.
@[inline]
pub fn (syntax &SourceTypeSyntax) raw_tuple_parse_components() []u32 {
	return syntax.ids(syntax.span(38))
}

// union_parts returns a process-owned immutable array view.
@[inline]
pub fn (syntax &SourceTypeSyntax) union_parts() []u32 {
	return syntax.ids(syntax.span(39))
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

// A bounded recent cache shortcuts page lookup for positive IDs. Every pointer
// refers to immutable process-owned storage; the payload owner rejects collisions.
__global g_source_type_syntax_recent [4096]voidptr

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
	// Temporary render buffers and numeric arrays have been copied before return.
	mut scratch_scope := voidptr(unsafe { nil })
	$if prealloc {
		scratch_scope = unsafe { prealloc_scope_begin() }
	}
	defer {
		$if prealloc {
			unsafe { prealloc_scope_end(scratch_scope) }
		}
	}

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
	// Temporary render buffers and numeric arrays have been copied before return.
	mut scratch_scope := voidptr(unsafe { nil })
	$if prealloc {
		scratch_scope = unsafe { prealloc_scope_begin() }
	}
	defer {
		$if prealloc {
			unsafe { prealloc_scope_end(scratch_scope) }
		}
	}

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
	// Temporary render buffers and numeric arrays have been copied before return.
	mut scratch_scope := voidptr(unsafe { nil })
	$if prealloc {
		scratch_scope = unsafe { prealloc_scope_begin() }
	}
	defer {
		$if prealloc {
			unsafe { prealloc_scope_end(scratch_scope) }
		}
	}

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
	recent_slot := voidptr(&g_source_type_syntax_recent[id & 4095])
	recent := unsafe { &SourceTypeSyntaxEntry(source_type_syntax_pointer_load(recent_slot)) }
	if !isnil(recent) {
		// The acquire load makes the immutable owner and payload visible together.
		owner := unsafe { *&u32(recent.syntax.payload) }
		if owner == id {
			return recent
		}
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
	entry := unsafe { &SourceTypeSyntaxEntry(source_type_syntax_pointer_load(slot)) }
	if !isnil(entry) {
		source_type_syntax_pointer_store(recent_slot, voidptr(entry))
	}
	return entry
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
	// Seed only complete entries, after publication in the authoritative pages.
	source_type_syntax_pointer_store(voidptr(&g_source_type_syntax_recent[id & 4095]), voidptr(entry))
}

fn source_type_syntax_miss(id u32) &SourceTypeSyntax {
	// Interning components can acquire the source spelling writer lock itself.
	// Finish every spelling operation before taking that lock for publication.
	parsed, scratch_scope := source_type_syntax_parse_scoped(id, source_type_name(id), true)
	defer {
		$if prealloc {
			// Only copied numeric IDs and process-owned spellings survive publication.
			unsafe { prealloc_scope_end(scratch_scope) }
		}
	}
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
	// The strict name constructor fixes 27 owner IDs and 43 zero scalar slots.
	// Pack only its 29 remaining fields, preserving their generic rank order.
	name_scalar_indices := [
		0,
		1,
		4,
		5,
		10,
		23,
		24,
		26,
		52,
		53,
		54,
		57,
		58,
		65,
		66,
		67,
		68,
		69,
		70,
		81,
		82,
		87,
		90,
		97,
		99,
		101,
		102,
		105,
		106,
	]!
	mut values := [107]u32{}
	if parsed.name_packing {
		values[0] = u32(parsed.qualifier_count)
		values[1] = u32(parsed.kind)
		values[2] = u32(parsed.unqualified)
		values[3] = u32(parsed.qualifier)
		values[4] = u32(parsed.main_base)
		values[5] = u32(parsed.first_dot_prefix)
		values[6] = u32(parsed.first_dot_suffix)
		values[7] = u32(parsed.builtin_base)
		values[8] = u32(parsed.main_unlocked)
		values[9] = u32(parsed.encoded_fixed_length)
		values[10] = u32(parsed.encoded_fixed_base)
		values[11] = u32(parsed.c_fixed_typedef_element)
		values[12] = u32(parsed.c_fixed_typedef_length)
		values[13] = u32(parsed.c_array_payload)
		values[14] = u32(parsed.c_fixed_array_payload)
		values[15] = u32(parsed.fastc_array_marker_payload)
		values[16] = u32(parsed.c_map_payload)
		values[17] = u32(parsed.c_ptr_name_base)
		values[18] = u32(parsed.c_tagged_type)
		values[19] = u32(parsed.thread_value_payload)
		values[20] = u32(parsed.thread_value_type)
		values[21] = u32(parsed.parallel_preseed_short)
		values[22] = u32(parsed.field_owner_module)
		values[23] = u32(parsed.fastc_fixed_array_length)
		values[24] = u32(parsed.typ_suffix_base)
		values[25] = u32(parsed.top_level_dot_prefix)
		values[26] = u32(parsed.top_level_dot_suffix)
		values[27] = u32(parsed.named_variant_dot_prefix)
		values[28] = u32(parsed.named_variant_dot_suffix)
	} else {
		values[0] = u32(parsed.qualifier_count)
		values[1] = u32(parsed.kind)
		values[2] = u32(parsed.trimmed)
		values[3] = u32(parsed.base)
		values[4] = u32(parsed.unqualified)
		values[5] = u32(parsed.qualifier)
		values[6] = u32(parsed.generic_base)
		values[7] = u32(parsed.pointer_base)
		values[9] = u32(parsed.raw_base)
		values[10] = u32(parsed.main_base)
		values[11] = u32(parsed.application_base)
		values[12] = u32(parsed.generic_app_base)
		values[13] = u32(parsed.postfix_base)
		values[14] = u32(parsed.parameter_type)
		values[15] = u32(parsed.parameter_mut_type)
		values[16] = u32(parsed.bracket_base)
		values[17] = u32(parsed.bracket_inner)
		values[18] = u32(parsed.bracket_raw_inner)
		values[19] = u32(parsed.bracket_prefix)
		values[20] = u32(parsed.function_result)
		values[21] = u32(parsed.first_bracket_base)
		values[22] = u32(parsed.last_space_word)
		values[23] = u32(parsed.first_dot_prefix)
		values[24] = u32(parsed.first_dot_suffix)
		values[25] = u32(parsed.pointer_raw_base)
		values[26] = u32(parsed.builtin_base)
		values[27] = u32(parsed.cgen_app_base)
		values[28] = u32(parsed.top_space_head)
		values[29] = u32(parsed.top_space_tail)
		values[30] = u32(parsed.bracket_suffix)
		values[31] = u32(parsed.bracket_from)
		values[32] = u32(parsed.first_bracket_prefix)
		values[33] = u32(parsed.dots_base)
		values[34] = u32(parsed.cgen_raw_app_base)
		values[35] = u32(parsed.generic_parameter_payload)
		values[36] = u32(parsed.legacy_parameter_type)
		values[37] = u32(parsed.legacy_parameter_mut_type)
		values[38] = u32(parsed.first_bracket_length)
		values[39] = u32(parsed.c_pointer_base)
		values[40] = u32(parsed.source_wrapper_base)
		values[41] = u32(parsed.declaration_head)
		values[42] = u32(parsed.compact_fn_spelling)
		values[43] = u32(parsed.first_bracket_raw_inner)
		values[44] = u32(parsed.typeof_inner)
		values[45] = u32(parsed.postfix_index_base)
		values[46] = u32(parsed.last_bracket_prefix)
		values[47] = u32(parsed.last_bracket_length)
		values[48] = u32(parsed.trailing_bracket_prefix)
		values[49] = u32(parsed.array_chars_base)
		values[50] = u32(parsed.named_variant_owner)
		values[51] = u32(parsed.named_variant_name)
		values[52] = u32(parsed.main_unlocked)
		values[53] = u32(parsed.encoded_fixed_length)
		values[54] = u32(parsed.encoded_fixed_base)
		values[55] = u32(parsed.suffix_bracket_length)
		values[56] = u32(parsed.bracket_source_inner)
		values[57] = u32(parsed.c_fixed_typedef_element)
		values[58] = u32(parsed.c_fixed_typedef_length)
		values[59] = u32(parsed.fixed_array_query_length)
		values[60] = u32(parsed.last_bracket_span_inner)
		values[61] = u32(parsed.generic_app_legacy_suffix)
		values[62] = u32(parsed.wrapper_prefix)
		values[63] = u32(parsed.wrapper_base)
		values[64] = u32(parsed.qualified_modifiers)
		values[65] = u32(parsed.c_array_payload)
		values[66] = u32(parsed.c_fixed_array_payload)
		values[67] = u32(parsed.fastc_array_marker_payload)
		values[68] = u32(parsed.c_map_payload)
		values[69] = u32(parsed.c_ptr_name_base)
		values[70] = u32(parsed.c_tagged_type)
		values[71] = u32(parsed.backward_bracket_prefix)
		values[73] = u32(parsed.receiver_app_base)
		values[74] = u32(parsed.declaration_parameter_payload)
		values[75] = u32(parsed.prefix_dimension_tail)
		values[76] = u32(parsed.backward_bracket_inner)
		values[80] = u32(parsed.c_pointer_raw_base)
		values[81] = u32(parsed.thread_value_payload)
		values[82] = u32(parsed.thread_value_type)
		values[84] = u32(parsed.parameter_name)
		values[85] = u32(parsed.receiver_collection_id)
		values[86] = u32(parsed.ampersand_trimmed)
		values[87] = u32(parsed.parallel_preseed_short)
		values[88] = u32(parsed.constraint_receiver_base)
		values[89] = u32(parsed.space_removed)
		values[90] = u32(parsed.field_owner_module)
		values[91] = u32(parsed.question_trimmed)
		values[92] = u32(parsed.function_space_result)
		values[95] = u32(parsed.main_at_base)
		values[96] = u32(parsed.receiver_app_inner)
		values[97] = u32(parsed.fastc_fixed_array_length)
		values[98] = u32(parsed.first_bracket_source_inner)
		values[99] = u32(parsed.typ_suffix_base)
		values[100] = u32(parsed.checker_parse_function_result)
		values[101] = u32(parsed.top_level_dot_prefix)
		values[102] = u32(parsed.top_level_dot_suffix)
		values[103] = u32(parsed.parser_fn_decl_params)
		values[104] = u32(parsed.parser_fn_decl_result)
		values[105] = u32(parsed.named_variant_dot_prefix)
		values[106] = u32(parsed.named_variant_dot_suffix)
	}
	mut scalar_present := [2]u64{}
	mut scalar_identity := if parsed.name_packing {
		[u64(0x207fa0240e0c4), u64(0x48b410c00)]!
	} else {
		[2]u64{}
	}
	mut scalar_count := 0
	mut scalar_first_count := 0
	scalar_limit := if parsed.name_packing { name_scalar_indices.len } else { values.len }
	for ordinal in 0 .. scalar_limit {
		index := if parsed.name_packing { name_scalar_indices[ordinal] } else { ordinal }
		value := values[ordinal]
		if value == 0 { continue }
		word := index >> 6
		bit := u64(1) << u32(index & 63)
		if value == id {
			scalar_identity[word] |= bit
		} else {
			scalar_present[word] |= bit
			// Compact only into the consumed prefix; unread field values stay intact.
			values[scalar_count] = value
			scalar_count++
			if word == 0 { scalar_first_count++ }
		}
	}
	// The other 99 fields fit u32. These eight int fields keep their full bits,
	// with -1 represented by the otherwise unused identity bit for that field.
	integer_indices := [8, 72, 77, 78, 79, 83, 93, 94]!
	mut integers := [u64(parsed.pointer_depth), u64(parsed.byte_len),
		u64(parsed.nested_option_result_marker), u64(parsed.c_pointer_depth),
		u64(parsed.c_pointer_count), u64(parsed.expr_id), u64(parsed.decimal_value),
		u64(parsed.integer_value)]!
	mut integer_present := u8(0)
	mut integer_count := 0
	for ordinal in 0 .. integers.len {
		value := integers[ordinal]
		if value == 0 { continue }
		if value == ~u64(0) {
			index := integer_indices[ordinal]
			scalar_identity[index >> 6] |= u64(1) << u32(index & 63)
		} else {
			integer_present |= u8(1) << u32(ordinal)
			// As above, writes stay in the prefix already consumed by this loop.
			integers[integer_count] = value
			integer_count++
		}
	}
	mut bool_flags := [2]u64{}
	if parsed.function_has_result { bool_flags[0] |= u64(1) << 0 }
	if parsed.bare_generic { bool_flags[0] |= u64(1) << 1 }
	if parsed.application_valid { bool_flags[0] |= u64(1) << 2 }
	if parsed.generic_app_valid { bool_flags[0] |= u64(1) << 3 }
	if parsed.bracket_closed { bool_flags[0] |= u64(1) << 4 }
	if parsed.function_closed { bool_flags[0] |= u64(1) << 5 }
	if parsed.tuple_like { bool_flags[0] |= u64(1) << 6 }
	if parsed.function_space_prefix { bool_flags[0] |= u64(1) << 7 }
	if parsed.spaced { bool_flags[0] |= u64(1) << 8 }
	if parsed.first_bracket_closed { bool_flags[0] |= u64(1) << 9 }
	if parsed.named_type { bool_flags[0] |= u64(1) << 10 }
	if parsed.c_qualified { bool_flags[0] |= u64(1) << 11 }
	if parsed.builtin_prefix { bool_flags[0] |= u64(1) << 12 }
	if parsed.main_fragment { bool_flags[0] |= u64(1) << 13 }
	if parsed.function_prefix { bool_flags[0] |= u64(1) << 14 }
	if parsed.fn_prefix { bool_flags[0] |= u64(1) << 15 }
	if parsed.shared_fragment { bool_flags[0] |= u64(1) << 16 }
	if parsed.cgen_app_valid { bool_flags[0] |= u64(1) << 17 }
	if parsed.tuple_contains_comma { bool_flags[0] |= u64(1) << 18 }
	if parsed.top_space_valid { bool_flags[0] |= u64(1) << 19 }
	if parsed.typeof_expr_prefix { bool_flags[0] |= u64(1) << 20 }
	if parsed.bracket_inner_fixed_length { bool_flags[0] |= u64(1) << 21 }
	if parsed.void_pointer_postfix { bool_flags[0] |= u64(1) << 22 }
	if parsed.canonical_dotted_name { bool_flags[0] |= u64(1) << 23 }
	if parsed.decimal { bool_flags[0] |= u64(1) << 24 }
	if parsed.contains_comma { bool_flags[0] |= u64(1) << 25 }
	if parsed.function_fragment { bool_flags[0] |= u64(1) << 26 }
	if parsed.cgen_raw_app_valid { bool_flags[0] |= u64(1) << 27 }
	if parsed.c_ptr_name_suffix { bool_flags[0] |= u64(1) << 28 }
	if parsed.c_fn_type_fragment { bool_flags[0] |= u64(1) << 29 }
	if parsed.underscored { bool_flags[0] |= u64(1) << 30 }
	if parsed.fn_fragment { bool_flags[0] |= u64(1) << 31 }
	if parsed.cgen_generic_placeholder { bool_flags[0] |= u64(1) << 32 }
	if parsed.fixed_array_type { bool_flags[0] |= u64(1) << 33 }
	if parsed.typeof_valid { bool_flags[0] |= u64(1) << 34 }
	if parsed.trailing_bracket_closed { bool_flags[0] |= u64(1) << 35 }
	if parsed.lowercase_value_ident { bool_flags[0] |= u64(1) << 36 }
	if parsed.typeof_prefix { bool_flags[0] |= u64(1) << 37 }
	if parsed.first_capital { bool_flags[0] |= u64(1) << 38 }
	if parsed.named_variant { bool_flags[0] |= u64(1) << 39 }
	if parsed.named_variant_valid { bool_flags[0] |= u64(1) << 40 }
	if parsed.map_name_prefix { bool_flags[0] |= u64(1) << 41 }
	if parsed.suffix_bracket_valid { bool_flags[0] |= u64(1) << 42 }
	if parsed.map_function_fragment { bool_flags[0] |= u64(1) << 43 }
	if parsed.c_fixed_typedef_valid { bool_flags[0] |= u64(1) << 44 }
	if parsed.last_bracket_span_valid { bool_flags[0] |= u64(1) << 45 }
	if parsed.plain_type_name { bool_flags[0] |= u64(1) << 46 }
	if parsed.checker_fixed_array_may_be { bool_flags[0] |= u64(1) << 47 }
	if parsed.dollar_prefix { bool_flags[0] |= u64(1) << 48 }
	if parsed.define_prefix { bool_flags[0] |= u64(1) << 49 }
	if parsed.define_fragment { bool_flags[0] |= u64(1) << 50 }
	if parsed.paren_prefix { bool_flags[0] |= u64(1) << 51 }
	if parsed.fixed_length { bool_flags[0] |= u64(1) << 52 }
	if parsed.plain_alias_target { bool_flags[0] |= u64(1) << 53 }
	if parsed.fastc_array_marker { bool_flags[0] |= u64(1) << 54 }
	if parsed.backward_bracket_closed { bool_flags[0] |= u64(1) << 55 }
	if parsed.opening_paren { bool_flags[0] |= u64(1) << 56 }
	if parsed.receiver_app_valid { bool_flags[0] |= u64(1) << 57 }
	if parsed.declaration_parameter_mut { bool_flags[0] |= u64(1) << 58 }
	if parsed.prefix_dimensions_closed { bool_flags[0] |= u64(1) << 59 }
	if parsed.backward_bracket_after_prefix_dims { bool_flags[0] |= u64(1) << 60 }
	if parsed.option_result_void_prefix { bool_flags[0] |= u64(1) << 61 }
	if parsed.thread_value_prefix { bool_flags[0] |= u64(1) << 62 }
	if parsed.thread_value_type_valid { bool_flags[0] |= u64(1) << 63 }
	if parsed.contains_closing_bracket { bool_flags[1] |= u64(1) << 0 }
	if parsed.atomic_val_fragment { bool_flags[1] |= u64(1) << 1 }
	if parsed.expr_prefix { bool_flags[1] |= u64(1) << 2 }
	if parsed.parameter_named { bool_flags[1] |= u64(1) << 3 }
	if parsed.parameter_is_mut { bool_flags[1] |= u64(1) << 4 }
	if parsed.transform_fixed_length { bool_flags[1] |= u64(1) << 5 }
	if parsed.contains_doubleunderscore { bool_flags[1] |= u64(1) << 6 }
	if parsed.parallel_preseed_fn { bool_flags[1] |= u64(1) << 7 }
	if parsed.closing_bracket { bool_flags[1] |= u64(1) << 8 }
	if parsed.result_suffix { bool_flags[1] |= u64(1) << 9 }
	if parsed.cgen_decl_unusable { bool_flags[1] |= u64(1) << 10 }
	if parsed.cgen_decl_authority { bool_flags[1] |= u64(1) << 11 }
	if parsed.generic_encoded_fragment { bool_flags[1] |= u64(1) << 12 }
	if parsed.fn_ptr_encoded_prefix { bool_flags[1] |= u64(1) << 13 }
	if parsed.c_result_prefix { bool_flags[1] |= u64(1) << 14 }
	if parsed.c_option_prefix { bool_flags[1] |= u64(1) << 15 }
	if parsed.c_multi_return_prefix { bool_flags[1] |= u64(1) << 16 }
	if parsed.c_lower_map_prefix { bool_flags[1] |= u64(1) << 17 }
	if parsed.open_generic_receiver_marker { bool_flags[1] |= u64(1) << 18 }
	if parsed.qualified_name_chars { bool_flags[1] |= u64(1) << 19 }
	if parsed.const_name_prefix { bool_flags[1] |= u64(1) << 20 }
	if parsed.pthread_prefix { bool_flags[1] |= u64(1) << 21 }
	if parsed.thread_bracket_prefix { bool_flags[1] |= u64(1) << 22 }
	if parsed.fixed_typedef_bracket { bool_flags[1] |= u64(1) << 23 }
	if parsed.c_array_fixed_prefix { bool_flags[1] |= u64(1) << 24 }
	if parsed.encoded_alnum_dotted { bool_flags[1] |= u64(1) << 25 }
	if parsed.internal_symbol_prefix { bool_flags[1] |= u64(1) << 26 }
	if parsed.incremental_generic_application { bool_flags[1] |= u64(1) << 27 }
	if parsed.system_error_suffix { bool_flags[1] |= u64(1) << 28 }
	if parsed.fastc_fixed_array_length_valid { bool_flags[1] |= u64(1) << 29 }
	if parsed.contains_capital { bool_flags[1] |= u64(1) << 30 }
	if parsed.c_function_pointer_hole { bool_flags[1] |= u64(1) << 31 }
	if parsed.channel_fragment { bool_flags[1] |= u64(1) << 32 }
	if parsed.array_receiver_fragment { bool_flags[1] |= u64(1) << 33 }
	if parsed.mangled_open_generic_marker { bool_flags[1] |= u64(1) << 34 }
	if parsed.array_suffix { bool_flags[1] |= u64(1) << 35 }
	if parsed.empty_function_prefix { bool_flags[1] |= u64(1) << 36 }
	if parsed.c_shared_fragment { bool_flags[1] |= u64(1) << 37 }
	if parsed.typ_suffix { bool_flags[1] |= u64(1) << 38 }
	if parsed.checker_parse_function_has_params { bool_flags[1] |= u64(1) << 39 }
	if parsed.top_level_dot_valid { bool_flags[1] |= u64(1) << 40 }
	if parsed.parser_fn_decl_valid { bool_flags[1] |= u64(1) << 41 }
	if parsed.contains_question { bool_flags[1] |= u64(1) << 42 }
	if parsed.contains_union { bool_flags[1] |= u64(1) << 43 }
	if parsed.named_variant_dot_valid { bool_flags[1] |= u64(1) << 44 }
	span_count := if parsed.name_packing {
		source_type_syntax_packed_span_len(parsed.voidptr_parts, id) +
			source_type_syntax_packed_span_len(parsed.vls_generic_words, id) +
			source_type_syntax_packed_span_len(parsed.markused_generic_components, id)
	} else {
		source_type_syntax_packed_span_len(parsed.args, id) +
			source_type_syntax_packed_span_len(parsed.application_args, id) +
			source_type_syntax_packed_span_len(parsed.generic_app_args, id) +
			source_type_syntax_packed_span_len(parsed.postfix_dims, id) +
			source_type_syntax_packed_span_len(parsed.function_params, id) +
			source_type_syntax_packed_span_len(parsed.tuple_components, id) +
			source_type_syntax_packed_span_len(parsed.cgen_app_args, id) +
			source_type_syntax_packed_span_len(parsed.cgen_function_params, id) +
			source_type_syntax_packed_span_len(parsed.checker_function_params, id) +
			source_type_syntax_packed_span_len(parsed.checker_tuple_components, id) +
			source_type_syntax_packed_span_len(parsed.cgen_raw_app_args, id) +
			source_type_syntax_packed_span_len(parsed.cgen_parts, id) +
			source_type_syntax_packed_span_len(parsed.checker_raw_tuple_components, id) +
			source_type_syntax_packed_span_len(parsed.bracket_raw_parts, id) +
			source_type_syntax_packed_span_len(parsed.bracket_generic_parts, id) +
			source_type_syntax_packed_span_len(parsed.tuple_function_components, id) +
			source_type_syntax_packed_span_len(parsed.plain_comma_parts, id) +
			source_type_syntax_packed_span_len(parsed.generic_mentions, id) +
			source_type_syntax_packed_span_len(parsed.receiver_app_args, id) +
			source_type_syntax_packed_span_len(parsed.checker_bracket_components, id) +
			source_type_syntax_packed_span_len(parsed.voidptr_parts, id) +
			source_type_syntax_packed_span_len(parsed.inferred_parts, id) +
			source_type_syntax_packed_span_len(parsed.inferred_annotations, id) +
			source_type_syntax_packed_span_len(parsed.inferred_expr_ids, id) +
			source_type_syntax_packed_span_len(parsed.substitution_function_params, id) +
			source_type_syntax_packed_span_len(parsed.written_applications, id) +
			source_type_syntax_packed_span_len(parsed.written_application_offsets, id) +
			source_type_syntax_packed_span_len(parsed.checker_raw_bracket_components, id) +
			source_type_syntax_packed_span_len(parsed.cgen_placeholder_candidates, id) +
			source_type_syntax_packed_span_len(parsed.reflection_parts, id) +
			source_type_syntax_packed_span_len(parsed.reflection_tuple_parts, id) +
			source_type_syntax_packed_span_len(parsed.receiver_raw_app_args, id) +
			source_type_syntax_packed_span_len(parsed.vls_generic_words, id) +
			source_type_syntax_packed_span_len(parsed.eval_tuple_parts, id) +
			source_type_syntax_packed_span_len(parsed.implicit_tuple_components, id) +
			source_type_syntax_packed_span_len(parsed.generic_application_heads, id) +
			source_type_syntax_packed_span_len(parsed.markused_application_heads, id) +
			source_type_syntax_packed_span_len(parsed.markused_generic_components, id) +
			source_type_syntax_packed_span_len(parsed.raw_tuple_parse_components, id) +
			source_type_syntax_packed_span_len(parsed.union_parts, id)
	}
	// The owner ID and narrow scalars precede an optional u32 padding slot.
	// At most 99 narrow and eight integer fields put span_offset below 117.
	integer_offset := (scalar_count + 2) >> 1
	span_base := 2 * integer_offset + 2 * integer_count
	payload_bytes := u64(sizeof(SourceTypeSyntaxEntry)) +
		(u64(span_base) + 2 * u64(span_count) + u64(id_count) - 1) * u64(sizeof(u32))
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
		id_offset:   span_base + 2 * span_count
		ids:         unsafe { voidptr(&u8(entry) + sizeof(SourceTypeSyntaxEntry)) }
	}
	unsafe { *&u32(payload.ids) = id }
	for index in 0 .. scalar_count {
		unsafe { (&u32(payload.ids))[1 + index] = values[index] }
	}
	for index in 0 .. integer_count {
		// The 32-byte header and rounded narrow prefix align these u64 slots.
		unsafe { (&u64(payload.ids))[integer_offset + index] = integers[index] }
	}

	// Records and slices belong to the process, independently of parser workers
	// and preallocated arenas. The release store publishes them only when complete.
	mut spans := [40]SourceTypeSyntaxSpan{}

	// The strict name constructor proves that thirty views are empty. Their
	// zeroed span fields need no generic length/copy work; identity views use slot 0.
	if parsed.name_packing {
		identity_span := SourceTypeSyntaxSpan{ len: 1 }
		spans[11] = identity_span
		spans[16] = identity_span
		spans[21] = identity_span
		spans[29] = identity_span
		spans[39] = identity_span
		if parsed.generic_mentions.len != 0 {
			spans[17] = identity_span
		}
		if parsed.cgen_placeholder_candidates.len != 0 {
			spans[28] = identity_span
		}
		spans[20] = source_type_syntax_owned_ids(parsed.voidptr_parts, mut payload)
		spans[32] = source_type_syntax_owned_ids(parsed.vls_generic_words, mut payload)
		spans[37] = source_type_syntax_owned_ids(parsed.markused_generic_components, mut payload)
	} else {
		spans[0] = source_type_syntax_owned_ids(parsed.args, mut payload)
		spans[1] = source_type_syntax_owned_ids(parsed.application_args, mut payload)
		spans[2] = source_type_syntax_owned_ids(parsed.generic_app_args, mut payload)
		spans[3] = source_type_syntax_owned_ids(parsed.postfix_dims, mut payload)
		spans[4] = source_type_syntax_owned_ids(parsed.function_params, mut payload)
		spans[5] = source_type_syntax_owned_ids(parsed.tuple_components, mut payload)
		spans[6] = source_type_syntax_owned_ids(parsed.cgen_app_args, mut payload)
		spans[7] = source_type_syntax_owned_ids(parsed.cgen_function_params, mut payload)
		spans[8] = source_type_syntax_owned_ids(parsed.checker_function_params, mut payload)
		spans[9] = source_type_syntax_owned_ids(parsed.checker_tuple_components, mut payload)
		spans[10] = source_type_syntax_owned_ids(parsed.cgen_raw_app_args, mut payload)
		spans[11] = source_type_syntax_owned_ids(parsed.cgen_parts, mut payload)
		spans[12] = source_type_syntax_owned_ids(parsed.checker_raw_tuple_components, mut payload)
		spans[13] = source_type_syntax_owned_ids(parsed.bracket_raw_parts, mut payload)
		spans[14] = source_type_syntax_owned_ids(parsed.bracket_generic_parts, mut payload)
		spans[15] = source_type_syntax_owned_ids(parsed.tuple_function_components, mut payload)
		spans[16] = source_type_syntax_owned_ids(parsed.plain_comma_parts, mut payload)
		spans[17] = source_type_syntax_owned_ids(parsed.generic_mentions, mut payload)
		spans[18] = source_type_syntax_owned_ids(parsed.receiver_app_args, mut payload)
		spans[19] = source_type_syntax_owned_ids(parsed.checker_bracket_components, mut payload)
		spans[20] = source_type_syntax_owned_ids(parsed.voidptr_parts, mut payload)
		spans[21] = source_type_syntax_owned_ids(parsed.inferred_parts, mut payload)
		spans[22] = source_type_syntax_owned_ids(parsed.inferred_annotations, mut payload)
		spans[23] = source_type_syntax_owned_ids(parsed.inferred_expr_ids, mut payload)
		spans[24] = source_type_syntax_owned_ids(parsed.substitution_function_params, mut payload)
		spans[25] = source_type_syntax_owned_ids(parsed.written_applications, mut payload)
		spans[26] = source_type_syntax_owned_ids(parsed.written_application_offsets, mut payload)
		spans[27] = source_type_syntax_owned_ids(parsed.checker_raw_bracket_components, mut payload)
		spans[28] = source_type_syntax_owned_ids(parsed.cgen_placeholder_candidates, mut payload)
		spans[29] = source_type_syntax_owned_ids(parsed.reflection_parts, mut payload)
		spans[30] = source_type_syntax_owned_ids(parsed.reflection_tuple_parts, mut payload)
		spans[31] = source_type_syntax_owned_ids(parsed.receiver_raw_app_args, mut payload)
		spans[32] = source_type_syntax_owned_ids(parsed.vls_generic_words, mut payload)
		spans[33] = source_type_syntax_owned_ids(parsed.eval_tuple_parts, mut payload)
		spans[34] = source_type_syntax_owned_ids(parsed.implicit_tuple_components, mut payload)
		spans[35] = source_type_syntax_owned_ids(parsed.generic_application_heads, mut payload)
		spans[36] = source_type_syntax_owned_ids(parsed.markused_application_heads, mut payload)
		spans[37] = source_type_syntax_owned_ids(parsed.markused_generic_components, mut payload)
		spans[38] = source_type_syntax_owned_ids(parsed.raw_tuple_parse_components, mut payload)
		spans[39] = source_type_syntax_owned_ids(parsed.union_parts, mut payload)
	}
	mut span_present := u64(0)
	mut span_identity := u64(0)
	mut span_offset := span_base
	// Strict names only populate these ten views, in the same rank order.
	name_span_indices := [11, 16, 17, 20, 21, 28, 29, 32, 37, 39]!
	span_limit := if parsed.name_packing { name_span_indices.len } else { spans.len }
	for ordinal in 0 .. span_limit {
		index := if parsed.name_packing { name_span_indices[ordinal] } else { ordinal }
		span := spans[index]
		if span.len == 0 { continue }
		bit := u64(1) << u32(index)
		if span.offset == 0 && span.len == 1 {
			span_identity |= bit
			continue
		}
		span_present |= bit
		// Metadata and ID arrays occupy disjoint regions of this unpublished payload.
		unsafe { *&SourceTypeSyntaxSpan(&(&u32(payload.ids))[span_offset]) = span }
		span_offset += 2
	}
	unsafe {
		*entry = SourceTypeSyntaxEntry{
			syntax: SourceTypeSyntax{
				payload:    payload.ids
				layout:     source_type_syntax_layout(SourceTypeSyntaxLayout{
					scalar_present:     scalar_present
					scalar_identity:    scalar_identity
					span_present:       span_present
					span_identity:      span_identity
					scalar_count:       u8(scalar_count)
					scalar_first_count: u8(scalar_first_count)
					integer_present:    integer_present
					span_offset:        u8(span_base)
				})
				bool_flags: bool_flags
			}
		}
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

@[inline]
fn source_type_syntax_packed_span_len(ids []u32, identity_id u32) int {
	return if ids.len == 0 || (ids.len == 1 && ids[0] == identity_id) { 0 } else { 1 }
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
	return source_type_id(unsafe { raw.substr_unsafe(1, end) }), source_type_id(unsafe { raw.substr_unsafe(end, raw.len) })
}

fn source_type_syntax_c_tagged_type(id u32, raw string) u32 {
	if raw.starts_with('struct_') {
		return source_type_id('struct ' + unsafe { raw.substr_unsafe('struct_'.len, raw.len) })
	}
	if raw.starts_with('union_') {
		return source_type_id('union ' + unsafe { raw.substr_unsafe('union_'.len, raw.len) })
	}
	if raw.starts_with('enum_') {
		return source_type_id('enum ' + unsafe { raw.substr_unsafe('enum_'.len, raw.len) })
	}
	if raw == 'unsigned_int' { return source_type_id('unsigned int') }
	return id
}

fn source_type_syntax_c_fixed_typedef(raw string) (u32, u32, bool) {
	if !raw.starts_with('Array_fixed_') { return 0, 0, false }
	payload_start := 'Array_fixed_'.len
	separator := raw.last_index_u8(`_`)
	if separator <= payload_start || separator + 1 >= raw.len { return 0, 0, false }
	return source_type_id(unsafe { raw.substr_unsafe(payload_start, separator) }), source_type_id(unsafe { raw.substr_unsafe(separator + 1, raw.len) }), true
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
	payload := unsafe { raw.substr_unsafe('FixedArray_'.len, raw.len) }
	marker := payload.index_('_FASTC_ARRAY_OF_')
	// A marker can overlap the prefix's final underscore and be absent from payload.
	part := unsafe { payload.substr_unsafe(0, if marker < 0 { payload.len } else { marker }) }
	if part.starts_with('EXPRHEX') {
		decoded := hex.decode(unsafe { part.substr_unsafe('EXPRHEX'.len, part.len) }) or {
			return false, 0
		}
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
	parsed, _ := source_type_syntax_parse_scoped(id, raw, false)
	return parsed
}

// The caller keeps a general parser's arena alive until all array values are copied.
// Direct parser checks use the unscoped wrapper and retain their existing lifetime.
fn source_type_syntax_parse_scoped(id u32, raw string, scoped bool) (SourceTypeSyntaxParsed, voidptr) {
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
	if simple { return source_type_syntax_name(id, raw, -1, -1), unsafe { nil } }
	if qualified && !segment_start && first_dot >= 0 {
		return source_type_syntax_name(id, raw, first_dot, last_dot), unsafe { nil }
	}
	mut scratch_scope := voidptr(unsafe { nil })
	$if prealloc {
		if scoped {
			// Every escaping spelling and ID tuple is copied to process-owned storage.
			scratch_scope = unsafe { prealloc_scope_begin() }
		}
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
	}, scratch_scope
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
	integer_value := if raw[0].is_digit() { raw.int() } else { 0 }
	decimal := source_type_syntax_decimal(raw)
	mut contains_capital := false
	mut contains_doubleunderscore := false
	mut open_generic_receiver_marker := false
	mut mangled_generic_marker := false
	cgen_decl_unusable, cgen_decl_authority := source_type_syntax_cgen_declaration(raw)
	thread_value_type_valid, thread_value_type := source_type_syntax_thread_value(raw)
	mut last_underscore := -1
	mut qualifier_count := u32(0)
	mut field_owner_c := -1
	mut field_owner_js := -1
	for i, c in raw {
		if c >= `A` && c <= `Z` {
			contains_capital = true
			// Strict names contain no bracket syntax; only raw underscore boundaries apply.
			if !open_generic_receiver_marker && source_type_syntax_receiver_marker_byte(c)
				&& (i == 0 || raw[i - 1] == `_`)
				&& (i + 1 == raw.len || raw[i + 1] == `_`) {
				open_generic_receiver_marker = true
			}
		}
		if c == `_` {
			if i + 1 < raw.len && raw[i + 1] == `_` { contains_doubleunderscore = true }
			if !mangled_generic_marker && i + 3 < raw.len
				&& raw[i + 1] >= `A` && raw[i + 1] <= `Z`
				&& raw[i + 2] == `_` && raw[i + 3] == `_` {
				mangled_generic_marker = true
			}
			last_underscore = i
		} else if c == `.` {
			qualifier_count++
			if field_owner_c < 0 && i + 3 <= raw.len && raw[i + 1] == `C` && raw[i + 2] == `.` {
				field_owner_c = i
			}
			if field_owner_js < 0 && i + 4 <= raw.len && raw[i + 1] == `J`
				&& raw[i + 2] == `S` && raw[i + 3] == `.` {
				field_owner_js = i
			}
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
	fastc_array_marker := raw.index_('_FASTC_ARRAY_OF_')
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
			source_type_syntax_child_range_id(id, raw, 'Array_'.len, raw.len)
		} else {
			u32(0)
		}
		c_fixed_array_payload:          if raw.starts_with('FixedArray_') {
			source_type_syntax_child_range_id(id, raw, 'FixedArray_'.len, raw.len)
		} else {
			u32(0)
		}
		fastc_array_marker:             fastc_array_marker >= 0
		fastc_array_marker_payload:     if fastc_array_marker >= 0 {
			source_type_syntax_child_range_id(id, raw,
				fastc_array_marker + '_FASTC_ARRAY_OF_'.len, raw.len)
		} else {
			u32(0)
		}
		c_map_payload:                  if raw.starts_with('Map_') {
			source_type_syntax_child_range_id(id, raw, 'Map_'.len, raw.len)
		} else {
			u32(0)
		}
		c_ptr_name_base:                if raw.ends_with('_ptr') {
			source_type_syntax_child_range_id(id, raw, 0, raw.len - 4)
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
			source_type_syntax_child_range_id(id, raw, '__vf_thread_k'.len, raw.len)
		} else {
			u32(0)
		}
		atomic_val_fragment:            raw.contains('AtomicVal')
		inferred_parts:                 identity_ids
		expr_prefix:                    raw.starts_with('__expr_')
		expr_id:                        if raw.starts_with('__expr_') {
			unsafe { raw.substr_unsafe('__expr_'.len, raw.len) }.int()
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
		open_generic_receiver_marker:   open_generic_receiver_marker
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
		encoded_alnum_dotted:           last_underscore < 0
		internal_symbol_prefix:         raw.starts_with('__v3_internal_symbol_')
		main_at_base:                   id
		mangled_open_generic_marker:    mangled_generic_marker
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
	if syntax.qualifier_count() == 0 { return false }
	if prefix == syntax.first_dot_prefix() || prefix == syntax.qualifier() { return true }
	for candidate in syntax.qualifier_prefixes() {
		if candidate == prefix { return true }
	}
	return false
}

// source_type_qualified_suffix returns the raw suffix following prefix and a dot.
// Both matching and the returned spelling use numeric IDs after the first query.
pub fn source_type_qualified_suffix(id u32, prefix u32) ?u32 {
	syntax := source_type_syntax(id)
	if syntax.qualifier_count() == 0 { return none }
	if prefix == syntax.first_dot_prefix() { return syntax.first_dot_suffix() }
	if prefix == syntax.qualifier() { return syntax.unqualified() }
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
