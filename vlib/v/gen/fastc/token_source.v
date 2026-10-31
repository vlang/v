module fastc

// FastcTokenSource is immutable and shared by token copies in the same arena.
// Keep the complete string header; its backing text has the same lifetime as
// the token's former inline source field.
struct FastcTokenSource {
	text string
}

@[inline; unsafe]
fn fastc_token_source(text string) &FastcTokenSource {
	if text.len == 0 { return unsafe { nil } }
	return &FastcTokenSource{ text: text }
}

@[inline]
fn (value &FastcExpressionToken) has_source() bool {
	return !isnil(value.source) && value.source.text.len != 0
}

@[inline]
fn (value &FastcExpressionToken) source_text() string {
	if isnil(value.source) { return '' }
	return value.source.text
}

// Token equality compares source contents, including independently built
// synthetic atoms. Copies may share a payload without sharing mutable typ/lit.
fn (left FastcExpressionToken) == (right FastcExpressionToken) bool {
	return left.source_text() == right.source_text()
		&& left.unsafe_depth == right.unsafe_depth && left.tok == right.tok
		&& left.chan_element_type == right.chan_element_type
		&& left.option_value_type == right.option_value_type
		&& left.is_mut_argument == right.is_mut_argument
		&& left.is_statement == right.is_statement
		&& left.is_capturing_closure == right.is_capturing_closure
		&& left.is_storage == right.is_storage && left.typ == right.typ && left.lit == right.lit
}
