module token

// Token lists token values used by token.
pub enum Token {
	amp        // &
	and        // &&
	and_assign // &=
	arrow      // <-
	assign     // =
	attribute
	bit_not // ~
	char    // `A` - rune
	colon   // :
	comma   // ,
	comment
	dec         // --
	decl_assign // :=
	div         // /
	div_assign  // /=
	dollar      // $
	dot         // .
	dotdot      // ..
	ellipsis    // ...
	eof
	eq   // ==
	ge   // >=
	gt   // >
	hash // #
	inc  // ++
	key_as
	key_asm
	key_assert
	key_atomic
	key_break
	key_const
	key_continue
	key_defer
	key_dump
	key_else
	key_enum
	key_false
	key_fn
	key_for
	key_global
	key_go
	key_goto
	key_if
	key_import
	key_in
	key_interface
	key_is
	key_isreftype
	key_likely
	key_lock
	key_match
	key_module
	key_mut
	key_nil
	key_none
	key_offsetof
	key_or
	key_pub
	key_return
	key_rlock
	key_select
	key_shared
	key_sizeof
	key_spawn
	key_static
	key_struct
	key_true
	key_type
	key_typeof
	key_union
	key_unlikely
	key_unsafe
	key_volatile
	lcbr                        // {
	le                          // <=
	left_shift                  // <<
	left_shift_assign           // >>=
	logical_or                  // ||
	lpar                        // (
	lsbr                        // [
	lt                          // <
	minus                       // -
	minus_assign                // -=
	mod                         // %
	mod_assign                  // %=
	mul                         // *
	mul_assign                  // *=
	name                        // user
	ne                          // !=
	not                         // !
	not_in                      // !in
	not_is                      // !is
	number                      // 123
	or_assign                   // |=
	pipe                        // |
	plus                        // +
	plus_assign                 // +=
	question                    // ?
	rcbr                        // }
	right_shift                 // >>
	right_shift_assign          // <<=
	right_shift_unsigned        // >>>
	right_shift_unsigned_assign // >>>=
	rpar                        // )
	rsbr                        // ]
	semicolon                   // ;
	str_dollar
	string // 'foo'
	unknown
	xor        // ^
	xor_assign // ^=
}

// BindingPower lists binding power values used by token.
pub enum BindingPower {
	lowest
	logical_or  // ||
	logical_and // &&
	compare     // ==, !=, <, <=, >, >=, in, !in, is, !is
	bit_or      // |
	bit_xor     // ^
	shift       // <<, >>, >>>
	add         // +, -
	product     // *, /, %, &
	highest
}

const token_and_id = 1
const token_arrow_id = 3
const token_assign_id = 4
const token_bit_not_id = 6
const token_dec_id = 11
const token_decl_assign_id = 12
const token_div_id = 13
const token_div_assign_id = 14
const token_eq_id = 20
const token_ge_id = 21
const token_gt_id = 22
const token_inc_id = 24
const token_key_in_id = 44
const token_key_is_id = 46
const token_le_id = 74
const token_left_shift_id = 75
const token_left_shift_assign_id = 76
const token_logical_or_id = 77
const token_lt_id = 80
const token_minus_id = 81
const token_minus_assign_id = 82
const token_mod_id = 83
const token_mod_assign_id = 84
const token_mul_id = 85
const token_mul_assign_id = 86
const token_ne_id = 88
const token_not_id = 89
const token_not_in_id = 90
const token_not_is_id = 91
const token_or_assign_id = 93
const token_pipe_id = 94
const token_plus_id = 95
const token_plus_assign_id = 96
const token_right_shift_id = 99
const token_right_shift_assign_id = 100
const token_right_shift_unsigned_id = 101
const token_right_shift_unsigned_assign_id = 102
const token_xor_id = 109
const token_xor_assign_id = 110

// left_binding_power supports left binding power handling for Token.
@[inline]
pub fn (t Token) left_binding_power() BindingPower {
	tv := int(t)
	if tv == token_logical_or_id {
		return BindingPower.logical_or
	}
	if tv == token_and_id {
		return BindingPower.logical_and
	}
	if tv == token_eq_id || tv == token_ne_id || tv == token_lt_id || tv == token_le_id
		|| tv == token_gt_id || tv == token_ge_id || tv == token_key_in_id || tv == token_not_in_id
		|| tv == token_key_is_id || tv == token_not_is_id {
		return BindingPower.compare
	}
	if tv == token_pipe_id {
		return BindingPower.bit_or
	}
	if tv == token_xor_id {
		return BindingPower.bit_xor
	}
	if tv == token_left_shift_id || tv == token_right_shift_id
		|| tv == token_right_shift_unsigned_id {
		return BindingPower.shift
	}
	if tv == token_plus_id || tv == token_minus_id {
		return BindingPower.add
	}
	if tv == token_mul_id || tv == token_div_id || tv == token_mod_id || tv == 0 {
		return BindingPower.product
	}
	return BindingPower.lowest
}

// right_binding_power supports right binding power handling for Token.
@[inline]
pub fn (t Token) right_binding_power() BindingPower {
	return unsafe { BindingPower((int(t.left_binding_power()) + 1)) }
}

// is_keyword reports whether is keyword applies in token.
@[inline]
pub fn (t Token) is_keyword() bool {
	return int(t) >= int(Token.key_as) && int(t) <= int(Token.key_volatile)
}

// is_prefix reports whether is prefix applies in token.
@[inline]
pub fn (t Token) is_prefix() bool {
	tv := int(t)
	return tv == token_minus_id || tv == 0 || tv == token_and_id || tv == token_mul_id
		|| tv == token_not_id || tv == token_bit_not_id || tv == token_arrow_id
}

// is_infix reports whether is infix applies in token.
@[inline]
pub fn (t Token) is_infix() bool {
	tv := int(t)
	return tv == 0 || tv == token_and_id || tv == token_arrow_id || tv == token_div_id
		|| tv == token_eq_id || tv == token_ge_id || tv == token_gt_id || tv == token_key_in_id
		|| tv == token_key_is_id || tv == token_le_id || tv == token_left_shift_id
		|| tv == token_logical_or_id || tv == token_lt_id || tv == token_minus_id
		|| tv == token_mod_id || tv == token_mul_id || tv == token_ne_id || tv == token_not_in_id
		|| tv == token_not_is_id || tv == token_pipe_id || tv == token_plus_id
		|| tv == token_right_shift_id || tv == token_right_shift_unsigned_id || tv == token_xor_id
}

// is_postfix reports whether is postfix applies in token.
@[inline]
pub fn (t Token) is_postfix() bool {
	tv := int(t)
	return tv == token_dec_id || tv == token_inc_id
}

// is_assignment reports whether is assignment applies in token.
@[inline]
pub fn (t Token) is_assignment() bool {
	tv := int(t)
	return tv == 2 || tv == token_assign_id || tv == token_decl_assign_id
		|| tv == token_div_assign_id || tv == token_left_shift_assign_id
		|| tv == token_minus_assign_id || tv == token_mod_assign_id || tv == token_mul_assign_id
		|| tv == token_or_assign_id || tv == token_plus_assign_id
		|| tv == token_right_shift_assign_id || tv == token_right_shift_unsigned_assign_id
		|| tv == token_xor_assign_id
}

// is_overloadable reports whether is overloadable applies in token.
@[inline]
pub fn (t Token) is_overloadable() bool {
	tv := int(t)
	return tv == token_div_id || tv == token_eq_id || tv == token_ge_id || tv == token_gt_id
		|| tv == token_le_id || tv == token_lt_id || tv == token_minus_id || tv == token_mod_id
		|| tv == token_mul_id || tv == token_ne_id || tv == token_pipe_id || tv == token_plus_id
		|| tv == token_xor_id
}

// is_comparison reports whether is comparison applies in token.
@[inline]
pub fn (t Token) is_comparison() bool {
	tv := int(t)
	return tv == token_eq_id || tv == token_ge_id || tv == token_gt_id || tv == token_key_in_id
		|| tv == token_key_is_id || tv == token_le_id || tv == token_lt_id || tv == token_ne_id
		|| tv == token_not_in_id || tv == token_not_is_id
}

// str returns the string form for Token.
pub fn (t Token) str() string {
	return match t {
		.amp { '&' }
		.and { '&&' }
		.and_assign { '&=' }
		.arrow { '<-' }
		.assign { '=' }
		.attribute { '@[' }
		.bit_not { '~' }
		.char { 'char' }
		.colon { ':' }
		.comma { ',' }
		.comment { '// comment' }
		.dec { '--' }
		.decl_assign { ':=' }
		.div { '/' }
		.div_assign { '/=' }
		.dollar { '$' }
		.dot { '.' }
		.dotdot { '..' }
		.ellipsis { '...' }
		.eof { 'eof' }
		.eq { '==' }
		.ge { '>=' }
		.gt { '>' }
		.hash { '#' }
		.inc { '++' }
		.key_as { 'as' }
		.key_asm { 'asm' }
		.key_assert { 'assert' }
		.key_atomic { 'atomic' }
		.key_break { 'break' }
		.key_const { 'const' }
		.key_continue { 'continue' }
		.key_defer { 'defer' }
		.key_dump { 'dump' }
		.key_else { 'else' }
		.key_enum { 'enum' }
		.key_false { 'false' }
		.key_fn { 'fn' }
		.key_for { 'for' }
		.key_global { '__global' }
		.key_go { 'go' }
		.key_goto { 'goto' }
		.key_if { 'if' }
		.key_import { 'import' }
		.key_in { 'in' }
		.key_interface { 'interface' }
		.key_is { 'is' }
		.key_isreftype { 'isreftype' }
		.key_likely { '_likely_' }
		.key_lock { 'lock' }
		.key_match { 'match' }
		.key_module { 'module' }
		.key_mut { 'mut' }
		.key_nil { 'nil' }
		.key_none { 'none' }
		.key_offsetof { '__offsetof' }
		.key_or { 'or' }
		.key_pub { 'pub' }
		.key_return { 'return' }
		.key_rlock { 'rlock' }
		.key_select { 'select' }
		.key_shared { 'shared' }
		.key_sizeof { 'sizeof' }
		.key_spawn { 'spawn' }
		.key_static { 'static' }
		.key_struct { 'struct' }
		.key_true { 'true' }
		.key_type { 'type' }
		.key_typeof { 'typeof' }
		.key_union { 'union' }
		.key_unlikely { '_unlikely_' }
		.key_unsafe { 'unsafe' }
		.key_volatile { 'volatile' }
		.lcbr { '{' }
		.le { '<=' }
		.left_shift { '<<' }
		.left_shift_assign { '<<=' }
		.logical_or { '||' }
		.lpar { '(' }
		.lsbr { '[' }
		.lt { '<' }
		.minus { '-' }
		.minus_assign { '-=' }
		.mod { '%' }
		.mod_assign { '%=' }
		.mul { '*' }
		.mul_assign { '*=' }
		.name { 'name' }
		.ne { '!=' }
		.not { '!' }
		.not_in { '!in' }
		.not_is { '!is' }
		.number { 'number' }
		.or_assign { '|=' }
		.pipe { '|' }
		.plus { '+' }
		.plus_assign { '+=' }
		.question { '?' }
		.rcbr { '}' }
		.right_shift { '>>' }
		.right_shift_assign { '>>=' }
		.right_shift_unsigned { '>>>' }
		.right_shift_unsigned_assign { '>>>=' }
		.rpar { ')' }
		.rsbr { ']' }
		.semicolon { ';' }
		.str_dollar { '\${' }
		.string { 'string' }
		.unknown { 'unknown' }
		.xor { '^' }
		.xor_assign { '^=' }
	}
}
