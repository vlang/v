// Copyright (c) 2020-2024 Joe Conigliaro. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module token

pub enum Token {
	amp        // &
	and        // &&
	and_assign // &=
	arrow      // <-
	assign     // =
	// at // @
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

pub enum BindingPower {
	lowest
	one
	two
	three
	four
	highest
}

@[inline]
pub fn (t Token) left_binding_power() BindingPower {
	return match t {
		// `||`
		.logical_or { .lowest }
		// `&&`
		.and { .one }
		// `==` | `!=` | `<` | `<=` | `>` | `>=` | `in` | `!in` | `is` | `!is`
		.eq, .ne, .lt, .le, .gt, .ge, .key_in, .not_in, .key_is, .not_is { .two }
		// `+` |  `-` |  `|` | `^`
		.plus, .minus, .pipe, .xor { .three }
		// `*` |  `/` | `%` | `<<` | `>>` | `>>>` | `&`
		.mul, .div, .mod, .left_shift, .right_shift, .right_shift_unsigned, .amp { .four }
		else { .lowest }
	}
}

// TODO: double check / fix this. just use what is needed instead of this
@[inline]
pub fn (t Token) right_binding_power() BindingPower {
	return unsafe { BindingPower((int(t.left_binding_power()) + 1)) }
}

@[inline]
pub fn (t Token) is_prefix() bool {
	return t in [.minus, .amp, .mul, .not, .bit_not, .arrow]
}

@[inline]
pub fn (t Token) is_infix() bool {
	return t in [
		.amp,
		.and,
		.arrow,
		.div,
		// .dot,
		.eq,
		.ge,
		.gt,
		// .key_as,
		.key_in,
		.key_is,
		.le,
		.left_shift,
		.logical_or,
		.lt,
		.minus,
		.mod,
		.mul,
		.ne,
		.not_in,
		.not_is,
		.pipe,
		.plus,
		.right_shift,
		.right_shift_unsigned,
		.xor,
	]
}

@[inline]
pub fn (t Token) is_postfix() bool {
	return t in [.dec, .inc]
	// return t in [.dec, .inc, .not, .question]
}

@[inline]
pub fn (t Token) is_assignment() bool {
	return t in [
		.and_assign,
		.assign,
		.decl_assign,
		.div_assign,
		.left_shift_assign,
		.minus_assign,
		.mod_assign,
		.mul_assign,
		.or_assign,
		.plus_assign,
		.right_shift_assign,
		.right_shift_unsigned_assign,
		.xor_assign,
	]
}

@[inline]
pub fn (t Token) is_overloadable() bool {
	// `/` | `==` | `>=` | `>` | `<=` | `<` | `-` | `%` | `*` | `!=` | `|` | `+` | `^`
	return t in [.div, .eq, .ge, .gt, .le, .lt, .minus, .mod, .mul, .ne, .pipe, .plus, .xor]
}

@[inline]
pub fn (t Token) is_comparison() bool {
	// `==` | `>=` | `>` | `in` | `is` | `<=` | `<` | `!=` | `!in` | `!is`
	return t in [.eq, .ge, .gt, .key_in, .key_is, .le, .lt, .ne, .not_in, .not_is]
}

// NOTE: probably switch back to map again later.
// for dev this is easier to see if any tokens are missing.
pub fn (t Token) str() string {
	return match t {
		.amp { '&' }
		.and { '&&' }
		.and_assign { '&=' }
		.arrow { '<-' }
		.assign { '=' }
		// .at { '@' }
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
