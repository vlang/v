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
// The levels mirror V's documented operator precedence (see the language
// reference, Appendix II): `+ - | ^` all share the `sum` level and
// `* / % << >> >>> &` all share the `product` level. Do not split these into
// finer levels — doing so changes the parse of expressions that mix operators
// of the same documented precedence, e.g. `1 | 2 ^ 3` must fold to `(1 | 2) ^ 3`
// at `sum` and `a << b * c` must fold to `(a << b) * c` at `product`.
pub enum BindingPower {
	lowest
	logical_or  // ||
	logical_and // &&
	compare     // ==, !=, <, <=, >, >=, in, !in, is, !is
	sum         // +, -, |, ^
	product     // *, /, %, <<, >>, >>>, &
	highest
}

// left_binding_power supports left binding power handling for Token.
@[inline]
pub fn (t Token) left_binding_power() BindingPower {
	return match t {
		.logical_or { .logical_or }
		.and { .logical_and }
		.eq, .ne, .lt, .le, .gt, .ge, .key_in, .not_in, .key_is, .not_is { .compare }
		.plus, .minus, .pipe, .xor { .sum }
		.mul, .div, .mod, .amp, .left_shift, .right_shift, .right_shift_unsigned { .product }
		else { .lowest }
	}
}

// right_binding_power supports right binding power handling for Token.
// Every binary operator here is left-associative, so the right binding power is
// the level immediately above the operator's own level: parsing the right
// operand stops as soon as it sees another operator at the same level, which
// hands that operator back to the outer loop and folds left.
@[inline]
pub fn (t Token) right_binding_power() BindingPower {
	return match t.left_binding_power() {
		.logical_or { .logical_and }
		.logical_and { .compare }
		.compare { .sum }
		.sum { .product }
		.product { .highest }
		else { .lowest }
	}
}

// is_keyword reports whether is keyword applies in token.
@[inline]
pub fn (t Token) is_keyword() bool {
	return t in [.key_as, .key_asm, .key_assert, .key_atomic, .key_break, .key_const, .key_continue,
		.key_defer, .key_dump, .key_else, .key_enum, .key_false, .key_fn, .key_for, .key_global,
		.key_go, .key_goto, .key_if, .key_import, .key_in, .key_interface, .key_is, .key_isreftype,
		.key_likely, .key_lock, .key_match, .key_module, .key_mut, .key_nil, .key_none, .key_offsetof,
		.key_or, .key_pub, .key_return, .key_rlock, .key_select, .key_shared, .key_sizeof, .key_spawn,
		.key_static, .key_struct, .key_true, .key_type, .key_typeof, .key_union, .key_unlikely,
		.key_unsafe, .key_volatile]
}

// is_prefix reports whether is prefix applies in token.
@[inline]
pub fn (t Token) is_prefix() bool {
	return t in [.minus, .amp, .and, .mul, .not, .bit_not, .arrow]
}

// is_infix reports whether is infix applies in token.
@[inline]
pub fn (t Token) is_infix() bool {
	return t in [.amp, .and, .arrow, .div, .eq, .ge, .gt, .key_in, .key_is, .le, .left_shift,
		.logical_or, .lt, .minus, .mod, .mul, .ne, .not_in, .not_is, .pipe, .plus, .right_shift,
		.right_shift_unsigned, .xor]
}

// is_postfix reports whether is postfix applies in token.
@[inline]
pub fn (t Token) is_postfix() bool {
	return t in [.dec, .inc]
}

// is_assignment reports whether is assignment applies in token.
@[inline]
pub fn (t Token) is_assignment() bool {
	return t in [.and_assign, .assign, .decl_assign, .div_assign, .left_shift_assign, .minus_assign,
		.mod_assign, .mul_assign, .or_assign, .plus_assign, .right_shift_assign,
		.right_shift_unsigned_assign, .xor_assign]
}

// is_overloadable reports whether is overloadable applies in token.
@[inline]
pub fn (t Token) is_overloadable() bool {
	return t in [.div, .eq, .ge, .gt, .le, .lt, .minus, .mod, .mul, .ne, .pipe, .plus, .xor]
}

// is_comparison reports whether is comparison applies in token.
@[inline]
pub fn (t Token) is_comparison() bool {
	return t in [.eq, .ge, .gt, .key_in, .key_is, .le, .lt, .ne, .not_in, .not_is]
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
