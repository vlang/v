// Copyright (c) 2019-2020 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module token

pub struct Token {
pub:
	kind    Kind // the token number/enum; for quick comparisons
	lit     string // literal representation of the token
	line_nr int // the line number in the source where the token occured
	// name_idx int // name table index for O(1) lookup
	pos     int // the position of the token in scanner text
	len     int // length of the literal
	tidx    int // the index of the token
}

pub enum Kind {
	unknown
	eof
	name // user
	number // 123
	string // 'foo'
	str_inter // 'name=$user.name'
	chartoken // `A`
	plus
	minus
	mul
	div
	mod
	xor // ^
	pipe // |
	inc // ++
	dec // --
	and // &&
	logical_or
	not
	bit_not
	question
	comma
	semicolon
	colon
	arrow // <-
	amp
	hash
	dollar
	at // @
	str_dollar
	left_shift
	right_shift
	not_in // !in
	not_is // !is
	assign // =
	decl_assign // :=
	plus_assign // +=
	minus_assign // -=
	div_assign
	mult_assign
	xor_assign
	mod_assign
	or_assign
	and_assign
	right_shift_assign
	left_shift_assign // {}  () []
	lcbr
	rcbr
	lpar
	rpar
	lsbr
	rsbr // == != <= < >= >
	eq
	ne
	gt
	lt
	ge
	le
	comment
	nl
	dot
	dotdot
	ellipsis // keywords
	keyword_beg
	key_as
	key_asm
	key_assert
	key_atomic
	key_break
	key_const
	key_continue
	key_defer
	key_else
	key_enum
	key_false
	key_for
	key_fn
	key_global
	key_go
	key_goto
	key_if
	key_import
	key_in
	key_interface
	key_is // key_it
	key_match
	key_module
	key_mut
	key_shared
	key_lock
	key_rlock
	key_none
	key_return
	key_select
	key_sizeof
	key_likely
	key_unlikely
	key_offsetof
	key_struct
	key_true
	key_type
	key_typeof
	key_orelse
	key_union
	key_pub
	key_static
	key_unsafe
	keyword_end
	_end_
}

const (
	assign_tokens = [Kind.assign, .plus_assign, .minus_assign, .mult_assign, .div_assign, .xor_assign,
		.mod_assign, .or_assign, .and_assign, .right_shift_assign, .left_shift_assign]
	nr_tokens     = int(Kind._end_)
)

// @FN => will be substituted with the name of the current V function
// @MOD => will be substituted with the name of the current V module
// @STRUCT => will be substituted with the name of the current V struct
// @VEXE => will be substituted with the path to the V compiler
// @FILE => will be substituted with the path of the V source file
// @LINE => will be substituted with the V line number where it appears (as a string).
// @COLUMN => will be substituted with the column where it appears (as a string).
// @VHASH  => will be substituted with the shortened commit hash of the V compiler (as a string).
// @VMOD_FILE => will be substituted with the contents of the nearest v.mod file (as a string).
// This allows things like this:
// println( 'file: ' + @FILE + ' | line: ' + @LINE + ' | fn: ' + @MOD + '.' + @FN)
// ... which is useful while debugging/tracing
//
// @VROOT is special and handled in places like '#include ...'
// @<type> is allowed for keyword variable names. E.g. 'type'
pub enum AtKind {
	unknown
	fn_name
	mod_name
	struct_name
	vexe_path
	file_path
	line_nr
	column_nr
	vhash
	vmod_file
}

const (
	valid_at_tokens = ['@FN', '@MOD', '@STRUCT', '@VEXE', '@FILE', '@LINE', '@COLUMN', '@VHASH',
		'@VMOD_FILE',
	]
)

// build_keys genereates a map with keywords' string values:
// Keywords['return'] == .key_return
fn build_keys() map[string]Kind {
	mut res := map[string]Kind{}
	for t in int(Kind.keyword_beg) + 1 .. int(Kind.keyword_end) {
		key := token_str[t]
		res[key] = t
	}
	return res
}

// TODO remove once we have `enum Kind { name('name') if('if') ... }`
fn build_token_str() []string {
	mut s := []string{len: nr_tokens}
	s[Kind.unknown] = 'unknown'
	s[Kind.eof] = 'eof'
	s[Kind.name] = 'name'
	s[Kind.number] = 'number'
	s[Kind.string] = 'string'
	s[Kind.chartoken] = 'char'
	s[Kind.plus] = '+'
	s[Kind.minus] = '-'
	s[Kind.mul] = '*'
	s[Kind.div] = '/'
	s[Kind.mod] = '%'
	s[Kind.xor] = '^'
	s[Kind.bit_not] = '~'
	s[Kind.pipe] = '|'
	s[Kind.hash] = '#'
	s[Kind.amp] = '&'
	s[Kind.inc] = '++'
	s[Kind.dec] = '--'
	s[Kind.and] = '&&'
	s[Kind.logical_or] = '||'
	s[Kind.not] = '!'
	s[Kind.dot] = '.'
	s[Kind.dotdot] = '..'
	s[Kind.ellipsis] = '...'
	s[Kind.comma] = ','
	s[Kind.not_in] = '!in'
	s[Kind.not_is] = '!is'
	s[Kind.semicolon] = ';'
	s[Kind.colon] = ':'
	s[Kind.arrow] = '<-'
	s[Kind.assign] = '='
	s[Kind.decl_assign] = ':='
	s[Kind.plus_assign] = '+='
	s[Kind.minus_assign] = '-='
	s[Kind.mult_assign] = '*='
	s[Kind.div_assign] = '/='
	s[Kind.xor_assign] = '^='
	s[Kind.mod_assign] = '%='
	s[Kind.or_assign] = '|='
	s[Kind.and_assign] = '&='
	s[Kind.right_shift_assign] = '>>='
	s[Kind.left_shift_assign] = '<<='
	s[Kind.lcbr] = '{'
	s[Kind.rcbr] = '}'
	s[Kind.lpar] = '('
	s[Kind.rpar] = ')'
	s[Kind.lsbr] = '['
	s[Kind.rsbr] = ']'
	s[Kind.eq] = '=='
	s[Kind.ne] = '!='
	s[Kind.gt] = '>'
	s[Kind.lt] = '<'
	s[Kind.ge] = '>='
	s[Kind.le] = '<='
	s[Kind.question] = '?'
	s[Kind.left_shift] = '<<'
	s[Kind.right_shift] = '>>'
	s[Kind.comment] = '// comment'
	s[Kind.nl] = 'NLL'
	s[Kind.dollar] = '$'
	s[Kind.at] = '@'
	s[Kind.str_dollar] = '$2'
	s[Kind.key_assert] = 'assert'
	s[Kind.key_struct] = 'struct'
	s[Kind.key_if] = 'if'
	// s[Kind.key_it] = 'it'
	s[Kind.key_else] = 'else'
	s[Kind.key_asm] = 'asm'
	s[Kind.key_return] = 'return'
	s[Kind.key_module] = 'module'
	s[Kind.key_sizeof] = 'sizeof'
	s[Kind.key_likely] = '_likely_'
	s[Kind.key_unlikely] = '_unlikely_'
	s[Kind.key_go] = 'go'
	s[Kind.key_goto] = 'goto'
	s[Kind.key_const] = 'const'
	s[Kind.key_mut] = 'mut'
	s[Kind.key_shared] = 'shared'
	s[Kind.key_lock] = 'lock'
	s[Kind.key_rlock] = 'rlock'
	s[Kind.key_type] = 'type'
	s[Kind.key_for] = 'for'
	s[Kind.key_fn] = 'fn'
	s[Kind.key_true] = 'true'
	s[Kind.key_false] = 'false'
	s[Kind.key_continue] = 'continue'
	s[Kind.key_break] = 'break'
	s[Kind.key_import] = 'import'
	s[Kind.key_unsafe] = 'unsafe'
	s[Kind.key_typeof] = 'typeof'
	s[Kind.key_enum] = 'enum'
	s[Kind.key_interface] = 'interface'
	s[Kind.key_pub] = 'pub'
	s[Kind.key_in] = 'in'
	s[Kind.key_atomic] = 'atomic'
	s[Kind.key_orelse] = 'or'
	s[Kind.key_global] = '__global'
	s[Kind.key_union] = 'union'
	s[Kind.key_static] = 'static'
	s[Kind.key_as] = 'as'
	s[Kind.key_defer] = 'defer'
	s[Kind.key_match] = 'match'
	s[Kind.key_select] = 'select'
	s[Kind.key_none] = 'none'
	s[Kind.key_offsetof] = '__offsetof'
	s[Kind.key_is] = 'is'
	return s
}

const (
	token_str = build_token_str()
	keywords  = build_keys()
)

pub fn key_to_token(key string) Kind {
	return Kind(keywords[key])
}

pub fn is_key(key string) bool {
	return int(key_to_token(key)) > 0
}

pub fn is_decl(t Kind) bool {
	return t in
		[.key_enum, .key_interface, .key_fn, .key_struct, .key_type, .key_const, .key_pub, .eof]
}

pub fn (t Kind) is_assign() bool {
	return t in assign_tokens
}

pub fn (t Kind) str() string {
	return token_str[int(t)]
}

pub fn (t Token) str() string {
	return '$t.kind.str() "$t.lit"'
}

// Representation of highest and lowest precedence
/*
pub const (
	lowest_prec = 0
	highest_prec = 8
)
*/
pub enum Precedence {
	lowest
	cond // OR or AND
	in_as
	assign // =
	eq // == or !=
	// less_greater // > or <
	sum // + - | ^
	product // * / << >> &
	// mod // %
	prefix // -X or !X
	postfix // ++ or --
	call // func(X) or foo.method(X)
	index // array[index], map[key]
}

pub fn build_precedences() []Precedence {
	mut p := []Precedence{len: int(Kind._end_)}
	p[Kind.lsbr] = .index
	p[Kind.dot] = .call
	// `++` | `--` | `?`
	p[Kind.inc] = .postfix
	p[Kind.dec] = .postfix
	p[Kind.question] = .postfix
	// `*` |  `/` | `%` | `<<` | `>>` | `&`
	p[Kind.mul] = .product
	p[Kind.div] = .product
	p[Kind.mod] = .product
	p[Kind.left_shift] = .product
	p[Kind.right_shift] = .product
	p[Kind.amp] = .product
	p[Kind.arrow] = .product
	// `+` |  `-` |  `|` | `^`
	p[Kind.plus] = .sum
	p[Kind.minus] = .sum
	p[Kind.pipe] = .sum
	p[Kind.xor] = .sum
	// `==` | `!=` | `<` | `<=` | `>` | `>=`
	p[Kind.eq] = .eq
	p[Kind.ne] = .eq
	p[Kind.lt] = .eq
	p[Kind.le] = .eq
	p[Kind.gt] = .eq
	p[Kind.ge] = .eq
	// `=` | `+=` | ...
	p[Kind.assign] = .assign
	p[Kind.plus_assign] = .assign
	p[Kind.minus_assign] = .assign
	p[Kind.div_assign] = .assign
	p[Kind.mod_assign] = .assign
	p[Kind.or_assign] = .assign
	p[Kind.and_assign] = .assign
	// <<= | *= | ...
	p[Kind.left_shift_assign] = .assign
	p[Kind.right_shift_assign] = .assign
	p[Kind.mult_assign] = .assign
	p[Kind.xor_assign] = .assign
	p[Kind.key_in] = .in_as
	p[Kind.not_in] = .in_as
	p[Kind.key_as] = .in_as
	p[Kind.key_is] = .in_as
	p[Kind.not_is] = .in_as
	p[Kind.logical_or] = .cond
	p[Kind.and] = .cond
	return p
}

const (
	precedences = build_precedences()
)

// precedence returns a tokens precedence if defined, otherwise lowest_prec
pub fn (tok Token) precedence() int {
	return int(precedences[tok.kind])
}

// is_scalar returns true if the token is a scalar
pub fn (tok Token) is_scalar() bool {
	return tok.kind in [.number, .string]
}

// is_unary returns true if the token can be in a unary expression
pub fn (tok Token) is_unary() bool {
	return tok.kind in
		[
		/* `+` | `-` | `!` | `~` | `*` | `&` */.plus, .minus, .not, .bit_not, .mul, .amp, .arrow]
}

pub fn (tok Kind) is_relational() bool {
	return tok in [
		/* `<` | `<=` | `>` | `>=` */.lt, .le, .gt, .ge, .eq, .ne]
}

pub fn (k Kind) is_start_of_type() bool {
	return k in [.name, .lpar, .amp, .lsbr, .question]
}

pub fn (kind Kind) is_prefix() bool {
	return kind in [.minus, .amp, .mul, .not, .bit_not]
}

pub fn (kind Kind) is_infix() bool {
	return kind in
		[.plus, .minus, .mod, .mul, .div, .eq, .ne, .gt, .lt, .key_in, /*  */.key_as, .ge, .le, .logical_or, .xor, .not_in, .key_is, .not_is, /*  */.and, .dot, .pipe, .amp, .left_shift, .right_shift, .arrow]
}
