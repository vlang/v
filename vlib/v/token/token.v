// Copyright (c) 2019-2022 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module token

[minify]
pub struct Token {
pub:
	kind    Kind   // the token number/enum; for quick comparisons
	lit     string // literal representation of the token
	line_nr int    // the line number in the source where the token occured
	col     int    // the column in the source where the token occured
	// name_idx int // name table index for O(1) lookup
	pos  int // the position of the token in scanner text
	len  int // length of the literal
	tidx int // the index of the token
}

pub enum Kind {
	unknown
	eof
	name // user
	number // 123
	string // 'foo'
	str_inter // 'name=$user.name'
	chartoken // `A` - rune
	plus // +
	minus // -
	mul // *
	div // /
	mod // %
	xor // ^
	pipe // |
	inc // ++
	dec // --
	and // &&
	logical_or // ||
	not // !
	bit_not // ~
	question // ?
	comma // ,
	semicolon // ;
	colon // :
	arrow // <-
	amp // &
	hash // #
	dollar // $
	at // @
	str_dollar
	left_shift // <<
	right_shift // >>
	unsigned_right_shift // >>>
	not_in // !in
	not_is // !is
	assign // =
	decl_assign // :=
	plus_assign // +=
	minus_assign // -=
	div_assign // /=
	mult_assign // *=
	xor_assign // ^=
	mod_assign // %=
	or_assign // |=
	and_assign // &=
	right_shift_assign // <<=
	left_shift_assign // >>=
	unsigned_right_shift_assign // >>>=
	lcbr // {
	rcbr // }
	lpar // (
	rpar // )
	lsbr // [
	nilsbr // #[
	rsbr // ]
	eq // ==
	ne // !=
	gt // >
	lt // <
	ge // >=
	le // <=
	comment
	nl
	dot // .
	dotdot // ..
	ellipsis // ...
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
	key_is
	key_match
	key_module
	key_mut
	key_nil
	key_shared
	key_lock
	key_rlock
	key_none
	key_return
	key_select
	key_sizeof
	key_isreftype
	key_likely
	key_unlikely
	key_offsetof
	key_struct
	key_true
	key_type
	key_typeof
	key_dump
	key_orelse
	key_union
	key_pub
	key_static
	key_volatile
	key_unsafe
	key_spawn
	keyword_end
	_end_
}

// @FN => will be substituted with the name of the current V function
// @METHOD => will be substituted with ReceiverType.MethodName
// @MOD => will be substituted with the name of the current V module
// @STRUCT => will be substituted with the name of the current V struct
// @FILE => will be substituted with the path of the V source file
// @LINE => will be substituted with the V line number where it appears (as a string).
// @COLUMN => will be substituted with the column where it appears (as a string).
// @VHASH  => will be substituted with the shortened commit hash of the V compiler (as a string).
// @VMOD_FILE => will be substituted with the contents of the nearest v.mod file (as a string).
// @VMODROOT => will be substituted with the *folder* where the nearest v.mod file is (as a string).
// @VEXE => will be substituted with the path to the V compiler
// @VEXEROOT => will be substituted with the *folder* where the V executable is (as a string).
// @VROOT => the old name for @VMODROOT; sometimes it was used as @VEXEROOT;
//           Note: @VROOT is now deprecated, use either @VMODROOT or @VEXEROOT instead.
// Note: @VEXEROOT & @VMODROOT are used for compilation options like this:
//   #include "@VMODROOT/include/abc.h"
//   #flag -L@VEXEROOT/thirdparty/libgc
//
// The @XYZ tokens allow for code like this:
// println( 'file: ' + @FILE + ' | line: ' + @LINE + ' | fn: ' + @MOD + '.' + @FN)
// ... which is useful while debugging/tracing.
//
// @<type> is allowed for keyword variable names. E.g. 'type'
pub enum AtKind {
	unknown
	fn_name
	method_name
	mod_name
	struct_name
	vexe_path
	file_path
	line_nr
	column_nr
	vhash
	vmod_file
	vmodroot_path
	vroot_path // obsolete
	vexeroot_path
	file_path_line_nr
}

pub const (
	assign_tokens   = [Kind.assign, .plus_assign, .minus_assign, .mult_assign, .div_assign,
		.xor_assign, .mod_assign, .or_assign, .and_assign, .right_shift_assign, .left_shift_assign,
		.unsigned_right_shift_assign]

	valid_at_tokens = ['@VROOT', '@VMODROOT', '@VEXEROOT', '@FN', '@METHOD', '@MOD', '@STRUCT',
		'@VEXE', '@FILE', '@LINE', '@COLUMN', '@VHASH', '@VMOD_FILE', '@FILE_LINE']

	token_str       = build_token_str()

	keywords        = build_keys()
)

pub const scanner_matcher = new_keywords_matcher_trie<Kind>(keywords)

// build_keys genereates a map with keywords' string values:
// Keywords['return'] == .key_return
fn build_keys() map[string]Kind {
	mut res := map[string]Kind{}
	for t in int(Kind.keyword_beg) + 1 .. int(Kind.keyword_end) {
		key := token.token_str[t]
		res[key] = unsafe { Kind(t) }
	}
	return res
}

// TODO remove once we have `enum Kind { name('name') if('if') ... }`
fn build_token_str() []string {
	mut s := []string{len: int(Kind._end_)}
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
	s[Kind.unsigned_right_shift_assign] = '>>>='
	s[Kind.left_shift_assign] = '<<='
	s[Kind.lcbr] = '{'
	s[Kind.rcbr] = '}'
	s[Kind.lpar] = '('
	s[Kind.rpar] = ')'
	s[Kind.lsbr] = '['
	s[Kind.nilsbr] = '#['
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
	s[Kind.unsigned_right_shift] = '>>>'
	s[Kind.comment] = 'comment'
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
	s[Kind.key_isreftype] = 'isreftype'
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
	s[Kind.key_dump] = 'dump'
	s[Kind.key_enum] = 'enum'
	s[Kind.key_interface] = 'interface'
	s[Kind.key_pub] = 'pub'
	s[Kind.key_in] = 'in'
	s[Kind.key_atomic] = 'atomic'
	s[Kind.key_orelse] = 'or'
	s[Kind.key_global] = '__global'
	s[Kind.key_union] = 'union'
	s[Kind.key_static] = 'static'
	s[Kind.key_volatile] = 'volatile'
	s[Kind.key_as] = 'as'
	s[Kind.key_defer] = 'defer'
	s[Kind.key_match] = 'match'
	s[Kind.key_select] = 'select'
	s[Kind.key_none] = 'none'
	s[Kind.key_nil] = 'nil'
	s[Kind.key_offsetof] = '__offsetof'
	s[Kind.key_is] = 'is'
	s[Kind.key_spawn] = 'spawn'
	// The following kinds are not for tokens returned by the V scanner.
	// They are used just for organisation/ease of checking:
	s[Kind.keyword_beg] = 'keyword_beg'
	s[Kind.keyword_end] = 'keyword_end'
	s[Kind.str_inter] = 'str_inter'
	$if debug_build_token_str ? {
		for k, v in s {
			if v == '' {
				eprintln('>>> ${@MOD}.${@METHOD} missing k: ${k} | .${kind_to_string(unsafe { Kind(k) })}')
			}
		}
	}
	return s
}

[inline]
pub fn is_key(key string) bool {
	return int(token.keywords[key]) > 0
}

[inline]
pub fn is_decl(t Kind) bool {
	return t in [.key_enum, .key_interface, .key_fn, .key_struct, .key_type, .key_const, .key_pub,
		.eof]
}

[inline]
pub fn (t Kind) is_assign() bool {
	return t in token.assign_tokens
}

// note: used for some code generation, so no quoting
[inline]
pub fn (t Kind) str() string {
	idx := int(t)
	if idx < 0 || token.token_str.len <= idx {
		return 'unknown'
	}
	return token.token_str[idx]
}

pub fn (t Token) str() string {
	mut s := t.kind.str()
	if s.len == 0 {
		eprintln('missing token kind string')
	} else if !s[0].is_letter() {
		// punctuation, operators
		return 'token `${s}`'
	}
	if is_key(t.lit) {
		s = 'keyword'
	}
	if t.lit != '' {
		// string contents etc
		s += ' `${t.lit}`'
	}
	return s
}

pub fn (t Token) debug() string {
	ks := kind_to_string(t.kind)
	s := if t.lit == '' { t.kind.str() } else { t.lit }
	return 'tok: .${ks:-12} | lit: `${s}`'
}

// Representation of highest and lowest precedence
/*
pub const lowest_prec = 0
pub const highest_prec = 8
*/
pub enum Precedence {
	lowest
	cond // OR or AND
	in_as
	assign // =
	eq // == or !=
	// less_greater // > or <
	sum // + - | ^
	product // * / << >> >>> &
	// mod // %
	prefix // -X or !X
	postfix // ++ or --
	call // func(X) or foo.method(X)
	index // array[index], map[key]
}

pub fn build_precedences() []Precedence {
	mut p := []Precedence{len: int(Kind._end_)}
	p[Kind.lsbr] = .index
	p[Kind.nilsbr] = .index
	p[Kind.dot] = .call
	// `++` | `--` | `?`
	p[Kind.inc] = .postfix
	p[Kind.dec] = .postfix
	p[Kind.question] = .postfix
	// `*` |  `/` | `%` | `<<` | `>>` | `>>>` | `&`
	p[Kind.mul] = .product
	p[Kind.div] = .product
	p[Kind.mod] = .product
	p[Kind.left_shift] = .product
	p[Kind.right_shift] = .product
	p[Kind.unsigned_right_shift] = .product
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
	p[Kind.unsigned_right_shift_assign] = .assign
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

const precedences = build_precedences()

// precedence returns a tokens precedence if defined, otherwise lowest_prec
[inline]
pub fn (tok Token) precedence() int {
	return int(token.precedences[tok.kind])
}

// is_scalar returns true if the token is a scalar
[inline]
pub fn (tok Token) is_scalar() bool {
	return tok.kind in [.number, .string]
}

// is_unary returns true if the token can be in a unary expression
[inline]
pub fn (tok Token) is_unary() bool {
	// `+` | `-` | `!` | `~` | `*` | `&` | `<-`
	return tok.kind in [.plus, .minus, .not, .bit_not, .mul, .amp, .arrow]
}

[inline]
pub fn (tok Kind) is_relational() bool {
	// `<` | `<=` | `>` | `>=` | `==` | `!=`
	return tok in [.lt, .le, .gt, .ge, .eq, .ne]
}

[inline]
pub fn (k Kind) is_start_of_type() bool {
	return k in [.name, .lpar, .amp, .lsbr, .question, .key_shared, .not]
}

[inline]
pub fn (kind Kind) is_prefix() bool {
	return kind in [.minus, .amp, .mul, .not, .bit_not]
}

[inline]
pub fn (kind Kind) is_infix() bool {
	return kind in [.plus, .minus, .mod, .mul, .div, .eq, .ne, .gt, .lt, .key_in, .key_as, .ge,
		.le, .logical_or, .xor, .not_in, .key_is, .not_is, .and, .dot, .pipe, .amp, .left_shift,
		.right_shift, .unsigned_right_shift, .arrow]
}

[inline]
pub fn (kind Kind) is_postfix() bool {
	return kind in [.inc, .dec, .question]
}

pub fn kind_to_string(k Kind) string {
	return match k {
		.unknown { 'unknown' }
		.eof { 'eof' }
		.name { 'name' }
		.number { 'number' }
		.string { 'string' }
		.str_inter { 'str_inter' }
		.chartoken { 'chartoken' }
		.plus { 'plus' }
		.minus { 'minus' }
		.mul { 'mul' }
		.div { 'div' }
		.mod { 'mod' }
		.xor { 'xor' }
		.pipe { 'pipe' }
		.inc { 'inc' }
		.dec { 'dec' }
		.and { 'and' }
		.logical_or { 'logical_or' }
		.not { 'not' }
		.bit_not { 'bit_not' }
		.question { 'question' }
		.comma { 'comma' }
		.semicolon { 'semicolon' }
		.colon { 'colon' }
		.arrow { 'arrow' }
		.amp { 'amp' }
		.hash { 'hash' }
		.dollar { 'dollar' }
		.at { 'at' }
		.str_dollar { 'str_dollar' }
		.left_shift { 'left_shift' }
		.right_shift { 'right_shift' }
		.unsigned_right_shift { 'unsigned_right_shift' }
		.not_in { 'not_in' }
		.not_is { 'not_is' }
		.assign { 'assign' }
		.decl_assign { 'decl_assign' }
		.plus_assign { 'plus_assign' }
		.minus_assign { 'minus_assign' }
		.div_assign { 'div_assign' }
		.mult_assign { 'mult_assign' }
		.xor_assign { 'xor_assign' }
		.mod_assign { 'mod_assign' }
		.or_assign { 'or_assign' }
		.and_assign { 'and_assign' }
		.right_shift_assign { 'right_shift_assign' }
		.left_shift_assign { 'left_shift_assign' }
		.unsigned_right_shift_assign { 'unsigned_right_shift_assign' }
		.lcbr { 'lcbr' }
		.rcbr { 'rcbr' }
		.lpar { 'lpar' }
		.rpar { 'rpar' }
		.lsbr { 'lsbr' }
		.nilsbr { 'nilsbr' }
		.rsbr { 'rsbr' }
		.eq { 'eq' }
		.ne { 'ne' }
		.gt { 'gt' }
		.lt { 'lt' }
		.ge { 'ge' }
		.le { 'le' }
		.comment { 'comment' }
		.nl { 'nl' }
		.dot { 'dot' }
		.dotdot { 'dotdot' }
		.ellipsis { 'ellipsis' }
		.keyword_beg { 'keyword_beg' }
		.key_as { 'key_as' }
		.key_asm { 'key_asm' }
		.key_assert { 'key_assert' }
		.key_atomic { 'key_atomic' }
		.key_break { 'key_break' }
		.key_const { 'key_const' }
		.key_continue { 'key_continue' }
		.key_defer { 'key_defer' }
		.key_else { 'key_else' }
		.key_enum { 'key_enum' }
		.key_false { 'key_false' }
		.key_for { 'key_for' }
		.key_fn { 'key_fn' }
		.key_global { 'key_global' }
		.key_go { 'key_go' }
		.key_goto { 'key_goto' }
		.key_if { 'key_if' }
		.key_import { 'key_import' }
		.key_in { 'key_in' }
		.key_interface { 'key_interface' }
		.key_is { 'key_is' }
		.key_match { 'key_match' }
		.key_module { 'key_module' }
		.key_mut { 'key_mut' }
		.key_shared { 'key_shared' }
		.key_lock { 'key_lock' }
		.key_rlock { 'key_rlock' }
		.key_none { 'key_none' }
		.key_return { 'key_return' }
		.key_select { 'key_select' }
		.key_sizeof { 'key_sizeof' }
		.key_isreftype { 'key_isreftype' }
		.key_likely { 'key_likely' }
		.key_unlikely { 'key_unlikely' }
		.key_offsetof { 'key_offsetof' }
		.key_struct { 'key_struct' }
		.key_true { 'key_true' }
		.key_type { 'key_type' }
		.key_typeof { 'key_typeof' }
		.key_dump { 'key_dump' }
		.key_orelse { 'key_orelse' }
		.key_union { 'key_union' }
		.key_pub { 'key_pub' }
		.key_static { 'key_static' }
		.key_volatile { 'key_volatile' }
		.key_unsafe { 'key_unsafe' }
		.key_spawn { 'key_spawn' }
		.keyword_end { 'keyword_end' }
		._end_ { '_end_' }
		.key_nil { 'key_nil' }
	}
}

pub fn kind_from_string(s string) !Kind {
	return match s {
		'unknown' { .unknown }
		'eof' { .eof }
		'name' { .name }
		'number' { .number }
		'string' { .string }
		'str_inter' { .str_inter }
		'chartoken' { .chartoken }
		'plus' { .plus }
		'minus' { .minus }
		'mul' { .mul }
		'div' { .div }
		'mod' { .mod }
		'xor' { .xor }
		'pipe' { .pipe }
		'inc' { .inc }
		'dec' { .dec }
		'and' { .and }
		'logical_or' { .logical_or }
		'not' { .not }
		'bit_not' { .bit_not }
		'question' { .question }
		'comma' { .comma }
		'semicolon' { .semicolon }
		'colon' { .colon }
		'arrow' { .arrow }
		'amp' { .amp }
		'hash' { .hash }
		'dollar' { .dollar }
		'at' { .at }
		'str_dollar' { .str_dollar }
		'left_shift' { .left_shift }
		'right_shift' { .right_shift }
		'unsigned_right_shift' { .unsigned_right_shift }
		'not_in' { .not_in }
		'not_is' { .not_is }
		'assign' { .assign }
		'decl_assign' { .decl_assign }
		'plus_assign' { .plus_assign }
		'minus_assign' { .minus_assign }
		'div_assign' { .div_assign }
		'mult_assign' { .mult_assign }
		'xor_assign' { .xor_assign }
		'mod_assign' { .mod_assign }
		'or_assign' { .or_assign }
		'and_assign' { .and_assign }
		'right_shift_assign' { .right_shift_assign }
		'left_shift_assign' { .left_shift_assign }
		'unsigned_right_shift_assign' { .unsigned_right_shift_assign }
		'lcbr' { .lcbr }
		'rcbr' { .rcbr }
		'lpar' { .lpar }
		'rpar' { .rpar }
		'lsbr' { .lsbr }
		'nilsbr' { .nilsbr }
		'rsbr' { .rsbr }
		'eq' { .eq }
		'ne' { .ne }
		'gt' { .gt }
		'lt' { .lt }
		'ge' { .ge }
		'le' { .le }
		'comment' { .comment }
		'nl' { .nl }
		'dot' { .dot }
		'dotdot' { .dotdot }
		'ellipsis' { .ellipsis }
		'keyword_beg' { .keyword_beg }
		'key_as' { .key_as }
		'key_asm' { .key_asm }
		'key_assert' { .key_assert }
		'key_atomic' { .key_atomic }
		'key_break' { .key_break }
		'key_const' { .key_const }
		'key_continue' { .key_continue }
		'key_defer' { .key_defer }
		'key_else' { .key_else }
		'key_enum' { .key_enum }
		'key_false' { .key_false }
		'key_for' { .key_for }
		'key_fn' { .key_fn }
		'key_global' { .key_global }
		'key_go' { .key_go }
		'key_goto' { .key_goto }
		'key_if' { .key_if }
		'key_import' { .key_import }
		'key_in' { .key_in }
		'key_interface' { .key_interface }
		'key_is' { .key_is }
		'key_match' { .key_match }
		'key_module' { .key_module }
		'key_mut' { .key_mut }
		'key_shared' { .key_shared }
		'key_lock' { .key_lock }
		'key_rlock' { .key_rlock }
		'key_none' { .key_none }
		'key_return' { .key_return }
		'key_select' { .key_select }
		'key_sizeof' { .key_sizeof }
		'key_isreftype' { .key_isreftype }
		'key_likely' { .key_likely }
		'key_unlikely' { .key_unlikely }
		'key_offsetof' { .key_offsetof }
		'key_struct' { .key_struct }
		'key_true' { .key_true }
		'key_type' { .key_type }
		'key_typeof' { .key_typeof }
		'key_dump' { .key_dump }
		'key_orelse' { .key_orelse }
		'key_union' { .key_union }
		'key_pub' { .key_pub }
		'key_static' { .key_static }
		'key_volatile' { .key_volatile }
		'key_unsafe' { .key_unsafe }
		'key_spawn' { .key_spawn }
		'keyword_end' { .keyword_end }
		'_end_' { ._end_ }
		else { error('unknown') }
	}
}

pub fn assign_op_to_infix_op(op Kind) Kind {
	return match op {
		.plus_assign { .plus }
		.minus_assign { .minus }
		.mult_assign { .mul }
		.div_assign { .div }
		.xor_assign { .xor }
		.mod_assign { .mod }
		.or_assign { .pipe }
		.and_assign { .amp }
		.right_shift_assign { .right_shift }
		.unsigned_right_shift_assign { .unsigned_right_shift }
		.left_shift_assign { .left_shift }
		else { ._end_ }
	}
}
