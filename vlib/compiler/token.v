// Copyright (c) 2019-2020 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module compiler

struct Token {
	tok      TokenKind // the token number/enum; for quick comparisons
	lit      string // literal representation of the token
	line_nr  int // the line number in the source where the token occured
	name_idx int // name table index for O(1) lookup
	pos      int // the position of the token in scanner text
}

enum TokenKind {
	eof
	name // user
	number // 123
	str // 'foo'
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
	arrow // =>
	left_arrow // <-
	amp
	hash
	dollar
	str_dollar
	left_shift
	righ_shift
	// at // @
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
	righ_shift_assign
	left_shift_assign
	// {}  () []
	lcbr
	rcbr
	lpar
	rpar
	lsbr
	rsbr
	// == != <= < >= >
	eq
	ne
	gt
	lt
	ge
	le
	// comments
	line_comment
	mline_comment
	nl
	dot
	dotdot
	ellipsis
	// keywords
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
	key_embed
	key_enum
	key_false
	key_for
	key_fn
	key_global
	key_go
	key_goto
	key_if
	key_import
	key_import_const
	key_in
	key_interface
	// key_it
	key_match
	key_module
	key_mut
	key_none
	key_return
	key_select
	key_sizeof
	key_offsetof
	key_nameof
	key_struct
	key_switch
	key_true
	key_type
	key_typeof
	key_orelse
	key_union
	key_pub
	key_static
	key_unsafe
	keyword_end
}

// build_keys genereates a map with keywords' string values:
// Keywords['return'] == .key_return
fn build_keys() map[string]int {
	mut res := map[string]int
	for t := int(TokenKind.keyword_beg) + 1; t < int(TokenKind.keyword_end); t++ {
		key := TokenStr[t]
		res[key] = t
	}
	return res
}

// TODO remove once we have `enum TokenKind { name('name') if('if') ... }`
fn build_token_str() []string {
	mut s := [''].repeat(NrTokens)
	s[TokenKind.keyword_beg] = ''
	s[TokenKind.keyword_end] = ''
	s[TokenKind.eof] = 'eof'
	s[TokenKind.name] = 'name'
	s[TokenKind.number] = 'number'
	s[TokenKind.str] = 'STR'
	s[TokenKind.chartoken] = 'char'
	s[TokenKind.plus] = '+'
	s[TokenKind.minus] = '-'
	s[TokenKind.mul] = '*'
	s[TokenKind.div] = '/'
	s[TokenKind.mod] = '%'
	s[TokenKind.xor] = '^'
	s[TokenKind.bit_not] = '~'
	s[TokenKind.pipe] = '|'
	s[TokenKind.hash] = '#'
	s[TokenKind.amp] = '&'
	s[TokenKind.inc] = '++'
	s[TokenKind.dec] = '--'
	s[TokenKind.and] = '&&'
	s[TokenKind.logical_or] = '||'
	s[TokenKind.not] = '!'
	s[TokenKind.dot] = '.'
	s[TokenKind.dotdot] = '..'
	s[TokenKind.ellipsis] = '...'
	s[TokenKind.comma] = ','
	// s[TokenKind.at] = '@'
	s[TokenKind.semicolon] = ';'
	s[TokenKind.colon] = ':'
	s[TokenKind.arrow] = '=>'
	s[TokenKind.assign] = '='
	s[TokenKind.decl_assign] = ':='
	s[TokenKind.plus_assign] = '+='
	s[TokenKind.minus_assign] = '-='
	s[TokenKind.mult_assign] = '*='
	s[TokenKind.div_assign] = '/='
	s[TokenKind.xor_assign] = '^='
	s[TokenKind.mod_assign] = '%='
	s[TokenKind.or_assign] = '|='
	s[TokenKind.and_assign] = '&='
	s[TokenKind.righ_shift_assign] = '>>='
	s[TokenKind.left_shift_assign] = '<<='
	s[TokenKind.lcbr] = '{'
	s[TokenKind.rcbr] = '}'
	s[TokenKind.lpar] = '('
	s[TokenKind.rpar] = ')'
	s[TokenKind.lsbr] = '['
	s[TokenKind.rsbr] = ']'
	s[TokenKind.eq] = '=='
	s[TokenKind.ne] = '!='
	s[TokenKind.gt] = '>'
	s[TokenKind.lt] = '<'
	s[TokenKind.ge] = '>='
	s[TokenKind.le] = '<='
	s[TokenKind.question] = '?'
	s[TokenKind.left_shift] = '<<'
	s[TokenKind.righ_shift] = '>>'
	s[TokenKind.line_comment] = '// line comment'
	s[TokenKind.mline_comment] = '/* mline comment */'
	s[TokenKind.nl] = 'NLL'
	s[TokenKind.dollar] = '$'
	s[TokenKind.str_dollar] = '$2'
	s[TokenKind.key_assert] = 'assert'
	s[TokenKind.key_struct] = 'struct'
	s[TokenKind.key_if] = 'if'
	// s[TokenKind.key_it] = 'it'
	s[TokenKind.key_else] = 'else'
	s[TokenKind.key_asm] = 'asm'
	s[TokenKind.key_return] = 'return'
	s[TokenKind.key_module] = 'module'
	s[TokenKind.key_sizeof] = 'sizeof'
	s[TokenKind.key_go] = 'go'
	s[TokenKind.key_goto] = 'goto'
	s[TokenKind.key_const] = 'const'
	s[TokenKind.key_mut] = 'mut'
	s[TokenKind.key_type] = 'type'
	s[TokenKind.key_for] = 'for'
	s[TokenKind.key_switch] = 'switch'
	s[TokenKind.key_fn] = 'fn'
	s[TokenKind.key_true] = 'true'
	s[TokenKind.key_false] = 'false'
	s[TokenKind.key_continue] = 'continue'
	s[TokenKind.key_break] = 'break'
	s[TokenKind.key_import] = 'import'
	s[TokenKind.key_embed] = 'embed'
	s[TokenKind.key_unsafe] = 'unsafe'
	s[TokenKind.key_typeof] = 'typeof'
	s[TokenKind.key_enum] = 'enum'
	s[TokenKind.key_interface] = 'interface'
	s[TokenKind.key_pub] = 'pub'
	s[TokenKind.key_import_const] = 'import_const'
	s[TokenKind.key_in] = 'in'
	s[TokenKind.key_atomic] = 'atomic'
	s[TokenKind.key_orelse] = 'or'
	s[TokenKind.key_global] = '__global'
	s[TokenKind.key_union] = 'union'
	s[TokenKind.key_static] = 'static'
	s[TokenKind.key_as] = 'as'
	s[TokenKind.key_defer] = 'defer'
	s[TokenKind.key_match] = 'match'
	s[TokenKind.key_select] = 'select'
	s[TokenKind.key_none] = 'none'
	s[TokenKind.key_offsetof] = '__offsetof'
	s[TokenKind.key_nameof] = 'nameof'
	return s
}

const (
	NrTokens = 141
	TokenStr = build_token_str()
	KEYWORDS = build_keys()
)

fn key_to_token(key string) TokenKind {
	a := TokenKind(KEYWORDS[key])
	return a
}

fn is_key(key string) bool {
	return int(key_to_token(key)) > 0
}

pub fn (t TokenKind) str() string {
	return TokenStr[int(t)]
}

fn (t TokenKind) is_decl() bool {
	return t in [.key_enum, .key_interface, .key_fn, .key_struct, .key_type, .key_const, .key_import_const, .key_pub, .eof]
}

const (
	AssignTokens = [TokenKind.assign, .plus_assign, .minus_assign, .mult_assign, .div_assign, .xor_assign, .mod_assign, .or_assign, .and_assign, .righ_shift_assign, .left_shift_assign]
)

fn (t TokenKind) is_assign() bool {
	return t in AssignTokens
}

fn (t []TokenKind) contains(val TokenKind) bool {
	for tt in t {
		if tt == val {
			return true
		}
	}
	return false
}

pub fn (t Token) str() string {
	if t.tok == .number {
		return t.lit
	}
	if t.tok == .chartoken {
		return '`$t.lit`'
	}
	if t.tok == .str {
		return "'$t.lit'"
	}
	if t.tok == .eof {
		return '.EOF'
	}
	if t.tok < .plus {
		return t.lit // string, number etc
	}
	return t.tok.str()
}

pub fn (t Token) detailed_str() string {
	return 'Token{ .line:${t.line_nr:4d}, .pos:${t.pos:5d}, .tok: ${t.tok:3d} } = $t '
}

