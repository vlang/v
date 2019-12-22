// Copyright (c) 2019 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module token

/*
struct Token {
	tok      TokenKind // the token number/enum; for quick comparisons
	lit      string // literal representation of the token
	line_nr  int // the line number in the source where the token occured
	//name_idx int // name table index for O(1) lookup
	pos      int // the position of the token in scanner text
}
*/

pub enum Token {
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
	key_struct
	key_switch
	key_true
	key_type
	// typeof
	key_orelse
	key_union
	key_pub
	key_static
	key_unsafe
	keyword_end
}

const (
	assign_tokens = [Token.assign, .plus_assign, .minus_assign, .mult_assign,
		.div_assign, .xor_assign, .mod_assign, .or_assign, .and_assign,
		.righ_shift_assign, .left_shift_assign]

	nr_tokens = 141
)


// build_keys genereates a map with keywords' string values:
// Keywords['return'] == .key_return
fn build_keys() map[string]int {
	mut res := map[string]int
	for t := int(Token.keyword_beg) + 1; t < int(Token.keyword_end); t++ {
		key := token_str[t]
		res[key] = t
	}
	return res
}

// TODO remove once we have `enum Token { name('name') if('if') ... }`
fn build_token_str() []string {
	mut s := [''].repeat(nr_tokens)
	s[Token.keyword_beg] = ''
	s[Token.keyword_end] = ''
	s[Token.eof] = 'eof'
	s[Token.name] = 'name'
	s[Token.number] = 'number'
	s[Token.str] = 'STR'
	s[Token.chartoken] = 'char'
	s[Token.plus] = '+'
	s[Token.minus] = '-'
	s[Token.mul] = '*'
	s[Token.div] = '/'
	s[Token.mod] = '%'
	s[Token.xor] = '^'
	s[Token.bit_not] = '~'
	s[Token.pipe] = '|'
	s[Token.hash] = '#'
	s[Token.amp] = '&'
	s[Token.inc] = '++'
	s[Token.dec] = '--'
	s[Token.and] = '&&'
	s[Token.logical_or] = '||'
	s[Token.not] = '!'
	s[Token.dot] = '.'
	s[Token.dotdot] = '..'
	s[Token.ellipsis] = '...'
	s[Token.comma] = ','
	// s[Token.at] = '@'
	s[Token.semicolon] = ';'
	s[Token.colon] = ':'
	s[Token.arrow] = '=>'
	s[Token.assign] = '='
	s[Token.decl_assign] = ':='
	s[Token.plus_assign] = '+='
	s[Token.minus_assign] = '-='
	s[Token.mult_assign] = '*='
	s[Token.div_assign] = '/='
	s[Token.xor_assign] = '^='
	s[Token.mod_assign] = '%='
	s[Token.or_assign] = '|='
	s[Token.and_assign] = '&='
	s[Token.righ_shift_assign] = '>>='
	s[Token.left_shift_assign] = '<<='
	s[Token.lcbr] = '{'
	s[Token.rcbr] = '}'
	s[Token.lpar] = '('
	s[Token.rpar] = ')'
	s[Token.lsbr] = '['
	s[Token.rsbr] = ']'
	s[Token.eq] = '=='
	s[Token.ne] = '!='
	s[Token.gt] = '>'
	s[Token.lt] = '<'
	s[Token.ge] = '>='
	s[Token.le] = '<='
	s[Token.question] = '?'
	s[Token.left_shift] = '<<'
	s[Token.righ_shift] = '>>'
	s[Token.line_comment] = '// line comment'
	s[Token.mline_comment] = '/* mline comment */'
	s[Token.nl] = 'NLL'
	s[Token.dollar] = '$'
	s[Token.str_dollar] = '$2'
	s[Token.key_assert] = 'assert'
	s[Token.key_struct] = 'struct'
	s[Token.key_if] = 'if'
	// s[Token.key_it] = 'it'
	s[Token.key_else] = 'else'
	s[Token.key_asm] = 'asm'
	s[Token.key_return] = 'return'
	s[Token.key_module] = 'module'
	s[Token.key_sizeof] = 'sizeof'
	s[Token.key_go] = 'go'
	s[Token.key_goto] = 'goto'
	s[Token.key_const] = 'const'
	s[Token.key_mut] = 'mut'
	s[Token.key_type] = 'type'
	s[Token.key_for] = 'for'
	s[Token.key_switch] = 'switch'
	s[Token.key_fn] = 'fn'
	s[Token.key_true] = 'true'
	s[Token.key_false] = 'false'
	s[Token.key_continue] = 'continue'
	s[Token.key_break] = 'break'
	s[Token.key_import] = 'import'
	s[Token.key_embed] = 'embed'
	s[Token.key_unsafe] = 'unsafe'
	// Tokens[key_typeof] = 'typeof'
	s[Token.key_enum] = 'enum'
	s[Token.key_interface] = 'interface'
	s[Token.key_pub] = 'pub'
	s[Token.key_import_const] = 'import_const'
	s[Token.key_in] = 'in'
	s[Token.key_atomic] = 'atomic'
	s[Token.key_orelse] = 'or'
	s[Token.key_global] = '__global'
	s[Token.key_union] = 'union'
	s[Token.key_static] = 'static'
	s[Token.key_as] = 'as'
	s[Token.key_defer] = 'defer'
	s[Token.key_match] = 'match'
	s[Token.key_select] = 'select'
	s[Token.key_none] = 'none'
	s[Token.key_offsetof] = '__offsetof'
	return s
}

const (
	token_str = build_token_str()
	keywords = build_keys()
)

pub fn key_to_token(key string) Token {
	a := Token(keywords[key])
	return a
}

pub fn is_key(key string) bool {
	return int(key_to_token(key)) > 0
}

pub fn is_decl(t Token) bool {
	return t in [.key_enum,
.key_interface, .key_fn, .key_struct, .key_type, .key_const, .key_import_const,
.key_pub, .eof]
}

fn (t Token) is_assign() bool {
	return t in assign_tokens
}

fn (t []Token) contains(val Token) bool {
	for tt in t {
		if tt == val {
			return true
		}
	}
	return false
}

pub fn (t Token) str() string {
	lit := 't.lit'
	if t == .number {
		return lit
	}
	if t == .chartoken {
		return '`lit`'
	}
	if t == .str {
		return "'lit'"
	}
	if t < .plus {
		return lit // string, number etc
	}
	return token_str[int(t)]
}

