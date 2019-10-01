// Copyright (c) 2019 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.

module main

enum Token {
	eof
	name        // user
	number      // 123
	str         // 'foo'
	str_inter   // 'name=$user.name'
	chartoken   // `A`
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
	left_shift
	righ_shift
	//at // @
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
	//line_com
	//mline_com
	nl
	dot
	dotdot
	ellipsis
	// keywords
	keyword_beg
	key_as
	key_assert
	key_atomic
	key_break
	key_case
	key_const
	key_continue
	key_default
	key_defer
	key_else
	key_embed
	key_enum
	key_false
	key_for
	func
	key_global
	key_go
	key_goto
	key_if
	key_import
	key_import_const
	key_in
	key_interface
	key_match
	key_module
	key_mut
	key_none
	key_return
	key_select
	key_sizeof
	key_struct
	key_switch
	key_true
	key_type
	//typeof
	key_orelse
	key_union
	key_pub
	key_static
	keyword_end
}

// build_keys genereates a map with keywords' string values:
// Keywords['return'] == .key_return
fn build_keys() map[string]int {
	mut res := map[string]int
	for t := int(Token.keyword_beg) + 1; t < int(Token.keyword_end); t++ {
		key := TokenStr[t]
		res[key] = int(t)
	}
	return res
}

// TODO remove once we have `enum Token { name('name') if('if') ... }`
fn build_token_str() []string {
	mut s := [''].repeat(NrTokens)
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
	//s[Token.at] = '@'
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
	//s[Token.line_com] = '//'
	s[Token.nl] = 'NLL'
	s[Token.dollar] = '$'
	s[Token.key_assert] = 'assert'
	s[Token.key_struct] = 'struct'
	s[Token.key_if] = 'if'
	s[Token.key_else] = 'else'
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
	s[Token.key_case] = 'case'
	s[Token.func] = 'fn'
	s[Token.key_true] = 'true'
	s[Token.key_false] = 'false'
	s[Token.key_continue] = 'continue'
	s[Token.key_break] = 'break'
	s[Token.key_import] = 'import'
	s[Token.key_embed] = 'embed'
	//Tokens[key_typeof] = 'typeof'
	s[Token.key_default] = 'default'
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
	return s
}

const (
	NrTokens = 140
	TokenStr = build_token_str()
	KEYWORDS = build_keys()
)

fn key_to_token(key string) Token {
	a := Token(KEYWORDS[key])
	return a
}

fn is_key(key string) bool {
	return int(key_to_token(key)) > 0
}

fn (t Token) str() string {
	return TokenStr[int(t)]
}

fn (t Token) is_decl() bool {
	// TODO i
	//return t in [.key_enum, .key_interface, .func, .typ, .key_const,
		//.key_import_const, .key_struct, .key_pub, .eof]
	return t == .key_enum || t == .key_interface || t == .func ||
	t == .key_struct || t == .key_type ||
	t == .key_const || t == .key_import_const || t == .key_pub || t == .eof
}

const (
	AssignTokens = [
		Token.assign, Token.plus_assign, Token.minus_assign,
		Token.mult_assign, Token.div_assign, Token.xor_assign,
		Token.mod_assign,
		Token.or_assign, Token.and_assign, Token.righ_shift_assign,
		Token.left_shift_assign
	]
	
)

fn (t Token) is_assign() bool {
	return t in AssignTokens
}

fn (t []Token) contains(val Token) bool {
	for tt in t {
		if tt == val {
			return true
		}
	}
	return false
}

