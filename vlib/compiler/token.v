// Copyright (c) 2019 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.

module compiler

enum TokenKind {
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
	//key_it
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
	key_unsafe
	keyword_end
}

// build_keys genereates a map with keywords' string values:
// Keywords['return'] == .key_return
fn build_keys() map[string]int {
	mut res := map[string]int
	for t := int(TokenKind.keyword_beg) + 1; t < int(TokenKind.keyword_end); t++ {
		key := TokenStr[t]
		res[key] = int(t)
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
	//s[TokenKind.at] = '@'
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
	//s[TokenKind.line_com] = '//'
	s[TokenKind.nl] = 'NLL'
	s[TokenKind.dollar] = '$'
	s[TokenKind.key_assert] = 'assert'
	s[TokenKind.key_struct] = 'struct'
	s[TokenKind.key_if] = 'if'
	//s[TokenKind.key_it] = 'it'
	s[TokenKind.key_else] = 'else'
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
	s[TokenKind.key_case] = 'case'
	s[TokenKind.func] = 'fn'
	s[TokenKind.key_true] = 'true'
	s[TokenKind.key_false] = 'false'
	s[TokenKind.key_continue] = 'continue'
	s[TokenKind.key_break] = 'break'
	s[TokenKind.key_import] = 'import'
	s[TokenKind.key_embed] = 'embed'
	s[TokenKind.key_unsafe] = 'unsafe'
	//Tokens[key_typeof] = 'typeof'
	s[TokenKind.key_default] = 'default'
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
	return s
}

const (
	NrTokens = 140
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

fn (t TokenKind) str() string {
	return TokenStr[int(t)]
}

fn (t TokenKind) is_decl() bool {
	// TODO i
	//return t in [.key_enum, .key_interface, .func, .typ, .key_const,
		//.key_import_const, .key_struct, .key_pub, .eof]
	return t == .key_enum || t == .key_interface || t == .func ||
	t == .key_struct || t == .key_type ||
	t == .key_const || t == .key_import_const || t == .key_pub || t == .eof
}

const (
	AssignTokens = [
		TokenKind.assign, TokenKind.plus_assign, TokenKind.minus_assign,
		TokenKind.mult_assign, TokenKind.div_assign, TokenKind.xor_assign,
		TokenKind.mod_assign,
		TokenKind.or_assign, TokenKind.and_assign, TokenKind.righ_shift_assign,
		TokenKind.left_shift_assign
	]
	
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

