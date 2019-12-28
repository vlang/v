// Copyright (c) 2019 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module token

pub struct Token {
pub:
	kind TokenKind // the token number/enum; for quick comparisons
	lit  string // literal representation of the token
	// line_nr  int // the line number in the source where the token occured
	// name_idx int // name table index for O(1) lookup
	// pos      int // the position of the token in scanner text
}

pub enum TokenKind {
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
	assign_tokens = [TokenKind.assign, .plus_assign, .minus_assign, .mult_assign,
	.div_assign, .xor_assign, .mod_assign, .or_assign, .and_assign,
	.righ_shift_assign, .left_shift_assign]
	nr_tokens = 141
)
// build_keys genereates a map with keywords' string values:
// Keywords['return'] == .key_return
fn build_keys() map[string]int {
	mut res := map[string]int
	for t := int(TokenKind.keyword_beg) + 1; t < int(TokenKind.keyword_end); t++ {
		key := token_str[t]
		res[key] = t
	}
	return res
}

// TODO remove once we have `enum TokenKind { name('name') if('if') ... }`
fn build_token_str() []string {
	mut s := [''].repeat(nr_tokens)
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
	// TokenKinds[key_typeof] = 'typeof'
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
	return s
}

const (
	token_str = build_token_str()
	keywords = build_keys()
)

pub fn key_to_token(key string) TokenKind {
	a := TokenKind(keywords[key])
	return a
}

pub fn is_key(key string) bool {
	return int(key_to_token(key)) > 0
}

pub fn is_decl(t TokenKind) bool {
	return t in [.key_enum, .key_interface, .key_fn, .key_struct, .key_type, .key_const, .key_import_const, .key_pub, .eof]
}

pub fn (t Token) is_assign() bool {
	return t.kind in assign_tokens
}

fn (t []TokenKind) contains(val TokenKind) bool {
	for tt in t {
		if tt == val {
			return true
		}
	}
	return false
}

pub fn (t TokenKind) str() string {
	if t == .number {
		return 'number'
	}
	if t == .chartoken {
		return 'char' // '`lit`'
	}
	if t == .str {
		return 'str' // "'lit'"
	}
	/*
	if t < .plus {
		return lit // string, number etc
	}
	*/

	return token_str[int(t)]
}

pub fn (t Token) str() string {
	return '$t.kind.str() "$t.lit"'
}

// Representation of highest and lowest precedence
pub const (
	lowest_prec = 0
	highest_prec = 7
)
// Precedence returns a tokens precedence if defined, otherwise lowest_prec
pub fn (tok Token) precedence() int {
	match tok.kind {
		// `*` |  `/` | `%` | `<<` | `>>` | `&`
		.mul, .div, .left_shift, .righ_shift, .amp {
			return 7
		}
		// `+` |  `-` |  `|` | `^`
		.plus, .minus, .pipe, .xor {
			return 6
		}
		// `==` | `!=` | `<` | `<=` | `>` | `>=`
		.eq, .ne, .lt, .le, .gt, .ge {
			return 5
		}
		// `&&`
		.and {
			return 4
		}
		// `||`
		.logical_or {
			return 3
		}
		// /.plus_assign {
		// /return 2
		// /}
		else {
			return lowest_prec
		}
	}
}

// is_scalar returns true if the token is a scalar
pub fn (tok Token) is_scalar() bool {
	return tok.kind in [.number, .str]
}

// is_unary returns true if the token can be in a unary expression
pub fn (tok Token) is_unary() bool {
	return tok.kind in [
	// `+` | `-` | `!` | `~` | `*` | `&`
	.plus, .minus, .not, .bit_not, .mul, .amp]
}

// NOTE: do we need this for all tokens (is_left_assoc / is_right_assoc),
// or only ones with the same precedence?
// is_left_assoc returns true if the token is left associative
pub fn (tok Token) is_left_assoc() bool {
	return tok.kind in [
	// .number,
	// `*` | `/` | `%`
	.mul, .div, .mod,
	// `^` | `||` | `&`
	.xor, .logical_or, .and,
	// `,`
	.comma]
}

// is_right_assoc returns true if the token is right associative
pub fn (tok Token) is_right_assoc() bool {
	return tok.kind in [
	// `+` | `-` | `!` | `++` | `--`
	.plus, .minus, .not, .inc, .dec,
	// `=` | `+=` | `-=` | `*=` | `/=`
	.assign, .plus_assign, .minus_assign, .mult_assign, .div_assign,
	// `%=` | `>>=` | `<<=`
	.mod_assign, .righ_shift_assign, .left_shift_assign,
	// `&=` | `^=` | `|=`
	.and_assign, .xor_assign, .or_assign]
}
