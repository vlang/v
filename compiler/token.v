// Copyright (c) 2019 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.

module main

enum Token {
	EOF
	NAME
	INT
	STRING
	CHAR
	FLOAT
	PLUS
	MINUS
	MUL
	DIV
	MOD
	XOR
	PIPE
	INC
	DEC
	AND
	OR
	NOT
	BIT_NOT
	QUESTION
	COMMA
	SEMICOLON
	COLON
	AMP
	HASH
	AT
	DOLLAR
	LEFT_SHIFT
	RIGHT_SHIFT
	// = := += -=
	ASSIGN
	DECL_ASSIGN
	PLUS_ASSIGN
	MINUS_ASSIGN
	DIV_ASSIGN
	MULT_ASSIGN
	XOR_ASSIGN
	MOD_ASSIGN
	OR_ASSIGN
	AND_ASSIGN
	RIGHT_SHIFT_ASSIGN
	LEFT_SHIFT_ASSIGN
	// {}  () []
	LCBR
	RCBR
	LPAR
	RPAR
	LSBR
	RSBR
	// == != <= < >= >
	EQ
	NE
	GT
	LT
	GE
	LE
	// comments
	LINE_COM
	MLINE_COM
	NL
	DOT
	DOTDOT
	// keywords
	keyword_beg
	PACKAGE
	// MODULE
	STRUCT
	IF
	ELSE
	RETURN
	GO
	CONST
	IMPORT_CONST
	MUT
	TIP
	ENUM
	FOR
	SWITCH
	MATCH
	CASE
	FUNC
	TRUE
	FALSE
	CONTINUE
	BREAK
	EMBED
	IMPORT
	TYPEOF
	DEFAULT
	ENDIF
	ASSERT
	SIZEOF
	IN
	ATOMIC
	INTERFACE
	OR_ELSE
	GLOBAL
	UNION
	PUB
	GOTO
	STATIC
	keyword_end
}

// build_keys genereates a map with keywords' string values:
// Keywords['return'] == .return
fn build_keys() map_int {
	mut res := map[string]int{}
	for t := int(keyword_beg) + 1; t < int(keyword_end); t++ {
		key := TOKENSTR[t]
		res[key] = int(t)
	}
	return res
}

fn build_token_str() []string {
	mut s := [''; 140]// TODO define a const
	s[keyword_beg] = ''
	s[keyword_end] = ''
	s[EOF] = 'EOF'
	s[NAME] = 'NAME'
	s[INT] = 'INT'
	s[STRING] = 'STR'
	s[CHAR] = 'CHAR'
	s[FLOAT] = 'FLOAT'
	s[PLUS] = '+'
	s[MINUS] = '-'
	s[MUL] = '*'
	s[DIV] = '/'
	s[MOD] = '%'
	s[XOR] = '^'
	s[BIT_NOT] = '~'
	s[PIPE] = '|'
	s[HASH] = '#'
	s[AMP] = '&'
	s[AT] = '@'
	s[INC] = '++'
	s[DEC] = '--'
	s[AND] = '&&'
	s[OR] = '||'
	s[NOT] = '!'
	s[DOT] = '.'
	s[DOTDOT] = '..'
	s[COMMA] = ','
	s[SEMICOLON] = ';'
	s[COLON] = ':'
	s[ASSIGN] = '='
	s[DECL_ASSIGN] = ':='
	s[PLUS_ASSIGN] = '+='
	s[MINUS_ASSIGN] = '-='
	s[MULT_ASSIGN] = '*='
	s[DIV_ASSIGN] = '/='
	s[XOR_ASSIGN] = '^='
	s[MOD_ASSIGN] = '%='
	s[OR_ASSIGN] = '|='
	s[AND_ASSIGN] = '&='
	s[RIGHT_SHIFT_ASSIGN] = '>>='
	s[LEFT_SHIFT_ASSIGN] = '<<='
	s[LCBR] = '{'
	s[RCBR] = '}'
	s[LPAR] = '('
	s[RPAR] = ')'
	s[LSBR] = '['
	s[RSBR] = ']'
	s[EQ] = '=='
	s[NE] = '!='
	s[GT] = '>'
	s[LT] = '<'
	s[GE] = '>='
	s[LE] = '<='
	s[QUESTION] = '?'
	s[LEFT_SHIFT] = '<<'
	s[RIGHT_SHIFT] = '>>'
	s[LINE_COM] = '//'
	s[NL] = 'NLL'
	s[DOLLAR] = '$'
	s[ASSERT] = 'assert'
	s[STRUCT] = 'struct'
	s[IF] = 'if'
	s[ELSE] = 'else'
	s[RETURN] = 'return'
	s[PACKAGE] = 'module'
	s[SIZEOF] = 'sizeof'
	s[GO] = 'go'
	s[GOTO] = 'goto'
	s[CONST] = 'const'
	s[MUT] = 'mut'
	s[TIP] = 'type'
	s[FOR] = 'for'
	s[SWITCH] = 'switch'
	s[MATCH] = 'match'
	s[CASE] = 'case'
	s[FUNC] = 'fn'
	s[TRUE] = 'true'
	s[FALSE] = 'false'
	s[CONTINUE] = 'continue'
	s[BREAK] = 'break'
	s[IMPORT] = 'import'
	s[EMBED] = 'embed'
	s[TYPEOF] = 'typeof'
	s[DEFAULT] = 'default'
	s[ENDIF] = 'endif'
	s[ENUM] = 'enum'
	s[INTERFACE] = 'interface'
	s[PUB] = 'pub'
	s[IMPORT_CONST] = 'import_const'
	s[IN] = 'in'
	s[ATOMIC] = 'atomic'
	s[OR_ELSE] = 'or'
	s[GLOBAL] = '__global'
	s[UNION] = 'union'
	s[STATIC] = 'static'
	return s
}

const (
	TOKENSTR = build_token_str()
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
	return TOKENSTR[int(t)]
}

fn (t Token) is_decl() bool {
	// TODO return t in [FUNC ,TIP, CONST,  IMPORT_CONST ,AT ,EOF]
	return t == ENUM || t == INTERFACE || t == FUNC || t == STRUCT || t == TIP ||
	t == CONST || t == IMPORT_CONST || t == AT || t == EOF
}

const (
	AssignTokens = [
		ASSIGN, PLUS_ASSIGN, MINUS_ASSIGN,
		MULT_ASSIGN, DIV_ASSIGN, XOR_ASSIGN, MOD_ASSIGN,
		OR_ASSIGN, AND_ASSIGN, RIGHT_SHIFT_ASSIGN,
		LEFT_SHIFT_ASSIGN
	]
	
)

fn (t Token) is_assign() bool {
	return t in AssignTokens
}

fn (t[]Token) contains(val Token) bool {
	for tt in t {
		if tt == val {
			return true
		}
	}
	return false
}

