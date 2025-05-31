// Copyright (c) 2020-2024 Joe Conigliaro. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module ast

import v2.token

pub const empty_expr = Expr(EmptyExpr(0))
pub const empty_stmt = Stmt(EmptyStmt(0))

type EmptyExpr = u8
type EmptyStmt = u8

pub type Expr = ArrayInitExpr
	| AsCastExpr
	| AssocExpr
	| BasicLiteral
	| CallExpr
	| CallOrCastExpr
	| CastExpr
	| ComptimeExpr
	| EmptyExpr
	| FieldInit
	| FnLiteral
	| GenericArgOrIndexExpr
	| GenericArgs
	| Ident
	| IfExpr
	| IfGuardExpr
	| IndexExpr
	| InfixExpr
	| InitExpr
	| Keyword
	| KeywordOperator
	| LambdaExpr
	| LockExpr
	| MapInitExpr
	| MatchExpr
	| ModifierExpr
	| OrExpr
	| ParenExpr
	| PostfixExpr
	| PrefixExpr
	| RangeExpr
	| SelectExpr
	| SelectorExpr
	| SqlExpr
	| StringInterLiteral
	| StringLiteral
	| Tuple
	| Type
	| UnsafeExpr

// TODO: decide if this going to be done like this (FieldInit)

pub type Stmt = AsmStmt
	| AssertStmt
	| AssignStmt
	| BlockStmt
	| ComptimeStmt
	| ConstDecl
	| DeferStmt
	| Directive
	| EmptyStmt
	| EnumDecl
	| ExprStmt
	| FlowControlStmt
	| FnDecl
	| ForInStmt
	| ForStmt
	| GlobalDecl
	| ImportStmt
	| InterfaceDecl
	| LabelStmt
	| ModuleStmt
	| ReturnStmt
	| StructDecl
	| TypeDecl
	| []Attribute

// pub type Decl = ConstDecl | EnumDecl | FnDecl | GlobalDecl
// 	| InterfaceDecl | StructDecl | TypeDecl

// TODO: (re)implement nested sumtype like TS (was removed from v)
// currently need to cast to type in parser.type. Should I leave like
// this or add these directly to Expr until nesting is implemented?
pub type Type = AnonStructType
	| ArrayFixedType
	| ArrayType
	| ChannelType
	| FnType
	| GenericType
	| MapType
	| NilType
	| NoneType
	| OptionType
	| ResultType
	| ThreadType
	| TupleType

pub fn (exprs []Expr) name_list() string {
	mut name_list := ''
	for i, expr in exprs {
		name_list += expr.name()
		if i < exprs.len - 1 {
			name_list += ','
		}
	}
	return name_list
}

pub fn (t Type) name() string {
	return match t {
		GenericType {
			'${t.name.name()}[${t.params.name_list()}]'
		}
		else {
			panic('Type.name(): unsupported expr `${t.type_name()}`, currently only supports `ast.Ident` & `ast.SelectorExpr`')
			// expr.str()
		}
	}
}

// TODO: fix this, should be only what is needed
pub fn (expr Expr) name() string {
	return match expr {
		AsCastExpr {
			'${expr.expr.name()} as ${expr.typ.name()}'
		}
		BasicLiteral {
			expr.value
		}
		CallExpr {
			// TODO:
			'${expr.lhs.name()}()'
		}
		CallOrCastExpr {
			'${expr.lhs.name()}(${expr.expr.name()})'
		}
		EmptyExpr {
			// TODO:
			'EmptyExpr'
		}
		GenericArgs {
			'${expr.lhs.name()}[${expr.args.name_list()}]'
		}
		Ident {
			expr.name
		}
		IndexExpr {
			'${expr.lhs.name()}[${expr.expr.name()}]'
		}
		InfixExpr {
			'${expr.lhs.name()} ${expr.op} ${expr.rhs.name()}'
		}
		Keyword {
			expr.tok.str()
		}
		ModifierExpr {
			'${expr.kind} ${expr.expr.name()}'
		}
		ParenExpr {
			'(${expr.expr.name()})'
		}
		PrefixExpr {
			'${expr.op}${expr.expr.name()}'
		}
		SelectorExpr {
			expr.name()
		}
		StringLiteral {
			"'${expr.value}'"
		}
		Type {
			expr.name()
		}
		UnsafeExpr {
			// TODO:
			'UnsafeExpr'
		}
		else {
			panic('Expr.name(): unsupported expr `${expr.type_name()}`, add it?')
			// expr.str()
		}
	}
}

pub fn (expr Expr) pos() token.Pos {
	return match expr {
		Ident {
			expr.pos
		}
		SelectorExpr {
			expr.pos
			// NOTE: should we remove `pos` from `SelectorExpr` and use `expr.lhs.pos()` instead?
			// which would always get the position of the left most part of the `SelectorExpr`
			// expr.lhs.pos()
		}
		else {
			panic('Expr.pos(): TODO: add ${expr.type_name()}')
		}
	}
}

// File (AST container)
pub struct File {
pub:
	attributes []Attribute
	mod        string
	name       string
	stmts      []Stmt
	imports    []ImportStmt
}

pub enum Language {
	v
	c
	js
}

pub fn (lang Language) str() string {
	return match lang {
		.v { 'V' }
		.c { 'C' }
		.js { 'JS' }
	}
}

// Expressions
pub struct ArrayInitExpr {
pub:
	typ   Expr = empty_expr
	exprs []Expr
	init  Expr = empty_expr
	cap   Expr = empty_expr
	len   Expr = empty_expr
	pos   token.Pos
}

pub struct AsCastExpr {
pub:
	expr Expr
	typ  Expr
	pos  token.Pos
}

pub struct AssocExpr {
pub:
	typ    Expr
	expr   Expr
	fields []FieldInit
}

pub struct BasicLiteral {
pub:
	kind  token.Token
	value string
}

pub struct CallExpr {
pub:
	lhs  Expr
	args []Expr
	pos  token.Pos
}

pub struct CallOrCastExpr {
pub:
	lhs  Expr
	expr Expr
	pos  token.Pos
}

pub struct CastExpr {
pub:
	typ  Expr
	expr Expr
	pos  token.Pos
}

pub struct ComptimeExpr {
pub:
	expr Expr
	pos  token.Pos
}

pub struct FieldDecl {
pub:
	name       string
	typ        Expr = empty_expr // can be empty as used for const (unless we use something else)
	value      Expr = empty_expr
	attributes []Attribute
}

pub struct FieldInit {
pub:
	name  string
	value Expr
}

// anon fn / closure
pub struct FnLiteral {
pub:
	typ           FnType
	captured_vars []Expr
	stmts         []Stmt
}

pub struct GenericArgs {
pub:
	lhs  Expr
	args []Expr // concrete types
}

pub struct GenericArgOrIndexExpr {
pub:
	lhs  Expr
	expr Expr
}

pub struct Ident {
pub:
	pos  token.Pos
	name string
}

pub struct IfExpr {
pub:
	cond      Expr = empty_expr
	else_expr Expr = empty_expr
	stmts     []Stmt
}

pub struct IfGuardExpr {
pub:
	stmt AssignStmt
}

pub struct InfixExpr {
pub:
	op  token.Token
	lhs Expr
	rhs Expr
	pos token.Pos
}

pub struct IndexExpr {
pub:
	lhs      Expr
	expr     Expr
	is_gated bool
}

pub struct InitExpr {
pub:
	typ    Expr
	fields []FieldInit
}

pub struct Keyword {
pub:
	tok token.Token
}

pub struct KeywordOperator {
pub:
	op    token.Token
	exprs []Expr
}

pub struct Tuple {
pub:
	exprs []Expr
}

pub struct LambdaExpr {
pub:
	args []Ident
	expr Expr
}

pub struct LockExpr {
pub:
	lock_exprs  []Expr
	rlock_exprs []Expr
	stmts       []Stmt
}

pub struct MapInitExpr {
pub:
	typ  Expr = empty_expr
	keys []Expr
	vals []Expr
	pos  token.Pos
}

pub struct MatchBranch {
pub:
	cond  []Expr
	stmts []Stmt
	pos   token.Pos
}

pub struct MatchExpr {
pub:
	expr     Expr
	branches []MatchBranch
	pos      token.Pos
}

// TODO: possibly merge modifiers into a bitfield where
// multiple modifiers are used. this could also be used
// for struct decl `mut:`, `pub mut:` etc. consider this
// [flag]
// pub enum Modifier {
// 	.atomic
// 	.global
// 	.mutable
// 	.public
// 	.shared
// 	.static
// 	.volatile
// }

pub struct ModifierExpr {
pub:
	kind token.Token
	expr Expr
}

// pub fn (expr ModifierExpr) unwrap() Expr {
// 	return expr.expr
// }

pub struct OrExpr {
pub:
	expr  Expr
	stmts []Stmt
	pos   token.Pos
}

pub struct Parameter {
pub:
	name   string
	typ    Expr
	is_mut bool
	pos    token.Pos
}

pub struct ParenExpr {
pub:
	expr Expr
}

pub struct PostfixExpr {
pub:
	op   token.Token
	expr Expr
}

pub struct PrefixExpr {
pub:
	op   token.Token
	expr Expr
	pos  token.Pos
}

pub struct RangeExpr {
pub:
	op    token.Token // `..` exclusive | `...` inclusive
	start Expr
	end   Expr
}

pub struct SelectExpr {
pub:
	pos   token.Pos
	stmt  Stmt
	stmts []Stmt
	next  Expr = empty_expr
}

pub struct SelectorExpr {
pub:
	lhs Expr
	rhs Ident
	pos token.Pos
}

pub fn (se SelectorExpr) leftmost() Expr {
	if se.lhs is SelectorExpr {
		return se.lhs.leftmost()
	}
	return se.lhs
}

// pub fn (expr Expr) str() string {
// 	return 'Expr.str() - $expr.type_name()'
// }

pub fn (se SelectorExpr) name() string {
	return se.lhs.name() + '.' + se.rhs.name
}

pub enum StringLiteralKind {
	c
	js
	raw
	v
}

pub fn (s StringLiteralKind) str() string {
	return match s {
		.c { 'c' }
		.js { 'js' }
		.raw { 'r' }
		.v { 'v' }
	}
}

// TODO: allow overriding this method in main v compiler
// that is why this method was renamed from `from_string`
@[direct_array_access]
pub fn StringLiteralKind.from_string_tinyv(s string) !StringLiteralKind {
	match s[0] {
		`c` {
			return .c
		}
		`j` {
			if s[1] == `s` {
				return .js
			}
		}
		`r` {
			return .raw
		}
		else {}
	}
	return error('invalid string prefix `${s}`')
}

// NOTE: I'm using two nodes StringLiteral & StringInterLiteral
// to avoid the extra array allocations when not needed.
pub struct StringLiteral {
pub:
	kind  StringLiteralKind
	value string
}

pub struct StringInterLiteral {
pub:
	kind   StringLiteralKind
	values []string
	inters []StringInter
}

pub struct StringInter {
pub:
	format    StringInterFormat
	width     int
	precision int
	expr      Expr
	// TEMP: prob removed once individual
	// fields are set, precision etc
	format_expr Expr = empty_expr
}

pub enum StringInterFormat {
	unformatted
	binary
	character
	decimal
	exponent
	exponent_short
	float
	hex
	octal
	pointer_address
	string
}

pub fn StringInterFormat.from_u8(c u8) !StringInterFormat {
	return match c {
		`b` { .binary }
		`c` { .character }
		`d` { .decimal }
		`e`, `E` { .exponent }
		`g`, `G` { .exponent_short }
		`f`, `F` { .float }
		`x`, `X` { .hex }
		`o` { .octal }
		`p` { .pointer_address }
		`s` { .string }
		else { error('unknown formatter `${c.ascii_str()}`') }
	}
}

pub fn (sif StringInterFormat) str() string {
	return match sif {
		.unformatted { '' }
		.binary { 'b' }
		.character { 'c' }
		.decimal { 'd' }
		.exponent { 'e' }
		.exponent_short { 'g' }
		.float { 'f' }
		.hex { 'x' }
		.octal { 'o' }
		.pointer_address { 'p' }
		.string { 's' }
	}
}

pub struct SqlExpr {
pub:
	expr Expr
}

pub struct UnsafeExpr {
pub:
	stmts []Stmt
}

// Statements
pub struct AsmStmt {
pub:
	arch string
}

pub struct AssertStmt {
pub:
	expr  Expr
	extra Expr = empty_expr
}

pub struct AssignStmt {
pub:
	op  token.Token
	lhs []Expr
	rhs []Expr
	pos token.Pos
}

pub fn (attributes []Attribute) has(name string) bool {
	for attribute in attributes {
		if attribute.name == name {
			return true
		}
	}
	return false
}

pub struct Attribute {
pub:
	name          string
	value         Expr
	comptime_cond Expr
}

pub struct BlockStmt {
pub:
	stmts []Stmt
}

pub struct ComptimeStmt {
pub:
	stmt Stmt
}

pub struct ConstDecl {
pub:
	is_public bool
	fields    []FieldInit
}

pub struct DeferStmt {
pub:
	stmts []Stmt
}

// #flag / #include
pub struct Directive {
pub:
	name  string
	value string
}

pub struct EnumDecl {
pub:
	attributes []Attribute
	is_public  bool
	name       string
	as_type    Expr = empty_expr
	fields     []FieldDecl
}

pub struct ExprStmt {
pub:
	expr Expr
}

pub struct FlowControlStmt {
pub:
	op    token.Token
	label string
}

// enum FnArributes {
// 	method
// 	static
// }

pub struct FnDecl {
pub:
	attributes []Attribute
	is_public  bool
	is_method  bool
	is_static  bool
	receiver   Parameter
	language   Language = .v
	name       string
	typ        FnType
	stmts      []Stmt
	pos        token.Pos
}

pub struct ForStmt {
pub:
	init  Stmt = empty_stmt // initialization
	cond  Expr = empty_expr // condition
	post  Stmt = empty_stmt // post iteration (afterthought)
	stmts []Stmt
}

// NOTE: used as the initializer for ForStmt
pub struct ForInStmt {
pub:
	// key   		 string
	// value 		 string
	// value_is_mut bool
	// expr	     Expr
	// TODO:
	key   Expr = empty_expr
	value Expr
	expr  Expr
}

pub struct GlobalDecl {
pub:
	attributes []Attribute
	fields     []FieldDecl
}

pub struct ImportStmt {
pub:
	name       string
	alias      string
	is_aliased bool
	symbols    []Expr
}

pub struct InterfaceDecl {
pub:
	is_public      bool
	attributes     []Attribute
	name           string
	generic_params []Expr
	embedded       []Expr
	fields         []FieldDecl
}

pub struct LabelStmt {
pub:
	name string
	stmt Stmt = empty_stmt
}

pub struct ModuleStmt {
pub:
	name string
}

pub struct ReturnStmt {
pub:
	exprs []Expr
}

pub struct StructDecl {
pub:
	attributes     []Attribute
	is_public      bool
	embedded       []Expr
	language       Language = .v
	name           string
	generic_params []Expr
	fields         []FieldDecl
	pos            token.Pos
}

pub struct TypeDecl {
pub:
	is_public      bool
	language       Language
	name           string
	generic_params []Expr
	base_type      Expr = empty_expr
	variants       []Expr
}

// Type Nodes
pub struct ArrayType {
pub:
	elem_type Expr
}

pub struct ArrayFixedType {
pub:
	len       Expr
	elem_type Expr
}

pub struct ChannelType {
pub:
	cap       Expr
	elem_type Expr
}

pub struct ThreadType {
pub:
	elem_type Expr = empty_expr
}

pub struct FnType {
pub:
	generic_params []Expr
	params         []Parameter
	return_type    Expr = empty_expr
}

pub fn (ft &FnType) str() string {
	mut s := 'fn('
	for param in ft.params {
		s += param.name + param.typ.str()
	}
	s += ') ' + ft.return_type.str()
	return s
}

pub fn (ident &Ident) str() string {
	return ident.name.clone()
}

// pub fn (expr Expr) str() string {
// 	return match expr {
// 		Ident {
// 			expr.name
// 		}
// 		SelectorExpr {
// 			expr.lhs.str() + '.' + expr.rhs.name
// 		}
// 		else {
// 			'missing Expr.str() for ${expr.type_name()}'
// 			// expr.str()
// 		}
// 	}
// }

pub struct AnonStructType {
pub:
	generic_params []Expr
	embedded       []Expr
	fields         []FieldDecl
}

pub struct GenericType {
pub:
	name   Expr
	params []Expr
}

pub struct MapType {
pub:
	key_type   Expr
	value_type Expr
}

pub struct NilType {}

pub struct NoneType {}

pub struct OptionType {
pub:
	base_type Expr = empty_expr
}

pub struct ResultType {
pub:
	base_type Expr = empty_expr
}

pub struct TupleType {
pub:
	types []Expr
}
