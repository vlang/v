// Copyright (c) 2019-2020 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module ast

import (
	v.token
	v.table
)

pub type Expr = InfixExpr | IfExpr | StringLiteral | IntegerLiteral | CharLiteral | 	
FloatLiteral | Ident | CallExpr | BoolLiteral | StructInit | ArrayInit | SelectorExpr | PostfixExpr | 	
AssignExpr | PrefixExpr | MethodCallExpr | IndexExpr | RangeExpr

pub type Stmt = VarDecl | GlobalDecl | FnDecl | Return | Module | Import | ExprStmt | 	
ForStmt | StructDecl | ForCStmt | ForInStmt | CompIf | ConstDecl | Attr | BranchStmt | 	
HashStmt
// | IncDecStmt k
// Stand-alone expression in a statement list.
pub struct ExprStmt {
pub:
	expr Expr
	ti   table.Type
}

pub struct IntegerLiteral {
pub:
	val int
}

pub struct FloatLiteral {
pub:
// val f64
	val string
}

pub struct StringLiteral {
pub:
	val string
}

pub struct CharLiteral {
pub:
	val string
}

pub struct BoolLiteral {
pub:
	val bool
}

// `foo.bar`
pub struct SelectorExpr {
pub:
	pos   token.Position
	expr  Expr
	field string
}

// module declaration
pub struct Module {
pub:
	name string
	path string
	expr Expr
}

pub struct Field {
pub:
	name string
	// type_idx int
	typ  table.Type
}

pub struct ConstDecl {
pub:
	fields []Field
	exprs  []Expr
}

pub struct StructDecl {
pub:
	pos    token.Position
	name   string
	fields []Field
	is_pub bool
}

pub struct StructInit {
pub:
	pos    token.Position
	ti     table.Type
	fields []string
	exprs  []Expr
}

// import statement
pub struct Import {
pub:
	pos   token.Position
	mod   string
	alias string
	// expr Expr
}

pub struct Arg {
pub:
	ti   table.Type
	name string
}

pub struct FnDecl {
pub:
	name     string
	stmts    []Stmt
	ti       table.Type
	args     []Arg
	is_pub   bool
	receiver Field
}

pub struct BranchStmt {
pub:
	tok token.Token
}

pub struct CallExpr {
pub:
// tok        token.Token
	pos  token.Position
mut:
// func       Expr
	name string
	args []Expr
}

pub struct MethodCallExpr {
pub:
// tok        token.Token
	pos  token.Position
	expr Expr
	name string
	args []Expr
}

pub struct Return {
pub:
	pos         token.Position
	expected_ti table.Type // TODO: remove once checker updated
	exprs       []Expr
}

/*
pub enum Expr {
	Binary(InfixExpr)
	If(IfExpr)
	Integer(IntegerExpr)
}
*/

/*
pub struct Stmt {
	pos int
	//end int
}
*/


pub struct VarDecl {
pub:
	name   string
	expr   Expr
	is_mut bool
mut:
	typ    table.Type
	pos    token.Position
}

pub struct GlobalDecl {
pub:
	name string
	expr Expr
mut:
	typ  table.Type
}

pub struct File {
pub:
	path    string
	mod     Module
	imports []Import
	stmts   []Stmt
	unresolved []Expr
}

pub struct IdentVar {
pub:
	typ  table.Type
	//name string
}

type IdentInfo = IdentVar

pub enum IdentKind {
	blank_ident
	variable
	constant
}

// A single identifier
pub struct Ident {
pub:
	name     string
	tok_kind token.Kind
	pos      token.Position
	value    string
mut:
	kind     IdentKind
	info     IdentInfo
}

pub struct InfixExpr {
pub:
// op    BinaryOp
	op         token.Kind
	pos        token.Position
	left       Expr
	left_type  table.Type
	right      Expr
	right_type table.Type
}

/*
// renamed to PrefixExpr
pub struct UnaryExpr {
pub:
// tok_kind token.Kind
// op    BinaryOp
	op   token.Kind
	left Expr
}
*/


pub struct PostfixExpr {
pub:
	op   token.Kind
	expr Expr
	pos  token.Position
}

pub struct PrefixExpr {
pub:
	op    token.Kind
	right Expr
}

pub struct IndexExpr {
pub:
// op   token.Kind
	pos   token.Position
	left  Expr
	index Expr // [0], [start..end] etc
	// typ   table.Type
}

pub struct IfExpr {
pub:
	tok_kind   token.Kind
	cond       Expr
	stmts      []Stmt
	else_stmts []Stmt
	ti         table.Type
	left       Expr // `a` in `a := if ...`
	pos        token.Position
}

pub struct CompIf {
pub:
	cond       Expr
	stmts      []Stmt
	else_stmts []Stmt
}

pub struct ForStmt {
pub:
	cond  Expr
	stmts []Stmt
	pos   token.Position
}

pub struct ForInStmt {
pub:
	var   string
	cond  Expr
	stmts []Stmt
}

pub struct ForCStmt {
pub:
	init  Stmt // i := 0;
	cond  Expr // i < 10;
	inc   Stmt // i++;
	stmts []Stmt
}

pub struct ReturnStmt {
	tok_kind token.Kind // or pos
	pos      token.Position
	results  []Expr
}

// #include etc
pub struct HashStmt {
pub:
	name string
}

/*
pub struct AssignStmt {
pub:
	left  Expr
	right Expr
	op    token.Kind
}
*/

// e.g. `[unsafe_fn]`
pub struct Attr {
pub:
	name string
}

pub struct AssignExpr {
pub:
	op   token.Kind
	pos  token.Position
	left Expr
	val  Expr
}

pub struct ArrayInit {
pub:
	pos   token.Position
	exprs []Expr
	ti    table.Type
}

// s[10..20]
pub struct RangeExpr {
pub:
	low  Expr
	high Expr
}

// string representaiton of expr
pub fn (x Expr) str() string {
	match x {
		InfixExpr {
			return '(${it.left.str()} $it.op.str() ${it.right.str()})'
		}
		/*
		PrefixExpr {
			return it.left.str() + it.op.str()
		}
		*/

		IntegerLiteral {
			return it.val.str()
		}
		IntegerLiteral {
			return '"$it.val"'
		}
		else {
			return ''
		}
	}
}

pub fn (node Stmt) str() string {
	match node {
		VarDecl {
			return it.name + ' = ' + it.expr.str()
		}
		ExprStmt {
			return it.expr.str()
		}
		FnDecl {
			return 'fn ${it.name}() { $it.stmts.len stmts }'
		}
		else {
			return '[unhandled stmt str]'
		}
	}
}

/*
enum BinaryOp {
	sum
	difference
	product
	quotient
	remainder
	bitwise_and
	bitwise_or
	bitwise_xor
	left_shift
	right_shift

	equality
	inequality
	less_than
	less_than_or_equal
	more_than
	more_than_or_equal

	in_check

	//These are suffixed with `bool` to prevent conflict with the keyword `or`
	and_bool
	or_bool
}
*/
