// Copyright (c) 2019 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module ast

import (
	v.token
	v.types
)

pub type Expr = BinaryExpr | UnaryExpr | IfExpr | StringLiteral | IntegerLiteral | 	
FloatLiteral | Ident | CallExpr | BoolLiteral | StructInit | ArrayInit | SelectorExpr | PostfixExpr | AssignExpr | PrefixExpr | MethodCallExpr | IndexExpr

pub type Stmt = VarDecl | FnDecl | Return | Module | Import | ExprStmt | 	
ForStmt | StructDecl | ForCStmt | ForInStmt
// | IncDecStmt k
// Stand-alone expression in a statement list.
pub struct ExprStmt {
pub:
	expr Expr
	ti   types.TypeIdent
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

pub struct BoolLiteral {
pub:
	val bool
}

// `foo.bar`
pub struct SelectorExpr {
pub:
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
	ti   types.TypeIdent
}

pub struct StructDecl {
pub:
	name   string
	fields []Field
	is_pub bool
}

pub struct StructInit {
pub:
// typ    types.TypeIdent
	ti     types.TypeIdent
	fields []string
	exprs  []Expr
}

// import statement
pub struct Import {
pub:
	mods map[string]string // alias -> module
	// expr Expr
}

pub struct Arg {
pub:
	ti   types.TypeIdent
	name string
}

pub struct FnDecl {
pub:
	name     string
	stmts    []Stmt
	ti       types.TypeIdent
	args     []Arg
	is_pub   bool
	receiver Field
}

pub struct CallExpr {
pub:
// func       Expr
	name       string
	args       []Expr
	is_unknown bool
	tok        token.Token
}

pub struct MethodCallExpr {
pub:
	expr       Expr
	name       string
	args       []Expr
	is_unknown bool
	tok        token.Token
}

pub struct Return {
pub:
	exprs []Expr
}

/*
pub enum Expr {
	Binary(BinaryExpr)
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
	name string
	expr Expr
	ti   types.TypeIdent
}

pub struct File {
pub:
	stmts []Stmt
}

// A single identifier
pub struct Ident {
pub:
	name     string
	tok_kind token.Kind
	value    string
}

pub struct BinaryExpr {
pub:
// tok_kind token.Kind
// op    BinaryOp
	op    token.Kind
	left  Expr
	// left_ti types.TypeIdent
	right Expr
	// right_ti types.TypeIdent
}

pub struct UnaryExpr {
pub:
// tok_kind token.Kind
// op    BinaryOp
	op   token.Kind
	left Expr
}

pub struct PostfixExpr {
pub:
	op   token.Kind
	expr Expr
}

pub struct PrefixExpr {
pub:
	op    token.Kind
	right Expr
}

pub struct IndexExpr {
pub:
// op   token.Kind
	left  Expr
	index Expr
}

pub struct IfExpr {
pub:
	tok_kind   token.Kind
	cond       Expr
	stmts      []Stmt
	else_stmts []Stmt
	ti         types.TypeIdent
	left       Expr // `a` in `a := if ...`
}

pub struct ForStmt {
pub:
	cond  Expr
	stmts []Stmt
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
	results  []Expr
}

/*
pub struct AssignStmt {
pub:
	left  Expr
	right Expr
	op    token.Kind
}
*/


pub struct AssignExpr {
pub:
	left Expr
	val  Expr
	op   token.Kind
}

pub struct ArrayInit {
pub:
	exprs []Expr
	ti    types.TypeIdent
}

// string representaiton of expr
pub fn (x Expr) str() string {
	match x {
		BinaryExpr {
			return '(${it.left.str()} $it.op.str() ${it.right.str()})'
		}
		UnaryExpr {
			return it.left.str() + it.op.str()
		}
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
