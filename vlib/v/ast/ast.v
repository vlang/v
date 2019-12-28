// Copyright (c) 2019 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module ast

import (
	v.token
	v.types
)

struct Foo {}

pub type Expr = BinaryExpr | UnaryExpr | IfExpr | StringLiteral | IntegerLiteral | FloatLiteral | 	
VarDecl | FnDecl | Return

pub type Stmt = Foo // VarDecl
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

// module declaration
pub struct Module {
pub:
	name string
	path string
	expr Expr
}

// import statement
pub struct Import {
pub:
	name string
	expr Expr
	// imports map[string]string
}

pub struct FnDecl {
pub:
	name  string
	// stmts []Stmt
	exprs []Expr
	typ   types.Type
}

pub struct Return {
pub:
	expr Expr
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
	typ  types.Type
}

pub struct Program {
pub:
	exprs []Expr
	// stmts []Stmt
}
// A single identifier
struct Ident {
	tok_kind token.TokenKind
	value    string
}

pub struct BinaryExpr {
pub:
	tok_kind token.TokenKind
	// op    BinaryOp
	op       token.TokenKind
	left     Expr
	// left_type Type
	right    Expr
	// right_type Type
}

pub struct UnaryExpr {
pub:
// tok_kind token.TokenKind
// op    BinaryOp
	op   token.TokenKind
	left Expr
}

struct IfExpr {
	tok_kind token.TokenKind
	cond     Expr
	body     []Stmt
	else_    []Stmt
}

struct ReturnStmt {
	tok_kind token.TokenKind // or pos
	results  []Expr
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
