// Copyright (c) 2019 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module ast

import (
	compiler2.token
)


struct Foo {}

pub type Expr = Foo | IfExpr | BinaryExpr | UnaryExpr |
	StringLiteral  | IntegerLiteral
pub type Stmt = Foo | VarDecl

pub struct IntegerLiteral {
pub:
	val int
}

pub struct StringLiteral {
pub:
	val string
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

}

pub struct Program {
pub:
	exprs []Expr
}

// A single identifier
struct Ident {
	token token.Token
	value string
}

pub struct BinaryExpr {
pub:
	token token.Token
	//op    BinaryOp
	op    token.Token
	left  Expr
	right Expr
}

pub struct UnaryExpr {
pub:
	// token token.Token
	//op    BinaryOp
	op    token.Token
	left  Expr
}

struct IfExpr {
	token token.Token
	cond  Expr
	body  []Stmt
	else_ []Stmt
}

struct ReturnStmt {
	token   token.Token // or pos
	results []Expr
}

// string representaiton of expr
pub fn (x Expr) str() string {
	match x {
		BinaryExpr {
			return '(${it.left.str()}$it.op.str()${it.right.str()})'
		}
		//ScalarExpr {
			//return '${it.left.str()}$it.val'
		//}
		UnaryExpr {
			return '${it.left.str()}$it.op.str()'
		}
		else { return '' }
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
