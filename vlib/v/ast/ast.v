// Copyright (c) 2019-2021 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module ast

import v.token
import v.table

// NB: when you add a new Expr or Stmt type with a .pos field, remember to update
// the .position() token.Position methods too.
pub type Expr = AnonFn | ArrayDecompose | ArrayInit | AsCast | Assoc | AtExpr | BoolLiteral |
	CTempVar | CallExpr | CastExpr | ChanInit | CharLiteral | Comment | ComptimeCall |
	ComptimeSelector | ConcatExpr | EnumVal | FloatLiteral | GoExpr | Ident | IfExpr |
	IfGuardExpr | IndexExpr | InfixExpr | IntegerLiteral | Likely | LockExpr | MapInit |
	MatchExpr | None | OffsetOf | OrExpr | ParExpr | PostfixExpr | PrefixExpr | RangeExpr |
	SelectExpr | SelectorExpr | SizeOf | SqlExpr | StringInterLiteral | StringLiteral |
	StructInit | Type | TypeOf | UnsafeExpr

pub type Stmt = AssertStmt | AssignStmt | Block | BranchStmt | CompFor | ConstDecl | DeferStmt |
	EnumDecl | ExprStmt | FnDecl | ForCStmt | ForInStmt | ForStmt | GlobalDecl | GoStmt |
	GotoLabel | GotoStmt | HashStmt | Import | InterfaceDecl | Module | Return | SqlStmt |
	StructDecl | TypeDecl

// TODO: replace table.Param
pub type Node = ConstField | EnumField | Expr | Field | File | GlobalField | IfBranch |
	MatchBranch | ScopeObject | SelectBranch | Stmt | StructField | StructInitField |
	table.Param

pub type IdentInfo = IdentFn | IdentVar

pub type TypeDecl = AliasTypeDecl | FnTypeDecl | SumTypeDecl

pub type ScopeObject = ConstField | GlobalField | Var

// type IdentInfo
pub struct IdentFn {
pub mut:
	typ table.Type
}

// TODO: (joe) remove completely, use ident.obj
// instead which points to the scope object
pub struct IdentVar {
pub mut:
	typ         table.Type
	is_mut      bool
	is_static   bool
	is_optional bool
	share       table.ShareType
}

// type TypeDecl
pub struct AliasTypeDecl {
pub:
	name        string
	is_pub      bool
	parent_type table.Type
	pos         token.Position
	comments    []Comment
}

// New implementation of sum types
pub struct SumTypeDecl {
pub:
	name     string
	is_pub   bool
	pos      token.Position
	comments []Comment
pub mut:
	variants []SumTypeVariant
}

pub struct SumTypeVariant {
pub:
	typ table.Type
	pos token.Position
}

pub struct FnTypeDecl {
pub:
	name     string
	is_pub   bool
	typ      table.Type
	pos      token.Position
	comments []Comment
}

// type ScopeObject
// const field in const declaration group
pub struct ConstField {
pub:
	mod    string
	name   string
	expr   Expr // the value expr of field; everything after `=`
	is_pub bool
	pos    token.Position
pub mut:
	typ      table.Type // the type of the const field, it can be any type in V
	comments []Comment  // comments before current const field
}

pub struct GlobalField {
pub:
	name     string
	expr     Expr
	has_expr bool
	pos      token.Position
pub mut:
	typ      table.Type
	comments []Comment
}

pub struct Var {
pub:
	name            string
	expr            Expr
	share           table.ShareType
	is_mut          bool
	is_autofree_tmp bool
	is_arg          bool // fn args should not be autofreed
	is_auto_deref   bool
pub mut:
	typ            table.Type
	orig_type      table.Type   // original sumtype type; 0 if it's not a sumtype
	sum_type_casts []table.Type // nested sum types require nested smart casting, for that a list of types is needed
	// TODO: move this to a real docs site later
	// 10 <- original type (orig_type)
	//   [11, 12, 13] <- cast order (sum_type_casts)
	//        12 <- the current casted type (typ)
	pos        token.Position
	is_used    bool
	is_changed bool // to detect mutable vars that are never changed
	//
	// (for setting the position after the or block for autofree)
	is_or  bool // `x := foo() or { ... }`
	is_tmp bool // for tmp for loop vars, so that autofree can skip them
}

// TODO: remove this fugly hack :-|
// fe2ex/1 and ex2fe/1 are used to convert back and forth from
// table.FExpr to ast.Expr , which in turn is needed to break
// a dependency cycle between v.ast and v.table, for the single
// field table.Field.default_expr, which should be ast.Expr
pub fn fe2ex(x table.FExpr) Expr {
	res := Expr{}
	unsafe { C.memcpy(&res, &x, sizeof(Expr)) }
	return res
}

pub fn ex2fe(x Expr) table.FExpr {
	res := table.FExpr{}
	unsafe { C.memcpy(&res, &x, sizeof(table.FExpr)) }
	return res
}

pub struct StructEmbedding {
pub:
	name string
	typ  table.Type
	pos  token.Position
}

// experimental ast.Table
pub struct Table {
	// pub mut:
	// main_fn_decl_node FnDecl
}
