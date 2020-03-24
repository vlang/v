// Copyright (c) 2019-2020 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module ast

import (
	v.token
	v.table
)

pub type TypeDecl = AliasTypeDecl | SumTypeDecl | FnTypeDecl

pub type Expr = InfixExpr | IfExpr | StringLiteral | IntegerLiteral | CharLiteral | 	
FloatLiteral | Ident | CallExpr | BoolLiteral | StructInit | ArrayInit | SelectorExpr | PostfixExpr | 	
AssignExpr | PrefixExpr | MethodCallExpr | IndexExpr | RangeExpr | MatchExpr | 	
CastExpr | EnumVal | Assoc | SizeOf | None | MapInit | IfGuardExpr | ParExpr | OrExpr | 	
ConcatExpr | Type | AsCast | TypeOf | StringInterLiteral

pub type Stmt = GlobalDecl | FnDecl | Return | Module | Import | ExprStmt | 	
ForStmt | StructDecl | ForCStmt | ForInStmt | CompIf | ConstDecl | Attr | BranchStmt | 	
HashStmt | AssignStmt | EnumDecl | TypeDecl | DeferStmt | GotoLabel | GotoStmt | 	
LineComment | MultiLineComment | AssertStmt | UnsafeStmt | GoStmt | Block
// pub type Type = StructType | ArrayType
// pub struct StructType {
// fields []Field
// }
// pub struct ArrayType {}
pub struct Type {
pub:
	typ table.Type
}

pub struct Block {
pub:
	stmts []Stmt
}

// | IncDecStmt k
// Stand-alone expression in a statement list.
pub struct ExprStmt {
pub:
	expr Expr
	typ  table.Type
}

pub struct IntegerLiteral {
pub:
	val string
}

pub struct FloatLiteral {
pub:
// val f64
	val string
}

pub struct StringLiteral {
pub:
	val    string
	is_raw bool
	is_c   bool
}

// 'name: $name'
pub struct StringInterLiteral {
pub:
	vals       []string
	exprs      []Expr
mut:
	expr_types []table.Type
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
	pos       token.Position
	expr      Expr
	field     string
mut:
	expr_type table.Type
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
mut:
	typ  table.Type
	// typ2 Type
}

pub struct ConstDecl {
pub:
	fields []Field
	exprs  []Expr
	is_pub bool
}

pub struct StructDecl {
pub:
	pos         token.Position
	name        string
	fields      []Field
	is_pub      bool
	mut_pos     int // mut:
	pub_pos     int // pub:
	pub_mut_pos int // pub mut:
	is_c        bool
}

pub struct StructInit {
pub:
	pos            token.Position
	typ            table.Type
	fields         []string
	exprs          []Expr
mut:
	expr_types     []table.Type
	expected_types []table.Type
}

// import statement
pub struct Import {
pub:
	pos   token.Position
	mod   string
	alias string
	// expr Expr
}

pub struct FnDecl {
pub:
	name          string
	stmts         []Stmt
	return_type   table.Type
	args          []table.Arg
	is_deprecated bool
	is_pub        bool
	is_variadic   bool
	receiver      Field
	is_method     bool
	rec_mut       bool // is receiver mutable
	is_c          bool
	no_body       bool // just a definition `fn C.malloc()`
	pos           token.Position
}

pub struct BranchStmt {
pub:
	tok token.Token
}

pub struct CallExpr {
pub:
// tok          token.Token
	pos         token.Position
mut:
// func         Expr
	name        string
	args        []CallArg
	is_c        bool
	muts        []bool
	or_block    OrExpr
	return_type table.Type
}

pub struct MethodCallExpr {
pub:
// tok        token.Token
	pos           token.Position
	expr          Expr // `user` in `user.register()`
	name          string
	args          []CallArg
	or_block      OrExpr
mut:
	expr_type     table.Type // type of `user`
	receiver_type table.Type // User
	return_type   table.Type
}

pub struct CallArg {
pub:
	is_mut        bool
	expr          Expr
mut:
	typ           table.Type
	expected_type table.Type
}

pub struct Return {
pub:
	pos   token.Position
	exprs []Expr
mut:
	types []table.Type
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


pub struct Var {
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
	scope   &Scope
}

pub struct IdentFn {
pub mut:
	typ table.Type
}

pub struct IdentVar {
pub mut:
	typ         table.Type
	is_mut      bool
	is_static   bool
	is_optional bool
}

pub type IdentInfo = IdentFn | IdentVar

pub enum IdentKind {
	unresolved
	blank_ident
	variable
	constant
	function
}

// A single identifier
pub struct Ident {
pub:
	value    string
	is_c     bool
	tok_kind token.Kind
	pos      token.Position
mut:
	name     string
	kind     IdentKind
	info     IdentInfo
}

pub fn (i &Ident) var_info() IdentVar {
	match i.info {
		IdentVar {
			return it
		}
		else {
			// return IdentVar{}
			panic('Ident.var_info(): info is not IdentVar variant')
		}
	}
}

pub struct InfixExpr {
pub:
// op    BinaryOp
	op         token.Kind
	pos        token.Position
	left       Expr
	right      Expr
mut:
	left_type  table.Type
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
	pos            token.Position
	left           Expr
	index          Expr // [0], [start..end] etc
mut:
	container_type table.Type // array, map, fixed array
}

pub struct IfExpr {
pub:
	tok_kind token.Kind
	branches []IfBranch
	left     Expr // `a` in `a := if ...`
	pos      token.Position
mut:
	is_expr  bool
	typ      table.Type
	has_else bool
}

pub struct IfBranch {
pub:
	cond  Expr
	stmts []Stmt
	pos   token.Position
}

pub struct MatchExpr {
pub:
	tok_kind      token.Kind
	cond          Expr
	branches      []MatchBranch
	pos           token.Position
mut:
	is_expr       bool // returns a value
	return_type   table.Type
	cond_type     table.Type // type of `x` in `match x {`
	expected_type table.Type // for debugging only
	is_sum_type   bool
}

pub struct MatchBranch {
pub:
	exprs []Expr
	stmts []Stmt
	pos   token.Position
}

pub struct CompIf {
pub:
// cond       Expr
	val        string
	stmts      []Stmt
	is_not     bool
	pos        token.Position
mut:
	has_else   bool
	else_stmts []Stmt
}

pub struct ForStmt {
pub:
	cond   Expr
	stmts  []Stmt
	is_inf bool // `for {}`
	pos    token.Position
}

pub struct ForInStmt {
pub:
	key_var   string
	val_var   string
	cond      Expr
	is_range  bool
	high      Expr // `10` in `for i in 0..10 {`
	stmts     []Stmt
	pos       token.Position
mut:
	key_type  table.Type
	val_type  table.Type
	cond_type table.Type
	kind      table.Kind // array/map/string
}

pub struct ForCStmt {
pub:
	init     Stmt // i := 0;
	has_init bool
	cond     Expr // i < 10;
	// inc   Stmt // i++;
	inc      Expr // i++;
	stmts    []Stmt
	pos      token.Position
}

pub struct ReturnStmt {
pub:
	tok_kind token.Kind // or pos
	results  []Expr
	pos      token.Position
}

// #include etc
pub struct HashStmt {
pub:
	val string
}

// filter(), map()
pub struct Lambda {
pub:
	name string
}

pub struct AssignStmt {
pub:
	left        []Ident
	right       []Expr
	op          token.Kind
	pos         token.Position
mut:
	left_types  []table.Type
	right_types []table.Type
}

pub struct AsCast {
pub:
	expr      Expr
	typ       table.Type
	pos       token.Position
mut:
	expr_type table.Type
}

// e.g. `[unsafe_fn]`
pub struct Attr {
pub:
	name string
}

pub struct EnumVal {
pub:
	enum_name string
	val       string
	mod       string // for full path `mod_Enum_val`
	pos       token.Position
mut:
	typ       table.Type
}

pub struct EnumDecl {
pub:
	name   string
	is_pub bool
	vals   []string
}

pub struct AliasTypeDecl {
pub:
	name        string
	is_pub      bool
	parent_type table.Type
}

pub struct SumTypeDecl {
pub:
	name      string
	is_pub    bool
	sub_types []table.Type
}

pub struct FnTypeDecl {
pub:
	name   string
	is_pub bool
	typ    table.Type
}

pub struct DeferStmt {
pub:
	stmts []Stmt
}

pub struct UnsafeStmt {
pub:
	stmts []Stmt
}

// `(3+4)`
pub struct ParExpr {
pub:
	expr Expr
}

pub struct AssignExpr {
pub:
	op         token.Kind
	pos        token.Position
	left       Expr
	val        Expr
mut:
	left_type  table.Type
	right_type table.Type
}

pub struct GoStmt {
pub:
	expr Expr
}

pub struct GotoLabel {
pub:
	name string
}

pub struct GotoStmt {
pub:
	name string
}

pub struct ArrayInit {
pub:
	pos       token.Position
	exprs     []Expr
mut:
	elem_type table.Type
	typ       table.Type
}

pub struct MapInit {
pub:
	pos        token.Position
	keys       []Expr
	vals       []Expr
mut:
	typ        table.Type
	key_type   table.Type
	value_type table.Type
}

// s[10..20]
pub struct RangeExpr {
pub:
	low      Expr
	high     Expr
	has_high bool
	has_low  bool
}

pub struct CastExpr {
pub:
	expr      Expr // `buf`
	arg       Expr // `n` in `string(buf, n)`
	typ       table.Type // `string`
mut:
	expr_type table.Type // `byteptr`
	has_arg   bool
}

pub struct AssertStmt {
pub:
	expr Expr
}

// `if [x := opt()] {`
pub struct IfGuardExpr {
pub:
	var_name  string
	expr      Expr
mut:
	expr_type table.Type
}

// `or { ... }`
pub struct OrExpr {
pub:
	stmts []Stmt
	// var_name string
	// expr     Expr
}

pub struct Assoc {
pub:
	var_name string
	fields   []string
	exprs    []Expr
	pos      token.Position
mut:
	typ      table.Type
}

pub struct SizeOf {
pub:
	typ       table.Type
	type_name string
}

pub struct TypeOf {
pub:
	expr Expr
}

pub struct LineComment {
pub:
	text string
}

pub struct MultiLineComment {
pub:
	text string
}

pub struct ConcatExpr {
pub:
	vals []Expr
}

pub struct None {
pub:
	foo int // todo
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


[inline]
pub fn expr_is_blank_ident(expr Expr) bool {
	match expr {
		Ident {
			return it.kind == .blank_ident
		}
		else {
			return false
		}
	}
}

[inline]
pub fn expr_is_call(expr Expr) bool {
	return match expr {
		CallExpr{
			true
		}
		MethodCallExpr{
			true
		}
		else {
			false}
	}
}
