// Copyright (c) 2019-2021 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module ast

import v.token
import v.table

// anonymous function
pub struct AnonFn {
pub mut:
	decl FnDecl
	typ  table.Type // the type of anonymous fn. Both .typ and .decl.name are auto generated
}

pub struct ArrayDecompose {
pub:
	expr Expr
	pos  token.Position
pub mut:
	expr_type table.Type
	arg_type  table.Type
}

pub struct ArrayInit {
pub:
	pos           token.Position // `[]` in []Type{} position
	elem_type_pos token.Position // `Type` in []Type{} position
	exprs         []Expr      // `[expr, expr]` or `[expr]Type{}` for fixed array
	ecmnts        [][]Comment // optional iembed comments after each expr
	is_fixed      bool
	has_val       bool // fixed size literal `[expr, expr]!`
	mod           string
	len_expr      Expr // len: expr
	cap_expr      Expr // cap: expr
	default_expr  Expr // init: expr
	has_len       bool
	has_cap       bool
	has_default   bool
pub mut:
	expr_types []table.Type // [Dog, Cat] // also used for interface_types
	elem_type  table.Type   // element type
	typ        table.Type   // array type
}

pub struct AsCast {
pub:
	expr Expr
	typ  table.Type
	pos  token.Position
pub mut:
	expr_type table.Type
}

// deprecated
pub struct Assoc {
pub:
	var_name string
	fields   []string
	exprs    []Expr
	pos      token.Position
pub mut:
	typ   table.Type
	scope &Scope
}

// @FN, @STRUCT, @MOD etc. See full list in token.valid_at_tokens
pub struct AtExpr {
pub:
	name string
	pos  token.Position
	kind token.AtKind
pub mut:
	val string
}

pub struct BoolLiteral {
pub:
	val bool
	pos token.Position
}

// CTempVar is used in cgen only, to hold nodes for temporary variables
pub struct CTempVar {
pub:
	name   string     // the name of the C temporary variable; used by g.expr(x)
	orig   Expr       // the original expression, which produced the C temp variable; used by x.str()
	typ    table.Type // the type of the original expression
	is_ptr bool       // whether the type is a pointer
}

// function or method call expr
pub struct CallExpr {
pub:
	pos  token.Position
	left Expr // `user` in `user.register()`
	mod  string
pub mut:
	name               string // left.name()
	is_method          bool
	is_field           bool // temp hack, remove ASAP when re-impl CallExpr / Selector (joe)
	args               []CallArg
	expected_arg_types []table.Type
	language           table.Language
	or_block           OrExpr
	left_type          table.Type // type of `user`
	receiver_type      table.Type // User
	return_type        table.Type
	should_be_skipped  bool
	generic_types      []table.Type
	generic_list_pos   token.Position
	free_receiver      bool // true if the receiver expression needs to be freed
	scope              &Scope
	from_embed_type    table.Type // holds the type of the embed that the method is called from
	comments           []Comment
}

// function call argument: `f(callarg)`
pub struct CallArg {
pub:
	is_mut   bool
	share    table.ShareType
	expr     Expr
	comments []Comment
pub mut:
	typ             table.Type
	is_tmp_autofree bool // this tells cgen that a tmp variable has to be used for the arg expression in order to free it after the call
	pos             token.Position
	// tmp_name        string // for autofree
}

// NB: &string(x) gets parsed as ast.PrefixExpr{ right: ast.CastExpr{...} }
// TODO: that is very likely a parsing bug. It should get parsed as just
// ast.CastExpr{...}, where .typname is '&string' instead.
// The current situation leads to special cases in vfmt and cgen
// (see prefix_expr_cast_expr in fmt.v, and .is_amp in cgen.v)
// .in_prexpr is also needed because of that, because the checker needs to
// show warnings about the deprecated C->V conversions `string(x)` and
// `string(x,y)`, while skipping the real pointer casts like `&string(x)`.
pub struct CastExpr {
pub:
	expr Expr       // `buf` in `string(buf, n)`
	arg  Expr       // `n` in `string(buf, n)`
	typ  table.Type // `string` TODO rename to `type_to_cast_to`
	pos  token.Position
pub mut:
	typname   string     // TypeSymbol.name
	expr_type table.Type // `byteptr`
	has_arg   bool
	in_prexpr bool // is the parent node an ast.PrefixExpr
}

pub struct ChanInit {
pub:
	pos      token.Position
	cap_expr Expr
	has_cap  bool
pub mut:
	typ       table.Type
	elem_type table.Type
}

pub struct CharLiteral {
pub:
	val string
	pos token.Position
}

pub struct Comment {
pub:
	text     string
	is_multi bool
	line_nr  int
	pos      token.Position
}

pub struct ComptimeCall {
pub:
	has_parens  bool // if $() is used, for vfmt
	method_name string
	method_pos  token.Position
	scope       &Scope
	left        Expr
	args_var    string
	//
	is_vweb   bool
	vweb_tmpl File
	//
	is_embed   bool
	embed_file EmbeddedFile
	//
	is_env  bool
	env_pos token.Position
pub mut:
	sym         table.TypeSymbol
	result_type table.Type
	env_value   string
}

pub struct EmbeddedFile {
pub:
	rpath string // used in the source code, as an ID/key to the embed
	apath string // absolute path during compilation to the resource
}

pub struct ComptimeSelector {
pub:
	has_parens bool // if $() is used, for vfmt
	left       Expr
	field_expr Expr
pub mut:
	left_type table.Type
	typ       table.Type
}

pub struct ConcatExpr {
pub:
	vals []Expr
	pos  token.Position
pub mut:
	return_type table.Type
}

// an enum value, like OS.macos or .macos
pub struct EnumVal {
pub:
	enum_name string
	val       string
	mod       string // for full path `mod_Enum_val`
	pos       token.Position
pub mut:
	typ table.Type
}

pub struct FloatLiteral {
pub:
	val string
	pos token.Position
}

pub struct GoExpr {
pub:
	pos token.Position
pub mut:
	go_stmt GoStmt
mut:
	return_type table.Type
}

// A single identifier
pub struct Ident {
pub:
	language table.Language
	tok_kind token.Kind
	pos      token.Position
	mut_pos  token.Position
pub mut:
	scope  &Scope
	obj    ScopeObject
	mod    string
	name   string
	kind   IdentKind
	info   IdentInfo
	is_mut bool
}

pub enum IdentKind {
	unresolved
	blank_ident
	variable
	constant
	global
	function
}

pub fn (i &Ident) var_info() IdentVar {
	match mut i.info {
		IdentVar {
			return i.info
		}
		else {
			// return IdentVar{}
			panic('Ident.var_info(): info is not IdentVar variant')
		}
	}
}

pub struct IfExpr {
pub:
	is_comptime   bool
	tok_kind      token.Kind
	left          Expr // `a` in `a := if ...`
	pos           token.Position
	post_comments []Comment
pub mut:
	branches []IfBranch // includes all `else if` branches
	is_expr  bool
	typ      table.Type
	has_else bool
}

// `if [x := opt()] {`
pub struct IfGuardExpr {
pub:
	var_name string
	expr     Expr
	pos      token.Position
pub mut:
	expr_type table.Type
}

pub struct IndexExpr {
pub:
	pos     token.Position
	left    Expr
	index   Expr // [0], RangeExpr [start..end] or map[key]
	or_expr OrExpr
pub mut:
	left_type table.Type // array, map, fixed array
	is_setter bool
}

// left op right
// See: token.Kind.is_infix
pub struct InfixExpr {
pub:
	op  token.Kind
	pos token.Position
pub mut:
	left        Expr
	right       Expr
	left_type   table.Type
	right_type  table.Type
	auto_locked string
	or_block    OrExpr
}

pub struct IntegerLiteral {
pub:
	val string
	pos token.Position
}

/*
// filter(), map(), sort()
pub struct Lambda {
pub:
	name string
}
*/

pub struct Likely {
pub:
	expr      Expr
	pos       token.Position
	is_likely bool // false for _unlikely_
}

pub struct LockExpr {
pub:
	stmts    []Stmt
	is_rlock []bool
	pos      token.Position
pub mut:
	lockeds []Ident // `x`, `y` in `lock x, y {`
	is_expr bool
	typ     table.Type
}

pub struct MapInit {
pub:
	pos  token.Position
	keys []Expr
	vals []Expr
pub mut:
	typ        table.Type
	key_type   table.Type
	value_type table.Type
}

pub struct MatchExpr {
pub:
	tok_kind token.Kind
	cond     Expr
	branches []MatchBranch
	pos      token.Position
	comments []Comment // comments before the first branch
pub mut:
	is_expr       bool // returns a value
	return_type   table.Type
	cond_type     table.Type // type of `x` in `match x {`
	expected_type table.Type // for debugging only
	is_sum_type   bool
}

pub struct None {
pub:
	pos token.Position
	foo int // todo
}

pub struct OffsetOf {
pub:
	struct_type table.Type
	field       string
	pos         token.Position
}

// `or { ... }`
pub struct OrExpr {
pub:
	stmts []Stmt
	kind  OrKind
	pos   token.Position
}

/*
// `or { ... }`
pub struct OrExpr2 {
pub:
	call_expr CallExpr
	stmts     []Stmt // inside `or { }`
	kind      OrKind
	pos       token.Position
}
*/

pub enum OrKind {
	absent
	block
	propagate
}

// `(3+4)`
pub struct ParExpr {
pub:
	expr Expr
	pos  token.Position
}

// ++, --
pub struct PostfixExpr {
pub:
	op   token.Kind
	expr Expr
	pos  token.Position
pub mut:
	auto_locked string
}

// See: token.Kind.is_prefix
pub struct PrefixExpr {
pub:
	op    token.Kind
	right Expr
	pos   token.Position
pub mut:
	right_type table.Type
	or_block   OrExpr
}

// s[10..20]
pub struct RangeExpr {
pub:
	low      Expr
	high     Expr
	has_high bool
	has_low  bool
	pos      token.Position
}

pub struct SelectExpr {
pub:
	branches      []SelectBranch
	pos           token.Position
	has_exception bool
pub mut:
	is_expr       bool       // returns a value
	expected_type table.Type // for debugging only
}

// `foo.bar`
pub struct SelectorExpr {
pub:
	pos        token.Position
	expr       Expr // expr.field_name
	field_name string
	is_mut     bool // is used for the case `if mut ident.selector is MyType {`, it indicates if the root ident is mutable
	mut_pos    token.Position
	next_token token.Kind
pub mut:
	expr_type       table.Type // type of `Foo` in `Foo.bar`
	typ             table.Type // type of the entire thing (`Foo.bar`)
	name_type       table.Type // T in `T.name` or typeof in `typeof(expr).name`
	scope           &Scope
	from_embed_type table.Type // holds the type of the embed that the method is called from
}

// root_ident returns the origin ident where the selector started.
pub fn (e &SelectorExpr) root_ident() Ident {
	mut root := e.expr
	for root is SelectorExpr {
		// TODO: remove this line
		selector_expr := root as SelectorExpr
		root = selector_expr.expr
	}
	return root as Ident
}

pub struct SizeOf {
pub:
	is_type bool
	expr    Expr // checker uses this to set typ
	pos     token.Position
pub mut:
	typ table.Type
}

pub struct SqlExpr {
pub:
	typ         table.Type
	is_count    bool
	db_expr     Expr // `db` in `sql db {`
	has_where   bool
	has_offset  bool
	offset_expr Expr
	has_order   bool
	order_expr  Expr
	has_desc    bool
	is_array    bool
	pos         token.Position
	has_limit   bool
	limit_expr  Expr
pub mut:
	where_expr  Expr
	table_expr  Type
	fields      []table.Field
	sub_structs map[int]SqlExpr
}

// 'name: $name'
pub struct StringInterLiteral {
pub:
	vals       []string
	exprs      []Expr
	fwidths    []int
	precisions []int
	pluss      []bool
	fills      []bool
	fmt_poss   []token.Position
	pos        token.Position
pub mut:
	expr_types []table.Type
	fmts       []byte
	need_fmts  []bool // an explicit non-default fmt required, e.g. `x`
}

pub struct StringLiteral {
pub:
	val      string
	is_raw   bool
	language table.Language
	pos      token.Position
}

pub struct StructInitEmbed {
pub:
	expr          Expr
	pos           token.Position
	comments      []Comment
	next_comments []Comment
pub mut:
	name          string
	typ           table.Type
	expected_type table.Type
}

pub struct StructInit {
pub:
	pos      token.Position
	is_short bool
pub mut:
	unresolved           bool
	pre_comments         []Comment
	typ                  table.Type
	update_expr          Expr
	update_expr_type     table.Type
	update_expr_comments []Comment
	has_update_expr      bool
	fields               []StructInitField
	embeds               []StructInitEmbed
}

pub struct Type {
pub:
	typ table.Type
	pos token.Position
}

pub struct TypeOf {
pub:
	expr Expr
	pos  token.Position
pub mut:
	expr_type table.Type
}

pub struct UnsafeExpr {
pub:
	expr Expr
	pos  token.Position
}

[inline]
pub fn (expr Expr) is_blank_ident() bool {
	match expr {
		Ident { return expr.kind == .blank_ident }
		else { return false }
	}
}

pub fn (expr Expr) position() token.Position {
	// all uncommented have to be implemented
	match expr {
		// KEKW2
		AnonFn {
			return expr.decl.pos
		}
		ArrayInit, AsCast, Assoc, AtExpr, BoolLiteral, CallExpr, CastExpr, ChanInit, CharLiteral,
		ConcatExpr, Comment, EnumVal, FloatLiteral, GoExpr, Ident, IfExpr, IndexExpr, IntegerLiteral,
		Likely, LockExpr, MapInit, MatchExpr, None, OffsetOf, OrExpr, ParExpr, PostfixExpr, PrefixExpr,
		RangeExpr, SelectExpr, SelectorExpr, SizeOf, SqlExpr, StringInterLiteral, StringLiteral,
		StructInit, Type, TypeOf, UnsafeExpr {
			return expr.pos
		}
		ArrayDecompose {
			return expr.pos
		}
		IfGuardExpr {
			return expr.expr.position()
		}
		ComptimeCall, ComptimeSelector {
			return expr.left.position()
		}
		InfixExpr {
			left_pos := expr.left.position()
			right_pos := expr.right.position()
			return token.Position{
				line_nr: expr.pos.line_nr
				pos: left_pos.pos
				len: right_pos.pos - left_pos.pos + right_pos.len
				last_line: right_pos.last_line
			}
		}
		CTempVar {
			return token.Position{}
		}
		// Please, do NOT use else{} here.
		// This match is exhaustive *on purpose*, to help force
		// maintaining/implementing proper .pos fields.
	}
}

pub fn (expr Expr) is_lvalue() bool {
	match expr {
		Ident { return true }
		CTempVar { return true }
		IndexExpr { return expr.left.is_lvalue() }
		SelectorExpr { return expr.expr.is_lvalue() }
		ParExpr { return expr.expr.is_lvalue() } // for var := &{...(*pointer_var)}
		PrefixExpr { return expr.right.is_lvalue() }
		else {}
	}
	return false
}

pub fn (expr Expr) is_expr() bool {
	match expr {
		IfExpr { return expr.is_expr }
		MatchExpr { return expr.is_expr }
		else {}
	}
	return true
}

pub fn (expr Expr) is_lit() bool {
	return match expr {
		BoolLiteral, StringLiteral, IntegerLiteral { true }
		else { false }
	}
}

pub fn (expr Expr) is_mut_ident() bool {
	if expr is Ident {
		if expr.obj is Var {
			if expr.obj.is_auto_deref {
				return true
			}
		}
	}
	return false
}
