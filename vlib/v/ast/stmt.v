// Copyright (c) 2019-2021 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module ast

import v.token
import v.table

pub struct AssertStmt {
pub:
	pos token.Position
pub mut:
	expr Expr
}

// variable assign statement
pub struct AssignStmt {
pub:
	op           token.Kind // include: =,:=,+=,-=,*=,/= and so on; for a list of all the assign operators, see vlib/token/token.v
	pos          token.Position
	comments     []Comment
	end_comments []Comment
pub mut:
	right         []Expr
	left          []Expr
	left_types    []table.Type
	right_types   []table.Type
	is_static     bool // for translated code only
	is_simple     bool // `x+=2` in `for x:=1; ; x+=2`
	has_cross_var bool
}

// `{stmts}` or `unsafe {stmts}`
pub struct Block {
pub:
	stmts     []Stmt
	is_unsafe bool
	pos       token.Position
}

// break, continue
pub struct BranchStmt {
pub:
	kind  token.Kind
	label string
	pos   token.Position
}

pub enum CompForKind {
	methods
	fields
}

pub struct CompFor {
pub:
	val_var string
	stmts   []Stmt
	kind    CompForKind
	pos     token.Position
pub mut:
	// expr    Expr
	typ table.Type
}

// const declaration
pub struct ConstDecl {
pub:
	is_pub bool
	pos    token.Position
pub mut:
	fields       []ConstField // all the const fields in the `const (...)` block
	end_comments []Comment    // comments that after last const field
	is_block     bool // const() block
}

// TODO: handle this differently
// v1 excludes non current os ifdefs so
// the defer's never get added in the first place
pub struct DeferStmt {
pub:
	stmts []Stmt
	pos   token.Position
pub mut:
	ifdef     string
	idx_in_fn int = -1 // index in FnDecl.defer_stmts
}

// enum declaration
pub struct EnumDecl {
pub:
	name             string
	is_pub           bool
	is_flag          bool         // true when the enum has [flag] tag,for bit field enum
	is_multi_allowed bool         // true when the enum has [_allow_multiple_values] tag
	comments         []Comment    // comments before the first EnumField
	fields           []EnumField  // all the enum fields
	attrs            []table.Attr // attributes of enum declaration
	pos              token.Position
}

// | IncDecStmt k
// Stand-alone expression in a statement list.
pub struct ExprStmt {
pub:
	expr     Expr
	pos      token.Position
	comments []Comment
	is_expr  bool
pub mut:
	typ table.Type
}

// function or method declaration
pub struct FnDecl {
pub:
	name            string
	mod             string
	params          []table.Param
	is_deprecated   bool
	is_pub          bool
	is_variadic     bool
	is_anon         bool
	is_manualfree   bool // true, when [manualfree] is used on a fn
	receiver        Field
	receiver_pos    token.Position // `(u User)` in `fn (u User) name()` position
	is_method       bool
	method_type_pos token.Position // `User` in ` fn (u User)` position
	method_idx      int
	rec_mut         bool // is receiver mutable
	rec_share       table.ShareType
	language        table.Language
	no_body         bool // just a definition `fn C.malloc()`
	is_builtin      bool // this function is defined in builtin/strconv
	pos             token.Position // function declaration position
	body_pos        token.Position // function bodys position
	file            string
	generic_params  []GenericParam
	is_direct_arr   bool // direct array access
	attrs           []table.Attr
	skip_gen        bool // this function doesn't need to be generated (for example [if foo])
pub mut:
	stmts         []Stmt
	defer_stmts   []DeferStmt
	return_type   table.Type
	has_return    bool
	comments      []Comment // comments *after* the header, but *before* `{`; used for InterfaceDecl
	next_comments []Comment // coments that are one line after the decl; used for InterfaceDecl
	source_file   &File = 0
	scope         &Scope
	label_names   []string
}

pub struct GenericParam {
pub:
	name string
}

pub struct ForCStmt {
pub:
	init     Stmt // i := 0;
	has_init bool
	cond     Expr // i < 10;
	has_cond bool
	inc      Stmt // i++; i += 2
	has_inc  bool
	stmts    []Stmt
	pos      token.Position
pub mut:
	label string // `label: for {`
	scope &Scope
}

pub struct ForInStmt {
pub:
	key_var    string
	val_var    string
	cond       Expr
	is_range   bool
	high       Expr // `10` in `for i in 0..10 {`
	stmts      []Stmt
	pos        token.Position
	val_is_mut bool // `for mut val in vals {` means that modifying `val` will modify the array
	// and the array cannot be indexed inside the loop
pub mut:
	key_type  table.Type
	val_type  table.Type
	cond_type table.Type
	kind      table.Kind // array/map/string
	label     string     // `label: for {`
	scope     &Scope
}

pub struct ForStmt {
pub:
	cond   Expr
	stmts  []Stmt
	is_inf bool // `for {}`
	pos    token.Position
pub mut:
	label string // `label: for {`
	scope &Scope
}

pub struct GlobalDecl {
pub:
	pos token.Position
pub mut:
	fields       []GlobalField
	end_comments []Comment
}

pub struct GoStmt {
pub:
	pos token.Position
pub mut:
	call_expr CallExpr
}

pub struct GotoLabel {
pub:
	name string
	pos  token.Position
}

pub struct GotoStmt {
pub:
	name string
	pos  token.Position
}

// #include etc
pub struct HashStmt {
pub:
	mod         string
	pos         token.Position
	source_file string
pub mut:
	val  string // example: 'include <openssl/rand.h> # please install openssl // comment'
	kind string // : 'include'
	main string // : '<openssl/rand.h>'
	msg  string // : 'please install openssl'
}

// import statement
pub struct Import {
pub:
	mod       string // the module name of the import
	alias     string // the `x` in `import xxx as x`
	pos       token.Position
	mod_pos   token.Position
	alias_pos token.Position
pub mut:
	syms          []ImportSymbol // the list of symbols in `import {symbol1, symbol2}`
	comments      []Comment
	next_comments []Comment
}

// import symbol,for import {symbol} syntax
pub struct ImportSymbol {
pub:
	pos  token.Position
	name string
}

pub struct InterfaceDecl {
pub:
	name         string
	field_names  []string
	is_pub       bool
	methods      []FnDecl
	fields       []StructField
	pos          token.Position
	pre_comments []Comment
}

// module declaration
pub struct Module {
pub:
	name       string // encoding.base64
	short_name string // base64
	attrs      []table.Attr
	pos        token.Position
	name_pos   token.Position // `name` in import name
	is_skipped bool // module main can be skipped in single file programs
}

// function return statement
pub struct Return {
pub:
	pos      token.Position
	exprs    []Expr
	comments []Comment
pub mut:
	types []table.Type
}

pub enum SqlStmtKind {
	insert
	update
	delete
}

pub struct SqlStmt {
pub:
	kind            SqlStmtKind
	db_expr         Expr   // `db` in `sql db {`
	object_var_name string // `user`
	pos             token.Position
	where_expr      Expr
	updated_columns []string // for `update set x=y`
	update_exprs    []Expr   // for `update`
pub mut:
	table_expr  Type
	fields      []table.Field
	sub_structs map[int]SqlStmt
}

pub struct Embed {
pub:
	typ table.Type
	pos token.Position
}

pub struct StructDecl {
pub:
	pos       token.Position
	name      string
	gen_types []table.Type
	is_pub    bool
	// _pos fields for vfmt
	mut_pos      int // mut:
	pub_pos      int // pub:
	pub_mut_pos  int // pub mut:
	global_pos   int // __global:
	module_pos   int // module:
	language     table.Language
	is_union     bool
	attrs        []table.Attr
	end_comments []Comment
	embeds       []Embed
pub mut:
	fields []StructField
}

// check if stmt can be an expression in C
pub fn (stmt Stmt) check_c_expr() ? {
	match stmt {
		AssignStmt {
			return
		}
		ExprStmt {
			if stmt.expr.is_expr() {
				return
			}
			return error('unsupported statement (`$stmt.expr.type_name()`)')
		}
		else {}
	}
	return error('unsupported statement (`$stmt.type_name()`)')
}

pub fn (stmt Stmt) position() token.Position {
	match stmt {
		AssertStmt, AssignStmt, Block, BranchStmt, CompFor, ConstDecl, DeferStmt, EnumDecl, ExprStmt,
		FnDecl, ForCStmt, ForInStmt, ForStmt, GotoLabel, GotoStmt, Import, Return, StructDecl,
		GlobalDecl, HashStmt, InterfaceDecl, Module, SqlStmt {
			return stmt.pos
		}
		GoStmt {
			return stmt.call_expr.pos
		}
		TypeDecl {
			match stmt {
				AliasTypeDecl, FnTypeDecl, SumTypeDecl { return stmt.pos }
			}
		}
		// Please, do NOT use else{} here.
		// This match is exhaustive *on purpose*, to help force
		// maintaining/implementing proper .pos fields.
	}
}
