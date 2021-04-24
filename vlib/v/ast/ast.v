// Copyright (c) 2019-2021 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module ast

import v.token
import v.errors
import v.pref

pub type TypeDecl = AliasTypeDecl | FnTypeDecl | SumTypeDecl

pub type Expr = AnonFn | ArrayDecompose | ArrayInit | AsCast | Assoc | AtExpr | BoolLiteral |
	CTempVar | CallExpr | CastExpr | ChanInit | CharLiteral | Comment | ComptimeCall |
	ComptimeSelector | ConcatExpr | DumpExpr | EmptyExpr | EnumVal | FloatLiteral | GoExpr |
	Ident | IfExpr | IfGuardExpr | IndexExpr | InfixExpr | IntegerLiteral | Likely | LockExpr |
	MapInit | MatchExpr | NodeError | None | OffsetOf | OrExpr | ParExpr | PostfixExpr |
	PrefixExpr | RangeExpr | SelectExpr | SelectorExpr | SizeOf | SqlExpr | StringInterLiteral |
	StringLiteral | StructInit | TypeNode | TypeOf | UnsafeExpr

pub type Stmt = AsmStmt | AssertStmt | AssignStmt | Block | BranchStmt | CompFor | ConstDecl |
	DeferStmt | EmptyStmt | EnumDecl | ExprStmt | FnDecl | ForCStmt | ForInStmt | ForStmt |
	GlobalDecl | GotoLabel | GotoStmt | HashStmt | Import | InterfaceDecl | Module | NodeError |
	Return | SqlStmt | StructDecl | TypeDecl

// NB: when you add a new Expr or Stmt type with a .pos field, remember to update
// the .position() token.Position methods too.
pub type ScopeObject = AsmRegister | ConstField | GlobalField | Var

// TODO: replace Param
pub type Node = CallArg | ConstField | EmptyNode | EnumField | Expr | File | GlobalField |
	IfBranch | MatchBranch | NodeError | Param | ScopeObject | SelectBranch | Stmt | StructField |
	StructInitField | SumTypeVariant

pub struct TypeNode {
pub:
	typ Type
	pos token.Position
}

pub struct EmptyExpr {
	x int
}

pub fn empty_expr() Expr {
	return EmptyExpr{}
}

pub struct EmptyStmt {
pub:
	pos token.Position
}

pub fn empty_stmt() Stmt {
	return EmptyStmt{}
}

pub struct EmptyNode {
	x int
}

pub fn empty_node() Node {
	return EmptyNode{}
}

// `{stmts}` or `unsafe {stmts}`
pub struct Block {
pub:
	stmts     []Stmt
	is_unsafe bool
	pos       token.Position
}

// | IncDecStmt k
// Stand-alone expression in a statement list.
pub struct ExprStmt {
pub:
	expr     Expr
	pos      token.Position
	comments []Comment
pub mut:
	is_expr bool
	typ     Type
}

pub struct IntegerLiteral {
pub:
	val string
	pos token.Position
}

pub struct FloatLiteral {
pub:
	val string
	pos token.Position
}

pub struct StringLiteral {
pub:
	val      string
	is_raw   bool
	language Language
	pos      token.Position
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
	expr_types []Type
	fmts       []byte
	need_fmts  []bool // an explicit non-default fmt required, e.g. `x`
}

pub struct CharLiteral {
pub:
	val string
	pos token.Position
}

pub struct BoolLiteral {
pub:
	val bool
	pos token.Position
}

// `foo.bar`
pub struct SelectorExpr {
pub:
	pos        token.Position
	field_name string
	is_mut     bool // is used for the case `if mut ident.selector is MyType {`, it indicates if the root ident is mutable
	mut_pos    token.Position
	next_token token.Kind
pub mut:
	expr            Expr // expr.field_name
	expr_type       Type // type of `Foo` in `Foo.bar`
	typ             Type // type of the entire thing (`Foo.bar`)
	name_type       Type // T in `T.name` or typeof in `typeof(expr).name`
	scope           &Scope
	from_embed_type Type // holds the type of the embed that the method is called from
}

// root_ident returns the origin ident where the selector started.
pub fn (e &SelectorExpr) root_ident() ?Ident {
	mut root := e.expr
	for root is SelectorExpr {
		// TODO: remove this line
		selector_expr := root as SelectorExpr
		root = selector_expr.expr
	}
	if root is Ident {
		return root as Ident
	}

	return none
}

// module declaration
pub struct Module {
pub:
	name       string // encoding.base64
	short_name string // base64
	attrs      []Attr
	pos        token.Position
	name_pos   token.Position // `name` in import name
	is_skipped bool // module main can be skipped in single file programs
}

pub struct StructField {
pub:
	pos              token.Position
	type_pos         token.Position
	comments         []Comment
	default_expr     Expr
	has_default_expr bool
	attrs            []Attr
	is_pub           bool
	default_val      string
	is_mut           bool
	is_global        bool
pub mut:
	default_expr_typ Type
	name             string
	typ              Type
}

/*
pub struct Field {
pub:
	name string
	pos  token.Position
pub mut:
	typ Type
}
*/

// const field in const declaration group
pub struct ConstField {
pub:
	mod    string
	name   string
	expr   Expr // the value expr of field; everything after `=`
	is_pub bool
	pos    token.Position
pub mut:
	typ      Type      // the type of the const field, it can be any type in V
	comments []Comment // comments before current const field
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

pub struct StructDecl {
pub:
	pos           token.Position
	name          string
	generic_types []Type
	is_pub        bool
	// _pos fields for vfmt
	mut_pos      int // mut:
	pub_pos      int // pub:
	pub_mut_pos  int // pub mut:
	global_pos   int // __global:
	module_pos   int // module:
	language     Language
	is_union     bool
	attrs        []Attr
	end_comments []Comment
	embeds       []Embed
pub mut:
	fields []StructField
}

pub struct Embed {
pub:
	typ Type
	pos token.Position
}

pub struct StructEmbedding {
pub:
	name string
	typ  Type
	pos  token.Position
}

pub struct InterfaceDecl {
pub:
	name         string
	name_pos     token.Position
	field_names  []string
	is_pub       bool
	methods      []FnDecl
	mut_pos      int // mut:
	fields       []StructField
	pos          token.Position
	pre_comments []Comment
}

pub struct StructInitField {
pub:
	expr          Expr
	pos           token.Position
	name_pos      token.Position
	comments      []Comment
	next_comments []Comment
pub mut:
	name          string
	typ           Type
	expected_type Type
	parent_type   Type
}

pub struct StructInitEmbed {
pub:
	expr          Expr
	pos           token.Position
	comments      []Comment
	next_comments []Comment
pub mut:
	name          string
	typ           Type
	expected_type Type
}

pub struct StructInit {
pub:
	pos      token.Position
	name_pos token.Position
	is_short bool
pub mut:
	unresolved           bool
	pre_comments         []Comment
	typ                  Type
	update_expr          Expr
	update_expr_type     Type
	update_expr_comments []Comment
	has_update_expr      bool
	fields               []StructInitField
	embeds               []StructInitEmbed
}

// import statement
pub struct Import {
pub:
	mod       string // the module name of the import
	alias     string // the `x` in `import xxx as x`
	pos       token.Position
	mod_pos   token.Position
	alias_pos token.Position
	syms_pos  token.Position
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

// anonymous function
pub struct AnonFn {
pub mut:
	decl    FnDecl
	typ     Type // the type of anonymous fn. Both .typ and .decl.name are auto generated
	has_gen bool // has been generated
}

// function or method declaration
pub struct FnDecl {
pub:
	name            string
	mod             string
	params          []Param
	is_deprecated   bool
	is_pub          bool
	is_variadic     bool
	is_anon         bool
	is_manualfree   bool           // true, when [manualfree] is used on a fn
	is_main         bool           // true for `fn main()`
	is_test         bool           // true for `fn test_abcde`
	is_conditional  bool           // true for `[if abc] fn abc(){}`
	is_keep_alive   bool           // passed memory must not be freed (by GC) before function returns
	receiver        StructField    // TODO this is not a struct field
	receiver_pos    token.Position // `(u User)` in `fn (u User) name()` position
	is_method       bool
	method_type_pos token.Position // `User` in ` fn (u User)` position
	method_idx      int
	rec_mut         bool // is receiver mutable
	rec_share       ShareType
	language        Language
	no_body         bool // just a definition `fn C.malloc()`
	is_builtin      bool // this function is defined in builtin/strconv
	body_pos        token.Position // function bodys position
	file            string
	generic_names   []string
	is_direct_arr   bool // direct array access
	attrs           []Attr
	skip_gen        bool // this function doesn't need to be generated (for example [if foo])
pub mut:
	stmts             []Stmt
	defer_stmts       []DeferStmt
	return_type       Type
	return_type_pos   token.Position // `string` in `fn (u User) name() string` position
	has_return        bool
	comments          []Comment // comments *after* the header, but *before* `{`; used for InterfaceDecl
	next_comments     []Comment // coments that are one line after the decl; used for InterfaceDecl
	source_file       &File = 0
	scope             &Scope
	label_names       []string
	pos               token.Position // function declaration position
	cur_generic_types []Type
}

// break, continue
pub struct BranchStmt {
pub:
	kind  token.Kind
	label string
	pos   token.Position
}

// function or method call expr
pub struct CallExpr {
pub:
	pos      token.Position
	name_pos token.Position
	mod      string
pub mut:
	name               string // left.name()
	is_method          bool
	is_field           bool // temp hack, remove ASAP when re-impl CallExpr / Selector (joe)
	is_keep_alive      bool // GC must not free arguments before fn returns
	args               []CallArg
	expected_arg_types []Type
	language           Language
	or_block           OrExpr
	left               Expr // `user` in `user.register()`
	left_type          Type // type of `user`
	receiver_type      Type // User
	return_type        Type
	should_be_skipped  bool
	concrete_types     []Type // concrete types, e.g. <int, string>
	concrete_list_pos  token.Position
	free_receiver      bool // true if the receiver expression needs to be freed
	scope              &Scope
	from_embed_type    Type // holds the type of the embed that the method is called from
	comments           []Comment
}

/*
pub struct AutofreeArgVar {
	name string
	idx  int
}
*/
// function call argument: `f(callarg)`
pub struct CallArg {
pub:
	is_mut   bool
	share    ShareType
	expr     Expr
	comments []Comment
pub mut:
	typ             Type
	is_tmp_autofree bool // this tells cgen that a tmp variable has to be used for the arg expression in order to free it after the call
	pos             token.Position
	// tmp_name        string // for autofree
}

// function return statement
pub struct Return {
pub:
	pos      token.Position
	exprs    []Expr
	comments []Comment
pub mut:
	types []Type
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
	name            string
	expr            Expr
	share           ShareType
	is_mut          bool
	is_autofree_tmp bool
	is_arg          bool // fn args should not be autofreed
	is_auto_deref   bool
pub mut:
	typ        Type
	orig_type  Type   // original sumtype type; 0 if it's not a sumtype
	smartcasts []Type // nested sum types require nested smart casting, for that a list of types is needed
	// TODO: move this to a real docs site later
	// 10 <- original type (orig_type)
	//   [11, 12, 13] <- cast order (smartcasts)
	//        12 <- the current casted type (typ)
	pos        token.Position
	is_used    bool
	is_changed bool // to detect mutable vars that are never changed
	//
	// (for setting the position after the or block for autofree)
	is_or  bool // `x := foo() or { ... }`
	is_tmp bool // for tmp for loop vars, so that autofree can skip them
}

// used for smartcasting only
// struct fields change type in scopes
pub struct ScopeStructField {
pub:
	struct_type Type // type of struct
	name        string
	pos         token.Position
	typ         Type
	smartcasts  []Type // nested sum types require nested smart casting, for that a list of types is needed
	orig_type   Type   // original sumtype type; 0 if it's not a sumtype
	// TODO: move this to a real docs site later
	// 10 <- original type (orig_type)
	//   [11, 12, 13] <- cast order (smartcasts)
	//        12 <- the current casted type (typ)
}

pub struct GlobalField {
pub:
	name     string
	expr     Expr
	has_expr bool
	pos      token.Position
	typ_pos  token.Position
pub mut:
	typ      Type
	comments []Comment
}

pub struct GlobalDecl {
pub:
	pos      token.Position
	is_block bool // __global() block
pub mut:
	fields       []GlobalField
	end_comments []Comment
}

pub struct EmbeddedFile {
pub:
	rpath string // used in the source code, as an ID/key to the embed
	apath string // absolute path during compilation to the resource
}

// Each V source file is represented by one File structure.
// When the V compiler runs, the parser will fill an []File.
// That array is then passed to V's checker.
pub struct File {
pub:
	path         string // absolute path of the source file - '/projects/v/file.v'
	path_base    string // file name - 'file.v' (useful for tracing)
	lines        int    // number of source code lines in the file (including newlines and comments)
	bytes        int    // number of processed source code bytes
	mod          Module // the module of the source file (from `module xyz` at the top)
	global_scope &Scope
	is_test      bool // true for _test.v files
pub mut:
	scope            &Scope
	stmts            []Stmt            // all the statements in the source file
	imports          []Import          // all the imports
	auto_imports     []string          // imports that were implicitely added
	embedded_files   []EmbeddedFile    // list of files to embed in the binary
	imported_symbols map[string]string // used for `import {symbol}`, it maps symbol => module.symbol
	errors           []errors.Error    // all the checker errors in the file
	warnings         []errors.Warning  // all the checker warnings in the file
	notices          []errors.Notice   // all the checker notices in the file
	generic_fns      []&FnDecl
	global_labels    []string // from `asm { .globl labelname }`
}

[unsafe]
pub fn (f &File) free() {
	unsafe {
		f.path.free()
		f.path_base.free()
		f.scope.free()
		f.stmts.free()
		f.imports.free()
		f.auto_imports.free()
		f.embedded_files.free()
		f.imported_symbols.free()
		f.errors.free()
		f.warnings.free()
		f.notices.free()
		f.global_labels.free()
	}
}

pub struct IdentFn {
pub mut:
	typ Type
}

// TODO: (joe) remove completely, use ident.obj
// instead which points to the scope object
pub struct IdentVar {
pub mut:
	typ         Type
	is_mut      bool
	is_static   bool
	is_optional bool
	share       ShareType
}

pub type IdentInfo = IdentFn | IdentVar

pub enum IdentKind {
	unresolved
	blank_ident
	variable
	constant
	global
	function
}

// A single identifier
pub struct Ident {
pub:
	language Language
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

// left op right
// See: token.Kind.is_infix
pub struct InfixExpr {
pub:
	op      token.Kind
	pos     token.Position
	is_stmt bool
pub mut:
	left        Expr
	right       Expr
	left_type   Type
	right_type  Type
	auto_locked string
	or_block    OrExpr
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
	op  token.Kind
	pos token.Position
pub mut:
	right_type Type
	right      Expr
	or_block   OrExpr
	is_option  bool // IfGuard
}

pub struct IndexExpr {
pub:
	pos     token.Position
	index   Expr // [0], RangeExpr [start..end] or map[key]
	or_expr OrExpr
pub mut:
	left      Expr
	left_type Type // array, map, fixed array
	is_setter bool
	is_map    bool
	is_array  bool
	is_farray bool
	is_option bool // IfGuard
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
	typ      Type
	has_else bool
}

pub struct IfBranch {
pub:
	cond     Expr
	pos      token.Position
	body_pos token.Position
	comments []Comment
pub mut:
	stmts []Stmt
	scope &Scope
}

pub struct UnsafeExpr {
pub:
	expr Expr
	pos  token.Position
}

pub struct LockExpr {
pub:
	stmts    []Stmt
	is_rlock []bool
	pos      token.Position
pub mut:
	lockeds []Ident // `x`, `y` in `lock x, y {`
	is_expr bool
	typ     Type
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
	return_type   Type
	cond_type     Type // type of `x` in `match x {`
	expected_type Type // for debugging only
	is_sum_type   bool
}

pub struct MatchBranch {
pub:
	exprs         []Expr      // left side
	ecmnts        [][]Comment // inline comments for each left side expr
	stmts         []Stmt      // right side
	pos           token.Position
	is_else       bool
	post_comments []Comment // comments below ´... }´
pub mut:
	scope &Scope
}

pub struct SelectExpr {
pub:
	branches      []SelectBranch
	pos           token.Position
	has_exception bool
pub mut:
	is_expr       bool // returns a value
	expected_type Type // for debugging only
}

pub struct SelectBranch {
pub:
	stmt          Stmt   // `a := <-ch` or `ch <- a`
	stmts         []Stmt // right side
	pos           token.Position
	comment       Comment // comment above `select {`
	is_else       bool
	is_timeout    bool
	post_comments []Comment
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
	typ_pos token.Position
pub mut:
	// expr    Expr
	typ Type
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
	key_type  Type
	val_type  Type
	cond_type Type
	kind      Kind   // array/map/string
	label     string // `label: for {`
	scope     &Scope
}

pub struct ForCStmt {
pub:
	init     Stmt // i := 0;
	has_init bool
	cond     Expr // i < 10;
	has_cond bool
	inc      Stmt // i++; i += 2
	has_inc  bool
	is_multi bool // for a,b := 0,1; a < 10; a,b = a+b, a {...}
	stmts    []Stmt
	pos      token.Position
pub mut:
	label string // `label: for {`
	scope &Scope
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

/*
// filter(), map(), sort()
pub struct Lambda {
pub:
	name string
}
*/
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
	left_types    []Type
	right_types   []Type
	is_static     bool // for translated code only
	is_simple     bool // `x+=2` in `for x:=1; ; x+=2`
	has_cross_var bool
}

pub struct AsCast {
pub:
	expr Expr
	typ  Type
	pos  token.Position
pub mut:
	expr_type Type
}

// an enum value, like OS.macos or .macos
pub struct EnumVal {
pub:
	enum_name string
	val       string
	mod       string // for full path `mod_Enum_val`
	pos       token.Position
pub mut:
	typ Type
}

// enum field in enum declaration
pub struct EnumField {
pub:
	name          string
	pos           token.Position
	comments      []Comment // comment after Enumfield in the same line
	next_comments []Comment // comments between current EnumField and next EnumField
	expr          Expr      // the value of current EnumField; 123 in `ename = 123`
	has_expr      bool      // true, when .expr has a value
}

// enum declaration
pub struct EnumDecl {
pub:
	name             string
	is_pub           bool
	is_flag          bool        // true when the enum has [flag] tag,for bit field enum
	is_multi_allowed bool        // true when the enum has [_allow_multiple_values] tag
	comments         []Comment   // comments before the first EnumField
	fields           []EnumField // all the enum fields
	attrs            []Attr      // attributes of enum declaration
	pos              token.Position
}

pub struct AliasTypeDecl {
pub:
	name        string
	is_pub      bool
	parent_type Type
	pos         token.Position
	type_pos    token.Position
	comments    []Comment
}

// New implementation of sum types
pub struct SumTypeDecl {
pub:
	name     string
	is_pub   bool
	pos      token.Position
	comments []Comment
	typ      Type
pub mut:
	variants []SumTypeVariant
}

pub struct SumTypeVariant {
pub:
	typ Type
	pos token.Position
}

pub struct FnTypeDecl {
pub:
	name     string
	is_pub   bool
	typ      Type
	pos      token.Position
	type_pos token.Position
	comments []Comment
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

// `(3+4)`
pub struct ParExpr {
pub:
	expr Expr
	pos  token.Position
}

pub struct GoExpr {
pub:
	pos token.Position
pub mut:
	call_expr CallExpr
	is_expr   bool
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

pub struct ArrayInit {
pub:
	pos           token.Position // `[]` in []Type{} position
	elem_type_pos token.Position // `Type` in []Type{} position
	exprs         []Expr      // `[expr, expr]` or `[expr]Type{}` for fixed array
	ecmnts        [][]Comment // optional iembed comments after each expr
	pre_cmnts     []Comment
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
	expr_types []Type // [Dog, Cat] // also used for interface_types
	elem_type  Type   // element type
	typ        Type   // array type
}

pub struct ArrayDecompose {
pub:
	expr Expr
	pos  token.Position
pub mut:
	expr_type Type
	arg_type  Type
}

pub struct ChanInit {
pub:
	pos      token.Position
	cap_expr Expr
	has_cap  bool
pub mut:
	typ       Type
	elem_type Type
}

pub struct MapInit {
pub:
	pos       token.Position
	keys      []Expr
	vals      []Expr
	comments  [][]Comment // comments after key-value pairs
	pre_cmnts []Comment   // comments before the first key-value pair
pub mut:
	typ        Type
	key_type   Type
	value_type Type
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

// NB: &string(x) gets parsed as PrefixExpr{ right: CastExpr{...} }
// TODO: that is very likely a parsing bug. It should get parsed as just
// CastExpr{...}, where .typname is '&string' instead.
// The current situation leads to special cases in vfmt and cgen
// (see prefix_expr_cast_expr in fmt.v, and .is_amp in cgen.v)
// .in_prexpr is also needed because of that, because the checker needs to
// show warnings about the deprecated C->V conversions `string(x)` and
// `string(x,y)`, while skipping the real pointer casts like `&string(x)`.
pub struct CastExpr {
pub:
	expr Expr // `buf` in `string(buf, n)`
	arg  Expr // `n` in `string(buf, n)`
	typ  Type // `string` TODO rename to `type_to_cast_to`
	pos  token.Position
pub mut:
	typname   string // TypeSymbol.name
	expr_type Type   // `byteptr`
	has_arg   bool
	in_prexpr bool // is the parent node a PrefixExpr
}

pub struct AsmStmt {
pub:
	arch         pref.Arch
	is_top_level bool
	is_volatile  bool
	is_goto      bool
	clobbered    []AsmClobbered
	pos          token.Position
pub mut:
	templates     []AsmTemplate
	scope         &Scope
	output        []AsmIO
	input         []AsmIO
	global_labels []string // labels defined in assembly block, exported with `.globl`
	local_labels  []string // local to the assembly block
}

pub struct AsmTemplate {
pub mut:
	name         string
	is_label     bool // `example_label:`
	is_directive bool // .globl assembly_function
	args         []AsmArg
	comments     []Comment
	pos          token.Position
}

// [eax+5] | j | displacement literal (e.g. 123 in [rax + 123] ) | eax | true | `a` | 0.594 | 123 | label_name
pub type AsmArg = AsmAddressing | AsmAlias | AsmDisp | AsmRegister | BoolLiteral | CharLiteral |
	FloatLiteral | IntegerLiteral | string

pub struct AsmRegister {
pub mut:
	name string // eax or r12d etc.
	typ  Type
	size int
}

pub struct AsmDisp {
pub:
	val string
	pos token.Position
}

pub struct AsmAlias {
pub:
	pos token.Position
pub mut:
	name string // a
}

pub struct AsmAddressing {
pub:
	scale int = -1 // 1, 2, 4, or 8 literal
	mode  AddressingMode
	pos   token.Position
pub mut:
	displacement AsmArg // 8, 16 or 32 bit literal value
	base         AsmArg // gpr
	index        AsmArg // gpr
}

// adressing modes:
pub enum AddressingMode {
	invalid
	displacement // displacement
	base // base
	base_plus_displacement // base + displacement
	index_times_scale_plus_displacement // (index ∗ scale) + displacement
	base_plus_index_plus_displacement // base + (index ∗ scale) + displacement
	base_plus_index_times_scale_plus_displacement // base + index + displacement
	rip_plus_displacement // rip + displacement
}

pub struct AsmClobbered {
pub mut:
	reg      AsmRegister
	comments []Comment
}

// : [alias_a] '=r' (a) // this is a comment
pub struct AsmIO {
pub:
	alias      string    // [alias_a]
	constraint string    // '=r'
	expr       Expr      // (a)
	comments   []Comment // // this is a comment
	typ        Type
	pos        token.Position
}

pub const (
	// reference: https://en.wikipedia.org/wiki/X86#/media/File:Table_of_x86_Registers_svg.svg
	// map register size -> register name
	x86_no_number_register_list   = map{
		8:  ['al', 'ah', 'bl', 'bh', 'cl', 'ch', 'dl', 'dh', 'bpl', 'sil', 'dil', 'spl']
		16: ['ax', 'bx', 'cx', 'dx', 'bp', 'si', 'di', 'sp', /* segment registers */ 'cs', 'ss',
			'ds', 'es', 'fs', 'gs', 'flags', 'ip', /* task registers */ 'gdtr', 'idtr', 'tr', 'ldtr',
			// CSR register 'msw', /* FP core registers */ 'cw', 'sw', 'tw', 'fp_ip', 'fp_dp',
			'fp_cs', 'fp_ds', 'fp_opc']
		32: [
			'eax',
			'ebx',
			'ecx',
			'edx',
			'ebp',
			'esi',
			'edi',
			'esp',
			'eflags',
			'eip', /* CSR register */
			'mxcsr' /* 32-bit FP core registers 'fp_dp', 'fp_ip' (TODO: why are there duplicates?) */,
		]
		64: ['rax', 'rbx', 'rcx', 'rdx', 'rbp', 'rsi', 'rdi', 'rsp', 'rflags', 'rip']
	}
	// no comments because maps do not support comments
	// r#*: gp registers added in 64-bit extensions, can only be from 8-15 actually
	// *mm#: vector/simd registors
	// st#: floating point numbers
	// cr#: control/status registers
	// dr#: debug registers
	x86_with_number_register_list = map{
		8:   map{
			'r#b': 16
		}
		16:  map{
			'r#w': 16
		}
		32:  map{
			'r#d': 16
		}
		64:  map{
			'r#':  16
			'mm#': 16
			'cr#': 16
			'dr#': 16
		}
		80:  map{
			'st#': 16
		}
		128: map{
			'xmm#': 32
		}
		256: map{
			'ymm#': 32
		}
		512: map{
			'zmm#': 32
		}
	}
)

// TODO: saved priviled registers for arm
pub const (
	arm_no_number_register_list   = ['fp' /* aka r11 */, /* not instruction pointer: */ 'ip' /* aka r12 */,
		'sp' /* aka r13 */, 'lr' /* aka r14 */, /* this is instruction pointer ('program counter'): */
		'pc' /* aka r15 */,
	] // 'cpsr' and 'apsr' are special flags registers, but cannot be referred to directly
	arm_with_number_register_list = map{
		'r#': 16
	}
)

pub const (
	riscv_no_number_register_list   = ['zero', 'ra', 'sp', 'gp', 'tp']
	riscv_with_number_register_list = map{
		'x#': 32
		't#': 3
		's#': 12
		'a#': 8
	}
)

pub struct AssertStmt {
pub:
	pos token.Position
pub mut:
	expr    Expr
	is_used bool // asserts are used in _test.v files, as well as in non -prod builds of all files
}

// `if [x := opt()] {`
pub struct IfGuardExpr {
pub:
	var_name string
	pos      token.Position
pub mut:
	expr      Expr
	expr_type Type
}

pub enum OrKind {
	absent
	block
	propagate
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

// deprecated
pub struct Assoc {
pub:
	var_name string
	fields   []string
	exprs    []Expr
	pos      token.Position
pub mut:
	typ   Type
	scope &Scope
}

pub struct SizeOf {
pub:
	is_type bool
	expr    Expr // checker uses this to set typ
	pos     token.Position
pub mut:
	typ Type
}

pub struct OffsetOf {
pub:
	struct_type Type
	field       string
	pos         token.Position
}

pub struct Likely {
pub:
	expr      Expr
	pos       token.Position
	is_likely bool // false for _unlikely_
}

pub struct TypeOf {
pub:
	expr Expr
	pos  token.Position
pub mut:
	expr_type Type
}

pub struct DumpExpr {
pub:
	expr Expr
	pos  token.Position
pub mut:
	expr_type Type
	cname     string // filled in the checker
}

pub struct Comment {
pub:
	text     string
	is_multi bool
	line_nr  int
	pos      token.Position
}

pub struct ConcatExpr {
pub:
	vals []Expr
	pos  token.Position
pub mut:
	return_type Type
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

pub struct ComptimeSelector {
pub:
	has_parens bool // if $() is used, for vfmt
	left       Expr
	field_expr Expr
	pos        token.Position
pub mut:
	left_type Type
	typ       Type
}

pub struct ComptimeCall {
pub:
	pos         token.Position
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
	sym         TypeSymbol
	result_type Type
	env_value   string
	args        []CallArg
}

pub struct None {
pub:
	pos token.Position
}

pub enum SqlStmtKind {
	insert
	update
	delete
	create
	drop
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
	table_expr  TypeNode
	fields      []StructField
	sub_structs map[int]SqlStmt
}

pub struct SqlExpr {
pub:
	typ         Type
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
	table_expr  TypeNode
	fields      []StructField
	sub_structs map[int]SqlExpr
}

pub struct NodeError {
pub:
	idx int // index for referencing the related File error
	pos token.Position
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
	// NB: please do not print here. the language server will hang
	// as it uses STDIO primarly to communicate ~Ned
	match expr {
		AnonFn {
			return expr.decl.pos
		}
		EmptyExpr {
			// println('compiler bug, unhandled EmptyExpr position()')
			return token.Position{}
		}
		NodeError, ArrayDecompose, ArrayInit, AsCast, Assoc, AtExpr, BoolLiteral, CallExpr,
		CastExpr, ChanInit, CharLiteral, ConcatExpr, Comment, ComptimeCall, ComptimeSelector,
		EnumVal, DumpExpr, FloatLiteral, GoExpr, Ident, IfExpr, IntegerLiteral, Likely, LockExpr,
		MapInit, MatchExpr, None, OffsetOf, OrExpr, ParExpr, PostfixExpr, PrefixExpr, RangeExpr,
		SelectExpr, SelectorExpr, SizeOf, SqlExpr, StringInterLiteral, StringLiteral, StructInit,
		TypeNode, TypeOf, UnsafeExpr {
			return expr.pos
		}
		IndexExpr {
			if expr.or_expr.kind != .absent {
				return expr.or_expr.pos
			}
			return expr.pos
		}
		IfGuardExpr {
			return expr.expr.position()
		}
		InfixExpr {
			left_pos := expr.left.position()
			right_pos := expr.right.position()
			return token.Position{
				line_nr: expr.pos.line_nr
				pos: left_pos.pos
				len: right_pos.pos - left_pos.pos + right_pos.len
				col: left_pos.col
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
		LockExpr { return expr.is_expr }
		MatchExpr { return expr.is_expr }
		SelectExpr { return expr.is_expr }
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

pub fn (expr Expr) is_auto_deref_var() bool {
	match expr {
		Ident {
			if expr.obj is Var {
				if expr.obj.is_auto_deref {
					return true
				}
			}
		}
		PrefixExpr {
			if expr.op == .amp && expr.right.is_auto_deref_var() {
				return true
			}
		}
		else {}
	}
	return false
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

// CTempVar is used in cgen only, to hold nodes for temporary variables
pub struct CTempVar {
pub:
	name   string // the name of the C temporary variable; used by g.expr(x)
	orig   Expr   // the original expression, which produced the C temp variable; used by x.str()
	typ    Type   // the type of the original expression
	is_ptr bool   // whether the type is a pointer
}

pub fn (node Node) position() token.Position {
	match node {
		NodeError {
			return token.Position{}
		}
		EmptyNode {
			return token.Position{}
		}
		Stmt {
			mut pos := node.pos
			if node is Import {
				for sym in node.syms {
					pos = pos.extend(sym.pos)
				}
			} else if node is TypeDecl {
				match node {
					FnTypeDecl, AliasTypeDecl {
						pos = pos.extend(node.type_pos)
					}
					SumTypeDecl {
						for variant in node.variants {
							pos = pos.extend(variant.pos)
						}
					}
				}
			}
			if node is AssignStmt {
				return pos.extend(node.right.last().position())
			}
			if node is AssertStmt {
				return pos.extend(node.expr.position())
			}
			return pos
		}
		Expr {
			return node.position()
		}
		StructField {
			return node.pos.extend(node.type_pos)
		}
		MatchBranch, SelectBranch, EnumField, ConstField, StructInitField, GlobalField, CallArg,
		SumTypeVariant {
			return node.pos
		}
		Param {
			return node.pos.extend(node.type_pos)
		}
		IfBranch {
			return node.pos.extend(node.body_pos)
		}
		ScopeObject {
			match node {
				ConstField, GlobalField, Var {
					return node.pos
				}
				AsmRegister {
					return token.Position{
						len: -1
						line_nr: -1
						pos: -1
						last_line: -1
						col: -1
					}
				}
			}
		}
		File {
			mut pos := token.Position{}
			if node.stmts.len > 0 {
				first_pos := node.stmts.first().pos
				last_pos := node.stmts.last().pos
				pos = first_pos.extend_with_last_line(last_pos, last_pos.line_nr)
			}
			return pos
		}
	}
}

pub fn (node Node) children() []Node {
	mut children := []Node{}
	if node is Expr {
		match node {
			StringInterLiteral, Assoc, ArrayInit {
				return node.exprs.map(Node(it))
			}
			SelectorExpr, PostfixExpr, UnsafeExpr, AsCast, ParExpr, IfGuardExpr, SizeOf, Likely,
			TypeOf, ArrayDecompose {
				children << node.expr
			}
			LockExpr, OrExpr {
				return node.stmts.map(Node(it))
			}
			StructInit {
				return node.fields.map(Node(it))
			}
			AnonFn {
				children << Stmt(node.decl)
			}
			CallExpr {
				children << node.left
				children << node.args.map(Node(it))
				children << Expr(node.or_block)
			}
			InfixExpr {
				children << node.left
				children << node.right
			}
			PrefixExpr {
				children << node.right
			}
			IndexExpr {
				children << node.left
				children << node.index
			}
			IfExpr {
				children << node.left
				children << node.branches.map(Node(it))
			}
			MatchExpr {
				children << node.cond
				children << node.branches.map(Node(it))
			}
			SelectExpr {
				return node.branches.map(Node(it))
			}
			ChanInit {
				children << node.cap_expr
			}
			MapInit {
				children << node.keys.map(Node(it))
				children << node.vals.map(Node(it))
			}
			RangeExpr {
				children << node.low
				children << node.high
			}
			CastExpr {
				children << node.expr
				children << node.arg
			}
			ConcatExpr {
				return node.vals.map(Node(it))
			}
			ComptimeCall, ComptimeSelector {
				children << node.left
			}
			else {}
		}
	} else if node is Stmt {
		match node {
			Block, DeferStmt, ForCStmt, ForInStmt, ForStmt, CompFor {
				return node.stmts.map(Node(it))
			}
			ExprStmt, AssertStmt {
				children << node.expr
			}
			InterfaceDecl {
				children << node.methods.map(Node(Stmt(it)))
				children << node.fields.map(Node(it))
			}
			AssignStmt {
				children << node.left.map(Node(it))
				children << node.right.map(Node(it))
			}
			Return {
				return node.exprs.map(Node(it))
			}
			// NB: these four decl nodes cannot be merged as one branch
			StructDecl {
				return node.fields.map(Node(it))
			}
			GlobalDecl {
				return node.fields.map(Node(it))
			}
			ConstDecl {
				return node.fields.map(Node(it))
			}
			EnumDecl {
				return node.fields.map(Node(it))
			}
			FnDecl {
				if node.is_method {
					children << Node(node.receiver)
				}
				children << node.params.map(Node(it))
				children << node.stmts.map(Node(it))
			}
			TypeDecl {
				if node is SumTypeDecl {
					children << node.variants.map(Node(it))
				}
			}
			else {}
		}
	} else if node is ScopeObject {
		match node {
			GlobalField, ConstField, Var { children << node.expr }
			AsmRegister {}
		}
	} else {
		match node {
			GlobalField, ConstField, EnumField, StructInitField, CallArg {
				children << node.expr
			}
			SelectBranch {
				children << node.stmt
				children << node.stmts.map(Node(it))
			}
			IfBranch, File {
				return node.stmts.map(Node(it))
			}
			MatchBranch {
				children << node.stmts.map(Node(it))
				children << node.exprs.map(Node(it))
			}
			else {}
		}
	}
	return children
}

// helper for dealing with `m[k1][k2][k3][k3] = value`
pub fn (mut lx IndexExpr) recursive_mapset_is_setter(val bool) {
	lx.is_setter = val
	if mut lx.left is IndexExpr {
		if lx.left.is_map {
			lx.left.recursive_mapset_is_setter(val)
		}
	}
}

// return all the registers for a give architecture
pub fn all_registers(mut t Table, arch pref.Arch) map[string]ScopeObject {
	mut res := map[string]ScopeObject{}
	match arch {
		.amd64, .i386 {
			for bit_size, array in ast.x86_no_number_register_list {
				for name in array {
					res[name] = AsmRegister{
						name: name
						typ: t.bitsize_to_type(bit_size)
						size: bit_size
					}
				}
			}
			for bit_size, array in ast.x86_with_number_register_list {
				for name, max_num in array {
					for i in 0 .. max_num {
						hash_index := name.index('#') or {
							panic('all_registers: no hashtag found')
						}
						assembled_name := '${name[..hash_index]}$i${name[hash_index + 1..]}'
						res[assembled_name] = AsmRegister{
							name: assembled_name
							typ: t.bitsize_to_type(bit_size)
							size: bit_size
						}
					}
				}
			}
		}
		.aarch32 {
			aarch32 := gen_all_registers(mut t, ast.arm_no_number_register_list, ast.arm_with_number_register_list,
				32)
			for k, v in aarch32 {
				res[k] = v
			}
		}
		.aarch64 {
			aarch64 := gen_all_registers(mut t, ast.arm_no_number_register_list, ast.arm_with_number_register_list,
				64)
			for k, v in aarch64 {
				res[k] = v
			}
		}
		.rv32 {
			rv32 := gen_all_registers(mut t, ast.riscv_no_number_register_list, ast.riscv_with_number_register_list,
				32)
			for k, v in rv32 {
				res[k] = v
			}
		}
		.rv64 {
			rv64 := gen_all_registers(mut t, ast.riscv_no_number_register_list, ast.riscv_with_number_register_list,
				64)
			for k, v in rv64 {
				res[k] = v
			}
		}
		else { // TODO
			panic('all_registers: unhandled arch')
		}
	}

	return res
}

// only for arm and riscv because x86 has different sized registers
fn gen_all_registers(mut t Table, without_numbers []string, with_numbers map[string]int, bit_size int) map[string]ScopeObject {
	mut res := map[string]ScopeObject{}
	for name in without_numbers {
		res[name] = AsmRegister{
			name: name
			typ: t.bitsize_to_type(bit_size)
			size: bit_size
		}
	}
	for name, max_num in with_numbers {
		for i in 0 .. max_num {
			hash_index := name.index('#') or { panic('all_registers: no hashtag found') }
			assembled_name := '${name[..hash_index]}$i${name[hash_index + 1..]}'
			res[assembled_name] = AsmRegister{
				name: assembled_name
				typ: t.bitsize_to_type(bit_size)
				size: bit_size
			}
		}
	}
	return res
}
