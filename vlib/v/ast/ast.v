// Copyright (c) 2019-2024 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
@[has_globals]
module ast

import v.token
import v.errors
import v.util
import v.pref
import sync.stdatomic

// V type names that cannot be used as global var name
pub const global_reserved_type_names = ['byte', 'bool', 'char', 'i8', 'i16', 'int', 'i64', 'u8',
	'u16', 'u32', 'u64', 'f32', 'f64', 'map', 'string', 'rune', 'usize', 'isize', 'voidptr', 'thread',
	'array']

pub const result_name = '_result'
pub const option_name = '_option'

// V builtin types defined on .v files
pub const builtins = ['string', 'array', 'DenseArray', 'map', 'Error', 'IError', option_name,
	result_name]

pub type TypeDecl = AliasTypeDecl | FnTypeDecl | SumTypeDecl

// pub const int_type_name = $if amd64 || arm64 {
pub const int_type_name = $if new_int ? {
	//'int'
	'i64'
} $else {
	'int'
}

pub type Expr = NodeError
	| AnonFn
	| ArrayDecompose
	| ArrayInit
	| AsCast
	| Assoc
	| AtExpr
	| BoolLiteral
	| CTempVar
	| CallExpr
	| CastExpr
	| ChanInit
	| CharLiteral
	| Comment
	| ComptimeCall
	| ComptimeSelector
	| ComptimeType
	| ConcatExpr
	| DumpExpr
	| EmptyExpr
	| EnumVal
	| FloatLiteral
	| GoExpr
	| Ident
	| IfExpr
	| IfGuardExpr
	| IndexExpr
	| InfixExpr
	| IntegerLiteral
	| IsRefType
	| LambdaExpr
	| Likely
	| LockExpr
	| MapInit
	| MatchExpr
	| Nil
	| None
	| OffsetOf
	| OrExpr
	| ParExpr
	| PostfixExpr
	| PrefixExpr
	| RangeExpr
	| SelectExpr
	| SelectorExpr
	| SizeOf
	| SpawnExpr
	| SqlExpr
	| StringInterLiteral
	| StringLiteral
	| StructInit
	| TypeNode
	| TypeOf
	| UnsafeExpr

pub type Stmt = AsmStmt
	| AssertStmt
	| AssignStmt
	| Block
	| BranchStmt
	| ComptimeFor
	| ConstDecl
	| DebuggerStmt
	| DeferStmt
	| EmptyStmt
	| EnumDecl
	| ExprStmt
	| FnDecl
	| ForCStmt
	| ForInStmt
	| ForStmt
	| GlobalDecl
	| GotoLabel
	| GotoStmt
	| HashStmt
	| Import
	| InterfaceDecl
	| Module
	| NodeError
	| Return
	| SemicolonStmt
	| SqlStmt
	| StructDecl
	| TypeDecl

pub struct EmptyScopeObject {
pub mut:
	name string
	typ  Type
}

pub type ScopeObject = EmptyScopeObject | AsmRegister | ConstField | GlobalField | Var

// TODO: replace Param
pub type Node = CallArg
	| ConstField
	| EmptyNode
	| EnumField
	| Expr
	| File
	| GlobalField
	| IfBranch
	| MatchBranch
	| NodeError
	| Param
	| ScopeObject
	| SelectBranch
	| Stmt
	| StructField
	| StructInitField

pub struct TypeNode {
pub:
	pos token.Pos
pub mut:
	typ          Type
	stmt         Stmt = empty_stmt // for anon struct
	end_comments []Comment // comments that after current type node
}

pub enum ComptimeTypeKind {
	unknown
	map
	int
	float
	struct
	iface
	array
	array_fixed
	array_dynamic
	sum_type
	enum
	alias
	function
	option
	string
	pointer
	voidptr
}

pub struct ComptimeType {
pub:
	kind ComptimeTypeKind
	pos  token.Pos
}

pub fn (cty ComptimeType) str() string {
	return match cty.kind {
		.unknown { '\$unknown' }
		.map { '\$map' }
		.int { '\$int' }
		.float { '\$float' }
		.struct { '\$struct' }
		.iface { '\$interface' }
		.array { '\$array' }
		.array_dynamic { '\$array_dynamic' }
		.array_fixed { '\$array_fixed' }
		.sum_type { '\$sumtype' }
		.enum { '\$enum' }
		.alias { '\$alias' }
		.function { '\$function' }
		.option { '\$option' }
		.string { '\$string' }
		.pointer { '\$pointer' }
		.voidptr { '\$voidptr' }
	}
}

pub type EmptyExpr = u8

pub struct EmptyStmt {
pub:
	pos token.Pos
}

pub struct EmptyNode {
pub:
	pos token.Pos
}

pub const empty_expr = Expr(EmptyExpr(0))
pub const empty_stmt = Stmt(EmptyStmt{})
pub const empty_node = Node(EmptyNode{})
pub const empty_scope_object = ScopeObject(EmptyScopeObject{'empty_scope_object', 0})
pub const empty_comptime_const_value = ComptTimeConstValue(EmptyExpr(0))

// `{stmts}` or `unsafe {stmts}`
pub struct Block {
pub:
	is_unsafe bool
	pos       token.Pos
pub mut:
	stmts []Stmt
}

// | IncDecStmt k
// Stand-alone expression in a statement list.
pub struct ExprStmt {
pub:
	pos      token.Pos
	comments []Comment
pub mut:
	expr    Expr
	is_expr bool
	typ     Type
}

pub struct IntegerLiteral {
pub:
	val string
	pos token.Pos
}

pub struct FloatLiteral {
pub:
	val string
	pos token.Pos
}

@[minify]
pub struct StringLiteral {
pub:
	val      string
	is_raw   bool
	language Language
	pos      token.Pos
}

// 'name: $name'
pub struct StringInterLiteral {
pub:
	vals       []string
	fwidths    []int
	precisions []int
	pluss      []bool
	fills      []bool
	fmt_poss   []token.Pos
	pos        token.Pos
pub mut:
	exprs      []Expr
	expr_types []Type
	fmts       []u8
	need_fmts  []bool // an explicit non-default fmt required, e.g. `x`
}

pub struct CharLiteral {
pub:
	val string
	pos token.Pos
}

pub struct BoolLiteral {
pub:
	val bool
	pos token.Pos
}

pub struct Nil {
pub:
	pos token.Pos
}

pub enum GenericKindField {
	unknown
	name
	typ
	unaliased_typ
	indirections
}

// `foo.bar`
@[minify]
pub struct SelectorExpr {
pub:
	pos        token.Pos
	field_name string
	is_mut     bool // is used for the case `if mut ident.selector is MyType {`, it indicates if the root ident is mutable
	mut_pos    token.Pos
	next_token token.Kind
pub mut:
	expr                     Expr // expr.field_name
	expr_type                Type // type of `Foo` in `Foo.bar`
	typ                      Type // type of the entire thing (`Foo.bar`)
	name_type                Type // T in `T.name` or typeof in `typeof(expr).name`
	or_block                 OrExpr
	gkind_field              GenericKindField // `T.name` => ast.GenericKindField.name, `T.typ` => ast.GenericKindField.typ, or .unknown
	scope                    &Scope = unsafe { nil }
	from_embed_types         []Type   // holds the type of the embed that the method is called from
	generic_from_embed_types [][]Type // holds the types of the embeds for each generic instance when the same generic method is called.
	has_hidden_receiver      bool
	is_field_typ             bool // var.typ for comptime $for var
}

// root_ident returns the origin ident where the selector started.
pub fn (e &SelectorExpr) root_ident() ?Ident {
	mut root := e.expr
	for mut root is SelectorExpr {
		root = root.expr
	}
	if mut root is Ident {
		return root
	}

	return none
}

// module declaration
pub struct Module {
pub:
	name       string // encoding.base64
	short_name string // base64
	attrs      []Attr
	pos        token.Pos
	name_pos   token.Pos // `name` in import name
	is_skipped bool      // module main can be skipped in single file programs
}

pub struct SemicolonStmt {
pub:
	pos token.Pos
}

@[minify]
pub struct StructField {
pub:
	pos              token.Pos
	type_pos         token.Pos
	option_pos       token.Pos
	pre_comments     []Comment
	comments         []Comment
	i                int
	has_default_expr bool
	has_prev_newline bool
	has_break_line   bool
	attrs            []Attr
	is_pub           bool
	default_val      string
	is_mut           bool
	is_global        bool
	is_volatile      bool
	is_deprecated    bool
	is_embed         bool
pub mut:
	next_comments    []Comment
	is_recursive     bool
	is_part_of_union bool
	container_typ    Type
	default_expr     Expr
	default_expr_typ Type
	name             string
	typ              Type
	unaliased_typ    Type
	anon_struct_decl StructDecl // only if the field is an anonymous struct
}

pub fn (f &StructField) equals(o &StructField) bool {
	// TODO: f.is_mut == o.is_mut was removed here to allow read only access
	// to (mut/not mut), but otherwise equal fields; some other new checks are needed:
	// - if node is declared mut, and we mutate node.stmts, all stmts fields must be mutable
	// - same goes for pub and global, if we call the field from another module
	return f.name == o.name && f.typ == o.typ && f.is_pub == o.is_pub && f.is_global == o.is_global
}

// const field in const declaration group
pub struct ConstField {
pub:
	mod          string
	name         string
	is_pub       bool
	is_markused  bool // an explicit `@[markused]` tag; the const will NOT be removed by `-skip-unused`, no matter what
	pos          token.Pos
	attrs        []Attr // same value as `attrs` of the ConstDecl to which it belongs
	is_virtual_c bool   // `const C.MY_CONST u8`
pub mut:
	expr         Expr      // the value expr of field; everything after `=`
	typ          Type      // the type of the const field, it can be any type in V
	comments     []Comment // comments before current const field
	end_comments []Comment // comments that after const field
	// the comptime_expr_value field is filled by the checker, when it has enough
	// info to evaluate the constant at compile time
	comptime_expr_value ComptTimeConstValue = empty_comptime_const_value
}

// const declaration
@[minify]
pub struct ConstDecl {
pub:
	is_pub bool
	pos    token.Pos
	attrs  []Attr // tags like `@[markused]`, valid for all the consts in the list
pub mut:
	fields       []ConstField // all the const fields in the `const (...)` block
	end_comments []Comment    // comments that after last const field
	is_block     bool         // const() block
}

@[minify]
pub struct StructDecl {
pub:
	pos           token.Pos
	name          string
	scoped_name   string
	generic_types []Type
	is_pub        bool
	// _pos fields for vfmt
	mut_pos          int = -1 // mut:
	pub_pos          int = -1 // pub:
	pub_mut_pos      int = -1 // pub mut:
	global_pos       int = -1 // __global:
	module_pos       int = -1 // module:
	is_union         bool
	is_option        bool
	attrs            []Attr
	pre_comments     []Comment
	end_comments     []Comment
	embeds           []Embed
	is_implements    bool
	implements_types []TypeNode
pub mut:
	language Language
	fields   []StructField
	idx      int
}

pub struct Embed {
pub:
	typ      Type
	pos      token.Pos
	comments []Comment
}

pub struct InterfaceEmbedding {
pub:
	name     string
	typ      Type
	pos      token.Pos
	comments []Comment
}

@[minify]
pub struct InterfaceDecl {
pub:
	name          string
	typ           Type
	name_pos      token.Pos
	language      Language
	field_names   []string
	is_pub        bool
	mut_pos       int // mut:
	pos           token.Pos
	pre_comments  []Comment
	generic_types []Type
	attrs         []Attr
pub mut:
	methods             []FnDecl
	fields              []StructField
	embeds              []InterfaceEmbedding
	are_embeds_expanded bool
}

// `field1: val1`
pub struct StructInitField {
pub:
	pos              token.Pos
	name_pos         token.Pos
	pre_comments     []Comment
	end_comments     []Comment
	next_comments    []Comment
	has_prev_newline bool
	has_break_line   bool
	is_embed         bool
pub mut:
	expr          Expr   // `val1`
	name          string // 'field1'
	typ           Type   // the type of this field
	expected_type Type
	parent_type   Type
}

// `s := Foo{
//    ...a
//    field1: 'hello'
// }`
@[minify]
pub struct StructInit {
pub:
	pos             token.Pos
	name_pos        token.Pos
	no_keys         bool // `Foo{val1, val2}`
	is_short_syntax bool // `foo(field1: val1, field2: val2)`
	is_anon         bool //  `x: struct{ foo: bar }`
pub mut:
	unresolved           bool
	pre_comments         []Comment
	typ_str              string // 'Foo'
	typ                  Type   // the type of this struct
	update_expr          Expr   // `a` in `...a`
	update_expr_type     Type
	update_expr_pos      token.Pos
	update_expr_comments []Comment
	is_update_embed      bool
	has_update_expr      bool // has `...a`
	init_fields          []StructInitField
	generic_types        []Type
	language             Language
}

pub enum StructInitKind {
	normal
	short_syntax
	anon
}

// import statement
pub struct Import {
pub:
	source_name string // The original name in the source, `import abc.def` -> 'abc.def', *no matter* how the module is resolved

	mod       string // the module name of the import
	alias     string // the `x` in `import xxx as x`
	pos       token.Pos
	mod_pos   token.Pos
	alias_pos token.Pos
	syms_pos  token.Pos
pub mut:
	syms          []ImportSymbol // the list of symbols in `import {symbol1, symbol2}`
	comments      []Comment
	next_comments []Comment
}

// import symbol,for import {symbol} syntax
pub struct ImportSymbol {
pub:
	pos  token.Pos
	name string
}

// anonymous function
pub struct AnonFn {
pub mut:
	decl           FnDecl
	inherited_vars []Param
	typ            Type            // the type of anonymous fn. Both .typ and .decl.name are auto generated
	has_gen        map[string]bool // a map of the names of all generic anon functions, generated from it
}

// function or method declaration
@[minify]
pub struct FnDecl {
pub:
	name                  string // 'math.bits.normalize'
	short_name            string // 'normalize'
	mod                   string // 'math.bits'
	is_deprecated         bool
	is_pub                bool
	is_c_variadic         bool
	is_c_extern           bool
	is_variadic           bool
	is_anon               bool
	is_noreturn           bool        // true, when @[noreturn] is used on a fn
	is_manualfree         bool        // true, when @[manualfree] is used on a fn
	is_main               bool        // true for `fn main()`
	is_test               bool        // true for `fn test_abcde() {}`, false for `fn test_abc(x int) {}`, or for fns that do not start with test_
	is_conditional        bool        // true for `@[if abc] fn abc(){}`
	is_exported           bool        // true for `@[export: 'exact_C_name']`
	is_keep_alive         bool        // passed memory must not be freed (by GC) before function returns
	is_unsafe             bool        // true, when @[unsafe] is used on a fn
	is_must_use           bool        // true, when @[must_use] is used on a fn. Calls to such functions, that ignore the return value, will cause warnings.
	is_markused           bool        // true, when an explicit `@[markused]` tag was put on a fn; `-skip-unused` will not remove that fn
	is_file_translated    bool        // true, when the file it resides in is `@[translated]`
	receiver              StructField // TODO: this is not a struct field
	receiver_pos          token.Pos   // `(u User)` in `fn (u User) name()` position
	is_method             bool
	is_static_type_method bool      // true for `fn Foo.bar() {}`
	static_type_pos       token.Pos // `Foo` in `fn Foo.bar() {}`
	method_type_pos       token.Pos // `User` in ` fn (u User)` position
	method_idx            int
	rec_mut               bool // is receiver mutable
	has_prev_newline      bool
	has_break_line        bool
	rec_share             ShareType
	language              Language // V, C, JS
	file_mode             Language // whether *the file*, where a function was a '.c.v', '.js.v' etc.
	no_body               bool     // just a definition `fn C.malloc()`
	is_builtin            bool     // this function is defined in builtin/strconv
	name_pos              token.Pos
	body_pos              token.Pos // function bodys position
	file                  string
	generic_names         []string
	is_direct_arr         bool // direct array access
	attrs                 []Attr
	ctdefine_idx          int = -1 // the index in fn.attrs of `[if xyz]`, when such attribute exists
pub mut:
	idx               int // index in an external container; can be used to refer to the function in a more efficient way, just by its integer index
	params            []Param
	stmts             []Stmt
	defer_stmts       []DeferStmt
	trace_fns         map[string]FnTrace
	return_type       Type
	return_type_pos   token.Pos // `string` in `fn (u User) name() string` position
	has_return        bool
	should_be_skipped bool // true, when -skip-unused could not find any usages of that function, starting from main + other known used functions
	ninstances        int  // 0 for generic functions with no concrete instances
	has_await         bool // 'true' if this function uses JS.await

	comments      []Comment // comments *after* the header, but *before* `{`; used for InterfaceDecl
	end_comments  []Comment // comments *after* header declarations. E.g.: `fn C.C_func(x int) int // Comment`
	next_comments []Comment // comments that are one line after the decl; used for InterfaceDecl

	source_file &File  = unsafe { nil }
	scope       &Scope = unsafe { nil }
	label_names []string
	pos         token.Pos // function declaration position
	end_pos     token.Pos // end position
	//
	is_expand_simple_interpolation bool // true, when @[expand_simple_interpolation] is used on a fn. It should have a single string argument.
}

pub fn (f &FnDecl) new_method_with_receiver_type(new_type_ Type) FnDecl {
	new_type := if f.params[0].typ.is_ptr() && !new_type_.is_ptr() {
		new_type_.ref()
	} else {
		new_type_
	}
	unsafe {
		mut new_method := f
		new_method.params = f.params.clone()
		for i in 1 .. new_method.params.len {
			if new_method.params[i].typ == new_method.params[0].typ {
				new_method.params[i].typ = new_type
			}
		}
		new_method.params[0].typ = new_type
		return *new_method
	}
}

@[minify]
pub struct FnTrace {
pub mut:
	name string
pub:
	file        string
	line        i64
	return_type Type
	func        &Fn = unsafe { nil }
	is_fn_var   bool
}

@[minify]
pub struct Fn {
pub:
	is_variadic           bool
	is_c_variadic         bool
	language              Language
	is_pub                bool
	is_ctor_new           bool // `@[use_new] fn JS.Array.prototype.constructor()`
	is_deprecated         bool // `@[deprecated] fn abc(){}`
	is_noreturn           bool // `@[noreturn] fn abc(){}`
	is_unsafe             bool // `@[unsafe] fn abc(){}`
	is_must_use           bool // `@[must_use] fn abc(){}`
	is_placeholder        bool
	is_main               bool // `fn main(){}`
	is_test               bool // `fn test_abc(){}`
	is_keep_alive         bool // passed memory must not be freed (by GC) before function returns
	is_method             bool // true for `fn (x T) name()`, and for interface declarations (which are also for methods)
	is_static_type_method bool // true for `fn Foo.bar() {}`
	no_body               bool // a pure declaration like `fn abc(x int)`; used in .vh files, C./JS. fns.
	is_file_translated    bool // true, when the file it resides in is `@[translated]`
	mod                   string
	file                  string
	file_mode             Language
	pos                   token.Pos
	name_pos              token.Pos
	return_type_pos       token.Pos
pub mut:
	return_type        Type
	receiver_type      Type // != 0, when .is_method == true
	name               string
	params             []Param
	source_fn          voidptr // set in the checker, while processing fn declarations // TODO: get rid of voidptr
	usages             int
	generic_names      []string
	dep_names          []string // globals or consts dependent names
	attrs              []Attr   // all fn attributes
	is_conditional     bool     // true for `[if abc]fn(){}`
	ctdefine_idx       int      // the index of the attribute, containing the compile time define [if mytag]
	from_embedded_type Type     // for interface only, fn from the embedded interface
	//
	is_expand_simple_interpolation bool // for tagging b.f(s string), which is then called with `b.f('some $x $y')`,
	// when that call, should be expanded to `b.f('some '); b.f(x); b.f(' '); b.f(y);`
	// Note: the same type, has to support also a .write_decimal(n i64) method.
}

fn (f &Fn) method_equals(o &Fn) bool {
	return f.params[1..].equals(o.params[1..]) && f.return_type == o.return_type
		&& f.is_variadic == o.is_variadic && f.language == o.language
		&& f.generic_names == o.generic_names && f.is_pub == o.is_pub && f.mod == o.mod
		&& f.name == o.name
}

@[minify]
pub struct Param {
pub:
	pos        token.Pos
	name       string
	is_mut     bool
	is_shared  bool
	is_atomic  bool
	type_pos   token.Pos
	is_hidden  bool // interface first arg
	on_newline bool // whether the argument starts on a new line
pub mut:
	typ Type
}

pub fn (p &Param) specifier() string {
	return match true {
		p.is_shared { 'shared' }
		p.is_atomic { 'atomic' }
		p.is_mut { 'mut' }
		else { '' }
	}
}

pub fn (f &Fn) new_method_with_receiver_type(new_type_ Type) Fn {
	new_type := if f.params[0].typ.is_ptr() && !new_type_.is_ptr() {
		new_type_.ref()
	} else {
		new_type_
	}
	unsafe {
		mut new_method := f
		new_method.params = f.params.clone()
		for i in 1 .. new_method.params.len {
			if new_method.params[i].typ == new_method.params[0].typ {
				new_method.params[i].typ = new_type
			}
		}
		new_method.from_embedded_type = if f.from_embedded_type != 0 {
			f.from_embedded_type
		} else {
			f.params[0].typ
		}
		new_method.params[0].typ = new_type

		return *new_method
	}
}

fn (p &Param) equals(o &Param) bool {
	return p.name == o.name && p.is_mut == o.is_mut && p.typ == o.typ && p.is_hidden == o.is_hidden
}

fn (p []Param) equals(o []Param) bool {
	if p.len != o.len {
		return false
	}
	for i in 0 .. p.len {
		if !p[i].equals(o[i]) {
			return false
		}
	}
	return true
}

// break, continue
@[minify]
pub struct BranchStmt {
pub:
	kind  token.Kind
	label string
	pos   token.Pos
}

// function or method call expr
@[minify]
pub struct CallExpr {
pub:
	pos      token.Pos
	name_pos token.Pos
	mod      string
pub mut:
	name                   string // left.name()
	is_method              bool
	is_field               bool // temp hack, remove ASAP when re-impl CallExpr / Selector (joe)
	is_fn_var              bool // fn variable, `a := fn() {}`, then: `a()`
	is_fn_a_const          bool // fn const, `const c = abc`, where `abc` is a function, then: `c()`
	is_keep_alive          bool // GC must not free arguments before fn returns
	is_noreturn            bool // whether the function/method is marked as [noreturn]
	is_ctor_new            bool // if JS ctor calls requires `new` before call, marked as `[use_new]` in V
	is_file_translated     bool // true, when the file it resides in is `@[translated]`
	is_static_method       bool // it is a static method call
	args                   []CallArg
	expected_arg_types     []Type
	comptime_ret_val       bool
	language               Language
	or_block               OrExpr
	left                   Expr // `user` in `user.register()`
	left_type              Type // type of `user`
	receiver_type          Type // User / T, if receiver is generic, then cgen requires receiver_type to be T
	receiver_concrete_type Type // if receiver_type is T, then receiver_concrete_type is concrete type, otherwise it is the same as receiver_type
	return_type            Type
	return_type_generic    Type // the original generic return type from fn def
	nr_ret_values          int = -1 // amount of return values
	fn_var_type            Type   // the fn type, when `is_fn_a_const` or `is_fn_var` is true
	const_name             string // the fully qualified name of the const, i.e. `main.c`, given `const c = abc`, and callexpr: `c()`
	should_be_skipped      bool   // true for calls to `[if someflag?]` functions, when there is no `-d someflag`
	concrete_types         []Type // concrete types, e.g. <int, string>
	concrete_list_pos      token.Pos
	raw_concrete_types     []Type
	free_receiver          bool // true if the receiver expression needs to be freed
	scope                  &Scope = unsafe { nil }
	from_embed_types       []Type // holds the type of the embed that the method is called from
	comments               []Comment
	is_return_used         bool // return value is used for another expr
	//
	is_expand_simple_interpolation bool // true, when the function/method is marked as @[expand_simple_interpolation]
	is_unwrapped_fn_selector       bool // true, when the call is from an unwrapped selector (e.g. if t.foo != none { t.foo() })
	// Calls to it with an interpolation argument like `b.f('x ${y}')`, will be converted to `b.f('x ')` followed by `b.f(y)`.
	// The same type, has to support also a .write_decimal(n i64) method.
}

/*
pub struct AutofreeArgVar {
	name string
	idx  int
}
*/

// function call argument: `f(callarg)`
@[minify]
pub struct CallArg {
pub:
	is_mut   bool
	share    ShareType
	comments []Comment
pub mut:
	expr            Expr
	typ             Type
	is_tmp_autofree bool // this tells cgen that a tmp variable has to be used for the arg expression in order to free it after the call
	pos             token.Pos
	should_be_ptr   bool // fn expects a ptr for this arg
	// tmp_name        string // for autofree
	ct_expr bool // true, when the expression is a comptime/generic expression
}

// function return statement
pub struct Return {
pub:
	pos      token.Pos
	comments []Comment
pub mut:
	exprs []Expr
	types []Type
}

pub enum ComptimeVarKind {
	no_comptime   // it is not a comptime var
	key_var       // map key from `for k,v in t.$(field.name)`
	value_var     // map value from `for k,v in t.$(field.name)`
	field_var     // comptime field var `a := t.$(field.name)`
	generic_param // generic fn parameter
	generic_var   // generic var
	smartcast     // smart cast when used in `is v` (when `v` is from $for .variants)
	aggregate     // aggregate var
}

@[minify]
pub struct Var {
pub:
	name            string
	share           ShareType
	is_mut          bool
	is_static       bool
	is_volatile     bool
	is_autofree_tmp bool
	is_inherited    bool
	has_inherited   bool
pub mut:
	is_arg        bool // fn args should not be autofreed
	is_auto_deref bool
	is_unwrapped  bool // ct type smartcast unwrapped
	is_index_var  bool // index loop var
	expr          Expr
	typ           Type
	orig_type     Type   // original sumtype type; 0 if it's not a sumtype
	smartcasts    []Type // nested sum types require nested smart casting, for that a list of types is needed
	// TODO: move this to a real docs site later
	// 10 <- original type (orig_type)
	//   [11, 12, 13] <- cast order (smartcasts)
	//        12 <- the current casted type (typ)
	pos               token.Pos
	is_used           bool            // whether the local variable was used in other expressions
	is_changed        bool            // to detect mutable vars that are never changed
	ct_type_var       ComptimeVarKind // comptime variable type
	ct_type_unwrapped bool            // true when the comptime variable gets unwrapped
	// (for setting the position after the or block for autofree)
	is_or        bool // `x := foo() or { ... }`
	is_tmp       bool // for tmp for loop vars, so that autofree can skip them
	is_auto_heap bool // value whose address goes out of scope
	is_stack_obj bool // may be pointer to stack value (`mut` or `&` arg and not @[heap] struct)
}

// used for smartcasting only
// struct fields change type in scopes
@[minify]
pub struct ScopeStructField {
pub:
	struct_type Type // type of struct
	name        string
	pos         token.Pos
	typ         Type
	orig_type   Type   // original sumtype type; 0 if it's not a sumtype
	smartcasts  []Type // nested sum types require nested smart casting, for that a list of types is needed
	// TODO: move this to a real docs site later
	// 10 <- original type (orig_type)
	//   [11, 12, 13] <- cast order (smartcasts)
	//        12 <- the current casted type (typ)
}

@[minify]
pub struct GlobalField {
pub:
	name        string
	has_expr    bool
	pos         token.Pos
	typ_pos     token.Pos
	is_markused bool // an explicit `@[markused]` tag; the global will NOT be removed by `-skip-unused`
	is_volatile bool
	is_exported bool // an explicit `@[export]` tag; the global will NOT be removed by `-skip-unused`
pub mut:
	expr     Expr
	typ      Type
	comments []Comment
}

pub struct GlobalDecl {
pub:
	mod      string
	pos      token.Pos
	is_block bool   // __global() block
	attrs    []Attr // tags like `@[markused]`, valid for all the globals in the list
pub mut:
	fields       []GlobalField
	end_comments []Comment
}

@[minify]
pub struct EmbeddedFile {
pub:
	compression_type string
pub mut:
	rpath string // used in the source code, as an ID/key to the embed
	apath string // absolute path during compilation to the resource
	// these are set by gen_embed_file_init in v/gen/c/embed
	is_compressed bool
	bytes         []u8
	len           int
}

// Each V source file is represented by one File structure.
// When the V compiler runs, the parser will fill an []File.
// That array is then passed to V's checker.
@[heap]
pub struct File {
pub:
	nr_lines      int    // number of source code lines in the file (including newlines and comments)
	nr_bytes      int    // number of processed source code bytes
	nr_tokens     int    // number of processed tokens in the source code of the file
	mod           Module // the module of the source file (from `module xyz` at the top)
	global_scope  &Scope = unsafe { nil }
	is_test       bool // true for _test.v files
	is_generated  bool // true for `@[generated] module xyz` files; turn off notices
	is_translated bool // true for `@[translated] module xyz` files; turn off some checks
	language      Language
pub mut:
	idx              int    // index in an external container; can be used to refer to the file in a more efficient way, just by its integer index
	path             string // absolute path of the source file - '/projects/v/file.v'
	path_base        string // file name - 'file.v' (useful for tracing)
	scope            &Scope = unsafe { nil }
	stmts            []Stmt            // all the statements in the source file
	imports          []Import          // all the imports
	auto_imports     []string          // imports that were implicitly added
	embedded_files   []EmbeddedFile    // list of files to embed in the binary
	imported_symbols map[string]string // used for `import {symbol}`, it maps symbol => module.symbol
	errors           []errors.Error    // all the checker errors in the file
	warnings         []errors.Warning  // all the checker warnings in the file
	notices          []errors.Notice   // all the checker notices in the file
	generic_fns      []&FnDecl
	global_labels    []string // from `asm { .globl labelname }`
	template_paths   []string // all the .html/.md files that were processed with $tmpl
	unique_prefix    string   // a hash of the `.path` field, used for making anon fn generation unique
	//
	is_parse_text    bool // true for files, produced by parse_text
	is_template_text bool // true for files, produced by parse_comptime
}

@[unsafe]
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
@[minify]
pub struct IdentVar {
pub mut:
	typ         Type
	is_mut      bool
	is_static   bool
	is_volatile bool
	is_option   bool
	share       ShareType
}

pub type IdentInfo = IdentFn | IdentVar

pub enum IdentKind {
	unresolved
	blank_ident // discard identifier, `_` in `_ := 1`
	variable
	constant
	global
	function
}

// A single identifier
@[minify]
pub struct Ident {
pub:
	language Language
	tok_kind token.Kind
	pos      token.Pos
	mut_pos  token.Pos
	comptime bool
pub mut:
	scope          &Scope      = unsafe { nil }
	obj            ScopeObject = empty_scope_object
	mod            string
	name           string
	full_name      string
	cached_name    string
	kind           IdentKind
	info           IdentInfo
	is_mut         bool // if mut *token* is before name. Use `is_mut()` to lookup mut variable
	or_expr        OrExpr
	concrete_types []Type
	ct_expr        bool // is it a comptime expr?
}

// full_name returns the name of the ident, prefixed with the module name
pub fn (mut i Ident) full_name() string {
	if i.full_name != '' {
		return i.full_name
	}
	if i.name.contains('.') {
		i.full_name = i.name
	} else {
		i.full_name = i.mod + '.' + i.name
	}
	return i.full_name
}

@[inline]
pub fn (i &Ident) is_auto_heap() bool {
	return match i.obj {
		Var { i.obj.is_auto_heap }
		else { false }
	}
}

@[inline]
pub fn (i &Ident) is_mut() bool {
	match i.obj {
		Var { return i.obj.is_mut }
		ConstField, EmptyScopeObject { return false }
		AsmRegister, GlobalField { return true }
	}
}

pub fn (i &Ident) var_info() IdentVar {
	match i.info {
		IdentVar { return i.info }
		else { panic('Ident.var_info(): info is not IdentVar variant') }
	}
}

// left op right
// See: token.Kind.is_infix
@[minify]
pub struct InfixExpr {
pub:
	op      token.Kind
	pos     token.Pos
	is_stmt bool
pub mut:
	left          Expr
	right         Expr
	left_type     Type
	right_type    Type
	promoted_type Type = void_type
	auto_locked   string
	or_block      OrExpr

	ct_left_value_evaled  bool
	ct_left_value         ComptTimeConstValue = empty_comptime_const_value
	left_ct_expr          bool // true when left is comptime/generic expr
	ct_right_value_evaled bool
	ct_right_value        ComptTimeConstValue = empty_comptime_const_value
	right_ct_expr         bool // true when right is comptime/generic expr

	before_op_comments []Comment
	after_op_comments  []Comment
}

// ++, --
pub struct PostfixExpr {
pub:
	op            token.Kind
	pos           token.Pos
	is_c2v_prefix bool // for `--x` (`x--$`), only for translated code until c2v can handle it
pub mut:
	expr        Expr
	typ         Type
	auto_locked string
}

// See: token.Kind.is_prefix
@[minify]
pub struct PrefixExpr {
pub:
	op  token.Kind
	pos token.Pos
pub mut:
	right_type Type
	right      Expr
	or_block   OrExpr
	is_option  bool // IfGuard
}

@[minify]
pub struct IndexExpr {
pub:
	pos token.Pos
pub mut:
	index     Expr // [0], RangeExpr [start..end] or map[key]
	or_expr   OrExpr
	left      Expr
	left_type Type // array, map, fixed array
	is_setter bool
	is_map    bool
	is_array  bool
	is_farray bool // fixed array
	is_option bool // IfGuard
	is_direct bool // Set if the underlying memory can be safely accessed
	is_gated  bool // #[] gated array
}

@[minify]
pub struct IfExpr {
pub:
	is_comptime   bool
	tok_kind      token.Kind
	pos           token.Pos
	post_comments []Comment
pub mut:
	left     Expr       // `a` in `a := if ...`
	branches []IfBranch // includes all `else if` branches
	is_expr  bool
	typ      Type
	has_else bool
	// implements bool // comptime $if implements interface
}

pub struct IfBranch {
pub:
	pos      token.Pos
	body_pos token.Pos
	comments []Comment
pub mut:
	cond      Expr
	pkg_exist bool
	stmts     []Stmt
	scope     &Scope = unsafe { nil }
}

pub struct UnsafeExpr {
pub:
	pos token.Pos
pub mut:
	expr Expr
}

pub struct LockExpr {
pub:
	is_rlock []bool
	pos      token.Pos
pub mut:
	stmts    []Stmt
	lockeds  []Expr // `x`, `y.z` in `lock x, y.z {`
	comments []Comment
	is_expr  bool
	typ      Type
	scope    &Scope = unsafe { nil }
}

@[minify]
pub struct MatchExpr {
pub:
	tok_kind token.Kind
	pos      token.Pos
	comments []Comment // comments before the first branch
pub mut:
	cond          Expr
	branches      []MatchBranch
	is_expr       bool // returns a value
	return_type   Type
	cond_type     Type // type of `x` in `match x {`
	expected_type Type // for debugging only
	is_sum_type   bool
}

pub struct MatchBranch {
pub:
	ecmnts        [][]Comment // inline comments for each left side expr
	pos           token.Pos
	is_else       bool
	post_comments []Comment // comments below ´... }´
	branch_pos    token.Pos // for checker errors about invalid branches
pub mut:
	stmts []Stmt // right side
	exprs []Expr // left side
	scope &Scope = unsafe { nil }
}

pub struct SelectExpr {
pub:
	branches      []SelectBranch
	pos           token.Pos
	has_exception bool
pub mut:
	is_expr       bool // returns a value
	expected_type Type // for debugging only
}

@[minify]
pub struct SelectBranch {
pub:
	pos           token.Pos
	comment       Comment // comment above `select {`
	is_else       bool
	is_timeout    bool
	post_comments []Comment
pub mut:
	stmt  Stmt   // `a := <-ch` or `ch <- a`
	stmts []Stmt // right side
}

pub enum ComptimeForKind {
	methods
	fields
	attributes
	values
	variants
	params
}

pub struct ComptimeFor {
pub:
	val_var string
	kind    ComptimeForKind
	pos     token.Pos
	typ_pos token.Pos
pub mut:
	stmts []Stmt
	typ   Type
	expr  Expr
}

pub struct ForStmt {
pub:
	is_inf   bool // `for {}`
	pos      token.Pos
	comments []Comment
pub mut:
	cond  Expr
	stmts []Stmt
	label string // `label: for {`
	scope &Scope = unsafe { nil }
}

@[minify]
pub struct ForInStmt {
pub:
	key_var    string
	val_var    string
	is_range   bool
	pos        token.Pos
	kv_pos     token.Pos
	vv_pos     token.Pos
	comments   []Comment
	val_is_mut bool // `for mut val in vals {` means that modifying `val` will modify the array
	// and the array cannot be indexed inside the loop
pub mut:
	val_is_ref bool // `for val in &arr {` means that value of `val` will be the reference of the value in `arr`
	cond       Expr
	key_type   Type
	val_type   Type
	cond_type  Type
	high       Expr // `10` in `for i in 0..10 {`
	high_type  Type
	kind       Kind   // array/map/string
	label      string // `label: for {`
	scope      &Scope = unsafe { nil }
	stmts      []Stmt
}

pub struct ForCStmt {
pub:
	has_init bool
	has_cond bool
	has_inc  bool
	is_multi bool // for a,b := 0,1; a < 10; a,b = a+b, a {...}
	pos      token.Pos
	comments []Comment
pub mut:
	init  Stmt // i := 0;
	cond  Expr // i < 10;
	inc   Stmt // i++; i += 2
	stmts []Stmt
	label string // `label: for {`
	scope &Scope = unsafe { nil }
}

// #include, #define etc
pub struct HashStmt {
pub:
	mod         string
	pos         token.Pos
	source_file string
	is_use_once bool // true for @[use_once]
pub mut:
	val      string // example: 'include <openssl/rand.h> # please install openssl // comment'
	kind     string // : 'include'
	main     string // : '<openssl/rand.h>'
	msg      string // : 'please install openssl'
	ct_conds []Expr // *all* comptime conditions, that must be true, for the hash to be processed
	// ct_conds is filled by the checker, based on the current nesting of `$if cond1 {}` blocks
	attrs []Attr
}

// variable assign statement
@[minify]
pub struct AssignStmt {
pub:
	op           token.Kind // include: =,:=,+=,-=,*=,/= and so on; for a list of all the assign operators, see vlib/token/token.v
	pos          token.Pos
	end_comments []Comment
pub mut:
	right         []Expr
	left          []Expr
	left_types    []Type
	right_types   []Type
	is_static     bool // for translated code only
	is_volatile   bool // for disabling variable access optimisations (needed for hardware drivers)
	is_simple     bool // `x+=2` in `for x:=1; ; x+=2`
	has_cross_var bool
	attr          Attr
}

// `expr as Ident`
pub struct AsCast {
pub:
	typ Type // to type
	pos token.Pos
pub mut:
	expr      Expr // from expr: `expr` in `expr as Ident`
	expr_type Type // from type
}

// An enum value, like OS.macos or .macos
pub struct EnumVal {
pub:
	enum_name string
	val       string
	mod       string // for full path `mod_Enum_val`
	pos       token.Pos
pub mut:
	typ Type
}

// enum field in enum declaration
pub struct EnumField {
pub:
	name             string // just `lock`, or `abc`, etc, no matter if the name is a keyword or not.
	source_name      string // The name in the source, for example `lock`, and `abc`. Note that `lock` is a keyword in V.
	pos              token.Pos
	pre_comments     []Comment // comment before Enumfield
	comments         []Comment // comment after Enumfield in the same line
	next_comments    []Comment // comments between current EnumField and next EnumField
	has_expr         bool      // true, when .expr has a value
	has_prev_newline bool      // empty newline before Enumfield
	has_break_line   bool
	attrs            []Attr
pub mut:
	expr Expr // the value of current EnumField; 123 in `ename = 123`
}

// enum declaration
@[minify]
pub struct EnumDecl {
pub:
	name             string
	is_pub           bool
	is_flag          bool        // true when the enum has @[flag] tag,for bit field enum
	is_multi_allowed bool        // true when the enum has [_allow_multiple_values] tag
	comments         []Comment   // comments before the first EnumField
	fields           []EnumField // all the enum fields
	attrs            []Attr      // attributes of enum declaration
	typ              Type        // the default is `int`; can be changed by `enum Big as u64 { a = 5 }`
	typ_pos          token.Pos
	pos              token.Pos
}

pub struct AliasTypeDecl {
pub:
	name     string
	is_pub   bool
	typ      Type
	pos      token.Pos
	type_pos token.Pos
	comments []Comment
pub mut:
	parent_type Type
	is_markused bool
}

// SumTypeDecl is the ast node for `type MySumType = string | int`
pub struct SumTypeDecl {
pub:
	name          string
	is_pub        bool
	pos           token.Pos
	name_pos      token.Pos
	typ           Type
	generic_types []Type
	attrs         []Attr // attributes of type declaration
pub mut:
	variants    []TypeNode
	is_markused bool
}

pub struct FnTypeDecl {
pub:
	name          string
	is_pub        bool
	typ           Type
	pos           token.Pos
	type_pos      token.Pos
	comments      []Comment
	generic_types []Type
	attrs         []Attr // attributes of type declaration
	is_markused   bool
}

// TODO: handle this differently
// v1 excludes non current os ifdefs so
// the defer's never get added in the first place
@[minify]
pub struct DeferStmt {
pub:
	pos token.Pos
pub mut:
	stmts      []Stmt
	defer_vars []Ident
	ifdef      string
	idx_in_fn  int = -1 // index in FnDecl.defer_stmts
}

// `(3+4)`
pub struct ParExpr {
pub:
	pos token.Pos
pub mut:
	expr     Expr
	comments []Comment
}

@[minify]
pub struct GoExpr {
pub:
	pos token.Pos
pub mut:
	call_expr CallExpr
	is_expr   bool
}

@[minify]
pub struct SpawnExpr {
pub:
	pos token.Pos
pub mut:
	call_expr CallExpr
	is_expr   bool
}

pub struct GotoLabel {
pub:
	name string
	pos  token.Pos
pub mut:
	is_used bool
}

pub struct GotoStmt {
pub:
	name string
	pos  token.Pos
}

@[minify]
pub struct ArrayInit {
pub:
	pos           token.Pos   // `[]` in []Type{} position
	elem_type_pos token.Pos   // `Type` in []Type{} position
	ecmnts        [][]Comment // optional iembed comments after each expr
	pre_cmnts     []Comment
	is_fixed      bool
	is_option     bool // true if it was declared as ?[2]Type or ?[]Type
	has_val       bool // fixed size literal `[expr, expr]!`
	mod           string
	has_len       bool
	has_cap       bool
	has_init      bool
	has_index     bool // true if temp variable index is used
pub mut:
	exprs        []Expr // `[expr, expr]` or `[expr]Type{}` for fixed array
	len_expr     Expr   // len: expr
	cap_expr     Expr   // cap: expr
	init_expr    Expr   // init: expr
	expr_types   []Type // [Dog, Cat] // also used for interface_types
	elem_type    Type   // element type
	init_type    Type   // init: value type
	typ          Type   // array type
	alias_type   Type   // alias type
	has_callexpr bool   // has expr which needs tmp var to initialize it
}

pub struct ArrayDecompose {
pub:
	pos token.Pos
pub mut:
	expr      Expr
	expr_type Type
	arg_type  Type
}

pub struct ChanInit {
pub:
	pos           token.Pos
	elem_type_pos token.Pos
	has_cap       bool
pub mut:
	cap_expr  Expr
	typ       Type
	elem_type Type
}

@[minify]
pub struct MapInit {
pub:
	pos       token.Pos
	comments  [][]Comment // comments after key-value pairs
	pre_cmnts []Comment   // comments before the first key-value pair
pub mut:
	keys                 []Expr
	vals                 []Expr
	val_types            []Type
	typ                  Type
	key_type             Type
	value_type           Type
	has_update_expr      bool // has `...a`
	update_expr          Expr // `a` in `...a`
	update_expr_pos      token.Pos
	update_expr_comments []Comment
}

// s[10..20]
@[minify]
pub struct RangeExpr {
pub:
	has_high bool
	has_low  bool
	pos      token.Pos
	is_gated bool // #[] gated array
pub mut:
	low  Expr
	high Expr
	typ  Type // filled in by checker; the type of `0...1` is `int` for example, while `a`...`z` is `rune` etc
}

@[minify]
pub struct CastExpr {
pub mut:
	arg       Expr   // `n` in `string(buf, n)`
	typ       Type   // `string`
	expr      Expr   // `buf` in `string(buf, n)` and `&Type(buf)`
	typname   string // `&Type` in `&Type(buf)`
	expr_type Type   // `byteptr`, the type of the `buf` expression
	has_arg   bool   // true for `string(buf, n)`, false for `&Type(buf)`
	pos       token.Pos
}

@[minify]
pub struct AsmStmt {
pub:
	arch        pref.Arch
	is_basic    bool
	is_volatile bool
	is_goto     bool
	clobbered   []AsmClobbered
	pos         token.Pos
pub mut:
	templates     []AsmTemplate
	scope         &Scope = unsafe { nil }
	output        []AsmIO
	input         []AsmIO
	global_labels []string // labels defined in assembly block, exported with `.globl`
	local_labels  []string // local to the assembly block
}

@[minify]
pub struct AsmTemplate {
pub mut:
	name         string
	is_label     bool // `example_label:`
	is_directive bool // .globl assembly_function
	args         []AsmArg
	comments     []Comment
	pos          token.Pos
}

// [eax+5] | j | displacement literal (e.g. 123 in [rax + 123] ) | eax | true | `a` | 0.594 | 123 | label_name
pub type AsmArg = AsmAddressing
	| AsmAlias
	| AsmDisp
	| AsmRegister
	| BoolLiteral
	| CharLiteral
	| FloatLiteral
	| IntegerLiteral
	| string

pub struct AsmRegister {
pub mut:
	name string // eax or r12d etc.
	typ  Type
	size int
}

pub struct AsmDisp {
pub:
	val string
	pos token.Pos
}

pub struct AsmAlias {
pub:
	pos token.Pos
pub mut:
	name string // a
}

pub struct AsmAddressing {
pub:
	scale int = -1 // 1, 2, 4, or 8 literal
	mode  AddressingMode
	pos   token.Pos
pub mut:
	segment      string // fs:
	displacement AsmArg // 8, 16 or 32 bit literal value
	base         AsmArg // gpr
	index        AsmArg // gpr
}

// addressing modes:
pub enum AddressingMode {
	invalid
	displacement                                  // displacement
	base                                          // base
	base_plus_displacement                        // base + displacement
	index_times_scale_plus_displacement           // (index ∗ scale) + displacement
	base_plus_index_plus_displacement             // base + (index ∗ scale) + displacement
	base_plus_index_times_scale_plus_displacement // base + index + displacement
	rip_plus_displacement                         // rip + displacement
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
	constraint string    // '=r' TODO: allow all backends to easily use this with a struct
	comments   []Comment // // this is a comment
	typ        Type
	pos        token.Pos
pub mut:
	expr Expr // (a)
}

// reference: https://en.wikipedia.org/wiki/X86#/media/File:Table_of_x86_Registers_svg.svg
// map register size -> register name
pub const x86_no_number_register_list = {
	8:  ['al', 'ah', 'bl', 'bh', 'cl', 'ch', 'dl', 'dh', 'bpl', 'sil', 'dil', 'spl']
	16: ['ax', 'bx', 'cx', 'dx', 'bp', 'si', 'di', 'sp', // segment registers
	 	'cs', 'ss', 'ds', 'es', 'fs', 'gs', 'flags', 'ip', // task registers
	 	'gdtr', 'idtr', 'tr', 'ldtr', // CSR register 'msw', /* FP core registers */ 'cw', 'sw', 'tw', 'fp_ip', 'fp_dp', 'fp_cs',
	 	'fp_ds', 'fp_opc']
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
		'eip', // CSR register
		'mxcsr', // 32-bit FP core registers 'fp_dp', 'fp_ip' (TODO: why are there duplicates?)
	]
	64: ['rax', 'rbx', 'rcx', 'rdx', 'rbp', 'rsi', 'rdi', 'rsp', 'rflags', 'rip']
}
// no comments because maps do not support comments
// r#*: gp registers added in 64-bit extensions, can only be from 8-15 actually
// *mm#: vector/simd registers
// st#: floating point numbers
// cr#: control/status registers
// dr#: debug registers
pub const x86_with_number_register_list = {
	8:   {
		'r#b': 16
	}
	16:  {
		'r#w': 16
	}
	32:  {
		'r#d': 16
	}
	64:  {
		'r#':  16
		'mm#': 16
		'cr#': 16
		'dr#': 16
	}
	80:  {
		'st#': 16
	}
	128: {
		'xmm#': 32
	}
	256: {
		'ymm#': 32
	}
	512: {
		'zmm#': 32
	}
}

// TODO: saved priviled registers for arm
pub const arm_no_number_register_list = ['fp', // aka r11
 'ip', // not instruction pointer: aka r12
 'sp', // aka r13
 'lr', // aka r14
 'pc', // this is instruction pointer ('program counter'): aka r15
] // 'cpsr' and 'apsr' are special flags registers, but cannot be referred to directly

pub const arm_with_number_register_list = {
	'r#': 16
}

pub const riscv_no_number_register_list = ['zero', 'ra', 'sp', 'gp', 'tp']
pub const riscv_with_number_register_list = {
	'x#': 32
	't#': 3
	's#': 12
	'a#': 8
}

pub const s390x_no_number_register_list = []string{}
pub const s390x_with_number_register_list = {
	'f#': 16
	'r#': 16
	'v#': 32
}

pub const ppc64le_no_number_register_list = []string{}
pub const ppc64le_with_number_register_list = {
	'f#': 32
	'r#': 32
}

pub const loongarch64_no_number_register_list = []string{}
pub const loongarch64_with_number_register_list = {
	'f#': 32
	'r#': 32
}

pub struct DebuggerStmt {
pub:
	pos token.Pos
}

// `assert a == 0, 'a is zero'`
@[minify]
pub struct AssertStmt {
pub:
	pos       token.Pos
	extra_pos token.Pos
pub mut:
	expr    Expr // `a == 0`
	extra   Expr // `'a is zero'`
	is_used bool // asserts are used in _test.v files, as well as in non -prod builds of all files
}

pub struct IfGuardVar {
pub mut:
	name   string
	is_mut bool
	pos    token.Pos
}

// `if x := opt() {`
pub struct IfGuardExpr {
pub:
	vars []IfGuardVar
pub mut:
	expr      Expr // `opt()`
	expr_type Type // type of `opt()`
}

pub enum OrKind {
	absent
	block
	propagate_option
	propagate_result
}

// `or { ... }`
pub struct OrExpr {
pub:
	kind OrKind
	pos  token.Pos
pub mut:
	stmts []Stmt
}

/*
// `or { ... }`
pub struct OrExpr2 {
pub:
	call_expr CallExpr
	stmts     []Stmt // inside `or { }`
	kind      OrKind
	pos       token.Pos
}
*/

// deprecated
@[minify]
pub struct Assoc {
pub:
	var_name string
	fields   []string
	pos      token.Pos
pub mut:
	exprs []Expr
	typ   Type
	scope &Scope = unsafe { nil }
}

pub struct SizeOf {
pub:
	guessed_type bool // a legacy `sizeof( GuessedType )` => a deprecation notice, suggesting `v fmt -w .` => `sizeof[ Type ]()`
	is_type      bool
	pos          token.Pos
pub mut:
	expr Expr // checker uses this to set typ, when !is_type
	typ  Type
}

pub struct IsRefType {
pub:
	guessed_type bool // a legacy `isreftype( GuessedType )` => a deprecation notice, suggesting `v fmt -w .` => `isreftype[ Type ]()`
	is_type      bool
	pos          token.Pos
pub mut:
	expr Expr // checker uses this to set typ, when !is_type
	typ  Type
}

@[minify]
pub struct OffsetOf {
pub:
	struct_type Type
	field       string
	pos         token.Pos
}

pub struct LambdaExpr {
pub:
	pos token.Pos
pub mut:
	params     []Ident
	pos_expr   token.Pos
	expr       Expr
	pos_end    token.Pos
	scope      &Scope  = unsafe { nil }
	func       &AnonFn = unsafe { nil }
	is_checked bool
	typ        Type
	call_ctx   &CallExpr = unsafe { nil }
}

pub struct Likely {
pub:
	pos       token.Pos
	is_likely bool // false for _unlikely_
pub mut:
	expr Expr
}

@[minify]
pub struct TypeOf {
pub:
	is_type bool
	pos     token.Pos
pub mut:
	expr Expr // checker uses this to set typ, when !is_type
	typ  Type
}

@[minify]
pub struct DumpExpr {
pub:
	pos token.Pos
pub mut:
	expr      Expr
	expr_type Type
	cname     string // filled in the checker
}

pub struct Comment {
pub:
	text     string
	is_multi bool // true only for /* comment */, that use many lines
	pos      token.Pos
}

pub struct ConcatExpr {
pub:
	vals []Expr
	pos  token.Pos
pub mut:
	return_type Type
}

// @FN, @STRUCT, @MOD etc. See full list in token.valid_at_tokens
pub struct AtExpr {
pub:
	name string
	pos  token.Pos
	kind token.AtKind
pub mut:
	val string
}

@[minify]
pub struct ComptimeSelector {
pub:
	has_parens bool // if $() is used, for vfmt
	pos        token.Pos
	or_block   OrExpr
pub mut:
	left       Expr
	left_type  Type
	field_expr Expr
	typ        Type
	is_name    bool   // true if f.$(field.name)
	typ_key    string // `f.typ` cached key for type resolver
}

@[minify]
pub struct ComptimeCall {
pub:
	pos              token.Pos
	has_parens       bool // if $() is used, for vfmt
	method_name      string
	method_pos       token.Pos
	scope            &Scope = unsafe { nil }
	is_vweb          bool
	is_veb           bool
	is_embed         bool // $embed_file(...)
	is_env           bool // $env(...) // TODO: deprecate after $d() is stable
	is_compile_value bool // $d(...)
	env_pos          token.Pos
	is_pkgconfig     bool
mut:
	is_d_resolved bool
pub mut:
	veb_tmpl      File
	left          Expr
	left_type     Type
	result_type   Type
	env_value     string
	compile_value string
	args_var      string
	args          []CallArg
	embed_file    EmbeddedFile
	or_block      OrExpr
}

// resolve_compile_value resolves the value and return type of `$d()` calls.
// The result is stored in fields `compile_value` and `result_type`.
// The argument `compile_values` is expected to be the `Preferences.compile_values` field.
pub fn (mut cc ComptimeCall) resolve_compile_value(compile_values map[string]string) ! {
	if cc.is_d_resolved {
		return
	}
	if !cc.is_compile_value {
		return error('ComptimeCall is not \$d()')
	}
	arg := cc.args[0] or {
		return error('\$d() takes two arguments, a string and a primitive literal')
	}
	if !arg.expr.is_pure_literal() {
		return error('\$d() values can only be pure literals')
	}
	typ := arg.expr.get_pure_type()
	arg_as_string := arg.str().trim('`"\'')
	value := compile_values[cc.args_var] or { arg_as_string }
	validate_type_string_is_pure_literal(typ, value) or { return error(err.msg()) }
	cc.compile_value = value
	cc.result_type = typ
	cc.is_d_resolved = true
}

// expr_str returns the string representation of `ComptimeCall` for use with
// `ast.Expr`'s `str()' method (used by e.g. vfmt).
pub fn (cc ComptimeCall) expr_str() string {
	mut str := 'ast.ComptimeCall'
	if cc.is_compile_value {
		arg := cc.args[0] or { return str }
		if arg.expr.is_pure_literal() {
			str = "\$${cc.method_name}('${cc.args_var}', ${arg})"
		}
	}
	return str
}

pub struct None {
pub:
	pos token.Pos
}

pub enum SqlStmtKind {
	insert
	update
	delete
	create
	drop
}

pub enum SqlExprKind {
	insert
	select_
}

pub struct SqlStmt {
pub:
	pos token.Pos
pub mut:
	lines        []SqlStmtLine
	db_expr      Expr // `db` in `sql db {`
	or_expr      OrExpr
	db_expr_type Type // the type of the `db` in `sql db {`
}

pub struct SqlStmtLine {
pub:
	kind SqlStmtKind
	pos  token.Pos
	// is_generated indicates a statement is generated by ORM for complex queries with related tables.
	is_generated bool
	scope        &Scope = unsafe { nil }
pub mut:
	object_var      string   // `user`
	updated_columns []string // for `update set x=y`
	table_expr      TypeNode
	fields          []StructField
	sub_structs     map[int]SqlStmtLine
	where_expr      Expr
	update_exprs    []Expr // for `update`
	pre_comments    []Comment
	end_comments    []Comment
}

pub struct SqlExpr {
pub:
	is_count     bool
	is_insert    bool // for insert expressions
	inserted_var string

	has_where  bool
	has_order  bool
	has_limit  bool
	has_offset bool
	has_desc   bool
	is_array   bool
	// is_generated indicates a statement is generated by ORM for complex queries with related tables.
	is_generated bool
	pos          token.Pos
pub mut:
	typ         Type
	db_expr     Expr // `db` in `sql db {`
	where_expr  Expr
	order_expr  Expr
	limit_expr  Expr
	offset_expr Expr
	table_expr  TypeNode
	fields      []StructField
	sub_structs map[int]SqlExpr
	or_expr     OrExpr
}

pub struct NodeError {
pub:
	idx int // index for referencing the related File error
	pos token.Pos
}

@[inline]
pub fn (expr Expr) is_blank_ident() bool {
	if expr is Ident {
		return expr.kind == .blank_ident
	}
	return false
}

@[inline]
pub fn (expr Expr) is_as_cast() bool {
	if expr is ParExpr {
		return expr.expr.is_as_cast()
	} else if expr is SelectorExpr {
		return expr.expr.is_as_cast()
	} else {
		return expr is AsCast
	}
}

__global nested_expr_pos_calls = i64(0)
// values above 14000 risk stack overflow by default on macos in Expr.pos() calls
const max_nested_expr_pos_calls = 5000

pub fn (expr Expr) pos() token.Pos {
	pos_calls := stdatomic.add_i64(&nested_expr_pos_calls, 1)
	if pos_calls > max_nested_expr_pos_calls {
		$if panic_on_deeply_nested_expr_pos_calls ? {
			eprintln('${@LOCATION}: too many nested Expr.pos() calls: ${pos_calls}, expr type: ${expr.type_name()}')
			exit(1)
		}
		return token.Pos{}
	}
	defer {
		stdatomic.sub_i64(&nested_expr_pos_calls, 1)
	}
	// all uncommented have to be implemented
	// Note: please do not print here. the language server will hang
	// as it uses STDIO primarily to communicate ~Ned
	return match expr {
		AnonFn {
			expr.decl.pos
		}
		CTempVar, EmptyExpr {
			// println('compiler bug, unhandled EmptyExpr pos()')
			token.Pos{}
		}
		NodeError, ArrayDecompose, ArrayInit, AsCast, Assoc, AtExpr, BoolLiteral, CallExpr,
		CastExpr, ChanInit, CharLiteral, ConcatExpr, Comment, ComptimeCall, ComptimeSelector,
		EnumVal, DumpExpr, FloatLiteral, GoExpr, SpawnExpr, Ident, IfExpr, IntegerLiteral,
		IsRefType, Likely, LockExpr, MapInit, MatchExpr, None, OffsetOf, OrExpr, ParExpr,
		PostfixExpr, PrefixExpr, RangeExpr, SelectExpr, SelectorExpr, SizeOf, SqlExpr,
		StringInterLiteral, StringLiteral, StructInit, TypeNode, TypeOf, UnsafeExpr, ComptimeType,
		LambdaExpr, Nil {
			expr.pos
		}
		IndexExpr {
			if expr.or_expr.kind != .absent {
				expr.or_expr.pos
			} else {
				expr.pos
			}
		}
		IfGuardExpr {
			expr.expr.pos()
		}
		InfixExpr {
			left_pos := expr.left.pos()
			right_pos := expr.right.pos()
			token.Pos{
				line_nr:   expr.pos.line_nr
				pos:       left_pos.pos
				len:       right_pos.pos - left_pos.pos + right_pos.len
				col:       left_pos.col
				last_line: right_pos.last_line
			}
		}
		// Please, do NOT use else{} here.
		// This match is exhaustive *on purpose*, to help force
		// maintaining/implementing proper .pos fields.
	}
}

pub fn (expr Expr) is_lvalue() bool {
	return match expr {
		Ident, CTempVar { true }
		IndexExpr { expr.left.is_lvalue() }
		SelectorExpr { expr.expr.is_lvalue() }
		ParExpr { expr.expr.is_lvalue() } // for var := &{...(*pointer_var)}
		PrefixExpr { expr.right.is_lvalue() }
		ComptimeSelector { expr.field_expr.is_lvalue() }
		else { false }
	}
}

pub fn (expr Expr) is_expr() bool {
	return match expr {
		IfExpr, LockExpr, MatchExpr, SelectExpr { expr.is_expr }
		else { true }
	}
}

pub fn (expr Expr) get_pure_type() Type {
	return match expr {
		BoolLiteral { bool_type }
		CharLiteral { char_type }
		FloatLiteral { f64_type }
		StringLiteral { string_type }
		IntegerLiteral { i64_type }
		else { void_type }
	}
}

pub fn (expr Expr) is_pure_literal() bool {
	return match expr {
		BoolLiteral, CharLiteral, FloatLiteral, StringLiteral, IntegerLiteral { true }
		else { false }
	}
}

pub fn (expr Expr) is_auto_deref_var() bool {
	return match expr {
		Ident {
			if expr.obj is Var {
				expr.obj.is_auto_deref
			} else {
				false
			}
		}
		PrefixExpr {
			expr.op == .amp && expr.right.is_auto_deref_var()
		}
		else {
			false
		}
	}
}

// returns if an expression can be used in `lock x, y.z {`
pub fn (e &Expr) is_lockable() bool {
	return match e {
		Ident { true }
		SelectorExpr { e.expr.is_lockable() }
		else { false }
	}
}

// returns if an expression has call expr`
pub fn (e &Expr) has_fn_call() bool {
	return match e {
		CallExpr { true }
		SelectorExpr { e.expr.has_fn_call() }
		else { false }
	}
}

// CTempVar is used in cgen only, to hold nodes for temporary variables
pub struct CTempVar {
pub:
	name   string // the name of the C temporary variable; used by g.expr(x)
	typ    Type   // the type of the original expression
	is_ptr bool   // whether the type is a pointer
pub mut:
	orig         Expr // the original expression, which produced the C temp variable; used by x.str()
	is_fixed_ret bool // it is an array fixed returned from call
}

pub fn (node Node) pos() token.Pos {
	match node {
		NodeError {
			return token.Pos{}
		}
		EmptyNode {
			return token.Pos{}
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
				return pos.extend(node.right.last().pos())
			}
			if node is AssertStmt {
				return pos.extend(node.expr.pos())
			}
			return pos
		}
		Expr {
			return node.pos()
		}
		StructField {
			return node.pos.extend(node.type_pos)
		}
		MatchBranch, SelectBranch, EnumField, ConstField, StructInitField, GlobalField, CallArg {
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
				EmptyScopeObject, AsmRegister {
					return token.Pos{
						len:       -1
						line_nr:   -1
						pos:       -1
						last_line: -1
						col:       -1
					}
				}
			}
		}
		File {
			mut pos := token.Pos{}
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
			LambdaExpr {
				for p in node.params {
					children << Node(Expr(p))
				}
				children << node.expr
			}
			LockExpr, OrExpr {
				return node.stmts.map(Node(it))
			}
			StructInit {
				return node.init_fields.map(Node(it))
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
			Block, DeferStmt, ForCStmt, ForInStmt, ForStmt, ComptimeFor {
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
			// Note: these four decl nodes cannot be merged as one branch
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
					children << node.variants.map(Node(Expr(it)))
				}
			}
			else {}
		}
	} else if node is ScopeObject {
		match node {
			GlobalField, ConstField, Var { children << node.expr }
			AsmRegister, EmptyScopeObject {}
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
	if mut lx.left is IndexExpr && lx.left.is_map {
		lx.left.recursive_mapset_is_setter(val)
	}
}

pub fn (mut lx IndexExpr) recursive_arraymap_set_is_setter() {
	lx.is_setter = true
	if mut lx.left is IndexExpr {
		lx.left.recursive_arraymap_set_is_setter()
	} else if mut lx.left is SelectorExpr && lx.left.expr is IndexExpr {
		lx.left.expr.recursive_arraymap_set_is_setter()
	}
}

// return all the registers for the given architecture
pub fn all_registers(mut t Table, arch pref.Arch) map[string]ScopeObject {
	mut res := map[string]ScopeObject{}
	match arch {
		._auto {
			return all_registers(mut t, .amd64)
		}
		.amd64, .i386 {
			for bit_size, array in x86_no_number_register_list {
				for name in array {
					res[name] = AsmRegister{
						name: name
						typ:  t.bitsize_to_type(bit_size)
						size: bit_size
					}
				}
			}
			for bit_size, array in x86_with_number_register_list {
				for name, max_num in array {
					for i in 0 .. max_num {
						hash_index := name.index('#') or {
							panic('all_registers: no hashtag found')
						}
						assembled_name := '${name[..hash_index]}${i}${name[hash_index + 1..]}'
						res[assembled_name] = AsmRegister{
							name: assembled_name
							typ:  t.bitsize_to_type(bit_size)
							size: bit_size
						}
					}
				}
			}
		}
		.arm32 {
			arm32 := gen_all_registers(mut t, arm_no_number_register_list, arm_with_number_register_list,
				32)
			for k, v in arm32 {
				res[k] = v
			}
		}
		.arm64 {
			arm64 := gen_all_registers(mut t, arm_no_number_register_list, arm_with_number_register_list,
				64)
			for k, v in arm64 {
				res[k] = v
			}
		}
		.rv32 {
			rv32 := gen_all_registers(mut t, riscv_no_number_register_list, riscv_with_number_register_list,
				32)
			for k, v in rv32 {
				res[k] = v
			}
		}
		.rv64 {
			rv64 := gen_all_registers(mut t, riscv_no_number_register_list, riscv_with_number_register_list,
				64)
			for k, v in rv64 {
				res[k] = v
			}
		}
		.s390x {
			s390x := gen_all_registers(mut t, s390x_no_number_register_list, s390x_with_number_register_list,
				64)
			for k, v in s390x {
				res[k] = v
			}
		}
		.ppc64le {
			ppc64le := gen_all_registers(mut t, ppc64le_no_number_register_list, ppc64le_with_number_register_list,
				64)
			for k, v in ppc64le {
				res[k] = v
			}
		}
		.loongarch64 {
			loongarch64 := gen_all_registers(mut t, loongarch64_no_number_register_list,
				loongarch64_with_number_register_list, 64)
			for k, v in loongarch64 {
				res[k] = v
			}
		}
		.wasm32 {
			// no registers
		}
		else { // TODO
			panic('all_registers: unhandled arch: ${arch}')
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
			typ:  t.bitsize_to_type(bit_size)
			size: bit_size
		}
	}
	for name, max_num in with_numbers {
		for i in 0 .. max_num {
			hash_index := name.index('#') or { panic('all_registers: no hashtag found') }
			assembled_name := '${name[..hash_index]}${i}${name[hash_index + 1..]}'
			res[assembled_name] = AsmRegister{
				name: assembled_name
				typ:  t.bitsize_to_type(bit_size)
				size: bit_size
			}
		}
	}
	return res
}

pub fn (expr Expr) is_reference() bool {
	return match expr {
		PrefixExpr {
			expr.op == .amp
		}
		UnsafeExpr {
			expr.expr.is_reference()
		}
		ParExpr {
			expr.expr.is_reference()
		}
		else {
			false
		}
	}
}

// remove_par removes all parenthesis and gets the innermost Expr
pub fn (mut expr Expr) remove_par() Expr {
	mut e := expr
	for mut e is ParExpr {
		e = e.expr
	}
	return e
}

// is `expr` a literal, i.e. it does not depend on any other declarations (C compile time constant)
pub fn (expr Expr) is_literal() bool {
	return match expr {
		BoolLiteral, CharLiteral, FloatLiteral, IntegerLiteral, StringLiteral, StringInterLiteral {
			true
		}
		PrefixExpr {
			expr.right.is_literal()
		}
		InfixExpr {
			expr.left.is_literal() && expr.right.is_literal()
		}
		ParExpr {
			expr.expr.is_literal()
		}
		CastExpr {
			!expr.has_arg && expr.expr.is_literal() && (expr.typ.is_any_kind_of_pointer()
				|| expr.typ in [i8_type, i16_type, int_type, i64_type, u8_type, u16_type, u32_type, u64_type, f32_type, f64_type, char_type, bool_type, rune_type])
		}
		SizeOf, IsRefType {
			expr.is_type || expr.expr.is_literal()
		}
		else {
			false
		}
	}
}

@[inline]
pub fn (e Expr) is_nil() bool {
	return e is Nil || (e is UnsafeExpr && e.expr is Nil)
}

@[direct_array_access]
pub fn type_can_start_with_token(tok &token.Token) bool {
	return match tok.kind {
		.name {
			(tok.lit.len > 0 && tok.lit[0].is_capital())
				|| builtin_type_names_matcher.matches(tok.lit)
		}
		// Note: return type (T1, T2) should be handled elsewhere
		.amp, .key_fn, .lsbr, .question {
			true
		}
		else {
			false
		}
	}
}

// validate_type_string_is_pure_literal returns `Error` if `str` can not be converted
// to pure literal `typ` (`i64`, `f64`, `bool`, `char` or `string`).
pub fn validate_type_string_is_pure_literal(typ Type, str string) ! {
	if typ == bool_type {
		if !(str == 'true' || str == 'false') {
			return error('bool literal `true` or `false` expected, found "${str}"')
		}
	} else if typ == char_type {
		if str.starts_with('\\') {
			if str.len <= 1 {
				return error('empty escape sequence found')
			}
			if !util.is_escape_sequence(str[1]) {
				return error('char literal escape sequence expected, found "${str}"')
			}
		} else if str.len != 1 {
			return error('char literal expected, found "${str}"')
		}
	} else if typ == f64_type {
		if str.count('.') != 1 {
			return error('f64 literal expected, found "${str}"')
		}
	} else if typ == string_type {
	} else if typ == i64_type {
		if !str.is_int() {
			return error('i64 literal expected, found "${str}"')
		}
	} else {
		return error('expected pure literal, found "${str}"')
	}
}
