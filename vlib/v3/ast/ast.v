module ast

import v3.token

pub const empty_expr = Expr(EmptyExpr(0))
pub const empty_stmt = Stmt(EmptyStmt(0))

type EmptyExpr = u8
type EmptyStmt = u8

pub type Expr = ArrayInitExpr
	| AsCastExpr
	| AssocExpr
	| BasicLiteral
	| CallExpr
	| CallOrCastExpr
	| CastExpr
	| ComptimeExpr
	| EmptyExpr
	| FieldInit
	| FnLiteral
	| GenericArgOrIndexExpr
	| GenericArgs
	| Ident
	| IfExpr
	| IfGuardExpr
	| IndexExpr
	| InfixExpr
	| InitExpr
	| Keyword
	| KeywordOperator
	| LambdaExpr
	| LifetimeExpr
	| LockExpr
	| MapInitExpr
	| MatchExpr
	| ModifierExpr
	| OrExpr
	| ParenExpr
	| PostfixExpr
	| PrefixExpr
	| RangeExpr
	| SelectExpr
	| SelectorExpr
	| SqlExpr
	| StringInterLiteral
	| StringLiteral
	| Tuple
	| Type
	| UnsafeExpr

pub type Stmt = AsmStmt
	| AssertStmt
	| AssignStmt
	| BlockStmt
	| ComptimeStmt
	| ConstDecl
	| DeferStmt
	| Directive
	| EmptyStmt
	| EnumDecl
	| ExprStmt
	| FlowControlStmt
	| FnDecl
	| ForInStmt
	| ForStmt
	| GlobalDecl
	| ImportStmt
	| InterfaceDecl
	| LabelStmt
	| ModuleStmt
	| ReturnStmt
	| StructDecl
	| TypeDecl
	| []Attribute

pub type Type = AnonStructType
	| ArrayFixedType
	| ArrayType
	| ChannelType
	| FnType
	| GenericType
	| MapType
	| NilType
	| NoneType
	| OptionType
	| PointerType
	| ResultType
	| ThreadType
	| TupleType

pub fn (expr Expr) name() string {
	return match expr {
		BasicLiteral { expr.value }
		CallExpr { '${expr.lhs.name()}()' }
		EmptyExpr { 'EmptyExpr' }
		Ident { expr.name }
		InfixExpr { '${expr.lhs.name()} ${expr.op} ${expr.rhs.name()}' }
		ParenExpr { '(${expr.expr.name()})' }
		PrefixExpr { '${expr.op}${expr.expr.name()}' }
		SelectorExpr { expr.lhs.name() + '.' + expr.rhs.name }
		StringLiteral { "'${expr.value}'" }
		Type { 'Type' }
		else { 'Expr' }
	}
}

pub fn (expr Expr) pos() token.Pos {
	return match expr {
		AsCastExpr { expr.pos }
		ArrayInitExpr { expr.pos }
		AssocExpr { expr.pos }
		BasicLiteral { expr.pos }
		CallExpr { expr.pos }
		CallOrCastExpr { expr.pos }
		CastExpr { expr.pos }
		ComptimeExpr { expr.pos }
		FnLiteral { expr.pos }
		GenericArgOrIndexExpr { expr.pos }
		GenericArgs { expr.pos }
		Ident { expr.pos }
		IfExpr { expr.pos }
		IfGuardExpr { expr.pos }
		IndexExpr { expr.pos }
		InfixExpr { expr.pos }
		InitExpr { expr.pos }
		KeywordOperator { expr.pos }
		LifetimeExpr { expr.pos }
		LambdaExpr { expr.pos }
		LockExpr { expr.pos }
		MapInitExpr { expr.pos }
		MatchExpr { expr.pos }
		ModifierExpr { expr.pos }
		OrExpr { expr.pos }
		ParenExpr { expr.pos }
		PostfixExpr { expr.pos }
		PrefixExpr { expr.pos }
		RangeExpr { expr.pos }
		SelectExpr { expr.pos }
		SelectorExpr { expr.pos }
		SqlExpr { expr.pos }
		StringInterLiteral { expr.pos }
		StringLiteral { expr.pos }
		Tuple { expr.pos }
		UnsafeExpr { expr.pos }
		else { token.Pos{} }
	}
}

pub fn (exprs []Expr) name_list() string {
	mut out := ''
	for i := 0; i < exprs.len; i++ {
		out = out + exprs[i].name()
		if i < exprs.len - 1 {
			out = out + ','
		}
	}
	return out
}

pub fn (t Type) name() string {
	return match t {
		GenericType { '${t.name.name()}[${t.params.name_list()}]' }
		else { 'Type' }
	}
}

// File
pub struct File {
pub:
	attributes     []Attribute
	mod            string
	name           string
	stmts          []Stmt
	imports        []ImportStmt
	selector_names map[int]string
}

pub enum Language {
	v
	c
	js
}

pub fn (lang Language) str() string {
	return match lang {
		.v { 'V' }
		.c { 'C' }
		.js { 'JS' }
	}
}

pub enum DeferMode {
	scoped
	function
}

// Expressions
pub struct ArrayInitExpr {
pub mut:
	typ         Expr = empty_expr
	exprs       []Expr
	init        Expr = empty_expr
	cap         Expr = empty_expr
	len         Expr = empty_expr
	update_expr Expr = empty_expr
	pos         token.Pos
}

pub struct AsCastExpr {
pub:
	expr Expr
	typ  Expr
	pos  token.Pos
}

pub struct AssocExpr {
pub:
	typ    Expr
	expr   Expr
	fields []FieldInit
	pos    token.Pos
}

pub struct BasicLiteral {
pub:
	kind  token.Token
	value string
	pos   token.Pos
}

pub struct CallExpr {
pub mut:
	lhs Expr
pub:
	args []Expr
	pos  token.Pos
}

pub struct CallOrCastExpr {
pub mut:
	lhs  Expr
	expr Expr
pub:
	pos token.Pos
}

pub struct CastExpr {
pub mut:
	typ  Expr
	expr Expr
pub:
	pos token.Pos
}

pub struct ComptimeExpr {
pub:
	expr Expr
	pos  token.Pos
}

pub struct FieldDecl {
pub:
	name                string
	typ                 Expr = empty_expr
	value               Expr = empty_expr
	attributes          []Attribute
	is_public           bool
	is_mut              bool
	is_module_mut       bool
	is_interface_method bool
}

pub struct FieldInit {
pub:
	name string
pub mut:
	value Expr
}

pub struct FnLiteral {
pub:
	typ           FnType
	captured_vars []Expr
	stmts         []Stmt
	pos           token.Pos
}

pub struct GenericArgs {
pub:
	lhs  Expr
	args []Expr
	pos  token.Pos
}

pub struct GenericArgOrIndexExpr {
pub:
	lhs  Expr
	expr Expr
	pos  token.Pos
}

pub struct Ident {
pub mut:
	pos  token.Pos
	name string
}

pub fn (ident &Ident) str() string {
	return ident.name.clone()
}

pub struct IfExpr {
pub mut:
	cond      Expr = empty_expr
	else_expr Expr = empty_expr
	stmts     []Stmt
	pos       token.Pos
}

pub struct IfGuardExpr {
pub:
	stmt AssignStmt
	pos  token.Pos
}

pub struct InfixExpr {
pub mut:
	op  token.Token
	lhs Expr
	rhs Expr
	pos token.Pos
}

pub struct IndexExpr {
pub mut:
	lhs      Expr
	expr     Expr
	is_gated bool
	pos      token.Pos
}

pub struct InitExpr {
pub mut:
	typ    Expr
	fields []FieldInit
	pos    token.Pos
}

pub struct Keyword {
pub:
	tok token.Token
}

pub struct KeywordOperator {
pub:
	op    token.Token
	exprs []Expr
	pos   token.Pos
}

pub struct Tuple {
pub:
	exprs []Expr
	pos   token.Pos
}

pub struct LambdaExpr {
pub:
	args []Ident
	expr Expr
	pos  token.Pos
}

pub struct LockExpr {
pub:
	lock_exprs  []Expr
	rlock_exprs []Expr
	stmts       []Stmt
	pos         token.Pos
}

pub struct MapInitExpr {
pub:
	typ  Expr = empty_expr
	keys []Expr
	vals []Expr
	pos  token.Pos
}

pub struct MatchBranch {
pub:
	cond  []Expr
	stmts []Stmt
	pos   token.Pos
}

pub struct MatchExpr {
pub:
	expr     Expr
	branches []MatchBranch
	pos      token.Pos
}

pub struct ModifierExpr {
pub mut:
	kind token.Token
	expr Expr
	pos  token.Pos
}

pub struct OrExpr {
pub:
	expr  Expr
	stmts []Stmt
	pos   token.Pos
}

pub struct Parameter {
pub mut:
	name   string
	typ    Expr
	is_mut bool
	pos    token.Pos
}

pub struct LifetimeExpr {
pub:
	name string
	pos  token.Pos
}

pub struct ParenExpr {
pub:
	expr Expr
	pos  token.Pos
}

pub struct PostfixExpr {
pub:
	op   token.Token
	expr Expr
	pos  token.Pos
}

pub struct PrefixExpr {
pub mut:
	op   token.Token
	expr Expr
	pos  token.Pos
}

pub struct RangeExpr {
pub:
	op    token.Token
	start Expr
	end   Expr
	pos   token.Pos
}

pub struct SelectExpr {
pub:
	pos   token.Pos
	stmt  Stmt
	stmts []Stmt
	next  Expr = empty_expr
}

pub struct SelectorExpr {
pub mut:
	lhs Expr
	rhs Ident
	pos token.Pos
}

pub fn (se SelectorExpr) leftmost() Expr {
	if se.lhs is SelectorExpr {
		return se.lhs.leftmost()
	}
	return se.lhs
}

pub fn (se SelectorExpr) name() string {
	return se.lhs.name() + '.' + se.rhs.name
}

pub enum StringLiteralKind {
	c
	js
	raw
	v
}

pub fn (s StringLiteralKind) str() string {
	return match s {
		.c { 'c' }
		.js { 'js' }
		.raw { 'r' }
		.v { 'v' }
	}
}

@[direct_array_access]
pub fn StringLiteralKind.from_string_tinyv(s string) StringLiteralKind {
	match s[0] {
		`c` {
			return .c
		}
		`j` {
			if s[1] == `s` { return .js }
		}
		`r` {
			return .raw
		}
		else {}
	}

	return .v
}

pub struct StringLiteral {
pub:
	kind  StringLiteralKind
	value string
	pos   token.Pos
}

pub struct StringInterLiteral {
pub:
	kind   StringLiteralKind
	values []string
	inters []StringInter
	pos    token.Pos
}

pub struct StringInter {
pub:
	format       StringInterFormat
	width        int
	precision    int
	expr         Expr
	format_expr  Expr = empty_expr
	resolved_fmt string
}

pub enum StringInterFormat {
	unformatted
	binary
	character
	decimal
	exponent
	exponent_short
	float
	hex
	octal
	pointer_address
	string
}

pub fn StringInterFormat.from_u8(c u8) StringInterFormat {
	return match c {
		`b` { .binary }
		`c` { .character }
		`d` { .decimal }
		`e`, `E` { .exponent }
		`g`, `G` { .exponent_short }
		`f`, `F` { .float }
		`x`, `X` { .hex }
		`o` { .octal }
		`p` { .pointer_address }
		`s` { .string }
		else { .unformatted }
	}
}

pub struct SqlExpr {
pub:
	expr       Expr
	table_name string
	is_count   bool
	is_create  bool
	pos        token.Pos
}

pub struct UnsafeExpr {
pub:
	stmts []Stmt
	pos   token.Pos
}

// Statements
pub struct AsmStmt {
pub:
	arch string
}

pub struct AssertStmt {
pub:
	expr  Expr
	extra Expr = empty_expr
}

pub struct AssignStmt {
pub:
	op  token.Token
	lhs []Expr
	rhs []Expr
	pos token.Pos
}

pub struct Attribute {
pub:
	name          string
	value         Expr
	comptime_cond Expr
	pos           token.Pos
}

pub fn (attributes []Attribute) has(name string) bool {
	for attribute in attributes {
		if attribute.name == name {
			return true
		}
		if attribute.name == '' {
			if attribute.value is Ident && attribute.value.name == name {
				return true
			}
		}
	}
	return false
}

pub struct BlockStmt {
pub:
	stmts []Stmt
}

pub struct ComptimeStmt {
pub:
	stmt Stmt
}

pub struct ConstDecl {
pub:
	is_public bool
	fields    []FieldInit
}

pub struct DeferStmt {
pub:
	mode  DeferMode
	stmts []Stmt
}

pub struct Directive {
pub:
	name    string
	value   string
	ct_cond string
}

pub struct EnumDecl {
pub:
	attributes []Attribute
	is_public  bool
	name       string
	as_type    Expr = empty_expr
	fields     []FieldDecl
}

pub struct ExprStmt {
pub mut:
	expr Expr
}

pub struct FlowControlStmt {
pub:
	op    token.Token
	label string
}

pub struct FnDecl {
pub:
	attributes []Attribute
	is_public  bool
	is_method  bool
	is_static  bool
	receiver   Parameter
	language   Language = .v
	name       string
	typ        FnType
	stmts      []Stmt
	pos        token.Pos
}

pub struct ForStmt {
pub mut:
	init  Stmt = empty_stmt
	cond  Expr = empty_expr
	post  Stmt = empty_stmt
	stmts []Stmt
}

pub struct ForInStmt {
pub mut:
	key   Expr = empty_expr
	value Expr
	expr  Expr
}

pub struct GlobalDecl {
pub:
	attributes []Attribute
	fields     []FieldDecl
	is_public  bool
}

pub struct ImportStmt {
pub:
	name       string
	alias      string
	is_aliased bool
	symbols    []Expr
}

pub struct InterfaceDecl {
pub:
	is_public      bool
	attributes     []Attribute
	name           string
	generic_params []Expr
	embedded       []Expr
	fields         []FieldDecl
}

pub struct LabelStmt {
pub:
	name string
	stmt Stmt = empty_stmt
}

pub struct ModuleStmt {
pub:
	name string
}

pub struct ReturnStmt {
pub:
	exprs []Expr
}

pub struct StructDecl {
pub:
	attributes     []Attribute
	is_public      bool
	is_union       bool
	implements     []Expr
	embedded       []Expr
	language       Language = .v
	name           string
	generic_params []Expr
	fields         []FieldDecl
	pos            token.Pos
}

pub struct TypeDecl {
pub:
	is_public      bool
	language       Language
	name           string
	generic_params []Expr
	base_type      Expr = empty_expr
	variants       []Expr
}

// Types
pub struct ArrayType {
pub:
	elem_type Expr = empty_expr
}

pub struct ArrayFixedType {
pub:
	len       Expr = empty_expr
	elem_type Expr = empty_expr
}

pub struct ChannelType {
pub:
	cap       Expr = empty_expr
	elem_type Expr = empty_expr
}

pub struct ThreadType {
pub:
	elem_type Expr = empty_expr
}

pub struct FnType {
pub:
	generic_params []Expr
	params         []Parameter
pub mut:
	return_type Expr = empty_expr
}

pub fn (ft &FnType) str() string {
	mut s := 'fn('
	for i := 0; i < ft.params.len; i++ {
		param := ft.params[i]
		s = s + param.name + param.typ.name()
		if i < ft.params.len - 1 {
			s = s + ', '
		}
	}
	if ft.return_type is EmptyExpr {
		s = s + ')'
	} else {
		s = s + ') ' + ft.return_type.name()
	}
	return s
}

pub struct AnonStructType {
pub:
	generic_params []Expr
	embedded       []Expr
	fields         []FieldDecl
}

pub struct GenericType {
pub:
	name   Expr = empty_expr
	params []Expr
}

pub struct MapType {
pub:
	key_type   Expr = empty_expr
	value_type Expr = empty_expr
}

pub struct NilType {}

pub struct NoneType {}

pub struct OptionType {
pub:
	base_type Expr = empty_expr
}

pub struct PointerType {
pub:
	base_type Expr = empty_expr
	lifetime  string
}

pub struct ResultType {
pub:
	base_type Expr = empty_expr
}

pub struct TupleType {
pub:
	types []Expr
}
