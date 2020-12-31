# Vlang abstract syntax tree(AST) introduction

## Overview

The Vlang abstract syntax tree is implemented by sum type. the following is declaration:

```v
pub type TypeDecl = AliasTypeDecl | FnTypeDecl | SumTypeDecl

pub type Expr = AnonFn | ArrayInit | AsCast | Assoc | AtExpr | BoolLiteral | CTempVar |
	CallExpr | CastExpr | ChanInit | CharLiteral | Comment | ComptimeCall | ConcatExpr | EnumVal |
	FloatLiteral | Ident | IfExpr | IfGuardExpr | IndexExpr | InfixExpr | IntegerLiteral |
	Likely | LockExpr | MapInit | MatchExpr | None | OrExpr | ParExpr | PostfixExpr | PrefixExpr |
	RangeExpr | SelectExpr | SelectorExpr | SizeOf | SqlExpr | StringInterLiteral | StringLiteral |
	StructInit | Type | TypeOf | UnsafeExpr

pub type Stmt = AssertStmt | AssignStmt | Block | BranchStmt | CompFor | ConstDecl | DeferStmt |
	EnumDecl | ExprStmt | FnDecl | ForCStmt | ForInStmt | ForStmt | GlobalDecl | GoStmt |
	GotoLabel | GotoStmt | HashStmt | Import | InterfaceDecl | Module | Return | SqlStmt |
	StructDecl | TypeDecl
```

All the AST struct declarations can be found in V source code: [vlib/v/ast/ast.v](https://github.com/vlang/v/blob/master/vlib/v/ast/ast.v)

## AST tool

If you are new to Vlang AST, You can install the [vast tool](https://github.com/lydiandy/vast). It can generate example code to AST json file.

The json file can help you more understand the AST.

```shell
vast example.v       //generate example.json file and exit.

vast -w example.v    //generate example.json and watch,if file change,regenerate.

```

## File

AST struct

```v
// Each V source file is represented by one ast.File structure.
// When the V compiler runs, the parser will fill an []ast.File.
// That array is then passed to V's checker.
pub struct File {
pub:
	path             string // path of the source file
	mod              Module // the module of the source file (from `module xyz` at the top)
	global_scope     &Scope
pub mut:
	scope            &Scope
	stmts            []Stmt // all the statements in the source file
	imports          []Import // all the imports
	imported_symbols map[string]string // used for `import {symbol}`, it maps symbol => module.symbol
	errors           []errors.Error // all the checker errors in the file
	warnings         []errors.Warning // all the checker warings in the file
	generic_fns      []&FnDecl
}
```

example code

```v
module main

import os
import time

fn main() {
  
}
```

generate AST json

```json
{
	"ast_type": "ast.File",
	"path": "/Users/xxx/v/vprojects/v_test/main.v",
	"mod": {
		"ast_type": "Module",
		"name": "main",
		"is_skipped": false,
		"pos": {
			"line_nr": 0,
			"pos": 0,
			"len": 19
		}
	},
	"imports": [
		{
			"ast_type": "Import",
			"mod": "os",
			"alias": "os",
			"syms": [],
			"pos": {
				"line_nr": 2,
				"pos": 20,
				"len": 2
			}
		},
		{
			"ast_type": "Import",
			"mod": "time",
			"alias": "time",
			"syms": [],
			"pos": {
				"line_nr": 3,
				"pos": 30,
				"len": 4
			}
		}
	],
	"global_scope": {
		"ast_type": "Scope",
		"parent": "0",
		"children": [],
		"start_pos": 0,
		"end_pos": 0,
		"objects": {},
		"struct_fields": []
	},
	"scope": {
		"ast_type": "Scope",
		"parent": "7f970ef07c90",
		"children": [
			{
				"parent": "7f970ef081f0",
				"start_pos": 39,
				"end_pos": 51
			}
		],
		"start_pos": 0,
		"end_pos": 53,
		"objects": {},
		"struct_fields": []
	},
	"errors": [],
	"warnings": [],
	"imported_symbols": {},
	"generic_fns": [],
	"stmts": [
		{
			"ast_type": "Module",
			"name": "main",
			"is_skipped": false,
			"pos": {
				"line_nr": 0,
				"pos": 0,
				"len": 19
			}
		},
		{
			"ast_type": "Import",
			"mod": "os",
			"alias": "os",
			"syms": [],
			"pos": {
				"line_nr": 2,
				"pos": 20,
				"len": 2
			}
		},
		{
			"ast_type": "Import",
			"mod": "time",
			"alias": "time",
			"syms": [],
			"pos": {
				"line_nr": 3,
				"pos": 30,
				"len": 4
			}
		},
		{
			"ast_type": "FnDecl",
			"name": "main.main",
			"mod": "main",
			"is_deprecated": false,
			"is_pub": false,
			"is_variadic": false,
			"is_anon": false,
			"receiver": {
				"ast_type": "Field",
				"name": "",
				"typ": "void",
				"pos": {
					"line_nr": 0,
					"pos": 0,
					"len": 0
				}
			},
			"receiver_pos": {
				"line_nr": 0,
				"pos": 0,
				"len": 0
			},
			"is_method": false,
			"method_idx": 0,
			"rec_mut": false,
			"rec_share": "enum:0(mut)",
			"language": "enum:0(v)",
			"no_body": false,
			"is_builtin": false,
			"is_generic": false,
			"is_direct_arr": false,
			"pos": {
				"line_nr": 5,
				"pos": 36,
				"len": 9
			},
			"body_pos": {
				"line_nr": 7,
				"pos": 51,
				"len": 1
			},
			"file": "/Users/xxx/v/vprojects/v_test/main.v",
			"return_type": "void",
			"source_file": 0,
			"scope": 250645264,
			"attrs": [],
			"params": [],
			"stmts": [],
			"comments": [],
			"next_comments": []
		}
	]
}
```

##  Module

### module declaration

AST struct

```v
// module declaration
pub struct Module {
pub:
	name       string
	path       string
	expr       Expr
	pos        token.Position
	is_skipped bool // module main can be skipped in single file programs
}
```

example code

```v
module main

fn main() {
	
}
```

### module import

AST struct

```v
// import statement
pub struct Import {
pub:
	mod   string // the module name of the import
	alias string // the `x` in `import xxx as x`
	pos   token.Position
pub mut:
	syms  []ImportSymbol // the list of symbols in `import {symbol1, symbol2}`
}

// import symbol,for import {symbol} syntax
pub struct ImportSymbol {
pub:
	pos  token.Position
	name string
}
```

example code

```v
module main

import os
import time as t
import math { min, max }
```

## Const

AST struct

```v
// const declaration
pub struct ConstDecl {
pub:
	is_pub       bool
	pos          token.Position
pub mut:
	fields       []ConstField // all the const fields in the `const (...)` block
	end_comments []Comment // comments that after last const field
}

// const field in const declaration group
pub struct ConstField {
pub:
	mod      string
	name     string
	expr     Expr // the value expr of field; everything after `=`
	is_pub   bool
	pos      token.Position
pub mut:
	typ      table.Type // the type of the const field, it can be any type in V
	comments []Comment // comments before current const field
}
```

example code

```v
module main

const (
	// version comment 1
	version = '0.2.0' // version comment 2
	usage   = 'usage:xxxx'
	pi      = 3.14
	//end comment 1
	//end comment 2
)
```

## Enum

 AST struct

```v
// enum declaration
pub struct EnumDecl {
pub:
	name             string
	is_pub           bool
	is_flag          bool // true when the enum has [flag] tag,for bit field enum
	is_multi_allowed bool // true when the enum has [_allow_multiple_values] tag
	comments         []Comment // comments before the first EnumField
	fields           []EnumField // all the enum fields
	attrs            []table.Attr // attributes of enum declaration
	pos              token.Position
}

// enum field in enum declaration
pub struct EnumField {
pub:
	name          string
	pos           token.Position
	comments      []Comment // comment after Enumfield in the same line
	next_comments []Comment // comments between current EnumField and next EnumField
	expr          Expr // the value of current EnumField; 123 in `ename = 123`
	has_expr      bool // true, when .expr has a value
}

// an enum value, like OS.macos or .macos
pub struct EnumVal {
pub:
	enum_name string
	val       string
	mod       string // for full path `mod_Enum_val`
	pos       token.Position
pub mut:
	typ       table.Type
}
```

example code

```v
module main

[attr1]
['attr2=123']
  enum Color { // enum comment 1
	// black comment 1
	// black comment 2
	black = 2 // black comment 3
	// white comment 1
	// white comment 2
	white // white comment 3
	blue
	green // green comment
	// end comment 1
	// end comment 2
}

[flag]
enum BitEnum {
	e1
	e2
	e3
}

[_allow_multiple_values]
enum MultipleEnum {
	v1 = 1
}

fn main() {
	mut color := Color.black
	color = .blue
}
```

## Variable

### Assign

AST struct

```v
// variable assign statement
pub struct AssignStmt {
pub:
	right         []Expr
	op            token.Kind // include: =,:=,+=,-=,*=,/= and so on; for a list of all the assign operators, see vlib/token/token.v
	pos           token.Position
	comments      []Comment
	end_comments  []Comment
pub mut:
	left          []Expr
	left_types    []table.Type
	right_types   []table.Type
	is_static     bool // for translated code only
	is_simple     bool // `x+=2` in `for x:=1; ; x+=2`
	has_cross_var bool
}
```

example code

```v
module main

fn main() {
	// signle assign
	a := 'abc' // comment for a
	mut b := 1
	// more operator
	b = 2
	b += 2
	b -= 2
	b *= 2
	b /= 2
	b %= 2
	// multi assign
	x, y, z := 1, 'y', 3.3
	mut xx, mut yy, zz := 1, 3, 5
	// swap variable
	mut c := 1
	mut d := 2
	c, d = d, c
}

```

### Identifier

AST struct

```v
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
	language table.Language
	tok_kind token.Kind
	pos      token.Position
	mut_pos  token.Position
pub mut:
	scope    &Scope
	obj      ScopeObject
	mod      string
	name     string
	kind     IdentKind
	info     IdentInfo
	is_mut   bool
}
```

example code(todo: need more kind)

```v
module main

fn main() {
	i := 123 // common(unresolved) identifier
	_, x := 1, 2 // blank identifier
	mut s := 'abc' // with mut
	s = 'aaa'
}

```

### Literal

AST struct

```v
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
	language table.Language
	pos      token.Position
}

// string literal with `$xx` or `${xxx}`, e.g. 'name: $name'
pub struct StringInterLiteral {
pub:
	vals       []string // the string literal will be split by `$xxx` in vals array
	exprs      []Expr // all `$xxx` in string literal
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
```

example code

```v
module main

fn main() {
	a := 1 // integer literal
	b := 1.2 // float literal
	c := 'abc' // string literal
  name:='tom'
  age:= 33
  //string literal with `$xx` or `${xxx}`
	s1 := 'a is $a,b is $b,c is $c' 
	s2 := 'name is ${name}, age is ${age}'
	e := `c` // char literal
	f := true // bool literal
}
```

### AsCast

AST struct

```v
// as cast statement
pub struct AsCast {
pub:
	expr      Expr // `x` in `x as int`
	typ       table.Type // `int` in `x as int`
	pos       token.Position
pub mut:
	expr_type table.Type
}
```

example code

```v
module main

type Mysumtype = bool | f64 | int | string

fn main() {
	x := Mysumtype(3)
	x2 := x as int // as must be used for sumtype
	println(x2)
}
```

### SizeOf

AST struct

```v
// the builtin sizeof function,can be used for type and variable
pub struct SizeOf {
pub:
	is_type   bool // true, if argument is a type
	typ       table.Type
	type_name string
	expr      Expr
	pos       token.Position
}
```

example code

```v
module main

struct Point {
	x int
	y int
}

fn main() {
	a := sizeof(int) // basic type
	b := sizeof(bool) // basic type
	p := Point{
		x: 1
		y: 2
	}
	s1 := sizeof(Point) // struct type
	s2 := sizeof(p) // variable
}
```

### TypeOf

AST struct

```v
//the builtin typeof function
pub struct TypeOf {
pub:
	expr      Expr
	pos       token.Position
pub mut:
	expr_type table.Type
}
```

example code

```v
module main

type MySumType = f32 | int

fn myfn(i int) int {
	return i
}

fn main() {
	a := 123
	s := 'abc'
	aint := []int{}
	astring := []string{}
	println(typeof(a)) // int
	println(typeof(s)) // string
	println(typeof(aint)) // array_int
	println(typeof(astring)) // array_string
	// sumtype
	sa := MySumType(32)
	println(typeof(sa)) // int
	// function type
	println(typeof(myfn)) // fn (int) int
}

```

### CastExpr

AST struct

```v
pub struct CastExpr {
pub:
	expr      Expr // `buf` in `string(buf, n)`
	arg       Expr // `n` in `string(buf, n)`
	typ       table.Type // `string` TODO rename to `type_to_cast_to`
	pos       token.Position
pub mut:
	typname   string
	expr_type table.Type // `byteptr`
	has_arg   bool
	in_prexpr bool // is the parent node an ast.PrefixExpr
}
```

example code ( todo: need more about string(buf,n) )

```v
module main

fn main() {
	x:=byte(3)
	y:=f32(2.1)
}
```

## Array

### ArrayInit

AST struct

```v
pub struct ArrayInit {
pub:
	pos            token.Position // `[]` in []Type{} position
	elem_type_pos  token.Position // `Type` in []Type{} position
	exprs          []Expr // `[expr, expr]` or `[expr]Type{}` for fixed array
	ecmnts         [][]Comment // optional iembed comments after each expr
	is_fixed       bool
	has_val        bool // fixed size literal `[expr, expr]!!`
	mod            string
	len_expr       Expr // len: expr
	cap_expr       Expr // cap: expr
	default_expr   Expr // init: expr
	has_len        bool
	has_cap        bool
	has_default    bool
pub mut:
	expr_types     []table.Type // [Dog, Cat] // also used for interface_types
	is_interface   bool // array of interfaces e.g. `[]Animal` `[Dog{}, Cat{}]`
	interface_type table.Type // Animal
	elem_type      table.Type // element type
	typ            table.Type // array type
}

```

example code

```v
module main

fn main() {
	mut arr := []string{len: 3, cap: 6, init: 'default'}
	arr[0] = 'a'
	arr[1] = 'b'
	println(arr)
}
```

### IndexExpr

AST struct

```v
//index expr, can be used for array and map, e.g. `array[index]` or `map[key]`
pub struct IndexExpr {
pub:
	pos       token.Position
	left      Expr
	index     Expr // [0], RangeExpr [start..end] or map[key]
	or_expr   OrExpr
pub mut:
	left_type table.Type // array, map, fixed array
	is_setter bool
}
```

example code

```v
module main

fn main() {
	mut arr := []string{len: 3, cap: 6, init: 'default'}
	arr[0] = 'a' //index expr
	arr[1] = 'b'
	println(arr)
  mut m := map[string]string{}
	m['name'] = 'tom' //index expr
	m['age'] = '33'
}
```

### RangeExpr

AST struct

```v
// s[10..20]
pub struct RangeExpr {
pub:
	low      Expr
	high     Expr
	has_high bool
	has_low  bool
	pos      token.Position
}
```

example code

```v
module main

fn main() {
	n := [1, 2, 3, 4, 5]
	a1 := n[..2] //[1, 2]
	a2 := n[2..] //[3, 4, 5]
	a3 := n[2..4] //[3, 4]
}
```

### ArrayDecompose

AST struct

```v
pub struct ArrayDecompose {
pub:
	expr      Expr
	pos       token.Position
pub mut:
	expr_type table.Type
	arg_type  table.Type
}
```

example code

```v
module main

fn main() {
	a := ['a', 'b', 'c'] 
	println(variadic_fn_a(a...)) //ArrayDecompose
}

fn variadic_fn_a(a ...string) string {
	return variadic_fn_b(a...) //ArrayDecompose
}

fn variadic_fn_b(a ...string) string {
	a0 := a[0]
	a1 := a[1]
	a2 := a[2]
	return '$a0$a1$a2'
}

```

## Map

### MapInit

AST struct

```v
pub struct MapInit {
pub:
	keys       []Expr 	//save all keys, when it is map literal init
	vals       []Expr 	//save all values, when it is map literal init
	pos        token.Position
pub mut:
	typ        table.Type
	key_type   table.Type
	value_type table.Type
}
```

example code

```v
module main

fn main() {
	mut m := map[string]string{}
	m['name'] = 'tom'
	m['age'] = '33'
	//map literal init
	m2 := {
		'one':   1
		'two':   2
		'three': 3
	}
}
```

## Operator

### PrefixExpr

AST struct

```v
// See: token.Kind.is_prefix
pub struct PrefixExpr {
pub:
	op         token.Kind 	//prefix operator, e.g. -, &, *, !, ~
	right      Expr
	pos        token.Position
pub mut:
	right_type table.Type
	or_block   OrExpr
}
```

example code

```v
module main

fn main() {
	x := -1 // minus
	p := &x // get address of variable
	x2 := *p // get value of pointer
	b := !true // logic not
	bit := ~0x0000 // bit not
}

```

### InfixExpr

AST struct

```v
// left op right e.g. `1 + 2`
// See: token.Kind.is_infix
pub struct InfixExpr {
pub:
	op          token.Kind
	pos         token.Position
pub mut:
	left        Expr
	right       Expr
	left_type   table.Type
	right_type  table.Type
	auto_locked string
}
```

example code

```v
module main

fn main() {
	x := 1 + 2
	y := 1 - 2
	a := x == y // equal
	b := x > y // compare
	c := 1 in [1, 2] // in operator
	d := (x > y) && (1 < 2) // logic and
	e := 2 == 2 || 3 == 3 // logic or
	mut arr := [1, 2] // array append
	arr << 3
}
```

### PostfixExpr

AST struct

```v
// ++, --
pub struct PostfixExpr {
pub:
	op          token.Kind
	expr        Expr
	pos         token.Position
pub mut:
	auto_locked string
}
```

example code

```v
module main

fn main() {
	mut x:=1
	x++
	x--
}

```

### SelectorExpr

AST struct

```v
// `foo.bar`
pub struct SelectorExpr {
pub:
	pos             token.Position
	expr            Expr // expr.field_name
	field_name      string
	is_mut          bool // is used for the case `if mut ident.selector is MyType {`, it indicates if the root ident is mutable
	mut_pos         token.Position
pub mut:
	expr_type       table.Type // type of `Foo` in `Foo.bar`
	typ             table.Type // type of the entire thing (`Foo.bar`)
	name_type       table.Type // T in `T.name` or typeof in `typeof(expr).name`
	scope           &Scope
	from_embed_type table.Type // holds the type of the embed that the method is called from
}
```

example code

```v
module main

struct Point {
mut:
	x int
	y int
}

fn (mut p Point) move(a int, b int) {
	p.x += a // selector for struct field assign
	p.y += b
}

fn main() {
	mut p := Point{
		x: 1
		y: 3
	}
	p.x // selector for access field value
	p.move(2, 3)
}

```

### ParExpr

AST struct

```v
// `(3+4)`
pub struct ParExpr {
pub:
	expr Expr
	pos  token.Position
}
```

example code

```v
module main

fn main() {
	x:=(1+2)
	y:=(1<2)
}

```

### ConcatExpr

AST struct

```v
pub struct ConcatExpr {
pub:
	vals        []Expr
	pos         token.Position
pub mut:
	return_type table.Type
}
```

example code

```v
	a, b, c := match false {
		true { 1, 2, 3 }
		false { 4, 5, 6 }
		else { 7, 8, 9 }
	}
```

## Function/Method

### Function declaration

AST struct

```v
//function or method declaration
pub struct FnDecl {
pub:
	name            string
	mod             string
	params          []table.Param
	is_deprecated   bool
	is_pub          bool
	is_variadic     bool
	is_anon         bool
	receiver        Field
	receiver_pos    token.Position
	is_method       bool
	method_type_pos token.Position
	method_idx      int
	rec_mut         bool // is receiver mutable
	rec_share       table.ShareType
	language        table.Language
	no_body         bool // just a definition `fn C.malloc()`
	is_builtin      bool // this function is defined in builtin/strconv
	pos             token.Position
	body_pos        token.Position
	file            string
	is_generic      bool
	is_direct_arr   bool // direct array access
	attrs           []table.Attr
pub mut:
	stmts           []Stmt
	return_type     table.Type
	comments        []Comment // comments *after* the header, but *before* `{`; used for InterfaceDecl
	source_file     &File = 0
	scope           &Scope
}

//function or method call
pub struct CallExpr {
pub:
	pos                token.Position
	left               Expr // `user` in `user.register()`
	mod                string
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
	generic_type       table.Type // TODO array, to support multiple types
	generic_list_pos   token.Position
	free_receiver      bool // true if the receiver expression needs to be freed
	scope              &Scope
	from_embed_type    table.Type // holds the type of the embed that the method is called from
}
```

### Function call

AST struct

```v
// function or method call expr
pub struct CallExpr {
pub:
	pos                token.Position
	left               Expr // `user` in `user.register()`
	mod                string
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
	generic_type       table.Type // TODO array, to support multiple types
	generic_list_pos   token.Position
	free_receiver      bool // true if the receiver expression needs to be freed
	scope              &Scope
	from_embed_type    table.Type // holds the type of the embed that the method is called from

```

### CallArg

AST struct

```v
// function call argument: `f(callarg)`
pub struct CallArg {
pub:
	is_mut          bool
	share           table.ShareType
	expr            Expr
	comments        []Comment
pub mut:
	typ             table.Type
	is_tmp_autofree bool // this tells cgen that a tmp variable has to be used for the arg expression in order to free it after the call
	pos             token.Position
	// tmp_name        string // for autofree
}
```

### Return

AST struct

```v
// function return statement
pub struct Return {
pub:
	pos      token.Position
	exprs    []Expr
	comments []Comment
pub mut:
	types    []table.Type
}
```

example code

```v
module main

fn main() {
	s := add(1, 3)
	println(s)
	s2 := add_generic(2, 4)
	s3 := add_generic<int>(2, 4)
	println(s2)
	println(s3)
}

// function
fn add(x int, y int) int {
	return x + y
}

struct Point {
	x int
	y int
}

// method
pub fn (p Point) move(a int, b int) (int, int) {
	new_x := p.x + a
	new_y := p.y + b
	return new_x, new_y
}

// generic function
fn add_generic<T>(x T, y T) T {
	return x + y
}
```

### Anonymous function

AST struct

```v
//anonymous function
pub struct AnonFn {
pub:
	decl FnDecl
pub mut:
	typ  table.Type
}
```

example code

```v
module main

fn main() {
	f1 := fn (x int, y int) int {
		return x + y
	}
	f1(1,3)
}
```

### DeferStmt

AST struct

```v
// TODO: handle this differently
// v1 excludes non current os ifdefs so
// the defer's never get added in the first place
pub struct DeferStmt {
pub:
	stmts []Stmt
	pos   token.Position
pub mut:
	ifdef string
}
```

example code

```v
fn main() {
	println('main start')
	// defer {defer_fn1()} 
	// defer {defer_fn2()}
	defer {
		defer_fn1()
	}
	defer {
		defer_fn2()
	}
	println('main end')
}

fn defer_fn1() {
	println('from defer_fn1')
}

fn defer_fn2() {
	println('from defer_fn2')
}
```

## Struct

### StructDecl

AST struct

```v
// struct declaration
pub struct StructDecl {
pub:
	pos          token.Position
	name         string
	gen_types    []table.Type
	is_pub       bool
	mut_pos      int // mut:
	pub_pos      int // pub:
	pub_mut_pos  int // pub mut:
	language     table.Language
	is_union     bool // if true,will generate C union,instead of struct
	attrs        []table.Attr
	end_comments []Comment
	embeds       []Embed
pub mut:
	fields       []StructField
}
```

### StructField

AST struct

```v
pub struct StructField {
pub:
	pos              token.Position
	type_pos         token.Position
	comments         []Comment
	default_expr     Expr
	has_default_expr bool
	attrs            []table.Attr
	is_public        bool
pub mut:
	name             string
	typ              table.Type
}
```

### Embed

AST struct

```v
// struct embed
pub struct Embed {
pub:
	typ table.Type
	pos token.Position
}
```

example code

```v
module main

[attr1]
[attr2]
struct Point { //comment 1
mut:
	x int [attr3]
	y int ['attr4=123']
pub mut:
	z int = 1
//end comment
}

fn main() {
}
```

example code of  embed

```v
module main

struct Widget {
mut:
	x int
	y int
}

pub fn (mut w Widget) move(x_step int, y_step int) {
	w.x += x_step
	w.y += y_step
}

struct Widget2 {
mut:
	z int
}

pub fn (mut w Widget2) move_z(z_step int) {
	w.z += z_step
}

struct Button {
	Widget //embed
	Widget2 //embed
	title string
}

fn main() {
	mut button := Button{
		title: 'Click me'
	}
	button.x = 3 // x comes from Widget
	button.z = 4 // z comes from Widget2
	println('x:$button.x,y:$button.y,z:$button.z')
	button.move(3, 4) // move comes from Widget
	println('x:$button.x,y:$button.y,z:$button.z')
	button.move_z(5) // move_z comes from Widget2
	println('x:$button.x,y:$button.y,z:$button.z')
}
```

### StructInit

AST struct

```v
// struct initial
pub struct StructInit {
pub:
	pos          token.Position
	is_short     bool
	pre_comments []Comment
pub mut:
	typ          table.Type
	fields       []StructInitField
	embeds       []StructInitEmbed
}
```

### StructInitField

```v
pub struct StructInitField {
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
```

### StructInitEmbed

```v
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
```

example code 

```v
module main

struct User {
	name string
	age int
}

fn add(u User) {
	println(u)
}

fn main(){
	add(User{name:'jack',age:22}) //standard
	add({name:'tom',age:23}) //short
	add(name:'tt',age:33) // more short
}
```

### Assoc

AST struct

```v
// create new variable by associate variable,`new_var := { var_name | key: val, key: val }`
pub struct Assoc {
pub:
	var_name string // `var_name`
	fields   []string // `key`
	exprs    []Expr // `val`
	pos      token.Position
pub mut:
	typ      table.Type
	scope    &Scope
}
```

example code

```v
struct Foo {
	a int
	b int
	c int = 7
}

fn main() {
	foo := Foo{
		a: 1
		b: 33
	}
	//associate
	foo2 := {
		foo |
		a: 42
		b: 10
	}
	println(foo2.a) // 42
	println(foo2.b) // 10
	println(foo2.c) // 7
}

```

## Interface

### InterfaceDecl

AST struct

```v
// interface declaration
pub struct InterfaceDecl {
pub:
	is_pub       bool
	name         string
	field_names  []string
	methods      []FnDecl // methods need to be implemented
	pos          token.Position
	pre_comments []Comment
}
```

example code

```v
module main

interface Speaker { //comment 1
	speak() string
	silent()
}
```

## Type

### Alias Type 

AST struct

```v
//Alias type declaration
pub struct AliasTypeDecl {
pub:
	name        string
	is_pub      bool
	parent_type table.Type
	pos         token.Position
	comments    []Comment
}
```

example code

```v
module main

struct Human {
	name string
}
type Myint =  int /*comment 1*/ //comment 2
type Person = Human
```

### Function Type

AST struct

```v
// function type declaration
pub struct FnTypeDecl {
pub:
	name     string
	is_pub   bool
	typ      table.Type
	pos      token.Position
	comments []Comment
}
```

example code

```v
module main

type Mid_fn = fn (int, string) int /*comment 1*/ //comment 2
```

### Sum  type

AST struct

```v
// sum type declaration
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
```

example code

```v
module main

struct User {
	name string
	age  int
}

type MySumtype = User | int | string //comment 1
```



## FlowControl

### Block

AST struct

```v
// `{stmts}` or `unsafe {stmts}`
pub struct Block {
pub:
	stmts     []Stmt
	is_unsafe bool
	pos       token.Position
}
```

example code

```v
fn main() {
	my_fn()
}

fn my_fn() {
	// block
	{
		println('in block')
	}
	// unsafe block
	unsafe {
	}
}
```

### if

#### IfExpr

AST struct

```v
// if statement or expr
pub struct IfExpr {
pub:
	is_comptime   bool // true, if it is `$if`
	tok_kind      token.Kind
	left          Expr // `a` in `a := if ...`
	pos           token.Position
	post_comments []Comment
pub mut:
	branches      []IfBranch // includes all `else if` branches
	is_expr       bool
	typ           table.Type
	has_else      bool
}
```

#### IfBranch

AST struct

```v
pub struct IfBranch {
pub:
	cond      Expr
	pos       token.Position
	body_pos  token.Position
	comments  []Comment
pub mut:
	stmts     []Stmt
	smartcast bool // true when cond is `x is SumType`, set in checker.if_expr // no longer needed with union sum types TODO: remove
	scope     &Scope
}
```

example code

```v
module main

fn main() {
	a := 10
	b := 20
	// if statement
	if a < b {
		println('$a < $b')
	} else if a > b {
		println('$a > $b')
	} else {
		println('$a == $b')
	}
	// if expr
	num := 777
	s := if num % 2 == 0 { 'even' } else { 'odd' }
	x, y, z := if true { 1, 'awesome', 13 } else { 0, 'bad', 0 }
	// compile time if 
	$if macos {
	} $else {
	}
}
```

#### IfGuardExpr

AST struct

```v
// `if [x := opt()] {`
pub struct IfGuardExpr {
pub:
	var_name  string
	expr      Expr
	pos       token.Position
pub mut:
	expr_type table.Type
}
```

example code(todo)

```v

```

### match

#### MatchExpr

AST struct

```v
// match statement or expr
pub struct MatchExpr {
pub:
	tok_kind      token.Kind
	cond          Expr
	branches      []MatchBranch
	pos           token.Position
pub mut:
	is_expr       bool // returns a value
	return_type   table.Type
	cond_type     table.Type // type of `x` in `match x {`
	expected_type table.Type // for debugging only
	is_sum_type   bool // true, if match sum type valiable
}
```

#### MatchBranch

AST struct

```v
pub struct MatchBranch {
pub:
	exprs         []Expr // left side
	ecmnts        [][]Comment // inline comments for each left side expr
	stmts         []Stmt // right side
	pos           token.Position
	comments      []Comment // comment above `xxx {`
	is_else       bool
	post_comments []Comment
pub mut:
	scope         &Scope
}
```

example code

```v
fn main() {
	os := 'macos'
	// match statement
	match os {
		'windows' { println('windows') }
		'macos', 'linux' { println('macos or linux') }
		else { println('unknow') }
	}
	// match expr
	price := match os {
		'windows' { 100 }
		'linux' { 120 }
		'macos' { 150 }
		else { 0 }
	}
	// multi assign 
	a, b, c := match false {
		true { 1, 2, 3 }
		false { 4, 5, 6 }
		else { 7, 8, 9 }
	}
}

type MySum = bool | int | string

pub fn (ms MySum) str() string {
	// match sum type
	match ms {
		int { return ms.str() }
		string { return ms }
		else { return 'unknown' }
	}
}
```



### for

#### ForCStmt

AST struct

```v
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
	label    string // `label: for {`
	scope    &Scope
}
```

example code

```v
fn main() {
	for i := 0; i < 10; i++ {
		if i == 6 {
			continue
		}
		if i == 10 {
			break
		}
		println(i)
	}
}
```

#### ForInStmt

AST struct

```v
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
	key_type   table.Type
	val_type   table.Type
	cond_type  table.Type
	kind       table.Kind // array/map/string
	label      string // `label: for {`
	scope      &Scope
}
```

example code

```v
fn main() {
	// string
	str := 'abcdef'
	for s in str {
		println(s.str())
	}
	// array
	numbers := [1, 2, 3, 4, 5]
	for num in numbers {
		println('num:$num')
	}
	// range
	mut sum := 0
	for i in 1 .. 11 {
		sum += i
	}
	// map
	m := {
		'name': 'jack'
		'age':  '20'
		'desc': 'good man'
	}
	for key, value in m {
		println('key:$key,value:$value')
	}
}
```

#### ForStmt

AST struct

```v
pub struct ForStmt {
pub:
	cond   Expr
	stmts  []Stmt
	is_inf bool // `for {}`
	pos    token.Position
pub mut:
	label  string // `label: for {`
	scope  &Scope
}
```

example code

```v
fn main() {
	mut sum := 0
	mut x := 0
	for x <= 100 {
		sum += x
		x++
	}
	println(sum)
	// label for
	mut i := 4
	goto L1
	L1: for { // label for
		i++
		for {
			if i < 7 {
				continue L1
			} else {
				break L1
			}
		}
	}
}
```

#### BranchStmt

AST struct

```V
// break, continue
pub struct BranchStmt {
pub:
	kind  token.Kind
	label string // use in label for, `x` in `continue x` or `break x`
	pos   token.Position
}
```

### goto

#### GotoLabel

AST struct

```v
// goto label
pub struct GotoLabel {
pub:
	name string
	pos  token.Position
}
```

#### GotoStmt

AST struct

```v
// goto statement
pub struct GotoStmt {
pub:
	name string
	pos  token.Position
}
```

example code

```v
fn main() {
	mut i := 0
	a: // goto label
	i++
	if i < 3 {
		goto a
	}
	println(i)
}
```

## Error handle

### OrExpr

AST struct

```v
pub enum OrKind {
	absent // `fn()`
	block // `fn() or { }`
	propagate // `fn()?`
}

// `or { ... }`
pub struct OrExpr {
pub:
	stmts []Stmt
	kind  OrKind
	pos   token.Position
}
```

### None

AST struct

```v
pub struct None {
pub:
	pos token.Position
	foo int // todo
}
```

example code

```v
fn my_fn(i int) ?int {
	if i == 0 {
		return error('Not ok!')
	}
	if i == 1 {
		return none
	}
	return i
}

fn main() {
	println('from main') // OrKind is absent
	v1 := my_fn(0) or { // OrKind is block
		println('from 0')
		panic(err)
	}
	v2 := my_fn(1) or { 
		println('from 1') 
		panic('error msg is $err')	
	}
	v3 := my_fn(2) or {
		println('from 2')
		return
	}
	v4 := my_fn(3) ? // OrKind is propagate
}

```

## Concurrent

### ChanInit

AST struct

```v
// concurrent channel initial
pub struct ChanInit {
pub:
	pos       token.Position
	cap_expr  Expr
	has_cap   bool
pub mut:
	typ       table.Type
	elem_type table.Type
}
```

### GoStmt

AST struct

```v
// concurrent go statement
pub struct GoStmt {
pub:
	call_expr Expr
	pos       token.Position
}
```

example code

```v
module main

const (
	num_iterations = 10000
)

fn do_send(ch chan int) {
	for i in 0 .. num_iterations {
		ch <- i
	}
}

fn main() {
	ch := chan int{cap: 1000} // chan init
	go do_send(ch) // go statement
	mut sum := i64(0)
	for _ in 0 .. num_iterations {
		sum += <-ch
		println(sum)
	}
}
```

### SelectExpr

AST struct

```v
//concurrent select statement
pub struct SelectExpr {
pub:
	branches      []SelectBranch
	pos           token.Position
	has_exception bool
pub mut:
	is_expr       bool // returns a value
	expected_type table.Type // for debugging only
}
```

### SelectBranch

AST struct

```v
pub struct SelectBranch {
pub:
	stmt          Stmt // `a := <-ch` or `ch <- a`
	stmts         []Stmt // right side
	pos           token.Position
	comment       Comment // comment above `select {`
	is_else       bool
	is_timeout    bool
	post_comments []Comment
}
```

example code

```v
import time
import sync

fn main() {
	ch1 := chan int{}
	ch2 := chan int{}
	go send(ch1, ch2)
	mut x := 0
	mut y := 0
	for {
		select { // 
			x = <-ch1 { // read channel
				println('$x')
			}
			y = <-ch2 {
				println('$y')
			}
			> 2 * time.second { // timeout
				break
			}
		}
	}
}

fn send(ch1 chan int, ch2 chan int) {
	ch1 <- 1
	ch2 <- 2
	ch1 <- 3
	ch2 <- 4
	ch1 <- 5
	ch2 <- 6
}
```

### LockExpr

AST struct

```v
pub struct LockExpr {
pub:
	stmts    []Stmt
	is_rlock bool
	pos      token.Position
pub mut:
	lockeds  []Ident // `x`, `y` in `lock x, y {`
	is_expr  bool
	typ      table.Type
}
```

example code(todo)

```v

```

## Unsafe

### UnsafeExpr

AST struct

```v
pub struct UnsafeExpr {
pub:
	expr Expr
	pos  token.Position
}
```

example code

```v
module main

fn main() {
	a := ['a', 'b', 'c']
	p := unsafe { &a[2] } // unsafe expr
	println(p)
}
```

## SQL

### SqlStmt

AST struct

```v
// sql statement: insert, update, delete
pub struct SqlStmt {
pub:
	kind            SqlStmtKind
	db_expr         Expr // `db` in `sql db {`
	object_var_name string // `user`
	table_type      table.Type
	pos             token.Position
	where_expr      Expr
	updated_columns []string // for `update set x=y`
	update_exprs    []Expr // for `update`
pub mut:
	table_name      string
	fields          []table.Field
}
```

### SqlExpr

AST struct

```v
// sql select statement
pub struct SqlExpr {
pub:
	typ         table.Type
	is_count    bool
	db_expr     Expr // `db` in `sql db {`
	where_expr  Expr
	has_where   bool
	has_offset  bool
	offset_expr Expr
	has_order   bool
	order_expr  Expr
	has_desc    bool
	is_array    bool
	table_type  table.Type
	pos         token.Position
	has_limit   bool
	limit_expr  Expr
pub mut:
	table_name  string
	fields      []table.Field
}
```

example code

```v
module main

import sqlite

struct Module {
	id           int
	name         string
	nr_downloads int
}

struct User {
	id             int
	age            int
	name           string
	is_customer    bool
	skipped_string string [skip]
}

struct Foo {
	age int
}

fn main() {
	db := sqlite.connect(':memory:') or { panic(err) }
	db.exec('drop table if exists User')
	db.exec("create table User (id integer primary key, age int default 0, name text default '', is_customer int default 0);")
	name := 'Peter'
	db.exec("insert into User (name, age) values ('Sam', 29)")
	db.exec("insert into User (name, age) values ('Peter', 31)")
	db.exec("insert into User (name, age, is_customer) values ('Kate', 30, 1)")
	nr_all_users := sql db {
		select count from User
	}
	println('nr_all_users=$nr_all_users')
	//
	nr_users1 := sql db {
		select count from User where id == 1
	}
	println('nr_users1=$nr_users1')
	//
	nr_peters := sql db {
		select count from User where id == 2 && name == 'Peter'
	}
	//
	new_user := User{
		name: 'New user'
		age: 30
	}
	sql db {
		insert new_user into User
	}
	sql db {
		update User set age = 31 where name == 'Kate'
	}
	sql db {
		delete from User where age == 34
	}
}

```

## Test

### AssertStmt

AST struct

```v
//assert statement in test
pub struct AssertStmt {
pub:
	pos  token.Position
pub mut:
	expr Expr
}
```

example code

```v
fn test_abc() {
	x := 1
	assert x == 1
}
```

## Compile time

### CompFor

AST struct

```v
//compile time for,`$for {}`
pub struct CompFor {
pub:
	val_var string
	stmts   []Stmt
	kind    CompForKind
	pos     token.Position
pub mut:
	// expr    Expr
	typ     table.Type
}
```

example code

```v
struct App {
	a string
	b string
mut:
	c int
	d f32
pub:
	e f32
	f u64
pub mut:
	g string
	h byte
}

fn (mut app App) m1() {
}

fn (mut app App) m2() {
}

fn (mut app App) m3() int {
	return 0
}

fn main() {
	$for field in App.fields {
		println('field: $field.name')
	}
	$for method in App.methods {
		println('method: $method.name')
	}
}

```

### ComptimeCall

AST struct

```v
pub struct ComptimeCall {
pub:
	method_name string
	left        Expr
	is_vweb     bool
	vweb_tmpl   File
	args_var    string
pub mut:
	sym         table.TypeSymbol
}
```

example code(todo)

```v

```

## C Integration

### GlobalDecl

AST struct

```v
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

// global valiable declaration
pub struct GlobalDecl {
pub:
	pos          token.Position
pub mut:
	fields       []GlobalField
	end_comments []Comment
}
```

example code

```v
module main

// single
__global ( g1 int )

// group
__global (
	g2 byteptr 
	g3 byteptr 
)

fn main() {
	g1 = 123
	println(g1)
	println(g2)
	println(g3)
}
```

### HashStmt

AST struct

```v
// #include etc
pub struct HashStmt {
pub:
	mod  string
	pos  token.Position
pub mut:
	val  string // example: 'include <openssl/rand.h> # please install openssl // comment'
	kind string // : 'include'
	main string // : '<openssl/rand.h>'
	msg  string // : 'please install openssl'
}
```

example code

```v
module main
    
#include <stdio.h> 

#flag -lmysqlclient
#flag linux -I/usr/include/mysql
#include <mysql.h>

fn main() {

}
```

### Likely

AST struct

```v
pub struct Likely {
pub:
	expr      Expr
	pos       token.Position
	is_likely bool // false for _unlikely_
}
```

example code

```v
module main

fn main() {
	x := 1
	if _likely_(x == 1) {
		println('a')
	} else {
		println('b')
	}
	if _unlikely_(x == 1) {
		println('a')
	} else {
		println('b')
	}
}
```

### CTempVar

AST struct

```v
// CTempVar is used in cgen only, to hold nodes for temporary variables
pub struct CTempVar {
pub:
	name   string // the name of the C temporary variable; used by g.expr(x)
	orig   Expr // the original expression, which produced the C temp variable; used by x.str()
	typ    table.Type // the type of the original expression
	is_ptr bool // whether the type is a pointer
}
```

example code(todo)

```v

```



## Comment

### Comment

AST struct

```v
pub struct Comment {
pub:
	text     string
	is_multi bool
	line_nr  int
	pos      token.Position
}
```

example code

```v
module main

/*
multi line comment
multi line comment
*/
// signle line comment
fn main() {
	x := 1 // behind statement comment
}
```

## Other

### AtExpr

AST struct

```v
// @FN, @STRUCT, @MOD etc. See full list in token.valid_at_tokens
pub struct AtExpr {
pub:
	name string
	pos  token.Position
	kind token.AtKind
pub mut:
	val  string
}
pub enum AtKind {
	unknown
	fn_name
	mod_name
	struct_name
	vexe_path
	file_path
	line_nr
	column_nr
	vhash
	vmod_file
}
```

example code

```v
module main

fn main() {
	println(@MOD)
	println(@FN)
	println(@STRUCT)
	println(@VEXE)
	println(@FILE)
	println(@LINE)
	println(@COLUMN)
	println(@VHASH)
	println(@VMOD_FILE)
}
```

```v
module main

fn main() {
	x := 1
	if _likely_(x == 1) {
		println('a')
	} else {
		println('b')
	}
	if _unlikely_(x == 1) {
		println('a')
	} else {
		println('b')
	}
}
```
