// Copyright (c) 2019-2020 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module compiler

import strings

struct Table {
pub mut:
	typesmap              map[string]Type
	consts                []Var
	fns                   map[string]Fn
	obf_ids               map[string]int // obf_ids['myfunction'] == 23
	modules               []string // List of all modules registered by the application
	imports               []string // List of all imports
	cflags                []CFlag // ['-framework Cocoa', '-lglfw3']
	fn_cnt                int // atomic
	obfuscate             bool
	varg_access           []VargAccess
	// enum_vals map[string][]string
	// names        []Name
	max_field_len         map[string]int // for vfmt: max_field_len['Parser'] == 12
	generic_struct_params map[string][]string
	tuple_variants		  map[string][]string // enum( Bool(BoolExpr) )
	sum_types			  map[string][]string // SumType -> [Variants]
}

struct VargAccess {
	fn_name string
	tok_idx int
	index   int
}

enum NameCategory {
	constant
	mod
	var
	typ
}

struct Name {
	cat NameCategory
	idx int // e.g. typ := types[name.idx]
}

enum AccessMod {
	private // private immutable
	private_mut // private mutable
	public // public immutable (readonly)
	public_mut // public, but mutable only in this module
	global // public and mutable both inside and outside (not recommended to use, that's why it's so verbose)
}

fn (a []AccessMod) contains(b AccessMod) bool {
	for elm in a {
		if elm == b {
			return true
		}
	}
	return false
}

enum TypeCategory {
	builtin
	struct_
	func // 2
	interface_
	enum_
	union_ // 5
	c_struct
	c_typedef
	objc_interface // 8 Objective C @interface
	array
	alias // `type myint int`
}

struct Var {
pub mut:
	typ         string
	name        string
	idx         int // index in the local_vars array
	is_arg      bool
	is_const    bool
	args        []Var // function args
	attr        string // [json] etc
	is_mut      bool
	is_alloc    bool
	is_returned bool
	ptr         bool
	ref         bool
	parent_fn   string // Variables can only be defined in functions
	mod         string // module where this var is stored
	access_mod  AccessMod
	is_global   bool // __global (translated from C only)
	is_used     bool
	is_changed  bool
	scope_level int
	is_c        bool // todo remove once `typ` is `Type`, not string
	is_moved    bool
	line_nr     int
	token_idx   int // this is a token index, which will be used by error reporting
	is_for_var  bool
	is_public   bool // for consts
}

struct Type {
pub mut:
	mod            string
	name           string
	cat            TypeCategory
	is_public      bool
	fields         []Var
	methods        []Fn
	parent         string
	func           Fn // For cat == FN (type myfn fn())
	is_c           bool // `C.FILE`
	enum_vals      []string
	gen_types      []string
	default_vals   []string // `struct Foo { bar int = 2 }`
	parser_idx     int
	decl_tok_idx   int
	// `is_placeholder` is used for types that are not defined yet but are known to exist.
	// It allows having things like `fn (f Foo) bar()` before `Foo` is defined.
	// This information is needed in the first pass.
	is_placeholder bool
	gen_str        bool // needs `.str()` method generation
	is_flag        bool // enum bitfield flag
	// max_field_len  int
	is_generic     bool
	ctype_names    []string
}

struct TypeNode {
mut:
	next &TypeNode
	typ  Type
}

/*
// For debugging types
pub fn (t Type) str() string {
	mut s := 'type "$t.name" {'
	if t.fields.len > 0 {
		// s += '\n    $t.fields.len fields:\n'
		for field in t.fields {
			s += '\n    $field.name $field.typ'
		}
		s += '\n'
	}
	if t.methods.len > 0 {
		// s += '\n    $t.methods.len methods:\n'
		for method in t.methods {
			s += '\n    ${method.str()}'
		}
		s += '\n'
	}
	s += '}\n'
	return s
}
*/


const (
	c_reserved = ['delete', 'exit', 'unix',
	// 'print',
	// 'ok',
	'error', 'malloc',
//'calloc',
'free', 'panic',
	// Full list of C reserved words, from: https://en.cppreference.com/w/c/keyword
	'auto', 'char', 'default', 'do', 'double', 'extern', 'float', 'inline', 'int', 'long', 'register', 'restrict', 'short', 'signed', 'sizeof', 'static', 'switch', 'typedef', 'union', 'unsigned', 'void', 'volatile', 'while', ]
)
// This is used for debugging only
pub fn (f Fn) str() string {
	t := Table{
	}
	str_args := f.str_args(t)
	return '${f.name}($str_args) $f.typ'
}

pub fn (t &Table) debug_fns() string {
	mut s := strings.new_builder(1000)
	for _, f in t.fns {
		s.writeln(f.name)
	}
	return s.str()
}

// fn (types array_Type) print_to_file(f string)  {
// }
const (
	integer_types = ['int', 'i8', 'char', 'byte', 'i16', 'u16', 'u32', 'i64', 'u64']
	float_types = ['f32', 'f64']
	reserved_type_param_names = ['R', 'S', 'T', 'U', 'W']
	pointer_types = ['byte*', 'byteptr', 'char*', 'charptr', 'void*', 'voidptr', 'voidptr*', 'intptr']
	builtin_types = ['int', 'i8', 'char', 'byte', 'i16', 'u16', 'u32', 'i64', 'u64',
		'f64', 'f32', 'byteptr', 'charptr', 'voidptr', 'intptr', 'string', 'ustring']
)

fn is_number_type(typ string) bool {
	return typ in integer_types || typ in float_types
}

fn is_integer_type(typ string) bool {
	return typ in integer_types
}

fn is_float_type(typ string) bool {
	return typ in float_types
}

fn is_primitive_type(typ string) bool {
	return is_number_type(typ) || typ == 'string'
}

fn is_pointer_type(typ string) bool {
    return typ in pointer_types
}

/*
fn (t mut Table) register_enum_val(typ, val string) {
	if t.enum_vals.len == 0 {
		t.enum_vals = [val]
	}
}
*/


fn new_table(obfuscate bool) &Table {
	mut t := &Table{
		obfuscate: obfuscate
		// enum_vals: map[string][]string

	}
	t.register_builtin('int')
	t.register_builtin('size_t')
	t.register_type_with_parent('i8', 'int')
	t.register_type_with_parent('byte', 'int')
	t.register_type_with_parent('char', 'int') // for C functions only, to avoid warnings
	t.register_type_with_parent('i16', 'int')
	t.register_type_with_parent('u16', 'u32')
	t.register_type_with_parent('u32', 'int')
	t.register_type_with_parent('i64', 'int')
	t.register_type_with_parent('u64', 'u32')
	t.register_builtin('byteptr')
	t.register_builtin('charptr')
	t.register_builtin('intptr')
	t.register_builtin('f32')
	t.register_builtin('f64')
	t.register_builtin('rune')
	t.register_builtin('bool')
	t.register_builtin('void')
	t.register_builtin('voidptr')
	t.register_builtin('va_list')
	for c in reserved_type_param_names {
		t.register_builtin(c)
	}
	t.register_const('stdin', 'int', 'main', true)
	t.register_const('stdout', 'int', 'main', true)
	t.register_const('stderr', 'int', 'main', true)
	t.register_const('errno', 'int', 'main', true)
	t.register_type_with_parent('map_string', 'map')
	t.register_type_with_parent('map_int', 'map')
	return t
}

// If `name` is a reserved C keyword, returns `v_name` instead.
fn (t &Table) var_cgen_name(name string) string {
	if name in c_reserved {
		return 'v_$name'
	}
	else {
		return name
	}
}

fn (t mut Table) register_module(mod string) {
	if mod in t.modules {
		return
	}
	t.modules << mod
}

fn (p mut Parser) register_array(typ string) {
	if typ.contains('*') {
		println('bad arr $typ')
		return
	}
	if !p.table.known_type(typ) {
		p.register_type_with_parent(typ, 'array')
		p.cgen.typedefs << 'typedef array $typ;'
	}
}

fn (p mut Parser) register_map(typ string) {
	if typ.contains('*') {
		println('bad map $typ')
		return
	}
	if !p.table.known_type(typ) {
		p.register_type_with_parent(typ, 'map')
		p.cgen.typedefs << 'typedef map $typ;'
	}
}

fn (table &Table) known_mod(mod string) bool {
	return mod in table.modules
}

fn (t mut Table) register_const(name, typ, mod string, is_pub bool) {
	t.consts << Var{
		name: name
		typ: typ
		is_const: true
		mod: mod
		idx: -1
		is_public: is_pub
	}
}

// Only for translated code
fn (p mut Parser) register_global(name, typ string) {
	p.table.consts << Var{
		name: name
		typ: typ
		is_const: true
		is_global: true
		mod: p.mod
		is_mut: true
		idx: -1
	}
}

// Only for module functions, not methods.
// That's why searching by fn name works.
fn (t mut Table) register_fn(new_fn Fn) {
	t.fns[new_fn.name] = new_fn
}

fn (table &Table) known_type(typ_ string) bool {
	mut typ := typ_
	// 'byte*' => look up 'byte', but don't mess up fns
	if typ.ends_with('*') && !typ.contains(' ') {
		typ = typ.replace('*', '')
	}
	t := table.typesmap[typ]
	return t.name.len > 0 && !t.is_placeholder
}

fn (table &Table) known_type_fast(t &Type) bool {
	return t.name != '' && !t.is_placeholder
}

fn (t &Table) find_fn(name string) ?Fn {
	f := t.fns[name]
	if f.name.str != 0 {
		// TODO
		return f
	}
	return none
}

fn (t &Table) find_fn_is_script(name string, is_script bool) ?Fn {
	mut f := t.fns[name]
	if f.name.str != 0 {
		// TODO
		return f
	}
	// V script? Try os module.
	if is_script {
		println('trying replace $name')
		f = t.fns[name.replace('main__', 'os__')]
		if f.name.str != 0 {
			return f
		}
	}
	return none
}

fn (t &Table) known_fn(name string) bool {
	_ = t.find_fn(name) or {
		return false
	}
	return true
}

fn (t &Table) known_const(name string) bool {
	_ = t.find_const(name) or {
		return false
	}
	return true
}

fn (t mut Table) register_builtin(typ string) {
	if typ.len == 0 {
		return
	}
	if typ in t.typesmap {
		return
	}
	t.typesmap[typ] = Type{
		name: typ
		is_public: true
	}
}

fn (p mut Parser) register_type_with_parent(strtyp, parent string) {
	typ := Type{
		name: strtyp
		parent: parent
		mod: p.mod
		is_public: true
	}
	p.table.register_type(typ)
}

fn (t mut Table) register_type_with_parent(typ, parent string) {
	if typ.len == 0 {
		return
	}
	t.typesmap[typ] = Type{
		name: typ
		parent: parent
		is_public: true
		// mod: mod

	}
}

fn (t mut Table) register_type(typ Type) {
	if typ.name.len == 0 {
		return
	}
	t.typesmap[typ.name] = typ
}

fn (t mut Table) rewrite_type(typ Type) {
	if typ.name.len == 0 {
		return
	}
	t.typesmap[typ.name] = typ
}

fn (table mut Table) add_field(type_name, field_name, field_type string, is_mut bool, attr string, access_mod AccessMod) {
	if type_name == '' {
		print_backtrace()
		verror('add_field: empty type')
	}
	mut t := table.typesmap[type_name]
	t.fields << Var{
		name: field_name
		typ: field_type
		is_mut: is_mut
		attr: attr
		parent_fn: type_name // Name of the parent type

		access_mod: access_mod
	}
	table.typesmap[type_name] = t
}

fn (table mut Table) add_default_val(idx int, type_name, val_expr string) {
	mut t := table.typesmap[type_name]
	if t.default_vals.len == 0 {
		t.default_vals = [''].repeat(t.fields.len)
	}
	t.default_vals[idx] = val_expr
	table.typesmap[type_name] = t
}

fn (t &Type) has_field(name string) bool {
	_ = t.find_field(name) or {
		return false
	}
	return true
}

fn (t &Type) has_enum_val(name string) bool {
	return name in t.enum_vals
}

fn (t &Type) find_field(name string) ?Var {
	for field in t.fields {
		if field.name == name {
			return field
		}
	}
	return none
}

fn (table &Table) type_has_field(typ &Type, name string) bool {
	_ = table.find_field(typ, name) or {
		return false
	}
	return true
}

fn (table &Table) find_field(typ &Type, name string) ?Var {
	for field in typ.fields {
		if field.name == name {
			return field
		}
	}
	if typ.parent != '' {
		parent := table.find_type(typ.parent)
		for field in parent.fields {
			if field.name == name {
				return field
			}
		}
	}
	return none
}

fn (p mut Parser) add_method(type_name string, f Fn) {
	if !p.first_pass() && f.name != 'str' {
		return
	}
	if type_name == '' {
		print_backtrace()
		verror('add_method: empty type')
	}
	// TODO table.typesmap[type_name].methods << f
	mut t := p.table.typesmap[type_name]
	if f.name != 'str' && f in t.methods {
		p.error('redefinition of method `${type_name}.$f.name`')
	}
	t.methods << f
	p.table.typesmap[type_name] = t
}

fn (t &Type) has_method(name string) bool {
	_ = t.find_method(name) or {
		return false
	}
	return true
}

fn (table &Table) type_has_method(typ &Type, name string) bool {
	_ = table.find_method(typ, name) or {
		return false
	}
	return true
}

fn (table &Table) find_method(typ &Type, name string) ?Fn {
	t := table.typesmap[typ.name]
	for method in t.methods {
		if method.name == name {
			return method
		}
	}
	if typ.parent != '' {
		parent := table.find_type(typ.parent)
		for method in parent.methods {
			if method.name == name {
				return method
			}
		}
		return none
	}
	return none
}

fn (t &Type) find_method(name string) ?Fn {
	// println('$t.name find_method($name) methods.len=$t.methods.len')
	for method in t.methods {
		// println('method=$method.name')
		if method.name == name {
			return method
		}
	}
	return none
}

fn (table mut Table) add_gen_type(type_name, gen_type string) {
	mut t := table.typesmap[type_name]
	if gen_type in t.gen_types {
		return
	}
	t.gen_types << gen_type
	table.typesmap[type_name] = t
}

fn (p &Parser) find_type(name string) Type {
	typ := p.table.find_type(name)
	if typ.name == '' {
		return p.table.find_type(p.prepend_mod(name))
	}
	return typ
}

fn (t &Table) find_type(name_ string) Type {
	mut name := name_
	if name.ends_with('*') && !name.contains(' ') {
		name = name.replace('*', '')
	}
	if !(name in t.typesmap) {
		// println('ret Type')
		return Type{
		}
	}
	return t.typesmap[name]
}

fn (p mut Parser) check_types2(got_, expected_ string, throw bool) bool {
	//if p.fileis('type_test') {
		//println('got=$got_ exp=$expected_')
	//}
	mut got := got_
	mut expected := expected_
	// p.log('check types got="$got" exp="$expected"  ')
	if p.pref.translated {
		return true
	}
	if got == expected {
		return true
	}
	// generic return type
	if expected == '_ANYTYPE_' {
		p.cur_fn.typ = got
		return true
	}
	if throw && p.base_type(got) == p.base_type(expected) {
		return true
	}
	// variadic
	if expected.starts_with('varg_') {
		expected = expected[5..]
	}
	if got.starts_with('varg_') {
		got = got[5..]
	}
	// fn == 0 temporary
	if got == 'int' && expected.ends_with('Fn') {
		return true
	}
	// Allow ints to be used as floats
	if got == 'int' && expected == 'f32' {
		return true
	}
	if got == 'int' && expected == 'f64' {
		return true
	}
	if got == 'f64' && expected == 'f32' {
		return true
	}
	if got == 'f32' && expected == 'f64' {
		return true
	}
	// Allow ints to be used as longs
	if got == 'int' && expected == 'i64' {
		return true
	}
	if got == 'void*' && expected.starts_with('fn ') {
		return true
	}
	if got.starts_with('[') && expected == 'byte*' {
		return true
	}
	// Todo void* allows everything right now
	if got == 'void*' || expected == 'void*' {
		// || got == 'cvoid' || expected == 'cvoid' {
		return true
	}
	// TODO only allow numeric consts to be assigned to bytes, and
	// throw an error if they are bigger than 255
	if got == 'int' && expected == 'byte' {
		return true
	}
	if got == 'byteptr' && expected == 'byte*' {
		return true
	}
	if got == 'byte*' && expected == 'byteptr' {
		return true
	}
	if got == 'charptr' && expected == 'char*' {
		return true
	}
	if got == 'char*' && expected == 'charptr' {
		return true
	}
	if got == 'int' && expected == 'byte*' {
		return true
	}
	// if got=='int' && expected=='voidptr*' {
	// return true
	// }
	// byteptr += int
	if got == 'int' && expected in ['byteptr', 'charptr'] {
		return true
	}
	if got == 'Option' && expected.starts_with('Option_') {
		return true
	}
	// lines := new_array
	if got == 'array' && expected.starts_with('array_') {
		return true
	}
	// Expected type "Option_os__File", got "os__File"
	if expected.starts_with('Option_') && expected.ends_with(stringify_pointer(got)) {
		return true
	}
	// NsColor* return 0
	if expected.ends_with('*') && got == 'int' {
		return true
	}
	// if got == 'T' || got.contains('<T>') {
	// return true
	// }
	// if expected == 'T' || expected.contains('<T>') {
	// return true
	// }
	// TODO fn hack
	if got.starts_with('fn ') && (expected.ends_with('fn') || expected.ends_with('Fn')) {
		return true
	}
	if got.starts_with('fn ') && expected.starts_with('fn ') && p.mod == 'gg2' {
		return true
	}
	// Allow pointer arithmetic
	if expected == 'void*' && got == 'int' {
		return true
	}
	// if p.fileis('_test') && is_number_type(got) && is_number_type(expected)  {
	// p.warn('got=$got exp=$expected $p.is_const_literal')
	// }
	// Allow `myu64 == 1`, `myfloat == 2` etc
	if is_integer_type(got) && is_number_type(expected) && p.is_const_literal {
		return true
	}
	if expected == 'integer' {
		if is_integer_type(got) {
			return true
		}
		else {
			p.error('expected type `$expected`, but got `$got`')
		}
	}
	expected = expected.replace('*', '')
	got = got.replace('*', '').replace('ptr', '')
	if got != expected {
		// Interface check
		if expected.ends_with('er') || expected[0] == `I` {
			if p.satisfies_interface(expected, got, throw) {
				return true
			}
		}
		// Sum type
		if expected in p.table.sum_types {
			//println('checking sum')
			if got in p.table.sum_types[expected] {
				//println('yep $expected')
				return true
			}
		}
		if !throw {
			return false
		}
		else {
			p.error('cannot convert `$got` to `$expected`')
		}
	}
	return true
}

fn (p mut Parser) base_type(name string) string {
	typ := p.find_type(name)
	if typ.parent != '' {
		return p.base_type(typ.parent)
	}
	return name
}

// throw by default
fn (p mut Parser) check_types(got, expected string) bool {
	if p.first_pass() {
		return true
	}
	return p.check_types2(got, expected, true)
}

fn (p mut Parser) check_types_no_throw(got, expected string) bool {
	return p.check_types2(got, expected, false)
}

fn (p mut Parser) check_types_with_token_index(got, expected string, var_token_idx int) {
	if !p.check_types2(got, expected, false) {
		p.error_with_token_index('expected type `$expected`, but got `$got`', var_token_idx)
	}
}

fn (p mut Parser) satisfies_interface(interface_name, _typ string, throw bool) bool {
	int_typ := p.table.find_type(interface_name)
	typ := p.table.find_type(_typ)
	for method in int_typ.methods {
		if !typ.has_method(method.name) {
			// if throw {
			p.error("type `$_typ` doesn\'t satisfy interface " + '`$interface_name` (method `$method.name` is not implemented)')
			// }
			return false
		}
	}
	return true
}

fn (table &Table) is_interface(name string) bool {
	if !(name in table.typesmap) {
		return false
	}
	t := table.typesmap[name]
	return t.cat == .interface_
}

// Do we have fn main()?
fn (t &Table) main_exists() bool {
	for _, f in t.fns {
		if f.name == 'main__main' {
			return true
		}
	}
	return false
}

fn (t &Table) all_test_function_names() []string {
	mut fn_begin_test_name := ''
	mut fn_end_test_name := ''

	mut fn_test_names := []string
	for _, f in t.fns {
		if f.name.contains('__test_') {
			fn_test_names << f.name
		}
		else if f.name.contains('__testsuite_begin') {
			fn_begin_test_name = f.name
		}
		else if f.name.contains('__testsuite_end') {
			fn_end_test_name = f.name
		}
	}
	if fn_begin_test_name.len == 0 {
		if fn_end_test_name.len > 0 {
			fn_test_names << fn_end_test_name
		}
		return fn_test_names
	}
	else {
		mut res := []string
		res << fn_begin_test_name
		res << fn_test_names
		if fn_end_test_name.len > 0 {
			res << fn_end_test_name
		}
		return res
	}
}

fn (t &Table) find_const(name string) ?Var {
	// println('find const l=$t.consts.len')
	for c in t.consts {
		if c.name == name {
			return c
		}
	}
	return none
}

// ('s', 'string') => 'string s'
// ('nums', '[20]byte') => 'byte nums[20]'
// ('myfn', 'fn(int) string') => 'string (*myfn)(int)'
fn (table &Table) cgen_name_type_pair(name, typ string) string {
	// Special case for [10]int
	if typ.len > 0 && typ[0] == `[` {
		tmp := typ.all_after(']')
		size := typ.all_before(']')
		return '$tmp $name  $size ]'
	}
	// fn()
	else if typ.starts_with('fn (') {
		T := table.find_type(typ)
		if T.name == '' {
			eprintln('function type `$typ` not found')
			exit(1)
		}
		str_args := T.func.str_args(table)
		return '$T.func.typ (*$name)( $str_args /*FFF*/ )'
	}
	// TODO tm hack, do this for all C struct args
	else if typ == 'tm' {
		return 'struct /*TM*/ tm $name'
	}
	return '$typ $name'
}

fn is_valid_int_const(val, typ string) bool {
	x := val.int()
	return match typ {
		'char'{
			0 <= x && x <= 255
		}
		'byte'{
			0 <= x && x <= 255
		}
		'u16'{
			0 <= x && x <= 65535
		}
		// case 'u32': return 0 <= x && x <= math.MaxU32
		// case 'u64': return 0 <= x && x <= math.MaxU64
		// ////////////
		'i8'{
			-128 <= x && x <= 127
		}
		/*
	case 'i16': return math.min_i16 <= x && x <= math.max_i16
	case 'int': return math.min_i32 <= x && x <= math.max_i32
	*/

		// case 'i64':
		// x64 := val.i64()
		// return i64(-(1<<63)) <= x64 && x64 <= i64((1<<63)-1)
		else {
			true}}
}

fn (p mut Parser) typ_to_fmt(typ string, level int) string {
	t := p.table.find_type(typ)
	if t.cat == .enum_ {
		return '%d'
	}
	match typ {
		'string' {
			return '%.*s'
		}
		// case 'bool': return '%.*s'
		'ustring' {
			return '%.*s'
		}
		'byte', 'bool', 'int', 'char', 'i16', 'i8' {
			return '%d'
		}
		'u16', 'u32' {
			return '%u'
		}
		'f64', 'f32' {
			return '%f'
		}
		'i64' {
			return '%lld'
		}
		'u64' {
			return '%llu'
		}
		'byte*', 'byteptr' {
			return '%s'
		}
		// case 'array_string': return '%s'
		// case 'array_int': return '%s'
		'void' {
			p.error('cannot interpolate this value')
		}
		else {
			if typ.ends_with('*') {
				return '%p'
			}
		}}
	if t.parent != '' && level == 0 {
		return p.typ_to_fmt(t.parent, level + 1)
	}
	return ''
}

fn type_to_safe_str(typ string) string {
	r := typ.replace(' ', '').replace('(', '_').replace(')', '_')
	return r
}

fn is_compile_time_const(s_ string) bool {
	s := s_.trim_space()
	if s == '' {
		return false
	}
	if s.contains("\'") {
		return true
	}
	for c in s {
		if !((c >= `0` && c <= `9`) || c == `.`) {
			return false
		}
	}
	return true
}

fn (t &Type) contains_field_type(typ string) bool {
	if !t.name[0].is_capital() {
		return false
	}
	for field in t.fields {
		if field.typ == typ {
			return true
		}
	}
	return false
}

// check for a function / variable / module typo in `name`
fn (p &Parser) identify_typo(name string) string {
	// dont check if so short
	if name.len < 2 {
		return ''
	}
	name_dotted := mod_gen_name_rev(name.replace('__', '.'))
	min_match := 0.50 // for dice coefficient between 0.0 - 1.0
	mut output := ''
	// check imported modules
	mut n := p.table.find_misspelled_imported_mod(name_dotted, p, min_match)
	if n.len > 0 {
		output += '\n  * module: `$n`'
	}
	// check consts
	n = p.table.find_misspelled_const(name, p, min_match)
	if n != '' {
		output += '\n  * const: `$n`'
	}
	// check types
	typ,type_cat := p.table.find_misspelled_type(name, p, min_match)
	if typ.len > 0 {
		output += '\n  * $type_cat: `$typ`'
	}
	// check functions
	n = p.table.find_misspelled_fn(name, p, min_match)
	if n.len > 0 {
		output += '\n  * function: `$n`'
	}
	// check function local variables
	n = p.find_misspelled_local_var(name_dotted, min_match)
	if n.len > 0 {
		output += '\n  * variable: `$n`'
	}
	return output
}

// compare just name part, some items are mod prefied
fn typo_compare_name_mod(a, b, b_mod string) f32 {
	if a.len - b.len > 2 || b.len - a.len > 2 {
		return 0
	}
	auidx := a.index('__') or {
		return 0
	} // TODO or {-1} once cgen lines bug is fixed //-1 }
	buidx := b.index('__') or {
		return 0
	} // -1 }
	a_mod := if auidx != -1 { mod_gen_name_rev(a[..auidx]) } else { '' }
	a_name := if auidx != -1 { a[auidx + 2..] } else { a }
	b_name := if buidx != -1 { b[buidx + 2..] } else { b }
	if a_mod.len > 0 && b_mod.len > 0 && a_mod != b_mod {
		return 0
	}
	return strings.dice_coefficient(a_name, b_name)
}

// find function with closest name to `name`
fn (table &Table) find_misspelled_fn(name string, p &Parser, min_match f32) string {
	mut closest := f32(0)
	mut closest_fn := ''
	for _, f in table.fns {
		if f.name.contains('__') && !p.is_mod_in_scope(f.mod) {
			continue
		}
		c := typo_compare_name_mod(name, f.name, f.mod)
		if c > closest {
			closest = c
			closest_fn = mod_gen_name_rev(f.name.replace('__', '.'))
		}
	}
	return if closest >= min_match { closest_fn } else { '' }
}

// find imported module with closest name to `name`
fn (table &Table) find_misspelled_imported_mod(name string, p &Parser, min_match f32) string {
	mut closest := f32(0)
	mut closest_mod := ''
	n1 := if name.starts_with('main.') { name[5..] } else { name }
	for alias, mod in p.import_table.imports {
		c := typo_compare_name_mod(n1, alias, '')
		if c > closest {
			closest = c
			closest_mod = if alias == mod { alias } else { '$alias ($mod)' }
		}
	}
	return if closest >= min_match { closest_mod } else { '' }
}

// find const with closest name to `name`
fn (table &Table) find_misspelled_const(name string, p &Parser, min_match f32) string {
	mut closest := f32(0)
	mut closest_const := ''
	for cnst in table.consts {
		if cnst.name.contains('__') && !p.is_mod_in_scope(cnst.mod) {
			continue
		}
		c := typo_compare_name_mod(name, cnst.name, cnst.mod)
		if c > closest {
			closest = c
			closest_const = mod_gen_name_rev(cnst.name.replace('__', '.'))
		}
	}
	return if closest >= min_match { closest_const } else { '' }
}

// find type with closest name to `name`
fn (table &Table) find_misspelled_type(name string, p &Parser, min_match f32) (string,string) {
	mut closest := f32(0)
	mut closest_type := ''
	mut type_cat := ''
	for _, typ in table.typesmap {
		if typ.name.contains('__') && !p.is_mod_in_scope(typ.mod) {
			continue
		}
		c := typo_compare_name_mod(name, typ.name, typ.mod)
		if c > closest {
			closest = c
			closest_type = mod_gen_name_rev(typ.name.replace('__', '.'))
			type_cat = type_cat_str(typ.cat)
		}
	}
	if closest >= min_match {
		return closest_type,type_cat
	}
	return '',''
}

fn type_cat_str(tc TypeCategory) string {
	tc_str := match tc {
		.builtin{
			'builtin'
		}
		.struct_{
			'struct'
		}
		.func{
			'function'
		}
		.interface_{
			'interface'
		}
		.enum_{
			'enum'
		}
		.union_{
			'union'
		}
		.c_struct{
			'C struct'
		}
		.c_typedef{
			'C typedef'
		}
		.objc_interface{
			'obj C interface'
		}
		.array{
			'array'
		}
		.alias{
			'type alias'
		}
		else {
			'unknown'}}
	return tc_str
}
