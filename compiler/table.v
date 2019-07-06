// Copyright (c) 2019 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.

module main

struct Table {
mut:
	types     []Type
	consts    []Var
	fns       map[string]Fn 
	obf_ids   map[string]int // obf_ids['myfunction'] == 23
	packages  []string // List of all modules registered by the application
	imports   []string // List of all imports
	flags     []string //  ['-framework Cocoa', '-lglfw3']
	fn_cnt    int atomic
	obfuscate bool
}

enum AccessMod {
	PRIVATE        // private immutable
	PRIVET_MUT     // private mutable
	PUBLIC         // public immmutable (readonly)
	PUBLIC_MUT     // public, but mutable only in this module
	PUBLIC_MUT_MUT // public and mutable both inside and outside (not recommended to use, that's why it's so verbose)
}

struct Type {
mut:
	pkg            string
	name           string
	fields         []Var
	methods        []Fn
	parent         string
	func           Fn // For cat == FN (type kek fn())
	is_c           bool // C.FILE
	is_interface   bool
	is_enum        bool
	// This field is used for types that are not defined yet but are known to exist.
	// It allows having things like `fn (f Foo) bar()` before `Foo` is defined.
	// This information is needed in the first pass.
	is_placeholder bool
}

// For debugging types
fn (t Type) str() string {
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

const (
	CReserved = [
		'exit',
		'unix',
		'print',
		// 'ok',
		'error',
		'malloc',
		'calloc',
		'char',
		'free',
		'panic',
		'register'
	]

)

// This is used in generated C code
fn (f Fn) str() string {
	t := Table{}
	str_args := f.str_args(t)
	return '$f.name($str_args) $f.typ'
}

// fn (types array_Type) print_to_file(f string)  {
// }
const (
	NUMBER_TYPES = ['number', 'int', 'i8', 'u8', 'i16', 'u16', 'i32', 'u32', 'byte', 'i64', 'u64', 'f32', 'f64']
	FLOAT_TYPES  = ['f32', 'f64']
)

fn is_number_type(typ string) bool {
	return NUMBER_TYPES.contains(typ)
}

fn is_float_type(typ string) bool {
	return FLOAT_TYPES.contains(typ)
}

fn new_table(obfuscate bool) *Table {
	mut t := &Table {
		obf_ids: map[string]int{}
		fns: map[string]Fn{}
		obfuscate: obfuscate
	}
	t.register_type('int')
	t.register_type('size_t')
	t.register_type_with_parent('i8', 'int')
	t.register_type_with_parent('u8', 'int')
	t.register_type_with_parent('i16', 'int')
	t.register_type_with_parent('u16', 'int')
	t.register_type_with_parent('i32', 'int')
	t.register_type_with_parent('u32', 'int')
	t.register_type_with_parent('byte', 'int')
	// t.register_type_with_parent('i64', 'int')
	t.register_type('i64')
	t.register_type_with_parent('u64', 'int')
	t.register_type('byteptr')
	t.register_type('intptr')
	t.register_type('f32')
	t.register_type('f64')
	t.register_type('rune')
	t.register_type('bool')
	t.register_type('void')
	t.register_type('voidptr')
	t.register_type('va_list')
	t.register_const('stdin', 'int', 'main', false)
	t.register_const('stdout', 'int', 'main', false)
	t.register_const('stderr', 'int', 'main', false)
	t.register_const('errno', 'int', 'main', false)
	t.register_type_with_parent('map_string', 'map')
	t.register_type_with_parent('map_int', 'map')
	return t
}

// If `name` is a reserved C keyword, returns `v_name` instead.
fn (t mut Table) var_cgen_name(name string) string {
	if CReserved.contains(name) {
		return 'v_$name'
	}
	else {
		return name
	}
}

fn (t mut Table) register_package(pkg string) {
	if t.packages.contains(pkg) {
		return
	}
	t.packages << pkg
}

fn (table &Table) known_pkg(pkg string) bool {
	return pkg in table.packages
}

fn (t mut Table) register_const(name, typ, pkg string, is_imported bool) {
	t.consts << Var {
		name: name
		typ: typ
		is_const: true
		is_import_const: is_imported
		pkg: pkg
	}
}

// Only for translated code
fn (p mut Parser) register_global(name, typ string) {
	p.table.consts << Var {
		name: name
		typ: typ
		is_const: true
		is_global: true
		pkg: p.pkg
	}
}

fn (t mut Table) register_fn(new_fn Fn) {
	t.fns[new_fn.name] = new_fn 
}

fn (table &Table) known_type(typ string) bool {
	// 'byte*' => look up 'byte', but don't mess up fns
	if typ.ends_with('*') && !typ.contains(' ') {
		typ = typ.left(typ.len - 1)
	}
	for t in table.types {
		if t.name == typ && !t.is_placeholder {
			return true
		}
	}
	return false
}

fn (t &Table) find_fn(name string) Fn {
	f := t.fns[name] 
	if !isnil(f.name.str) { 
		return f 
	} 
	return Fn{}
}

fn (t &Table) known_fn(name string) bool {
	f := t.find_fn(name) 
	return f.name != '' 
}

fn (t &Table) known_const(name string) bool {
	v := t.find_const(name)
	// TODO use optional
	return v.name.len > 0
}

fn (t mut Table) register_type(typ string) {
	if typ.len == 0 {
		return
	}
	for typ2 in t.types {
		if typ2.name == typ {
			return
		}
	}
	// if t.types.filter( _.name == typ.name).len > 0 {
	// return
	// }
	t.types << Type {
		name: typ
	}
}

fn (p mut Parser) register_type_with_parent(strtyp, parent string) {
	typ := Type {
		name: strtyp
		parent: parent
		pkg: p.pkg
	}
	p.table.register_type2(typ)
}

fn (t mut Table) register_type_with_parent(typ, parent string) {
	if typ.len == 0 {
		return
	}
	// if t.types.filter(_.name == typ) > 0
	for typ2 in t.types {
		if typ2.name == typ {
			return
		}
	}
	/*
mut pkg := ''
if parent == 'array' {
pkg = 'builtin'
}
*/
	datyp := Type {
		name: typ
		parent: parent
	}
	t.types << datyp
}

fn (t mut Table) register_type2(typ Type) {
	if typ.name.len == 0 {
		return
	}
	// println('register type2 $typ.name')
	for typ2 in t.types {
		if typ2.name == typ.name {
			return
		}
	}
	t.types << typ
}

fn (t mut Type) add_field(name, typ string, is_mut bool, attr string, access_mod AccessMod) {
	// if t.name == 'Parser' {
	// println('adding field $name')
	// }
	v := Var {
		name: name
		typ: typ
		is_mut: is_mut
		attr: attr
		access_mod: access_mod
	}
	t.fields << v
}

fn (t &Type) has_field(name string) bool {
	field := t.find_field(name)
	return (field.name != '')
}

fn (t &Type) find_field(name string) Var {
	for field in t.fields {
		if field.name == name {
			return field
		}
	}
	return Var{}
}

fn (table &Table) type_has_field(typ &Type, name string) bool {
	field := table.find_field(typ, name)
	return (field.name != '')
}

fn (table &Table) find_field(typ &Type, name string) Var {
	field := typ.find_field(name)
	if field.name.len == 0 && typ.parent.len > 0 {
		parent := table.find_type(typ.parent)
		return parent.find_field(name)
	}
	return field
}

fn (t mut Type) add_method(f Fn) {
	// if t.name.contains('Parser') {
	// println('!!!add_method() $f.name to $t.name len=$t.methods.len cap=$t.methods.cap')
	// }
	t.methods << f
	// println('end add_method()')
}

fn (t &Type) has_method(name string) bool {
	method := t.find_method(name)
	return (method.name != '')
}

fn (table &Table) type_has_method(typ &Type, name string) bool {
	method := table.find_method(typ, name)
	return (method.name != '')
}

// TODO use `?Fn`
fn (table &Table) find_method(typ &Type, name string) Fn {
	// println('TYPE HAS METHOD $name')
	method := typ.find_method(name)
	if method.name.len == 0 && typ.parent.len > 0 {
		parent := table.find_type(typ.parent)
		return parent.find_method(name)
		// println('parent = $parent.name $res')
		// return res
	}
	return method
}

fn (t &Type) find_method(name string) Fn {
	// println('$t.name find_method($name) methods.len=$t.methods.len')
	for method in t.methods {
		// println('method=$method.name')
		if method.name == name {
			return method
		}
	}
	return Fn{}
}

/* 
fn (t mut Type) add_gen_type(type_name string) {
	// println('add_gen_type($s)')
	if t.gen_types.contains(type_name) {
		return
	}
	t.gen_types << type_name
}
*/ 

fn (p &Parser) find_type(name string) *Type {
	typ := p.table.find_type(name)
	if typ.name.len == 0 {
		return p.table.find_type(p.prepend_pkg(name))
	}
	return typ
}

fn (t &Table) find_type(name string) *Type {
	if name.ends_with('*') && !name.contains(' ') {
		name = name.left(name.len - 1)
	}
	// TODO PERF use map
	for i, typ in t.types {
		if typ.name == name {
			return &t.types[i]
		}
	}
	return &Type{}
}

fn (p mut Parser) _check_types(got, expected string, throw bool) bool {
	p.log('check types got="$got" exp="$expected"  ')
	if p.pref.translated {
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
	if got=='int' && expected=='i64' {
		return true
	}
	if got == 'void*' && expected.starts_with('fn ') {
		return true
	}
	if got.starts_with('[') && expected == 'byte*' {
		return true
	}
	// Todo void* allows everything right now
	if got=='void*' || expected=='void*' {
		// if !p.builtin_pkg {
		if p.pref.is_play {
			return false
		}
		return true
	}
	// TODO only allow numeric consts to be assigned to bytes, and
	// throw an error if they are bigger than 255
	if got=='int' && expected=='byte' {
		return true
	}
	if got=='byteptr' && expected=='byte*' {
		return true
	}
	if got=='byte*' && expected=='byteptr' {
		return true
	} 
	if got=='int' && expected=='byte*' {
		return true
	}
	// byteptr += int
	if got=='int' && expected=='byteptr' {
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
	if expected.starts_with('Option_') && expected.ends_with(got) {
		return true
	}
	// NsColor* return 0
	if !p.pref.is_play {
		if expected.ends_with('*') && got == 'int' {
			return true
		}
		// if got == 'T' || got.contains('<T>') {
		// return true
		// }
		// if expected == 'T' || expected.contains('<T>') {
		// return true
		// }
		// Allow pointer arithmetic
		if expected=='void*' && got=='int' {
			return true
		}
	}
	expected = expected.replace('*', '')
	got = got.replace('*', '')
	if got != expected {
		// Interface check
		if expected.ends_with('er') {
			if p.satisfies_interface(expected, got, throw) {
				return true
			}
		}
		if !throw {
			return false
		}
		else {
			p.error('expected type `$expected`, but got `$got`')
		}
	}
	return true
}

// throw by default
fn (p mut Parser) check_types(got, expected string) bool {
	return p._check_types(got, expected, true)
}

fn (p mut Parser) check_types_no_throw(got, expected string) bool {
	return p._check_types(got, expected, false)
}

fn (p mut Parser) satisfies_interface(interface_name, _typ string, throw bool) bool {
	int_typ := p.table.find_type(interface_name)
	typ := p.table.find_type(_typ)
	for method in int_typ.methods {
		if !typ.has_method(method.name) {
			// if throw {
			p.error('Type "$_typ" doesn\'t satisfy interface "$interface_name" (method "$method.name" is not implemented)')
			// }
			return false
		}
	}
	return true
}

fn type_default(typ string) string {
	if typ.starts_with('array_') {
		typ = typ.right(6)
		return 'new_array(0, 1, sizeof($typ))'
	}
	// Always set pointers to 0
	if typ.ends_with('*') {
		return '0'
	}
	// ?
	if typ.contains('__') {
		return ''
	}
	// Default values for other types are not needed because of mandatory initialization
	switch typ {
	case 'int': return '0'
	case 'string': return 'tos("", 0)'
	case 'void*': return '0'
	case 'byte*': return '0'
	case 'bool': return '0'
	}
	return '{}' 
	return ''
}

// TODO PERF O(n)
fn (t &Table) is_interface(name string) bool {
	for typ in t.types {
		if typ.is_interface && typ.name == name {
			return true
		}
	}
	return false
}

// Do we have fn main()?
fn (t &Table) main_exists() bool {
	for entry in t.fns.entries { 
		f := t.fns[entry.key] 
		if f.name == 'main' {
			return true
		}
	}
	return false
}

// TODO use `?Var`
fn (t &Table) find_const(name string) Var {
	for c in t.consts {
		if c.name == name {
			return c
		}
	}
	return Var{}
}

fn (table mut Table) cgen_name(f &Fn) string {
	mut name := f.name
	if f.is_method {
		name = '${f.receiver_typ}_$f.name'
		name = name.replace(' ', '')
		name = name.replace('*', '')
		name = name.replace('+', 'plus')
		name = name.replace('-', 'minus')
	}
	// Avoid name conflicts (with things like abs(), print() etc).
	// Generate b_abs(), b_print()
	// TODO duplicate functionality
	if f.pkg == 'builtin' && CReserved.contains(f.name) {
		return 'v_$name'
	}
	// Obfuscate but skip certain names
	// TODO ugly, fix
	if table.obfuscate && f.name != 'main' && f.name != 'WinMain' && f.pkg != 'builtin' && !f.is_c &&
	f.pkg != 'darwin' && f.pkg != 'os' && !f.name.contains('window_proc') && f.name != 'gg__vec2' &&
	f.name != 'build_token_str' && f.name != 'build_keys' && f.pkg != 'json' &&
	!name.ends_with('_str') && !name.contains('contains') {
		mut idx := table.obf_ids[name]
		// No such function yet, register it
		if idx == 0 {
			table.fn_cnt++
			table.obf_ids[name] = table.fn_cnt
			idx = table.fn_cnt
		}
		old := name
		name = 'f_$idx'
		println('$old ==> $name')
	}
	return name
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
			println('this should never happen')
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

