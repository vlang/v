// Copyright (c) 2019 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.

module main

import math
import strings

struct Var {
mut:
	typ             string
	name            string
	is_arg          bool
	is_const        bool
	args            []Var // function args
	attr            string //  [json] etc
	is_mut          bool
	is_alloc        bool
	ptr             bool
	ref             bool
	parent_fn       string // Variables can only be defined in functions
	mod             string // module where this var is stored
	line_nr         int
	access_mod      AccessMod
	is_global       bool // __global (translated from C only)
	is_used         bool
	is_changed      bool
	scope_level     int
}


struct Table {
mut:
	types        []Type
	consts       []Var
	fns          map[string]Fn
	generic_fns  []GenTable //map[string]GenTable // generic_fns['listen_and_serve'] == ['Blog', 'Forum']
	obf_ids      map[string]int // obf_ids['myfunction'] == 23
	modules      []string // List of all modules registered by the application
	imports      []string // List of all imports
	file_imports []FileImportTable // List of imports for file
	flags        []string //  ['-framework Cocoa', '-lglfw3']
	fn_cnt       int //atomic
	obfuscate    bool
}

struct GenTable {
	fn_name string
	types []string
}

// Holds import information scoped to the parsed file
struct FileImportTable {
mut:
	module_name string
	file_path   string
	imports     map[string]string
}

enum AccessMod {
	private        // private immutable
	private_mut    // private mutable
	public         // public immutable (readonly)
	public_mut     // public, but mutable only in this module
	public_mut_mut // public and mutable both inside and outside (not recommended to use, that's why it's so verbose)
}

enum TypeCategory {
	builtin
	struct_
	func
	interface_ // 2
	enum_
	union_
	c_struct // 5
	c_typedef
}

struct Type {
mut:
	mod            string
	name           string
	cat            TypeCategory
	fields         []Var
	methods        []Fn
	parent         string
	func           Fn // For cat == FN (type myfn fn())
	is_c           bool // `C.FILE`
	enum_vals []string
	gen_types []string
	// This field is used for types that are not defined yet but are known to exist.
	// It allows having things like `fn (f Foo) bar()` before `Foo` is defined.
	// This information is needed in the first pass.
	is_placeholder bool
	gen_str	       bool  // needs `.str()` method generation
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
		'free',
		'panic',

		// Full list of C reserved words, from: https://en.cppreference.com/w/c/keyword
		'auto',
		'break',
		'case',
		'char',
		'const',
		'continue',
		'default',
		'do',
		'double',
		'else',
		'enum',
		'extern',
		'float',
		'for',
		'goto',
		'if',
		'inline',
		'int',
		'long',
		'register',
		'restrict',
		'return',
		'short',
		'signed',
		'sizeof',
		'static',
		'struct',
		'switch',
		'typedef',
		'union',
		'unsigned',
		'void',
		'volatile',
		'while',
	]

)

// This is used in generated C code
fn (f Fn) str() string {
	t := Table{}
	str_args := f.str_args(t)
	return '$f.name($str_args) $f.typ'
}

fn (t &Table) debug_fns() string {
	mut s := strings.new_builder(1000)
	for _, f in t.fns {
		s.writeln(f.name)
	}
	return s.str()
}

// fn (types array_Type) print_to_file(f string)  {
// }
const (
	number_types = ['number', 'int', 'i8', 'u8', 'i16', 'u16', 'i32', 'u32', 'byte', 'i64', 'u64', 'f32', 'f64']
	float_types  = ['f32', 'f64']
)

fn is_number_type(typ string) bool {
	return typ in number_types
}

fn is_float_type(typ string) bool {
	return typ in float_types
}

fn is_primitive_type(typ string) bool {
	return is_number_type(typ) || typ == 'string'
}

fn new_table(obfuscate bool) *Table {
	mut t := &Table {
		obf_ids: map[string]int
		fns: map[string]Fn
		//generic_fns: map[string]GenTable{}
		generic_fns: []GenTable
		obfuscate: obfuscate
		file_imports: []FileImportTable
	}
	t.register_type('int')
	t.register_type('size_t')
	t.register_type_with_parent('i8', 'int')
	t.register_type_with_parent('u8', 'u32')
	t.register_type_with_parent('i16', 'int')
	t.register_type_with_parent('u16', 'u32')
	t.register_type_with_parent('i32', 'int')
	t.register_type_with_parent('u32', 'int')
	t.register_type_with_parent('byte', 'int')
	t.register_type_with_parent('i64', 'int')
	t.register_type_with_parent('u64', 'u32')
	t.register_type('byteptr')
	t.register_type('intptr')
	t.register_type('f32')
	t.register_type('f64')
	t.register_type('rune')
	t.register_type('bool')
	t.register_type('void')
	t.register_type('voidptr')
	t.register_type('T')
	t.register_type('va_list')
	t.register_const('stdin', 'int', 'main')
	t.register_const('stdout', 'int', 'main')
	t.register_const('stderr', 'int', 'main')
	t.register_const('errno', 'int', 'main')
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

fn (t mut Table) register_module(mod string) {
	if t.modules.contains(mod) {
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

fn (t mut Table) register_const(name, typ, mod string) {
	t.consts << Var {
		name: name
		typ: typ
		is_const: true
		mod: mod
	}
}

// Only for translated code
fn (p mut Parser) register_global(name, typ string) {
	p.table.consts << Var {
		name: name
		typ: typ
		is_const: true
		is_global: true
		mod: p.mod
		is_mut: true
	}
}

fn (t mut Table) register_fn(new_fn Fn) {
	t.fns[new_fn.name] = new_fn
}

fn (table &Table) known_type(typ_ string) bool {
	mut typ := typ_
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
		mod: p.mod
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
mut mod := ''
if parent == 'array' {
mod = 'builtin'
}
*/
	t.types << Type {
		name: typ
		parent: parent
		//mod: mod
	}
}

fn (t mut Table) register_type2(typ Type) {
	if typ.name.len == 0 {
		return
	}
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

fn (t &Type) has_enum_val(name string) bool {
	return name in t.enum_vals
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

fn (p &Parser) find_type(name string) &Type {
	typ := p.table.find_type(name)
	if typ.name.len == 0 {
		return p.table.find_type(p.prepend_mod(name))
	}
	return typ
}

fn (t &Table) find_type(name_ string) *Type {
	mut name := name_
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

fn (p mut Parser) _check_types(got_, expected_ string, throw bool) bool {
	mut got := got_
	mut expected := expected_
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
	if got=='void*' || expected=='void*' {// || got == 'cvoid' || expected == 'cvoid' {
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
	//if got=='int' && expected=='voidptr*' {
		//return true
	//}
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
		return 'new_array(0, 1, sizeof( ${typ.right(6)} ))'
	}
	// Always set pointers to 0
	if typ.ends_with('*') {
		return '0'
	}
	// User struct defined in another module.
	if typ.contains('__') {
		return '{0}'
	}
	// Default values for other types are not needed because of mandatory initialization
	switch typ {
	case 'bool': return '0'
	case 'string': return 'tos((byte *)"", 0)'
	case 'i8': return '0'
	case 'i16': return '0'
	case 'i32': return '0'
	case 'i64': return '0'
	case 'u8': return '0'
	case 'u16': return '0'
	case 'u32': return '0'
	case 'u64': return '0'
	case 'byte': return '0'
	case 'int': return '0'
	case 'rune': return '0'
	case 'f32': return '0.0'
	case 'f64': return '0.0'
	case 'byteptr': return '0'
	case 'voidptr': return '0'
	}
	return '{0}'
}

// TODO PERF O(n)
fn (t &Table) is_interface(name string) bool {
	for typ in t.types {
		if typ.cat == .interface_ && typ.name == name {
			return true
		}
	}
	return false
}

// Do we have fn main()?
fn (t &Table) main_exists() bool {
	for _, f in t.fns {
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
	if f.mod == 'builtin' && CReserved.contains(f.name) {
		return 'v_$name'
	}
	// Obfuscate but skip certain names
	// TODO ugly, fix
	if table.obfuscate && f.name != 'main' && f.name != 'WinMain' && f.mod != 'builtin' && !f.is_c &&
	f.mod != 'darwin' && f.mod != 'os' && !f.name.contains('window_proc') && f.name != 'gg__vec2' &&
	f.name != 'build_token_str' && f.name != 'build_keys' && f.mod != 'json' &&
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

fn is_valid_int_const(val, typ string) bool {
	x := val.int()
	switch typ {
	case 'byte', 'u8': return 0 <= x && x <= math.MaxU8
	case 'u16': return 0 <= x && x <= math.MaxU16
	//case 'u32': return 0 <= x && x <= math.MaxU32
	//case 'u64': return 0 <= x && x <= math.MaxU64
	//////////////
	case 'i8': return math.MinI8 <= x && x <= math.MaxI8
	case 'i16': return math.MinI16 <= x && x <= math.MaxI16
	case 'int', 'i32': return math.MinI32 <= x && x <= math.MaxI32
	//case 'i64':
		//x64 := val.i64()
		//return i64(-(1<<63)) <= x64 && x64 <= i64((1<<63)-1)
	}
	return true
}

fn (t mut Table) register_generic_fn(fn_name string) {
	t.generic_fns << GenTable{fn_name, []string}
}

fn (t mut Table) fn_gen_types(fn_name string) []string {
	for _, f in t.generic_fns {
		if f.fn_name == fn_name {
			return f.types
		}
	}
	cerror('function $fn_name not found')
	return []string
}

// `foo<Bar>()`
// fn_name == 'foo'
// typ == 'Bar'
fn (t mut Table) register_generic_fn_type(fn_name, typ string) {
	for i, f in t.generic_fns {
		if f.fn_name == fn_name {
			t.generic_fns[i].types << typ
			return
		}
	}
}

fn (p mut Parser) typ_to_fmt(typ string, level int) string {
	t := p.table.find_type(typ)
	if t.cat == .enum_ {
		return '%d'
	}
	switch typ {
	case 'string': return '%.*s'
	//case 'bool': return '%.*s'
	case 'ustring': return '%.*s'
	case 'byte', 'bool', 'int', 'char', 'byte', 'i32', 'i16', 'i8': return '%d'
	case 'u8', 'u16', 'u32': return '%u'
	case 'f64', 'f32': return '%f'
	case 'i64': return '%lld'
	case 'u64': return '%llu'
	case 'byte*', 'byteptr': return '%s'
		// case 'array_string': return '%s'
		// case 'array_int': return '%s'
	case 'void': p.error('cannot interpolate this value')
	default:
		if typ.ends_with('*') {
			return '%p'
		}
	}
	if t.parent != '' && level == 0 {
		return p.typ_to_fmt(t.parent, level+1)
	}
	return ''
}

fn is_compile_time_const(s_ string) bool {
	s := s_.trim_space()
	if s == '' {
		return false
	}
	if s.contains('\'') {
		return true
	}
	for c in s {
		if ! ((c >= `0` && c <= `9`) || c == `.`) {
			return false
		}
	}
	return true
}

// Once we have a module format we can read from module file instead
// this is not optimal
fn (table &Table) qualify_module(mod string, file_path string) string {
	for m in table.imports {
		if m.contains('.') && m.contains(mod) {
			m_parts := m.split('.')
			m_path := m_parts.join('/')
			if mod == m_parts[m_parts.len-1] && file_path.contains(m_path) {
				return m
			}
		}
	}
	return mod
}

fn new_file_import_table(file_path string) *FileImportTable {
	return &FileImportTable{
		file_path: file_path
		imports:   map[string]string
	}
}

fn (fit &FileImportTable) known_import(mod string) bool {
	return mod in fit.imports || fit.is_aliased(mod)
}

fn (fit mut FileImportTable) register_import(mod string) {
	fit.register_alias(mod, mod)
}

fn (fit mut FileImportTable) register_alias(alias string, mod string) {
	if alias in fit.imports {
		cerror('cannot import $mod as $alias: import name $alias already in use in "${fit.file_path}".')
	}
	if mod.contains('.internal.') {
		mod_parts := mod.split('.')
		mut internal_mod_parts := []string
		for part in mod_parts {
			if part == 'internal' { break }
			internal_mod_parts << part
		}
		internal_parent := internal_mod_parts.join('.')
		if !fit.module_name.starts_with(internal_parent) {
			cerror('module $mod can only be imported internally by libs.')
		}
	}
	fit.imports[alias] = mod
}

fn (fit &FileImportTable) known_alias(alias string) bool {
	return alias in fit.imports
}

fn (fit &FileImportTable) is_aliased(mod string) bool {
	for _, val in fit.imports {
		if val == mod {
			return true
		}
	}
	return false
}

fn (fit &FileImportTable) resolve_alias(alias string) string {
	return fit.imports[alias]
}

fn (t &Type) contains_field_type(typ string) bool {
	for field in t.fields {
		if field.typ == typ {
			return true
		}
	}
	return false
}
