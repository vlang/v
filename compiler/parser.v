// Copyright (c) 2019 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.

module main

import os
import rand
import strings

struct Var {
mut:
	typ             string
	name            string
	is_arg          bool
	is_const        bool
	is_import_const bool // TODO remove import consts entirely
	args            []Var // function args
	attr            string //  [json] etc
	is_mut          bool
	ptr             bool
	ref             bool
	parent_fn       string // Variables can only be defined in functions
	mod             string // module where this var is stored  
	line_nr         int
	access_mod      AccessMod
	is_global       bool // __global (translated from C only)
	is_used         bool
	scope_level     int
}

struct Parser {
	file_path      string // "/home/user/hello.v"
	file_name      string // "hello.v"
mut:
	scanner        *Scanner
	// tokens         []Token // TODO cache all tokens, right now they have to be scanned twice
	token_idx      int
	tok            Token
	prev_tok       Token
	prev_tok2      Token // TODO remove these once the tokens are cached
	lit            string
	cgen           *CGen
	table          *Table
	import_table   *FileImportTable // Holds imports for just the file being parsed
	run            Pass // TODO rename `run` to `pass`
	os             OS 
	mod            string
	inside_const   bool
	expr_var       Var
	assigned_type  string
	expected_type  string
	tmp_cnt        int
	is_script      bool
	pref           *Preferences // Setting and Preferences shared from V struct
	builtin_pkg    bool
	vh_lines       []string
	inside_if_expr bool
	is_struct_init bool
	if_expr_cnt    int
	for_expr_cnt   int // to detect whether `continue` can be used
	ptr_cast       bool
	calling_c      bool
	cur_fn         *Fn
	returns        bool
	vroot          string
	is_c_struct_init bool
	can_chash bool
	attr string 
	v_script bool // "V bash", import all os functions into global space 
}

const (
	EmptyFn = &Fn { }
	MainFn= &Fn{name:'main'} 
)

const (
	MaxModuleDepth = 4
) 

fn (c mut V) new_parser(path string, run Pass) Parser {
	c.log('new_parser("$path")')
	c.cgen.run = run
	mut p := Parser {
		file_path: path
		file_name: path.all_after('/')
		scanner: new_scanner(path)
		table: c.table
		import_table: new_file_import_table(path)
		cur_fn: EmptyFn
		cgen: c.cgen
		is_script: (c.pref.is_script && path == c.dir)
		pref: c.pref
		os: c.os
		run: run
		vroot: c.vroot
	}
	p.next()
	// p.scanner.debug_tokens()
	return p
}

fn (p mut Parser) next() {
	p.prev_tok2 = p.prev_tok
	p.prev_tok = p.tok
	p.scanner.prev_tok = p.tok 
	res := p.scanner.scan()
	p.tok = res.tok
	p.lit = res.lit
}

fn (p &Parser) log(s string) {
	if !p.pref.is_verbose {
		return
	}
	println(s)
}

fn (p mut Parser) parse() {
	p.log('\nparse() run=$p.run file=$p.file_name tok=${p.strtok()}')// , "script_file=", script_file)
	// `module main` is not required if it's a single file program
	if p.is_script || p.pref.is_test {
		p.mod = 'main'
		// User may still specify `module main`
		if p.tok == .key_module {
			p.next()
			p.fgen('module ')
			p.mod = p.check_name()
		}
	}
	else {
		p.check(.key_module)
		p.fspace() 
		p.mod = p.check_name()
	}
	p.fgenln('\n')
	p.builtin_pkg = p.mod == 'builtin'
	p.can_chash = p.mod == 'ft' || 	p.mod == 'http' ||  p.mod == 'glfw' || p.mod=='ui' // TODO tmp remove
	// Import pass - the first and the smallest pass that only analyzes imports
	// fully qualify the module name, eg base64 to encoding.base64
	fq_mod := p.table.qualify_module(p.mod, p.file_path)
	p.table.register_package(fq_mod)
	// replace "." with "_dot_" in module name for C variable names
	p.mod = fq_mod.replace('.', '_dot_')
	if p.run == .imports {
		for p.tok == .key_import && p.peek() != .key_const {
			p.imports()
		}
		if p.table.imports.contains('builtin') {
			p.error('module `builtin` cannot be imported') 
		} 
		return
	}
	// Go through every top level token or throw a compilation error if a non-top level token is met
	for {
		switch p.tok {
		case Token.key_import:
			if p.peek() == .key_const {
				p.const_decl()
			}
			else {
				// TODO remove imported consts from the language
				p.imports()
				if p.tok != .key_import { 
					p.fgenln('') 
				} 
			}
		case Token.key_enum:
			p.next()
			if p.tok == .name {
				p.fgen('enum ')
				name := p.check_name()
				p.fgen(' ')
				p.enum_decl(name)
			}
			// enum without a name, only allowed in code, translated from C
			// it's a very bad practice in C as well, but is used unfortunately (for example, by DOOM)
			// such fields are basically int consts
			else if p.pref.translated {
				p.enum_decl('int')
			}
			else {
				p.check(.name)
			}
		case Token.key_pub:
			if p.peek() == .func {
				p.fn_decl()
			} else if p.peek() == .key_struct {
				p.error('structs can\'t be declared public *yet*')
				// TODO public structs
			} else {
				p.error('wrong pub keyword usage')
			}
		case Token.func:
			p.fn_decl()
		case Token.key_type:
			p.type_decl()
		case Token.lsbr:
			// `[` can only mean an [attribute] before a function
			// or a struct definition
			p.attribute() 
		case Token.key_struct, Token.key_interface, Token.key_union, Token.lsbr:
			p.struct_decl()
		case Token.key_const:
			p.const_decl()
		case Token.hash:
			// insert C code, TODO this is going to be removed ASAP
			// some libraries (like UI) still have lots of C code
			// # puts("hello");
			p.chash()
		case Token.dollar:
			// $if, $else
			p.comp_time()
		case Token.key_global:
			if !p.pref.translated && !p.pref.is_live && !p.builtin_pkg && !p.building_v() {
				//p.error('__global is only allowed in translated code')
			}
			p.next()
			name := p.check_name()
			typ := p.get_type()
			p.register_global(name, typ)
			// p.genln(p.table.cgen_name_type_pair(name, typ))
			mut g := p.table.cgen_name_type_pair(name, typ)
			if p.tok == .assign {
				p.next()
				// p.gen(' = ')
				g += ' = '
				p.cgen.start_tmp()
				p.bool_expression()
				// g += '<<< ' + p.cgen.end_tmp() + '>>>'
				g += p.cgen.end_tmp()
			}
			// p.genln('; // global')
			g += '; // global' 
			p.cgen.consts << g
		case Token.eof:
			p.log('end of parse()')
			if p.is_script && !p.pref.is_test {
				p.cur_fn = MainFn
				p.check_unused_variables()
			}
			if false && !p.first_run() && p.fileis('main.v') {
				out := os.create('/var/tmp/fmt.v') or {
					panic('failed to create fmt.v') 
					return 
				} 
				out.writeln(p.scanner.fmt_out.str())
				out.close()
			}
			return
		default:
			// no `fn main`, add this "global" statement to cgen.fn_main
			if p.is_script && !p.pref.is_test {
				// cur_fn is empty since there was no fn main declared
				// we need to set it to save and find variables 
				if p.first_run() {
					if p.cur_fn.name == '' {
						p.cur_fn = MainFn 
					} 
					return 
				} 
				if p.cur_fn.name == '' {
					p.cur_fn = MainFn 
					if p.pref.is_repl {
						p.cur_fn.clear_vars()
					}
				} 
				mut start := p.cgen.lines.len
				p.statement(true)
				if p.cgen.lines[start - 1] != '' && p.cgen.fn_main != '' {
					start--
				}
				p.genln('') 
				end := p.cgen.lines.len
				lines := p.cgen.lines.slice(start, end)
				//mut line := p.cgen.fn_main + lines.join('\n') 
				//line = line.trim_space() 
				p.cgen.fn_main = p.cgen.fn_main + lines.join('\n')
				p.cgen.resetln('')
				for i := start; i < end; i++ {
					p.cgen.lines[i] = ''
				}
			}
			else {
				p.error('unexpected token `${p.strtok()}`')
			}
		}
	}
}

fn (p mut Parser) imports() {
	p.check(.key_import)
	// `import ()`
	if p.tok == .lpar {
		p.check(.lpar)
		for p.tok != .rpar && p.tok != .eof {
			pkg := p.lit.trim_space()
			p.next()
			// TODO: aliased for import() syntax
			// p.import_table.register_alias(alias, pkg)
			// p.import_table.register_import(pkg)
			if p.table.imports.contains(pkg) {
				continue
			}
			p.table.imports << pkg
			p.table.register_package(pkg)
		}
		p.check(.rpar)
		return
	}
	// `import foo`
	if p.tok != .name {
		p.error('bad import format')
	}
	mut pkg := p.lit.trim_space()
	mut mod_alias := pkg
	// submodule support
	mut depth := 1
	p.next()
	for p.tok == .dot {
		p.check(.dot) 
		submodule := p.check_name()
		mod_alias = submodule
		pkg += '.' + submodule
		depth++
		if depth > MaxModuleDepth { 
			p.error('module depth of $MaxModuleDepth exceeded: $pkg') 
		}
	}
	// aliasing (import encoding.base64 as b64)
	if p.tok == .key_as && p.peek() == .name {
		p.check(.key_as) 
		mod_alias = p.check_name()
	}
	p.fgenln(' ' + pkg)
	// add import to file scope import table
	p.import_table.register_alias(mod_alias, pkg)
	// Make sure there are no duplicate imports
	if p.table.imports.contains(pkg) {
		return
	}
	p.log('adding import $pkg')
	p.table.imports << pkg
	p.table.register_package(pkg)
}

fn (p mut Parser) const_decl() {
	is_import := p.tok == .key_import
	p.inside_const = true
	if is_import {
		p.next()
	}
	p.check(.key_const)
	p.fspace()
	p.check(.lpar)
	p.fgenln('')
	p.fmt_inc() 
	for p.tok == .name {
		// `Age = 20`
		mut name := p.check_name()
		if p.pref.is_play && ! (name[0] >= `A` && name[0] <= `Z`) {
			p.error('const name must be capitalized')
		}
		// Imported consts (like GL_TRIANG.leS) dont need pkg prepended (gl__GL_TRIANG.leS)
		if !is_import {
			name = p.prepend_pkg(name)
		}
		mut typ := 'int'
		if !is_import {
			p.check_space(.assign)
			typ = p.expression()
		}
		if p.first_run() && !is_import && p.table.known_const(name) {
			p.error('redefinition of `$name`')
		}
		p.table.register_const(name, typ, p.mod, is_import)
		if p.run == .main && !is_import {
			// TODO hack
			// cur_line has const's value right now. if it's just a number, then optimize generation:
			// output a #define so that we don't pollute the binary with unnecessary global vars
			if is_compile_time_const(p.cgen.cur_line) {
				p.cgen.consts << '#define $name $p.cgen.cur_line' 
				p.cgen.resetln('')
				p.fgenln('')
				continue
			}
			if typ.starts_with('[') {
				p.cgen.consts << p.table.cgen_name_type_pair(name, typ) +
				' = $p.cgen.cur_line;'
			}
			else {
				p.cgen.consts << p.table.cgen_name_type_pair(name, typ) + ';'
				p.cgen.consts_init << '$name = $p.cgen.cur_line;'
			}
			p.cgen.resetln('')
		}
		p.fgenln('')
	}
	p.fmt_dec() 
	p.check(.rpar)
	p.fgenln('\n')
	p.inside_const = false
}

// `type myint int`
// `type onclickfn fn(voidptr) int`
fn (p mut Parser) type_decl() {
	p.check(.key_type)
	name := p.check_name()
	parent := p.get_type()
	nt_pair := p.table.cgen_name_type_pair(name, parent)
	// TODO dirty C typedef hacks for DOOM
	// Unknown type probably means it's a struct, and it's used before the struct is defined,
	// so specify "struct"
	_struct := if !parent.contains('[') && !parent.starts_with('fn ') && !p.table.known_type(parent){'struct'} else { ''}
	p.gen_typedef('typedef $_struct $nt_pair; // type alias name="$name" parent="$parent"')
	p.register_type_with_parent(name, parent)
}

fn (p mut Parser) interface_method(field_name, receiver string) &Fn {
	mut method := &Fn {
		name: field_name
		is_interface: true
		is_method: true
		receiver_typ: receiver
	}
	p.log('is interface. field=$field_name run=$p.run')
	p.fn_args(mut method)
	if p.scanner.has_gone_over_line_end() {
		method.typ = 'void'
	} else {
		method.typ = p.get_type()// method return type
		p.fspace()
		p.fgenln('')
	}
	return method
}

// also unions and interfaces
fn (p mut Parser) struct_decl() {
	// Attribute before type?
	mut objc_parent := ''
	mut is_objc := false// V can generate Objective C for integration with Cocoa
	// [attr]
	if p.tok == .lsbr {
		p.check(.lsbr)
		// `[interface:ParentInterface]`
		is_objc = p.tok == .key_interface 
		p.next()
		if is_objc {
			p.check(.colon)
			objc_parent = p.check_name()
		}
		p.check(.rsbr)
	}
	is_interface := p.tok == .key_interface 
	is_union := p.tok == .key_union
	is_struct := p.tok == .key_struct
	p.fgen(p.tok.str() + ' ')
	// Get type name
	p.next()
	mut name := p.check_name()
	if name.contains('_') && !p.pref.translated {
		p.error('type names cannot contain `_`')
	}
	if is_interface && !name.ends_with('er') {
		p.error('interface names temporarily have to end with `er` (e.g. `Speaker`, `Reader`)')
	}
	is_c := name == 'C' && p.tok == .dot 
	if is_c {
		p.check(.dot)
		name = p.check_name()
	}
	// Specify full type name
	if !is_c && !p.builtin_pkg && p.mod != 'main' {
		name = p.prepend_pkg(name)
	}
	if p.run == .decl && p.table.known_type(name) {
		p.error('`$name` redeclared')
	}
	// Generate type definitions
	if is_objc {
		p.gen_type('@interface $name : $objc_parent { @public')
	}
	else {
		// type alias is generated later
		if !is_c {
			kind := if is_union {'union'} else {'struct'}
			p.gen_typedef('typedef $kind $name $name;')
			p.gen_type('$kind $name {')
		}
	}
	// V used to have 'type Foo struct', many Go users might use this syntax
	if !is_c && p.tok == .key_struct {
		p.error('use `struct $name {` instead of `type $name struct {`')
	}
	// Register the type
	mut typ := p.table.find_type(name)
	mut is_ph := false
	if typ.is_placeholder {
		is_ph = true
		typ.name = name
		typ.mod = p.mod
		typ.is_c = is_c
		typ.is_placeholder = false
	}
	else {
		typ = &Type {
			name: name
			mod: p.mod
			is_c: is_c
			is_interface: is_interface
		}
	}
	// Struct `C.Foo` declaration, no body
	if is_c && is_struct && p.tok != .lcbr {
		p.table.register_type2(typ)
		return
	}
	p.fgen(' ')
	p.check(.lcbr)
	// Struct fields
	mut is_pub := false
	mut is_mut := false
	mut names := []string// to avoid dup names TODO alloc perf
/* 
	mut fmt_max_len := 0 
	for field in typ.fields  {
		if field.name.len > max_len {
			fmt_max_len = field.name.len 
		} 
	} 
	println('fmt max len = $max_len nrfields=$typ.fields.len pass=$p.run') 
*/ 
	for p.tok != .rcbr {
		if p.tok == .key_pub {
			if is_pub {
				p.error('structs can only have one `pub:`, all public fields have to be grouped')
			}
			is_pub = true
			p.fmt_dec() 
			p.check(.key_pub)
			if p.tok != .key_mut {
				p.check(.colon)
			}
			p.fmt_inc() 
			p.fgenln('')
		}
		if p.tok == .key_mut {
			if is_mut {
				p.error('structs can only have one `mut:`, all private key_mut fields have to be grouped')
			}
			is_mut = true
			p.fmt_dec() 
			p.check(.key_mut)
			if p.tok != .key_mut {
				p.check(.colon)
			}
			p.fmt_inc() 
			p.fgenln('')
		}
		// if is_pub {
		// }
		// (mut) user *User
		// if p.tok == .plus {
		// p.next()
		// }
		// Check dups
		field_name := p.check_name()
		if field_name in names {
			p.error('duplicate field `$field_name`')
		}
		names << field_name
		// We are in an interface?
		// `run() string` => run is a method, not a struct field
		if is_interface {
			typ.add_method(p.interface_method(field_name, name))
			continue
		}
		// `pub` access mod
		access_mod := if is_pub{AccessMod.public} else { AccessMod.private}
		p.fgen(' ')
		field_type := p.get_type()
		is_atomic := p.tok == .key_atomic
		if is_atomic {
			p.next()
			p.gen_type('_Atomic ')
		}
		if !is_c {
			p.gen_type(p.table.cgen_name_type_pair(field_name, field_type) + ';')
		}
		// [ATTR]
		mut attr := ''
		if p.tok == .lsbr {
			p.next()
			attr = p.check_name()
			p.check(.rsbr)
		}
		typ.add_field(field_name, field_type, is_mut, attr, access_mod)
		p.fgenln('')
	}
	if !is_ph && p.first_run() {
		p.table.register_type2(typ)
		//println('registering 1 nrfields=$typ.fields.len') 
	}
	p.check(.rcbr)
	if !is_c {
		p.gen_type('}; ')
	}
	if is_objc {
		p.gen_type('@end')
	}
	p.fgenln('\n')
}

fn (p mut Parser) enum_decl(_enum_name string) {
	mut enum_name := _enum_name
	// Specify full type name
	if !p.builtin_pkg && p.mod != 'main' {
		enum_name = p.prepend_pkg(enum_name)
	}
	// Skip empty enums
	if enum_name != 'int' {
		p.cgen.typedefs << 'typedef int $enum_name;'
	}
	p.check(.lcbr)
	mut val := 0
	mut fields := []string 
	for p.tok == .name {
		field := p.check_name()
		fields << field 
		p.fgenln('')
		name := '${p.mod}__${enum_name}_$field'
		if p.run == .main {
			p.cgen.consts << '#define $name $val' 
		}
		if p.tok == .comma {
			p.next()
		}
		p.table.register_const(name, enum_name, p.mod, false)
		val++
	}
	p.table.register_type2(&Type {
		name: enum_name
		mod: p.mod
		parent: 'int'
		is_enum: true
		enum_vals: fields 
	})
	p.check(.rcbr)
	p.fgenln('\n')
}

// check_name checks for a name token and returns its literal
fn (p mut Parser) check_name() string {
	if p.tok == .key_type {
		p.check(.key_type) 
		return 'type' 
	} 
	name := p.lit
	 
	p.check(.name)
	return name
}

fn (p mut Parser) check_string() string {
	s := p.lit
	p.check(.str)
	return s
}

fn (p &Parser) strtok() string {
	if p.tok == .name {
		return p.lit
	}
	if p.tok == .str {
		return '"$p.lit"'
	}
	res := p.tok.str()
	if res == '' {
		n := int(p.tok)
		return n.str()
	}
	return res
}

// same as check(), but adds a space to the formatter output
// TODO bad name
fn (p mut Parser) check_space(expected Token) {
	p.fspace()
	p.check(expected)
	p.fspace()
}

fn (p mut Parser) check(expected Token) {
	if p.tok != expected {
		println('check()')
		mut s := 'expected `${expected.str()}` but got `${p.strtok()}`'
		p.next()
		println('next token = `${p.strtok()}`')
		print_backtrace()
		p.error(s)
	}
	if expected == .rcbr {
		p.fmt_dec() 
	}
	p.fgen(p.strtok())
	// vfmt: increase indentation on `{` unless it's `{}`
	if expected == .lcbr && p.scanner.text[p.scanner.pos + 1] != `}` {
		p.fgenln('')
		p.fmt_inc() 
	}
	p.next()

if p.scanner.line_comment != '' { 
	//p.fgenln('// ! "$p.scanner.line_comment"')
	//p.scanner.line_comment = '' 
} 
}

fn (p mut Parser) error(s string) {
	// Dump all vars and types for debugging
	if false {
		//file_types := os.create('$TmpPath/types')
		//file_vars := os.create('$TmpPath/vars')
		// ////debug("ALL T", q.J(p.table.types))
		// os.write_to_file('/var/tmp/lang.types', '')//pes(p.table.types))
		// //debug("ALL V", q.J(p.table.vars))
		// os.write_to_file('/var/tmp/lang.vars', q.J(p.table.vars))
		//file_types.close()
		//file_vars.close()
	}
	if p.pref.is_verbose { 
		println('pass=$p.run fn=`$p.cur_fn.name`')
	}
	p.cgen.save()
	// V git pull hint
	cur_path := os.getwd()
	if !p.pref.is_repl && ( p.file_path.contains('v/compiler') || cur_path.contains('v/compiler') ){
		println('\n=========================')
		println('It looks like you are building V. It is being frequently updated every day.') 
		println('If you didn\'t modify the compiler\'s code, most likely there was a change that ')
		println('lead to this error.')
		println('\nRun `git pull && make`, that will most likely fix it.')
		//println('\nIf this doesn\'t help, re-install V from source or download a precompiled' + ' binary from\nhttps://vlang.io.')
		println('\nIf this doesn\'t help, please create a GitHub issue.')
		println('=========================\n')
	}
	// p.scanner.debug_tokens()
	// Print `[]int` instead of `array_int` in errors
	p.scanner.error(s.replace('array_', '[]').replace('__', '.'))
}

fn (p &Parser) first_run() bool {
	return p.run == .decl
}

// TODO return Type instead of string?
fn (p mut Parser) get_type() string {
	debug := p.fileis('fn_test') && false
	mut mul := false
	mut nr_muls := 0
	mut typ := ''
	// fn type
	if p.tok == .func {
		if debug {
			println('\nget_type() .key_goT FN TYP line=$p.scanner.line_nr')
		}
		mut f := Fn{name: '_', pkg: p.mod}
		p.next()
		line_nr := p.scanner.line_nr
		p.fn_args(mut f)
		// Same line, it's a return type
		if p.scanner.line_nr == line_nr {
			if debug {
				println('same line getting type')
			}
			if p.tok == .name { 
				f.typ = p.get_type()
			} 
			else { 
				f.typ = 'void'
			} 
			// println('fn return typ=$f.typ')
		}
		else {
			f.typ = 'void'
		}
		// Register anon fn type
		fn_typ := Type {
			name: f.typ_str()// 'fn (int, int) string'
			mod: p.mod
			func: f
		}
		p.table.register_type2(fn_typ)
		return f.typ_str()
	}
	// arrays ([]int)
	mut is_arr := false
	mut is_arr2 := false// [][]int TODO remove this and allow unlimited levels of arrays
	is_question := p.tok == .question
	if is_question {
		p.check(.question)
	}
	if p.tok == .lsbr {
		p.check(.lsbr)
		// [10]int
		if p.tok == .number {
			typ = '[$p.lit]'
			p.next()
		}
		else {
			is_arr = true
		}
		p.check(.rsbr)
		// [10][3]int
		if p.tok == .lsbr {
			p.next()
			if p.tok == .number {
				typ += '[$p.lit]'
				p.check(.number)
			}
			else {
				is_arr2 = true
			}
			p.check(.rsbr)
		}
	}
	// map[string]int 
	if !p.builtin_pkg && p.tok == .name && p.lit == 'map' {
		p.next()
		p.check(.lsbr) 
		key_type := p.check_name() 
		if key_type != 'string' {
			p.error('maps only support string keys for now') 
		} 
		p.check(.rsbr) 
		val_type := p.check_name() 
		typ= 'map_$val_type' 
		p.register_map(typ)
		return typ 
	} 
	//  
	for p.tok == .mul {
		mul = true
		nr_muls++
		p.check(.mul) 
	}
	if p.tok == .amp {
		mul = true
		nr_muls++
		p.check(.amp) 
	}
	typ += p.lit
	if !p.is_struct_init {
		// Otherwise we get `foo := FooFoo{` because `Foo` was already generated in name_expr()
		p.fgen(p.lit)
	}
	// C.Struct import
	if p.lit == 'C' && p.peek() == .dot {
		p.next()
		p.check(.dot)
		typ = p.lit
	}
	else {
		// Package specified? (e.g. gx.Image)
		if p.peek() == .dot {
			p.next()
			p.check(.dot)
			typ += '__$p.lit'
		}
		mut t := p.table.find_type(typ)
		// "typ" not found? try "pkg__typ"
		if t.name == '' && !p.builtin_pkg {
			// && !p.first_run() {
			if !typ.contains('array_') && p.mod != 'main' && !typ.contains('__') &&
				!typ.starts_with('[') { 
				typ = p.prepend_pkg(typ)
			}
			t = p.table.find_type(typ)
			if t.name == '' && !p.pref.translated && !p.first_run() && !typ.starts_with('[') {
				println('get_type() bad type')
				// println('all registered types:')
				// for q in p.table.types {
				// println(q.name)
				// }
				p.error('unknown type `$typ`')
			}
		}
	}
	if typ == 'void' {
		p.error('unknown type `$typ`')
	}
	if mul {
		typ += strings.repeat(`*`, nr_muls)
	}
	// Register an []array type
	if is_arr2 {
		typ = 'array_array_$typ'
		p.register_array(typ)
	}
	else if is_arr {
		typ = 'array_$typ'
		// p.log('ARR TYPE="$typ" run=$p.run')
		// We come across "[]User" etc ?
		p.register_array(typ)
	}
	p.next()
	if p.tok == .question || is_question {
		typ = 'Option_$typ'
		p.table.register_type_with_parent(typ, 'Option')
		if p.tok == .question {
			p.next()
		}
	}
	// Because the code uses * to see if it's a pointer
	if typ == 'byteptr' {
		return 'byte*'
	}
	if typ == 'voidptr' {
		//if !p.builtin_pkg && p.mod != 'os' && p.mod != 'gx' && p.mod != 'gg' && !p.pref.translated {
			//p.error('voidptr can only be used in unsafe code')
		//}
		return 'void*'
	}
	if typ.last_index('__') > typ.index('__') {
		p.error('2 __ in gettype(): typ="$typ"')
	}
	return typ
}

fn (p &Parser) print_tok() {
	if p.tok == .name {
		println(p.lit)
		return
	}
	if p.tok == .str {
		println('"$p.lit"')
		return
	}
	println(p.tok.str())
}

// statements() returns the type of the last statement
fn (p mut Parser) statements() string {
	p.log('statements()')
	typ := p.statements_no_curly_end()
	if !p.inside_if_expr {
		p.genln('}')
	}
	if p.fileis('if_expr') {
		println('statements() ret=$typ line=$p.scanner.line_nr')
	}
	return typ
}

fn (p mut Parser) statements_no_curly_end() string {
	p.cur_fn.open_scope()
	if !p.inside_if_expr {
		p.genln('')
	}
	mut i := 0
	mut last_st_typ := ''
	for p.tok != .rcbr && p.tok != .eof && p.tok != .key_case && 
		p.tok != .key_default && p.peek() != .arrow {
		// println(p.tok.str())
		// p.print_tok()
		last_st_typ = p.statement(true)
		// println('last st typ=$last_st_typ')
		if !p.inside_if_expr {
			p.genln('')// // end st tok= ${p.strtok()}')
			p.fgenln('')
		}
		i++
		if i > 50000 {
			p.cgen.save()
			p.error('more than 50 000 statements in function `$p.cur_fn.name`')
		}
	}
	if p.tok != .key_case && p.tok != .key_default && p.peek() != .arrow {
		// p.next()
		p.check(.rcbr)
	}
	else {
		// p.check(.rcbr)
	}
	//p.fmt_dec() 
	// println('close scope line=$p.scanner.line_nr')
	p.cur_fn.close_scope()
	return last_st_typ
}

fn (p mut Parser) genln(s string) {
	p.cgen.genln(s)
}

fn (p mut Parser) gen(s string) {
	p.cgen.gen(s)
}

// Generate V header from V source
fn (p mut Parser) vh_genln(s string) {
	p.vh_lines << s
}

fn (p mut Parser) fmt_inc() {
	p.scanner.fmt_indent++
}

fn (p mut Parser) fmt_dec() {
	p.scanner.fmt_indent--
}

fn (p mut Parser) statement(add_semi bool) string {
	p.cgen.is_tmp = false
	tok := p.tok
	mut q := ''
	switch tok {
	case .name:
		next := p.peek()
		if p.pref.is_verbose {
			println(next.str())
		}
		// goto_label:
		if p.peek() == .colon {
			p.fmt_dec()
			label := p.check_name()
			p.fmt_inc()
			p.genln(label + ':')
			p.check(.colon)
			return ''
		}
		// `a := 777` 
		else if p.peek() == .decl_assign {
			p.log('var decl')
			p.var_decl()
		}
		else if p.lit == 'jsdecode' {
			p.js_decode()
		}
		else {
			// `a + 3`, `a(7)` or maybe just `a` 
			q = p.bool_expression()
		}
	case Token.key_goto:
		p.check(.key_goto)
		p.fgen(' ')
		label := p.check_name()
		p.genln('goto $label;')
		return ''
	case Token.key_defer:
		p.defer_st()
		return ''
	case Token.hash:
		p.chash()
		return ''
	case Token.dollar:
		p.comp_time()
	case Token.key_if:
		p.if_st(false, 0)
	case Token.key_for:
		p.for_st()
	case Token.key_switch, Token.key_match: 
		p.switch_statement()
	case Token.key_mut, Token.key_static:
		p.var_decl()
	case Token.key_return:
		p.return_st()
	case Token.lcbr:// {} block
		p.check(.lcbr) 
		p.genln('{')
		p.statements()
		return ''
	case Token.key_continue:
		if p.for_expr_cnt == 0 {
			p.error('`continue` statement outside `for`')
		}
		p.genln('continue')
		p.check(.key_continue) 
	case Token.key_break:
		if p.for_expr_cnt == 0 {
			p.error('`break` statement outside `for`')
		}
		p.genln('break')
		p.check(.key_break) 
	case Token.key_go:
		p.go_statement()
	case Token.key_assert:
		p.assert_statement()
	default:
		// An expression as a statement
		typ := p.expression()
		if p.inside_if_expr {
		}
		else {
			p.genln('; ')
		}
		return typ
	}
	// ? : uses , as statement separators
	if p.inside_if_expr && p.tok != .rcbr {
		p.gen(', ')
	}
	if add_semi && !p.inside_if_expr {
		p.genln(';')
	}
	return q
	// p.cgen.end_statement()
}

// is_map: are we in map assignment? (m[key] = val) if yes, dont generate '='
// this can be `user = ...`  or `user.field = ...`, in both cases `v` is `user`
fn (p mut Parser) assign_statement(v Var, ph int, is_map bool) {
	p.log('assign_statement() name=$v.name tok=')
	tok := p.tok
	if !v.is_mut && !v.is_arg && !p.pref.translated && !v.is_global{
		p.error('`$v.name` is immutable')
	}
	is_str := v.typ == 'string'
	switch tok {
	case Token.assign:
		if !is_map {
			p.gen(' = ')
		}
	case Token.plus_assign:
		if is_str {
			p.gen('= string_add($v.name, ')// TODO can't do `foo.bar += '!'`
		}
		else {
			p.gen(' += ')
		}
	default: p.gen(' ' + p.tok.str() + ' ')
	}
	p.fspace()
	p.fgen(tok.str()) 
	p.fspace()
	p.next()
	pos := p.cgen.cur_line.len
	expr_type := p.bool_expression()
	// Allow `num = 4` where `num` is an `?int`
	if p.assigned_type.starts_with('Option_') && expr_type == p.assigned_type.right('Option_'.len) {
		println('allowing option asss')
		expr := p.cgen.cur_line.right(pos)
		left := p.cgen.cur_line.left(pos)
		//p.cgen.cur_line = left + 'opt_ok($expr)'
		p.cgen.resetln(left + 'opt_ok($expr, sizeof($expr_type))')
	}
	else if !p.builtin_pkg && !p.check_types_no_throw(expr_type, p.assigned_type) {
		p.scanner.line_nr--
		p.error('cannot use type `$expr_type` as type `$p.assigned_type` in assignment')
	}
	if is_str && tok == .plus_assign {
		p.gen(')')
	}
	// p.assigned_var = ''
	p.assigned_type = ''
	if !v.is_used {
		p.cur_fn.mark_var_used(v)
	}
}

fn (p mut Parser) var_decl() {
	is_mut := p.tok == .key_mut || p.prev_tok == .key_for
	is_static := p.tok == .key_static
	if p.tok == .key_mut {
		p.check(.key_mut)
		p.fspace()
	}
	if p.tok == .key_static {
		p.check(.key_static)
		p.fspace()
	}
	// println('var decl tok=${p.strtok()} ismut=$is_mut')
	name := p.check_name()
	// Don't allow declaring a variable with the same name. Even in a child scope
	// (shadowing is not allowed)
	if !p.builtin_pkg && p.cur_fn.known_var(name) {
		v := p.cur_fn.find_var(name)
		p.error('redefinition of `$name`')
	}
	if name.len > 1 && contains_capital(name) {
		p.error('variable names cannot contain uppercase letters, use snake_case instead')
	}
	p.check_space(.decl_assign) // := 
	// Generate expression to tmp because we need its type first
	// [TYP .name =] bool_expression()
	pos := p.cgen.add_placeholder()
	mut typ := p.bool_expression()
	// Option check ? or {
	or_else := p.tok == .key_orelse 
	tmp := p.get_tmp()
	if or_else {
		// Option_User tmp = get_user(1);
		// if (!tmp.ok) { or_statement }
		// User user = *(User*)tmp.data;
		// p.assigned_var = ''
		p.cgen.set_placeholder(pos, '$typ $tmp = ')
		p.gen(';')
		typ = typ.replace('Option_', '')
		p.next()
		p.check(.lcbr)
		p.genln('if (!$tmp .ok) {')
		p.register_var(Var {
			name: 'err'
			typ: 'string'
			is_mut: false
			is_used: true
		})
		p.genln('string err = $tmp . error;')
		p.statements()
		p.genln('$typ $name = *($typ*) $tmp . data;')
		if !p.returns && p.prev_tok2 != .key_continue && p.prev_tok2 != .key_break {
			p.error('`or` block must return/continue/break/panic')
		}
	}
	p.register_var(Var {
		name: name
		typ: typ
		is_mut: is_mut
	})
	mut cgen_typ := typ
	if !or_else {
		gen_name := p.table.var_cgen_name(name)
		mut nt_gen := p.table.cgen_name_type_pair(gen_name, cgen_typ) + '='
		if is_static {
			nt_gen = 'static $nt_gen'
		}
		p.cgen.set_placeholder(pos, nt_gen)
	}
}

fn (p mut Parser) bool_expression() string {
	tok := p.tok
	typ := p.bterm()
	for p.tok == .and || p.tok == .logical_or {
		p.gen(' ${p.tok.str()} ')
		p.check_space(p.tok) 
		p.check_types(p.bterm(), typ)
	}
	if typ == '' {
		println('curline:')
		println(p.cgen.cur_line)
		println(tok.str())
		p.error('expr() returns empty type')
	}
	return typ
}

fn (p mut Parser) bterm() string {
	ph := p.cgen.add_placeholder()
	mut typ := p.expression()
	p.expected_type = typ 
	is_str := typ=='string' 
	tok := p.tok
	// if tok in [ .eq, .gt, .lt, .le, .ge, .ne] {
	if tok == .eq || tok == .gt || tok == .lt || tok == .le || tok == .ge || tok == .ne {
		p.fgen(' ${p.tok.str()} ')
		if is_str {
			p.gen(',')
		}
		else {
			p.gen(tok.str())
		}
		p.next()
		p.check_types(p.expression(), typ)
		typ = 'bool'
		if is_str {
			p.gen(')')
			switch tok {
			case Token.eq: p.cgen.set_placeholder(ph, 'string_eq(')
			case Token.ne: p.cgen.set_placeholder(ph, 'string_ne(')
			case Token.le: p.cgen.set_placeholder(ph, 'string_le(')
			case Token.ge: p.cgen.set_placeholder(ph, 'string_ge(')
			case Token.gt: p.cgen.set_placeholder(ph, 'string_gt(')
			case Token.lt: p.cgen.set_placeholder(ph, 'string_lt(')
			}
/* 
			 Token.eq => p.cgen.set_placeholder(ph, 'string_eq(')
			 Token.ne => p.cgen.set_placeholder(ph, 'string_ne(')
			 Token.le => p.cgen.set_placeholder(ph, 'string_le(')
			 Token.ge => p.cgen.set_placeholder(ph, 'string_ge(')
			 Token.gt => p.cgen.set_placeholder(ph, 'string_gt(')
			 Token.lt => p.cgen.set_placeholder(ph, 'string_lt(')
*/ 
		}
	}
	return typ
}

// also called on *, &, . (enum) 
fn (p mut Parser) name_expr() string {
	p.log('\nname expr() pass=$p.run tok=${p.tok.str()} $p.lit')
	// print('known type:')
	// println(p.table.known_type(p.lit))
	// hack for struct_init TODO
	hack_pos := p.scanner.pos
	hack_tok := p.tok
	hack_lit := p.lit
	ph := p.cgen.add_placeholder()
	// amp
	ptr := p.tok == .amp
	deref := p.tok == .mul
	if ptr || deref {
		p.next()
	}
	if deref {
		if p.pref.is_play && !p.builtin_pkg {
			p.error('dereferencing is temporarily disabled on the playground, will be fixed soon')
		}
	}
	mut name := p.lit
	p.fgen(name)
	// known_type := p.table.known_type(name)
	orig_name := name
	is_c := name == 'C' && p.peek() == .dot 
	mut is_c_struct_init := is_c && ptr// a := &C.mycstruct{}
	if is_c {
		p.next()
		p.check(.dot)
		name = p.lit
		p.fgen(name)
		// Currently struct init is set to true only we have `&C.Foo{}`, handle `C.Foo{}`:
		if !is_c_struct_init && p.peek() == .lcbr {
			is_c_struct_init = true
		}
	}
	// enum value? (`color == .green`) 
	if p.tok == .dot {
		//println('got enum dot val $p.left_type pass=$p.run $p.scanner.line_nr left=$p.left_type') 
		T := p.find_type(p.expected_type) 
		if T.is_enum {
			p.check(.dot) 
			val := p.check_name() 
			// Make sure this enum value exists 
			if !T.has_enum_val(val) {
				p.error('enum `$T.name` does not have value `$val`') 
			} 
			p.gen(T.mod + '__' + p.expected_type + '_' + val) 
		} 
		return p.expected_type 
	} 
	// //////////////////////////
	// module ?
	// Allow shadowing (gg = gg.newcontext(); gg.draw_triangle())
	if ((name == p.mod && p.table.known_pkg(name)) || p.import_table.known_alias(name))
		&& !p.cur_fn.known_var(name) && !is_c {
		mut pkg := name
		// must be aliased module
		if name != p.mod && p.import_table.known_alias(name) {
			// we replaced "." with "_dot_" in p.mod for C variable names, do same here.
			pkg = p.import_table.resolve_alias(name).replace('.', '_dot_')
		}
		p.next()
		p.check(.dot)
		name = p.lit
		p.fgen(name)
		name = prepend_pkg(pkg, name)
	}
	else if !p.table.known_type(name) && !p.cur_fn.known_var(name) &&
	!p.table.known_fn(name) && !p.table.known_const(name) && !is_c {
		name = p.prepend_pkg(name)
	}
	// Variable
	v := p.cur_fn.find_var(name)
	if v.name.len != 0 {
		if ptr {
			p.gen('& /*vvar*/ ')
		}
		else if deref {
			p.gen('*')
		}
		mut typ := p.var_expr(v)
		// *var
		if deref {
			if !typ.contains('*') && !typ.ends_with('ptr') {
				println('name="$name", t=$v.typ')
				p.error('dereferencing requires a pointer, but got `$typ`')
			}
			typ = typ.replace('ptr', '')// TODO
			typ = typ.replace('*', '')// TODO
		}
		// &var
		else if ptr {
			typ += '*'
		}
		return typ
	}
	// if known_type || is_c_struct_init || (p.first_run() && p.peek() == .lcbr) {
	// known type? int(4.5) or Color.green (enum)
	if p.table.known_type(name) {
		// float(5), byte(0), (*int)(ptr) etc
		if !is_c && ( p.peek() == .lpar || (deref && p.peek() == .rpar) ) {
			if deref {
				name += '*'
			}
			else if ptr {
				name += '*'
			}
			p.gen('(')
			mut typ := p.cast(name)
			p.gen(')')
			for p.tok == .dot {
				typ = p.dot(typ, ph)
			}
			return typ
		}
		// Color.green
		else if p.peek() == .dot {
			enum_type := p.table.find_type(name)
			if !enum_type.is_enum {
				p.error('`$name` is not an enum')
			} 
			p.next()
			p.check(.dot)
			val := p.lit
			// println('enum val $val')
			p.gen(enum_type.mod + '__' + enum_type.name + '_' + val)// `color = main__Color_green`
			p.next()
			return enum_type.name
		}
		else if p.peek() == .lcbr {
			// go back to name start (pkg.name)
			p.scanner.pos = hack_pos
			p.tok = hack_tok
			p.lit = hack_lit
			// TODO hack. If it's a C type, we may need to add struct before declaration:
			// a := &C.A{}  ==>  struct A* a = malloc(sizeof(struct A));
			if is_c_struct_init {
				p.is_c_struct_init = true
				p.cgen.insert_before('struct /*c struct init*/')
			}
			return p.struct_init(is_c_struct_init)
		}
	}
	// C fn
	if is_c {
		f := Fn {
			name: name// .replace('c_', '')
			is_c: true
		}
		p.fn_call(f, 0, '', '')
		// Try looking it up. Maybe its defined with "C.fn_name() fn_type",
		// then we know what type it returns
		cfn := p.table.find_fn(name)
		// Not Found? Return 'void*'
		if cfn.name == '' {
			return 'void*'
		}
		return cfn.typ
	}
	// Constant
	mut c := p.table.find_const(name)
	if c.name != '' && ptr && !c.is_global {
		p.error('cannot take the address of constant `$c.name`')
	}
	if c.name.len != 0 {
		if ptr {
			// c.ptr = true
			p.gen('& /*const*/ ')
		}
		p.log('calling var expr')
		mut typ := p.var_expr(c)
		if ptr {
			typ += '*'
		}
		return typ
	}
	// Function (not method btw, methods are handled in dot())
	mut f := p.table.find_fn(name)
	if f.name == '' {
		// We are in a second pass, that means this function was not defined, throw an error. 
		if !p.first_run() {
			// V script? Try os module. 
			if p.v_script { 
				name = name.replace('main__', 'os__') 
				f = p.table.find_fn(name) 
			} 
			if f.name == '' { 
				// If orig_name is a pkg, then printing undefined: `pkg` tells us nothing
				// if p.table.known_pkg(orig_name) {
				if p.table.known_pkg(orig_name) || p.import_table.known_alias(orig_name) {
					name = name.replace('__', '.').replace('_dot_', '.')
					p.error('undefined: `$name`')
				}
				else {
					p.error('undefined: `$orig_name`')
				}
			} 
		} else { 
			p.next()
			// First pass, the function can be defined later. 
			return 'void'
		} 
	}
	// no () after func, so func is an argument, just gen its name
	// TODO verify this and handle errors
	if p.peek() != .lpar {
		p.gen(p.table.cgen_name(f))
		p.next()
		return 'void*'
	}
	// TODO bring back
	if f.typ == 'void' && !p.inside_if_expr {
		// p.error('`$f.name` used as value')
	}
	p.log('calling function')
	p.fn_call(f, 0, '', '')
	// dot after a function call: `get_user().age`
	if p.tok == .dot {
		mut typ := ''
		for p.tok == .dot {
			// println('dot #$dc')
			typ = p.dot(f.typ, ph)
		}
		return typ
	}
	p.log('end of name_expr')
	return f.typ
}

fn (p mut Parser) var_expr(v Var) string {
	p.log('\nvar_expr() v.name="$v.name" v.typ="$v.typ"')
	// println('var expr is_tmp=$p.cgen.is_tmp\n')
	p.cur_fn.mark_var_used(v) 
	fn_ph := p.cgen.add_placeholder()
	p.expr_var = v
	p.gen(p.table.var_cgen_name(v.name))
	p.next()
	mut typ := v.typ
	// Function pointer? 
	if typ.starts_with('fn ') {
		//println('CALLING FN PTR')
		//p.print_tok()
		T := p.table.find_type(typ)
		p.gen('(')
		p.fn_call_args(T.func)
		p.gen(')')
		typ = T.func.typ
	}
	// users[0] before dot so that we can have
	// users[0].name
	if p.tok == .lsbr {
		typ = p.index_expr(typ, fn_ph)
		// ////println('QQQQ KEK $typ')
	}
	// a.b.c().d chain
	// mut dc := 0
	for p.tok ==.dot {
		// println('dot #$dc')
		typ = p.dot(typ, fn_ph)
		p.log('typ after dot=$typ')
		// print('tok after dot()')
		// p.print_tok()
		// dc++
		if p.tok == .lsbr {
			// typ = p.index_expr(typ, fn_ph, v)
		}
	}
	// a++ and a--
	if p.tok == .inc || p.tok == .dec {
		if !v.is_mut && !v.is_arg && !p.pref.translated {
			p.error('`$v.name` is immutable')
		}
		if typ != 'int' {
			if !p.pref.translated && !is_number_type(typ) {
				p.error('cannot ++/-- value of type `$typ`')
			}
		}
		p.gen(p.tok.str())
		p.fgen(p.tok.str())
		p.next()// ++/-- 
		// allow `a := c++` in translated code 
		if p.pref.translated {
			//return p.index_expr(typ, fn_ph)
		}
		else {
			return 'void'
		}
	}
	typ = p.index_expr(typ, fn_ph)
	// TODO hack to allow `foo.bar[0] = 2` 
	if p.tok == .dot { 
		for p.tok == .dot {
			typ = p.dot(typ, fn_ph)
		} 
		typ = p.index_expr(typ, fn_ph)
	} 
	return typ
}

fn (p &Parser) fileis(s string) bool {
	return p.scanner.file_path.contains(s)
}

// user.name => `str_typ` is `User`
// user.company.name => `str_typ` is `Company`
fn (p mut Parser) dot(str_typ string, method_ph int) string {
	p.check(.dot)
	mut field_name := p.lit
	if p.tok == .key_type {
		field_name = 'type' 
	} 
	p.fgen(field_name) 
	p.log('dot() field_name=$field_name typ=$str_typ')
	//if p.fileis('main.v') {
		//println('dot() field_name=$field_name typ=$str_typ prev_tok=${prev_tok.str()}') 
	//}
	typ := p.find_type(str_typ)
	if typ.name.len == 0 {
		p.error('dot(): cannot find type `$str_typ`')
	}
	has_field := p.table.type_has_field(typ, field_name)
	has_method := p.table.type_has_method(typ, field_name)
	if !typ.is_c && !has_field && !has_method && !p.first_run() {
		if typ.name.starts_with('Option_') {
			opt_type := typ.name.right(7) 
			p.error('unhandled option type: $opt_type?')
		}
		println('error in dot():')
		println('fields:')
		for field in typ.fields {
			println(field.name)
		}
		println('methods:')
		for field in typ.methods {
			println(field.name)
		}
		println('str_typ=="$str_typ"')
		p.error('type `$typ.name`  has no field or method `$field_name`')
	}
	mut dot := '.'
	if str_typ.contains('*') {
		dot = '->'
	}
	// field
	if has_field {
		field := p.table.find_field(typ, field_name)
		// Is the next token `=`, `+=` etc?  (Are we modifying the field?)
		next := p.peek()
		modifying := next.is_assign() || next == .inc || next == .dec
		is_vi := p.fileis('vi')
		if !p.builtin_pkg && !p.pref.translated && modifying && !field.is_mut && !is_vi {
			p.error('cannot modify immutable field `$field_name` (type `$typ.name`)')
		}
		if !p.builtin_pkg && p.mod != typ.mod {
		}
		// if p.pref.is_play && field.access_mod ==.private && !p.builtin_pkg && p.mod != typ.mod {
		// Don't allow `arr.data`
		if field.access_mod == .private && !p.builtin_pkg && !p.pref.translated && p.mod != typ.mod {
			// println('$typ.name :: $field.name ')
			// println(field.access_mod)
			p.error('cannot refer to unexported field `$field_name` (type `$typ.name`)')
		}
		// if field.access_mod ==.public && p.peek() == .assign && !p.builtin_pkg && p.mod != typ.mod {
		// Don't allow `str.len = 0`
		if field.access_mod == .public && !p.builtin_pkg && p.mod != typ.mod {
			if !field.is_mut && !p.pref.translated && modifying {
				p.error('cannot modify public immutable field `$field_name` (type `$typ.name`)')
			}
		}
		p.gen(dot + field_name)
		p.next()
		return field.typ
	}
	// method
	mut method := p.table.find_method(typ, field_name)
	p.fn_call(method, method_ph, '', str_typ)
	// Methods returning `array` should return `array_string` 
	if method.typ == 'array' && typ.name.starts_with('array_') {
		return typ.name
	}
	// Array methods returning `voidptr` (like `last()`) should return element type
	if method.typ == 'void*' && typ.name.starts_with('array_') {
		return typ.name.right(6)
	}
	//if false && p.tok == .lsbr {
		// if is_indexer {
		//return p.index_expr(method.typ, method_ph)
	//}
	return method.typ
}

fn (p mut Parser) index_expr(typ string, fn_ph int) string {
	//if p.fileis('main.v') {
		//println('index expr typ=$typ')
	//}
	// a[0]
	v := p.expr_var
	is_map := typ.starts_with('map_')
	is_str := typ == 'string'
	is_arr0 := typ.starts_with('array_')
	is_arr := is_arr0 || typ == 'array'
	is_ptr := typ == 'byte*' || typ == 'byteptr' || typ.contains('*')
	is_indexer := p.tok == .lsbr
	mut close_bracket := false
	if is_indexer {
		is_fixed_arr := typ[0] == `[`
		if !is_str && !is_arr && !is_map && !is_ptr && !is_fixed_arr {
			p.error('Cant [] non-array/string/map. Got type "$typ"')
		}
		p.check(.lsbr)
		// Get element type (set `typ` to it)
		if is_str {
			typ = 'byte'
			p.fgen('[')
			// Direct faster access to .str[i] in builtin package
			if p.builtin_pkg {
				p.gen('.str[')
				close_bracket = true
			}
			else {
				// Bounds check everywhere else
				p.gen(',')
			}
		}
		if is_fixed_arr {
			// `[10]int` => `int`, `[10][3]int` => `[3]int`
			if typ.contains('][') {
				pos := typ.index_after('[', 1)
				typ = typ.right(pos)
			}
			else {
				typ = typ.all_after(']')
			}
			p.gen('[')
			close_bracket = true
		}
		else if is_ptr {
			// typ = 'byte'
			typ = typ.replace('*', '')
			// modify(mut []string) fix
			if !is_arr {
				p.gen('[/*ptr*/')
				close_bracket = true
			}
		}
		if is_arr {
			//p.fgen('[')
			// array_int a; a[0]
			// type is "array_int", need "int"
			// typ = typ.replace('array_', '')
			if is_arr0 {
				if p.fileis('int_test') {
					println('\nRRRR0 $typ')
				}
				typ = typ.right(6)
				if p.fileis('int_test') {
					println('RRRR $typ')
				}
			}
			// array a; a.first() voidptr
			// type is "array", need "void*"
			if typ == 'array' {
				typ = 'void*'
			}
			// No bounds check in translated from C code
			if p.pref.translated && !p.builtin_pkg{
				// Cast void* to typ*: add (typ*) to the beginning of the assignment :
				// ((int*)a.data = ...
				p.cgen.set_placeholder(fn_ph, '(($typ*)(')
				p.gen('.data))[')
			}
			else {
				p.gen(',')
			}
		}
		// map is tricky
		// need to replace "m[key] = val" with "tmp = val; map_set(&m, key, &tmp)"
		// need to replace "m[key]"       with "tmp = val; map_get(&m, key, &tmp)"
		// can only do that later once we know whether there's an "=" or not
		if is_map {
			typ = typ.replace('map_', '')
			if typ == 'map' {
				typ = 'void*'
			}
			p.gen(',')
		}
		// expression inside [ ]
		if is_arr {
			T := p.table.find_type(p.expression())
			if T.parent != 'int' {
				p.check_types(T.name, 'int')
			}
		}
		else {
			p.expression()
		}
		p.check(.rsbr)
		// if (is_str && p.builtin_pkg) || is_ptr || is_fixed_arr && ! (is_ptr && is_arr) {
		if close_bracket {
			p.gen(']/*r$typ $v.is_mut*/')
		}
	}
	// TODO if p.tok in ...
	// if p.tok in [.assign, .plus_assign, .minus_assign]
	if p.tok == .assign || p.tok == .plus_assign || p.tok == .minus_assign ||
	p.tok == .mult_assign || p.tok == .div_assign || p.tok == .xor_assign || p.tok == .mod_assign ||
	p.tok == .or_assign || p.tok == .and_assign || p.tok == .righ_shift_assign ||
	p.tok == .left_shift_assign {
		if is_indexer && is_str && !p.builtin_pkg {
			p.error('strings are immutable')
		}
		assign_pos := p.cgen.cur_line.len
		p.assigned_type = typ
		p.expected_type = typ
		p.assign_statement(v, fn_ph, is_indexer && (is_map || is_arr))
		// m[key] = val
		if is_indexer && (is_map || is_arr) {
			// a[0] = 7
			// curline right now: "a , 0  =  7"
			// println('222 "$p.cgen.cur_line"')
			// Cant have &7, so need a tmp
			tmp := p.get_tmp()
			tmp_val := p.cgen.cur_line.right(assign_pos)
			p.cgen.resetln(p.cgen.cur_line.left(assign_pos))
			// val := p.cgen.end_tmp()
			if is_map {
				p.cgen.set_placeholder(fn_ph, 'map__set(&')
			}
			else {
				if is_ptr {
					p.cgen.set_placeholder(fn_ph, 'array_set(')
				}
				else {
					p.cgen.set_placeholder(fn_ph, 'array_set(&/*q*/')
				}
			}
			p.gen(', & $tmp)')
			p.cgen.insert_before('$typ $tmp = $tmp_val;')
		}
		return typ
		return 'void'
	}
	// else if p.pref.is_verbose && p.assigned_var != '' {
	// p.error('didnt assign')
	// }
	// m[key]. no =, just a getter
	else if (is_map || is_arr || (is_str && !p.builtin_pkg)) && is_indexer {
		// Erase var name we generated earlier:	"int a = m, 0"
		// "m, 0" gets killed since we need to start from scratch. It's messy.
		// "m, 0" is an index expression, save it before deleting and insert later in map_get()
		index_expr := p.cgen.cur_line.right(fn_ph)
		p.cgen.resetln(p.cgen.cur_line.left(fn_ph))
		// Can't pass integer literal, because map_get() requires a void*
		tmp := p.get_tmp()
		tmp_ok := p.get_tmp()
		if is_map {
			p.gen('$tmp')
			def := type_default(typ)
			p.cgen.insert_before('$typ $tmp = $def; bool $tmp_ok = map_get($index_expr, & $tmp);')
		}
		else if is_arr {
			if p.pref.translated && !p.builtin_pkg {
				p.gen('$index_expr ]')
			}
			else {
				p.gen('( *($typ*) array__get($index_expr) )')
			}
		}
		else if is_str && !p.builtin_pkg {
			p.gen('string_at($index_expr)')
		}
		// Zero the string after map_get() if it's nil, numbers are automatically 0
		// This is ugly, but what can I do without generics?
		// TODO what about user types?
		if is_map && typ == 'string' {
			// p.cgen.insert_before('if (!${tmp}.str) $tmp = tos("", 0);')
			p.cgen.insert_before('if (!$tmp_ok) $tmp = tos("", 0);')
		}
	}
	// else if is_arr && is_indexer{}
	return typ
}

// returns resulting type
fn (p mut Parser) expression() string {
	if p.scanner.file_path.contains('test_test') {
		println('expression() pass=$p.run tok=')
		p.print_tok()
	}
	p.cgen('/* expr start*/')
	ph := p.cgen.add_placeholder()
	mut typ := p.term()
	is_str := typ=='string' 
	// a << b ==> array2_push(&a, b)
	if p.tok == .left_shift {
		if typ.contains('array_') {
			// Can't pass integer literal, because push requires a void*
			// a << 7 => int tmp = 7; array_push(&a, &tmp);
			// _PUSH(&a, expression(), tmp, string)
			tmp := p.get_tmp()
			tmp_typ := typ.right(6)// skip "array_"
			p.check_space(.left_shift) 
			// Get the value we are pushing
			p.gen(', (')
			// Imkey_mut? Can we push?
			if !p.expr_var.is_mut && !p.pref.translated {
				p.error('`$p.expr_var.name` is immutable (can\'t <<)')
			}
			expr_type := p.expression() 
			// Two arrays of the same type? 
			push_array :=  typ == expr_type
			if push_array { 
				p.cgen.set_placeholder(ph, '_PUSH_MANY(&' ) 
				p.gen('), $tmp, $typ)')
			}  else { 
				p.check_types(expr_type, tmp_typ)
				// Pass tmp var info to the _PUSH macro
				// Prepend tmp initialisation and push call
				// Don't dereference if it's already a key_mut array argument  (`fn foo(mut []int)`)
				push_call := if typ.contains('*'){'_PUSH('} else { '_PUSH(&'} 
				p.cgen.set_placeholder(ph, push_call)
				p.gen('), $tmp, $tmp_typ)')
			} 
			return 'void'
		}
		else {
			p.next()
			p.gen(' << ')
			p.check_types(p.expression(), typ)
			return 'int'
		}
	}
	// a in [1,2,3]
	if p.tok == .key_in {
		p.fgen(' ')
		p.check(.key_in)
		p.fgen(' ')
		p.gen(', ')
		arr_typ := p.expression()
		if !arr_typ.starts_with('array_') {
			p.error('`in` requires an array')
		}
		T := p.table.find_type(arr_typ)
		if !T.has_method('contains') {
			p.error('$arr_typ has no method `contains`')
		}
		// `typ` is element type
		p.cgen.set_placeholder(ph, '_IN($typ, ')
		p.gen(')')
		return 'bool'
	}
	if p.tok == .righ_shift {
		p.next()
		p.gen(' >> ')
		p.check_types(p.expression(), typ)
		return 'int'
	}
	if p.tok == .dot  { 
		for p.tok == .dot {
			typ = p.dot(typ, ph)
		}
	}
	// + - |
	for p.tok == .plus || p.tok == .minus || p.tok == .pipe || p.tok == .amp || p.tok == .xor {
		// for p.tok in [.plus, .minus, .pipe, .amp, .xor] {
		tok_op := p.tok
		is_num := typ == 'void*' || typ == 'byte*' || is_number_type(typ)
		p.check_space(p.tok) 
		if is_str && tok_op == .plus {
			p.cgen.set_placeholder(ph, 'string_add(')
			p.gen(',')
		}
		// 3 + 4
		else if is_num {
			p.gen(tok_op.str())
		}
		// Vec + Vec
		else {
			if p.pref.translated {
				p.gen(tok_op.str() + ' /*doom hack*/')// TODO hack to fix DOOM's angle_t
			}
			else {
				p.gen(',')
			}
		}
		p.check_types(p.term(), typ)
		if is_str && tok_op == .plus {
			p.gen(')')
		}
		// Make sure operators are used with correct types
		if !p.pref.translated && !is_str && !is_num {
			T := p.table.find_type(typ)
			if tok_op == .plus {
				if T.has_method('+') {
					p.cgen.set_placeholder(ph, typ + '_plus(')
					p.gen(')')
				}
				else {
					p.error('operator + not defined on `$typ`')
				}
			}
			else if tok_op == .minus {
				if T.has_method('-') {
					p.cgen.set_placeholder(ph, '${typ}_minus(')
					p.gen(')')
				}
				else {
					p.error('operator - not defined on `$typ`')
				}
			}
		}
	}
	return typ
}

fn (p mut Parser) term() string {
	line_nr := p.scanner.line_nr
	if p.fileis('fn_test') {
		println('\nterm() $line_nr')
	}
	typ := p.unary()
	if p.fileis('fn_test') {
		println('2: $line_nr')
	}
	// `*` on a newline? Can't be multiplication, only dereference
	if p.tok == .mul && line_nr != p.scanner.line_nr {
		return typ
	}
	for p.tok == .mul || p.tok == .div || p.tok == .mod {
		tok := p.tok
		is_div := tok == .div
		is_mod := tok == .mod
		// is_mul := tok == .mod
		p.next()
		p.gen(tok.str())// + ' /*op2*/ ')
		p.fgen(' ' + tok.str() + ' ')
		if is_div && p.tok == .number && p.lit == '0' {
			p.error('division by zero')
		}
		if is_mod && (is_float_type(typ) || !is_number_type(typ)) {
			p.error('operator .mod requires integer types')
		}
		p.check_types(p.unary(), typ)
	}
	return typ
}

fn (p mut Parser) unary() string {
	mut typ := ''
	tok := p.tok
	switch tok {
	case Token.not:
		p.gen('!')
		p.check(.not) 
		typ = 'bool'
		p.bool_expression()
	case Token.bit_not:
		p.gen('~')
		p.check(.bit_not) 
		typ = p.bool_expression()
	default:
		typ = p.factor()
	}
	return typ
}

fn (p mut Parser) factor() string {
	mut typ := ''
	tok := p.tok
	switch tok {
	case .number:
		typ = 'int'
		// Check if float (`1.0`, `1e+3`) but not if is hexa
		if (p.lit.contains('.') || (p.lit.contains('e') || p.lit.contains('E'))) && 
			!(p.lit[0] == `0` && (p.lit[1] == `x` || p.lit[1] == `X`)) {
			typ = 'f32'
			// typ = 'f64' // TODO 
		} else {
			v_u64 := p.lit.u64()
			if u64(u32(v_u64)) < v_u64 {
				typ = 'u64'
			}
		}
		if p.expected_type != '' && !is_valid_int_const(p.lit, p.expected_type) { 
			p.error('constant `$p.lit` overflows `$p.expected_type`') 
		} 
		p.gen(p.lit)
		p.fgen(p.lit)
	case Token.minus:
		p.gen('-')
		p.fgen('-')
		p.next()
		return p.factor()
		// Variable
	case Token.key_sizeof:
		p.gen('sizeof(')
		p.fgen('sizeof(')
		p.next()
		p.check(.lpar)
		mut sizeof_typ := p.get_type()
		if sizeof_typ.ends_with('*') {
			// Move * from the end to the beginning, as C requires
			sizeof_typ = '*' + sizeof_typ.left(sizeof_typ.len - 1)
		}
		p.check(.rpar)
		p.gen('$sizeof_typ)')
		p.fgen('$sizeof_typ)')
		return 'int'
	case Token.amp:
		return p.name_expr()
	case Token.dot:
		return p.name_expr()// `.green` (enum)
	case Token.mul:
		return p.name_expr()
	case Token.name:
		// map[string]int
		if p.lit == 'map' && p.peek() == .lsbr {
			return p.map_init()
		}
		if p.lit == 'json' && p.peek() == .dot {
			return p.js_decode()
		}
		typ = p.name_expr()
		return typ
	case Token.key_default: 
		p.next()
		p.next()
		name := p.check_name()
		if name != 'T' {
			p.error('default needs T')
		}
		p.gen('default(T)')
		p.next()
		return 'T'
	case Token.lpar:
		//p.gen('(/*lpar*/')
		p.gen('(')
		p.check(.lpar) 
		typ = p.bool_expression()
		// Hack. If this `)` referes to a ptr cast `(*int__)__`, it was already checked
		// TODO: fix parser so that it doesn't think it's a par expression when it sees `(` in
		// __(__*int)(
		if !p.ptr_cast {
			p.check(.rpar)
		}
		p.ptr_cast = false
		p.gen(')')
		return typ
	case Token.chartoken:
		p.char_expr()
		typ = 'byte'
		return typ
	case Token.str:
		p.string_expr()
		typ = 'string'
		return typ
	case Token.key_false:
		typ = 'bool'
		p.gen('0')
		p.fgen('false')
	case Token.key_true:
		typ = 'bool'
		p.gen('1')
		p.fgen('true')
	case Token.lsbr:
		// `[1,2,3]` or `[]` or `[20]byte`
		// TODO have to return because arrayInit does next()
		// everything should do next()
		return p.array_init()
	case Token.lcbr:
		// { user | name :'new name' }
		return p.assoc()
	case Token.key_if:
		typ = p.if_st(true, 0)
		return typ
	default:
		next := p.peek()
		println('PREV=${p.prev_tok.str()}')
		println('.neXT=${next.str()}')
		p.error('unexpected token: `${p.tok.str()}`')
	}
	p.next()// TODO everything should next()
	return typ
}

// { user | name: 'new name' }
fn (p mut Parser) assoc() string {
	// println('assoc()')
	p.next()
	name := p.check_name()
	if !p.cur_fn.known_var(name) {
		p.error('unknown variable `$name`')
	}
	var := p.cur_fn.find_var(name)
	p.check(.pipe)
	p.gen('($var.typ){')
	mut fields := []string// track the fields user is setting, the rest will be copied from the old object
	for p.tok != .rcbr {
		field := p.check_name()
		fields << field
		p.gen('.$field = ')
		p.check(.colon)
		p.bool_expression()
		p.gen(',')
		if p.tok != .rcbr {
			p.check(.comma)
		}
	}
	// Copy the rest of the fields
	T := p.table.find_type(var.typ)
	for ffield in T.fields {
		f := ffield.name
		if f in fields {
			continue
		}
		p.gen('.$f = $name . $f,')
	}
	p.check(.rcbr)
	p.gen('}')
	return var.typ
}

fn (p mut Parser) char_expr() {
	p.gen('\'$p.lit\'')
	p.next()
}


fn (p mut Parser) typ_to_fmt(typ string, level int) string {
	t := p.table.find_type(typ)
	if t.is_enum { 
		return '%d'
	}
	switch typ {
	case 'string': return '%.*s'
	case 'ustring': return '%.*s'
	case 'byte', 'int', 'char', 'byte', 'bool', 'u32', 'i32', 'i16', 'u16', 'i8', 'u8': return '%d'
	case 'f64', 'f32': return '%f'
	case 'i64', 'u64': return '%lld'
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

fn format_str(str string) string {
	str = str.replace('"', '\\"')
	$if windows {
		str = str.replace('\r\n', '\\n')
	} 
	str = str.replace('\n', '\\n')
	return str
}

fn (p mut Parser) string_expr() {
	// println('.str EXPR')
	str := p.lit
	// No ${}, just return a simple string
	if p.peek() != .dollar {
		p.fgen('\'$str\'')
		// println('before format: "$str"')
		f := format_str(str)
		// println('after format: "$str"')
		if p.calling_c || (p.pref.translated && p.mod == 'main') {
			p.gen('"$f"')
		}
		else {
			p.gen('tos2("$f")') 
		}
		p.next()
		return
	}
	// tmp := p.get_tmp()
	mut args := '"'
	mut format := '"'
	p.fgen('\'') 
	mut complex_inter := false  // for vfmt 
	for p.tok == .str {
		// Add the string between %d's
		p.fgen(p.lit) 
		p.lit = p.lit.replace('%', '%%')
		format += format_str(p.lit)
		p.next()// skip $
		if p.tok != .dollar {
			continue
		}
		// Handle .dollar
		p.check(.dollar) 
		// If there's no string after current token, it means we are in
		// a complex expression (`${...}`) 
		if p.peek() != .str {
			p.fgen('{') 
			complex_inter = true 
		} 
		// Get bool expr inside a temp var
		p.cgen.start_tmp()
		typ := p.bool_expression()
		mut val := p.cgen.end_tmp()
		val = val.trim_space()
		args += ', $val'
		if typ == 'string' {
			// args += '.str'
			// printf("%.*s", a.len, a.str) syntax
			args += '.len, ${val}.str'
		}
		if typ == 'ustring' {
			args += '.len, ${val}.s.str'
		}
		// Custom format? ${t.hour:02d}
		custom := p.tok == .colon
		if custom {
			format += '%'
			p.next()
			if p.tok == .dot {
				format += '.'
				p.next()
			}
			format += p.lit// 02
			p.next()
			format += p.lit// f
			// println('custom str F=$format')
			p.next()
		}
		else {
			f := p.typ_to_fmt(typ, 0) 
			if f == '' { 
				p.error('unhandled sprintf format "$typ" ')
			} 
			format += f 
		}
	}
	if complex_inter {
	p.fgen('}') 
} 
	p.fgen('\'') 
	// println("hello %d", num) optimization.
	if p.cgen.nogen {
		return
	}
	// println: don't allocate a new string, just print	it. 
	cur_line := p.cgen.cur_line.trim_space()
	if cur_line.contains('println (') && p.tok != .plus && 
		!cur_line.contains('string_add') && !cur_line.contains('eprintln') {
		p.cgen.resetln(cur_line.replace('println (', 'printf('))
		p.gen('$format\\n$args')
		return
	}
	// '$age'! means the user wants this to be a tmp string (uses global buffer, no allocation,
	// won't be used	again)
	if p.tok == .not {
		p.check(.not) 
		p.gen('_STR_TMP($format$args)')
	}
	else {
		// Otherwise do ugly len counting + allocation + sprintf
		p.gen('_STR($format$args)')
	}
}

// m := map[string]int{}
fn (p mut Parser) map_init() string {
	p.next()
	p.check(.lsbr)
	key_type := p.check_name()
	if key_type != 'string' {
		p.error('only string key maps allowed for now')
	}
	p.check(.rsbr)
	val_type := p.check_name()
	if !p.table.known_type(val_type) {
		p.error('map init unknown type "$val_type"')
	}
	typ := 'map_$val_type'
	p.register_map(typ)
	p.gen('new_map(1, sizeof($val_type))')
	p.check(.lcbr)
	p.check(.rcbr)
	return typ
}

// [1,2,3]
fn (p mut Parser) array_init() string {
	p.check(.lsbr)
	is_integer := p.tok == .number
	lit := p.lit
	mut typ := ''
	new_arr_ph := p.cgen.add_placeholder()
	mut i := 0
	pos := p.cgen.cur_line.len// remember cur line to fetch first number in cgen  for [0; 10]
	for p.tok != .rsbr {
		val_typ := p.bool_expression()
		// Get type of the first expression
		if i == 0 {
			typ = val_typ
			// fixed width array initialization? (`arr := [20]byte`)
			if is_integer && p.tok == .rsbr && p.peek() == .name {
				nextc := p.scanner.text[p.scanner.pos + 1]
				// TODO whitespace hack
				// Make sure there's no space in `[10]byte`
				if !nextc.is_space() {
					p.check(.rsbr)
					name := p.check_name()
					if p.table.known_type(name) {
						p.cgen.resetln('')
						p.gen('{}')
						return '[$lit]$name'
					}
					else {
						p.error('bad type `$name`')
					}
				}
			}
		}
		if val_typ != typ {
			if !p.check_types_no_throw(val_typ, typ) {
				p.error('bad array element type `$val_typ` instead of `$typ`')
			}
		}
		if p.tok != .rsbr && p.tok != .semicolon {
			p.gen(', ')
			p.check(.comma)
			p.fspace() 
		}
		i++
		// Repeat (a = [0;5] )
		if i == 1 && p.tok == .semicolon {
			p.check_space(.semicolon)
			val := p.cgen.cur_line.right(pos)
			// p.cgen.cur_line = ''
			p.cgen.resetln(p.cgen.cur_line.left(pos))
			// Special case for zero
			if false && val.trim_space() == '0' {
				p.gen('array_repeat( & V_ZERO, ')
			}
			else {
				tmp := p.get_tmp()
				p.cgen.insert_before('$typ $tmp = $val;')
				p.gen('array_repeat(&$tmp, ')
			}
			p.check_types(p.bool_expression(), 'int')
			p.gen(', sizeof($typ) )')
			p.check(.rsbr)
			return 'array_$typ'
		}
	}
	p.check(.rsbr)
	// type after `]`? (e.g. "[]string")
	if p.tok != .name && i == 0 {
		p.error('specify array type: `[]typ` instead of `[]`')
	}
	if p.tok == .name && i == 0 {
		// vals.len == 0 {
		typ = p.get_type()
		// println('.key_goT TYP after [] $typ')
	}
	// ! after array => no malloc and no copy
	no_alloc := p.tok == .not 
	if no_alloc {
		p.next()
	}
	// [1,2,3]!! => [3]int{1,2,3}
	is_fixed_size := p.tok == .not 
	if is_fixed_size {
		p.next()
		p.gen(' }')
		if !p.first_run() {
			// If we are defining a const array, we don't need to specify the type:
			// `a = {1,2,3}`, not `a = (int[]) {1,2,3}`
			if p.inside_const {
				p.cgen.set_placeholder(new_arr_ph, '{ ')
			}
			else {
				p.cgen.set_placeholder(new_arr_ph, '($typ[]) { ')
			}
		}
		return '[$i]$typ'
	}
	// if ptr {
	// typ += '_ptr"
	// }
	mut new_arr := 'new_array_from_c_array'
	if no_alloc {
		new_arr += '_no_alloc'
	}
	p.gen(' })')
	// p.gen('$new_arr($vals.len, $vals.len, sizeof($typ), ($typ[]) $c_arr );')
	// TODO why need !first_run()?? Otherwise it goes to the very top of the out.c file
	if !p.first_run() {
		p.cgen.set_placeholder(new_arr_ph, '$new_arr($i, $i, sizeof($typ), ($typ[]) { ')
	}
	typ = 'array_$typ'
	p.register_array(typ)
	return typ
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

fn (p mut Parser) struct_init(is_c_struct_init bool) string {
	p.is_struct_init = true
	mut typ := p.get_type()
	p.scanner.fmt_out.cut(typ.len)
	ptr := typ.contains('*')
	// TODO tm struct struct bug
	if typ == 'tm' {
		p.cgen.lines[p.cgen.lines.len-1] = ''
		p.cgen.lines[p.cgen.lines.len-2] = ''
	}
	p.check(.lcbr)
	// tmp := p.get_tmp()
	if !ptr {
		if p.is_c_struct_init {
			p.gen('(struct $typ) {')
			p.is_c_struct_init = false
		}
		else {
			p.gen('($typ) {')
		}
	}
	else {
		// TODO tmp hack for 0 pointers init
		// &User{!} ==> 0
		if p.tok == .not {
			p.next()
			p.gen('0')
			p.check(.rcbr)
			return typ
		}
		no_star := typ.replace('*', '')
		p.gen('ALLOC_INIT($no_star, {')
	}
	// Loop thru all struct init keys and assign values
	// u := User{age:20, name:'bob'}
	// Remember which fields were set, so that we dont have to zero them later
	mut inited_fields := []string
	peek := p.peek()
	if peek == .colon || p.tok == .rcbr {
		t := p.table.find_type(typ)
		for p.tok != .rcbr {
			field := p.check_name()
			if !t.has_field(field) {
				p.error('`$t.name` has no field `$field`')
			}
			f := t.find_field(field) 
			inited_fields << field
			p.gen('.$field = ')
			p.check(.colon)
			p.fspace()
			p.check_types(p.bool_expression(),  f.typ) 
			if p.tok == .comma {
				p.next()
			}
			if p.tok != .rcbr {
				p.gen(',')
			}
			p.fgenln('')
		}
		// If we already set some fields, need to prepend a comma
		if t.fields.len != inited_fields.len && inited_fields.len > 0 {
			p.gen(',')
		}
		// Zero values: init all fields (ints to 0, strings to '' etc)
		for i, field in t.fields {
			// println('### field.name')
			// Skip if this field has already been assigned to
			if inited_fields.contains(field.name) {
				continue
			}
			field_typ := field.typ
			if !p.builtin_pkg && field_typ.ends_with('*') && field_typ.contains('Cfg') {
				p.error('pointer field `${typ}.${field.name}` must be initialized')
			}
			def_val := type_default(field_typ)
			if def_val != '' && def_val != '{}' {
				p.gen('.$field.name = $def_val')
				if i != t.fields.len - 1 {
					p.gen(',')
				}
			}
		}
	}
	// Point{3,4} syntax
	else {
		mut T := p.table.find_type(typ)
		// Aliases (TODO Hack, implement proper aliases)
		if T.fields.len == 0 && T.parent != '' {
			T = p.table.find_type(T.parent)
		}
		for i, ffield in T.fields {
			expr_typ := p.bool_expression()
			if !p.check_types_no_throw(expr_typ, ffield.typ) {
				p.error('field value #${i+1} `$ffield.name` has type `$ffield.typ`, got `$expr_typ` ')
			}
			if i < T.fields.len - 1 {
				if p.tok != .comma {
					p.error('too few values in `$typ` literal (${i+1} instead of $T.fields.len)')
				}
				p.gen(',')
				p.next()
			}
		}
		// Allow `user := User{1,2,3,}`
		// The final comma will be removed by vfmt, since we are not calling `p.fgen()`
		if p.tok == .comma {
			p.next()
		}
		if p.tok != .rcbr {
			p.error('too many fields initialized: `$typ` has $T.fields.len field(s)')
		}
	}
	p.gen('}')
	if ptr {
		p.gen(')')
	}
	p.check(.rcbr)
	p.is_struct_init = false
	return typ
}

// `f32(3)`
// tok is `f32` or `)` if `(*int)(ptr)`
fn (p mut Parser) cast(typ string) string {
	p.next()
	pos := p.cgen.add_placeholder()
	if p.tok == .rpar {
		// skip `)` if it's `(*int)(ptr)`, not `int(a)`
		p.ptr_cast = true
		p.next()
	}
	p.check(.lpar)
	p.expected_type = typ 
	expr_typ := p.bool_expression()
	p.expected_type = '' 
	// `string(buffer)` => `tos2(buffer)`
	// `string(buffer, len)` => `tos(buffer, len)`
	if typ == 'string' && (expr_typ == 'byte*' || expr_typ == 'byteptr') {
		if p.tok == .comma { 
			p.check(.comma) 
			p.cgen.set_placeholder(pos, 'tos(')
			p.gen(', ') 
			p.check_types(p.expression(), 'int') 
		}  else { 
			p.cgen.set_placeholder(pos, 'tos2(')
		} 
	}
	// `string(234)` => error
	else if typ == 'string' && expr_typ == 'int' {
		p.error('cannot cast `$expr_typ` to `$typ`, use `str()` method instead')
	} 
	else {
		p.cgen.set_placeholder(pos, '($typ)(')
	}
	p.check(.rpar)
	p.gen(')')
	return typ
}

fn (p mut Parser) get_tmp() string {
	p.tmp_cnt++
	return 'tmp$p.tmp_cnt'
}

fn (p mut Parser) get_tmp_counter() int {
	p.tmp_cnt++
	return p.tmp_cnt
}

fn os_name_to_ifdef(name string) string { 
	switch name {
		case 'windows': return '_WIN32'
		case 'mac': return '__APPLE__'
		case 'linux': return '__linux__' 
		case 'freebsd': return '__FreeBSD__' 
		case 'openbsd': return '__OpenBSD__' 
		case 'netbsd': return '__NetBSD__' 
		case 'dragonfly': return '__DragonFly__' 
	} 
	panic('bad os ifdef name "$name"') 
	return '' 
} 

fn (p mut Parser) comp_time() {
	p.check(.dollar) 
	if p.tok == .key_if {
		p.check(.key_if) 
		p.fspace() 
		not := p.tok == .not 
		if not {
			p.check(.not) 
		}
		name := p.check_name()
		p.fspace() 
		if name in SupportedPlatforms {
			ifdef_name := os_name_to_ifdef(name) 
			if not {
				p.genln('#ifndef $ifdef_name')
			}
			else {
				p.genln('#ifdef $ifdef_name')
			}
			p.check(.lcbr)
			p.statements_no_curly_end()
			if ! (p.tok == .dollar && p.peek() == .key_else) {
				p.genln('#endif')
			}
		}
		else {
			println('Supported platforms:')
			println(SupportedPlatforms)
			p.error('unknown platform `$name`')
		}
	}
	else if p.tok == .key_for {
		p.next()
		name := p.check_name()
		if name != 'field' {
			p.error('for field only')
		}
		p.check(.key_in)
		p.check_name()
		p.check(.dot)
		p.check_name()// fields
		p.check(.lcbr)
		// for p.tok != .rcbr && p.tok != .eof {
		res_name := p.check_name()
		println(res_name)
		p.check(.dot)
		p.check(.dollar)
		p.check(.name)
		p.check(.assign)
		p.cgen.start_tmp()
		p.bool_expression()
		val := p.cgen.end_tmp()
		println(val)
		p.check(.rcbr)
		// }
	}
	else if p.tok == .key_else {
		p.next()
		p.check(.lcbr)
		p.genln('#else')
		p.statements_no_curly_end()
		p.genln('#endif')
	}
	else {
		p.error('bad comptime expr')
	}
}

fn (p mut Parser) chash() {
	hash := p.lit.trim_space()
	// println('chsh() file=$p.file  is_sig=${p.is_sig()} hash="$hash"')
	p.next()
	is_sig := p.is_sig()
	if is_sig {
		// p.cgen.nogen = true
	}
	if hash.starts_with('flag ') {
		mut flag := hash.right(5)
		// No the right os? Skip!
		// mut ok := true
		if hash.contains('linux') && p.os != .linux {
			return
		}
		else if hash.contains('darwin') && p.os != .mac {  
			return
		}
		else if hash.contains('windows') && p.os != .windows {
			return
		}
		// Remove "linux" etc from flag
		if flag.contains('linux') || flag.contains('darwin') || flag.contains('windows') {
			pos := flag.index(' ')
			flag = flag.right(pos)
		}
		has_vroot := flag.contains('@VROOT') 
		flag = flag.trim_space().replace('@VROOT', p.vroot)
		if p.table.flags.contains(flag) {
			return
		}
		p.log('adding flag "$flag"')
		// `@VROOT/thirdparty/glad/glad.o`, make sure it exists, otherwise build it 
		if has_vroot && flag.contains('.o') { 
			build_thirdparty_obj_file(flag) 
		} 
		p.table.flags << flag 
		return
	}
	if hash.starts_with('include') {
		if p.first_run() && !is_sig {
			p.cgen.includes << '#$hash'
			return
		}
	}
	else if hash.starts_with('typedef') {
		if p.first_run() {
			p.cgen.typedefs << '$hash'
		}
	}
	// TODO remove after ui_mac.m is removed
	else if hash.contains('embed') {
		pos := hash.index('embed') + 5
		file := hash.right(pos)
		if p.pref.build_mode != BuildMode.default_mode {
			p.genln('#include $file')
		}
	}
	else if hash.contains('define') { 
		// Move defines on top 
		p.cgen.includes << '#$hash'
	}
	else if hash == 'v' {
		println('v script') 
		//p.v_script = true 
	} 
	else {
		if !p.can_chash {
			p.error('bad token `#` (embedding C code is no longer supported)')
		}
		p.genln(hash)
	}
}

fn (p mut Parser) if_st(is_expr bool, elif_depth int) string {
	if is_expr {
		if p.fileis('if_expr') {
			println('IF EXPR')
		}
		p.inside_if_expr = true
		p.gen('(')
	}
	else {
		p.gen('if (')
		p.fgen('if ')
	}
	p.next()
	p.check_types(p.bool_expression(), 'bool')
	if is_expr {
		p.gen(') ? (')
	}
	else {
		p.genln(') {')
	}
	p.fgen(' ')
	p.check(.lcbr)
	mut typ := ''
	// if { if hack
	if p.tok == .key_if && p.inside_if_expr {
		typ = p.factor()
		p.next()
	}
	else {
		typ = p.statements()
	}
	// println('IF TYp=$typ')
	if p.tok == .key_else {
		p.fgenln('') 
		p.check(.key_else)  
		p.fspace() 
		if p.tok == .key_if {
			if is_expr {
				p.gen(') : (')
				return p.if_st(is_expr, elif_depth + 1)
			}
			else {
				p.gen(' else ')
				return p.if_st(is_expr, 0)
			}
			// return ''
		}
		if is_expr {
			p.gen(') : (')
		}
		else {
			p.genln(' else { ')
		}
		p.check(.lcbr)
		// statements() returns the type of the last statement
		typ = p.statements()
		p.inside_if_expr = false
		if is_expr {
			p.gen(strings.repeat(`)`, elif_depth + 1))
		}
		return typ
	}
	p.inside_if_expr = false
	if p.fileis('test_test') {
		println('if ret typ="$typ" line=$p.scanner.line_nr')
	}
	return typ
}

fn (p mut Parser) for_st() {
	p.check(.key_for)
	p.fgen(' ')
	p.for_expr_cnt++
	next_tok := p.peek()
	//debug := p.scanner.file_path.contains('r_draw')
	p.cur_fn.open_scope()
	if p.tok == .lcbr {
		// Infinite loop
		p.gen('while (1) {')
	}
	else if p.tok == .key_mut {
		p.error('`mut` is not required in for loops')
	}
	// for i := 0; i < 10; i++ {
	else if next_tok == .decl_assign || next_tok == .assign || p.tok == .semicolon {
		p.genln('for (')
		if next_tok == .decl_assign {
			p.var_decl()
		}
		else if p.tok != .semicolon {
			// allow `for ;; i++ {`
			// Allow `for i = 0; i < ...`
			p.statement(false)
		}
		p.check(.semicolon)
		p.gen(' ; ')
		p.fgen(' ')
		if p.tok != .semicolon {
			p.bool_expression()
		}
		p.check(.semicolon)
		p.gen(' ; ')
		p.fgen(' ')
		if p.tok != .lcbr {
			p.statement(false)
		}
		p.genln(') { ')
	}
	// for i, val in array
	else if p.peek() == .comma {
		// for i, val in array { ==>
		// 
		// array_int tmp = array;
		// for (int i = 0; i < tmp.len; i++) {
		// int val = tmp[i];
		i := p.check_name()
		p.check(.comma)
		val := p.check_name()
		p.fgen(' ')
		p.check(.key_in)
		p.fgen(' ')
		tmp := p.get_tmp()
		p.cgen.start_tmp()
		typ := p.bool_expression()
		is_arr := typ.starts_with('array_') 
		is_map := typ.starts_with('map_') 
		is_str := typ == 'string' 
		if !is_arr && !is_str && !is_map { 
			p.error('cannot range over type `$typ`') 
		} 
		expr := p.cgen.end_tmp()
		p.genln('$typ $tmp = $expr ;')
		pad := if is_arr { 6 } else  { 4 } 
		var_typ := typ.right(pad)
		// typ = strings.Replace(typ, "_ptr", "*", -1)
		// Register temp var
		val_var := Var {
			name: val
			typ: var_typ
			ptr: typ.contains('*')
		}
		p.register_var(val_var)
		if is_arr || is_str { 
			i_var := Var {
				name: i
				typ: 'int'
				// parent_fn: p.cur_fn
				is_mut: true
			}
			p.register_var(i_var)
			p.genln(';\nfor (int $i = 0; $i < $tmp .len; $i ++) {')
			p.genln('$var_typ $val = (($var_typ *) $tmp . data)[$i];')
		}
		else if is_map {
			i_var := Var {
				name: i
				typ: 'string'
				is_mut: true
			}
			p.register_var(i_var)
			p.genln('array_string keys_$tmp = map_keys(& $tmp ); ') 
			p.genln('for (int l = 0; l < keys_$tmp .len; l++) {') 
			p.genln('  string $i = ((string*)keys_$tmp .data)[l];') 
			//p.genln('  string $i = *(string*) ( array__get(keys_$tmp, l) );') 
			def := type_default(var_typ) 
			// TODO don't call map_get() for each key, fetch values while traversing
			// the tree (replace `map_keys()` above with `map_key_vals()`) 
			p.genln('$var_typ $val = $def; map_get($tmp, $i, & $val);') 
			
			/* 
			p.genln('for (int l = 0; l < $tmp . entries.len; l++) {') 
			p.genln('Entry entry = *((Entry*) (array__get($tmp .entries, l)));') 
			p.genln('string $i = entry.key;') 
			p.genln('$var_typ $val; map_get($tmp, $i, & $val);') 
			*/ 
		} 
	}
	// `for val in vals`
	else if p.peek() == .key_in {
		val := p.check_name()
		p.fgen(' ')
		p.check(.key_in)
		p.fspace()
		tmp := p.get_tmp()
		p.cgen.start_tmp()
		typ := p.bool_expression()
		expr := p.cgen.end_tmp()
		is_range := p.tok == .dotdot
		mut range_end := ''
		if is_range {
			p.check_types(typ, 'int')
			p.check_space(.dotdot)
			p.cgen.start_tmp()
			p.check_types(p.bool_expression(), 'int')
			range_end = p.cgen.end_tmp()
		}
		is_arr := typ.contains('array')
		is_str := typ == 'string'
		if !is_arr && !is_str && !is_range {
			p.error('cannot range over type `$typ`') 
		}
		p.genln('$typ $tmp = $expr;')
		// TODO var_type := if...
		mut var_type := ''
		if is_arr {
			var_type = typ.right(6)// all after `array_`
		}
		else if is_str {
			var_type = 'byte'
		}
		else if is_range {
			var_type = 'int'
		}
		// println('for typ=$typ vartyp=$var_typ')
		// Register temp var
		val_var := Var {
			name: val
			typ: var_type
			ptr: typ.contains('*')
		}
		p.register_var(val_var)
		i := p.get_tmp()
		if is_range {
			p.genln(';\nfor (int $i = $tmp; $i < $range_end; $i++) {')
		}
		else {
			p.genln(';\nfor (int $i = 0; $i < $tmp .len; $i ++) {')
		}
		if is_arr {
			p.genln('$var_type $val = (($var_type *) ${tmp}.data)[$i];')
		}
		else if is_str {
			p.genln('$var_type $val = (($var_type *) ${tmp}.str)[$i];')
		}
		else if is_range {
			p.genln('$var_type $val = $i;')
		}
	}
	else {
		// `for a < b {`
		p.gen('while (')
		p.check_types(p.bool_expression(), 'bool')
		p.genln(') {')
	}
	p.fspace() 
	p.check(.lcbr)
	p.genln('') 
	p.statements()
	p.cur_fn.close_scope()
	p.for_expr_cnt--
}

fn (p mut Parser) switch_statement() {
	if p.tok == .key_switch { 
		p.check(.key_switch) 
	} else {
		p.check(.key_match) 
	} 
	p.cgen.start_tmp()
	typ := p.bool_expression()
	expr := p.cgen.end_tmp()
	p.check(.lcbr)
	mut i := 0
	for p.tok == .key_case || p.tok == .key_default || p.peek() == .arrow || p.tok == .key_else { 
		if p.tok == .key_default || p.tok == .key_else { 
			p.genln('else  { // default:')
			if p.tok == .key_default { 
				p.check(.key_default) 
				p.check(.colon)
			}  else { 
				p.check(.key_else) 
				p.check(.arrow)
			} 
			p.statements()
			break
		}
		if i > 0 {
			p.gen('else ')
		}
		p.gen('if (')
		// Multiple checks separated by comma
		mut got_comma := false
		for {
			if got_comma {
				p.gen(') ||  ')
			}
			if typ == 'string' {
				p.gen('string_eq($expr, ')
			}
			else {
				p.gen('($expr == ')
			}
			if p.tok == .key_case || p.tok == .key_default { 
				p.check(p.tok) 
			}
			p.bool_expression()
			if p.tok != .comma {
				break
			}
			p.check(.comma)
			got_comma = true
		}
		if p.tok == .colon { 
			p.check(.colon)
		} 
		else { 
			p.check(.arrow) 
		} 
		p.gen(')) {')
		p.genln('/* case */')
		p.statements()
		i++
	}
}

fn (p mut Parser) assert_statement() {
	if p.first_run() {
		return 
	} 
	p.check(.key_assert)
	p.fspace()
	tmp := p.get_tmp()
	p.gen('bool $tmp = ')
	p.check_types(p.bool_expression(), 'bool')
	// TODO print "expected:  got" for failed tests
	filename := p.file_path
	p.genln(';\n 
if (!$tmp) { 
  puts("\\x1B[31mFAILED: $p.cur_fn.name() in $filename:$p.scanner.line_nr\\x1B[0m");  
g_test_ok = 0 ; 
	// TODO
	// Maybe print all vars in a test function if it fails? 
} 
else { 
  puts("\\x1B[32mPASSED: $p.cur_fn.name()\\x1B[0m");
}')
}

fn (p mut Parser) return_st() {
	p.cgen.insert_before(p.cur_fn.defer_text)
	p.check(.key_return)
	p.fgen(' ') 
	fn_returns := p.cur_fn.typ != 'void'
	if fn_returns {
		if p.tok == .rcbr {
			p.error('`$p.cur_fn.name` needs to return `$p.cur_fn.typ`')
		}
		else {
			ph := p.cgen.add_placeholder()
			expr_type := p.bool_expression()
			// Automatically wrap an object inside an option if the function returns an option
			if p.cur_fn.typ.ends_with(expr_type) && p.cur_fn.typ.starts_with('Option_') {
				tmp := p.get_tmp()
				ret := p.cgen.cur_line.right(ph)

				p.cgen.resetln('$expr_type $tmp = ($expr_type)($ret);')
				p.gen('return opt_ok(&$tmp, sizeof($expr_type))')
			}
			else {
				ret := p.cgen.cur_line.right(ph)
				p.cgen.resetln('return $ret')
			}
			p.check_types(expr_type, p.cur_fn.typ)
		}
	}
	else {
		// Don't allow `return val` in functions that don't return anything
		if false && p.tok == .name || p.tok == .number {
			p.error('function `$p.cur_fn.name` does not return a value')
		}

		if p.cur_fn.name == 'main' {
			p.gen('return 0')
		}
		else {
			p.gen('return')
		}
	}
	p.returns = true
}

fn prepend_pkg(pkg, name string) string {
	return '${pkg}__${name}'
}

fn (p &Parser) prepend_pkg(name string) string {
	return prepend_pkg(p.mod, name)
}

fn (p mut Parser) go_statement() {
	p.check(.key_go)
	// TODO copypasta of name_expr() ?
	// Method
	if p.peek() == .dot {
		var_name := p.lit
		v := p.cur_fn.find_var(var_name)
		p.cur_fn.mark_var_used(v)
		p.next()
		p.check(.dot)
		typ := p.table.find_type(v.typ)
		mut method := p.table.find_method(typ, p.lit)
		p.async_fn_call(method, 0, var_name, v.typ)
	}
	// Normal function
	else {
		f := p.table.find_fn(p.lit)
		if f.name == 'println' {
			p.error('`go` cannot be used with `println`')
		}
		// println(f.name)
		p.async_fn_call(f, 0, '', '')
	}
}

fn (p mut Parser) register_var(v Var) {
	if v.line_nr == 0 {
		v.line_nr = p.scanner.line_nr
	}
	p.cur_fn.register_var(v)
}

// user:=jsdecode(User, user_json_string)
fn (p mut Parser) js_decode() string {
	p.check(.name)// json
	p.check(.dot)
	op := p.check_name()
	if op == 'decode' {
		// User tmp2; tmp2.foo = 0; tmp2.bar = 0;// I forgot to zero vals before => huge bug
		// Option_User tmp3 =  jsdecode_User(json_parse( s), &tmp2); ;
		// if (!tmp3 .ok) {
		// return
		// }
		// User u = *(User*) tmp3 . data;  // TODO remove this (generated in or {} block handler)
		p.check(.lpar)
		typ := p.get_type()
		p.check(.comma)
		p.cgen.start_tmp()
		p.check_types(p.bool_expression(), 'string')
		expr := p.cgen.end_tmp()
		p.check(.rpar)
		tmp := p.get_tmp()
		cjson_tmp := p.get_tmp()
		mut decl := '$typ $tmp; '
		// Init the struct
		T := p.table.find_type(typ)
		for field in T.fields {
			def_val := type_default(field.typ)
			if def_val != '' {
				decl += '$tmp . $field.name = $def_val;\n'
			}
		}
		p.gen_json_for_type(T)
		decl += 'cJSON* $cjson_tmp = json__json_parse($expr);'
		p.cgen.insert_before(decl)
		// p.gen('jsdecode_$typ(json_parse($expr), &$tmp);')
		p.gen('json__jsdecode_$typ($cjson_tmp, &$tmp); cJSON_Delete($cjson_tmp);')
		opt_type := 'Option_$typ'
		p.cgen.typedefs << 'typedef Option $opt_type;'
		p.table.register_type(opt_type)
		return opt_type
	}
	else if op == 'encode' {
		p.check(.lpar)
		p.cgen.start_tmp()
		typ := p.bool_expression()
		T := p.table.find_type(typ)
		p.gen_json_for_type(T)
		expr := p.cgen.end_tmp()
		p.check(.rpar)
		p.gen('json__json_print(json__jsencode_$typ($expr))')
		return 'string'
	}
	else {
		p.error('bad json op "$op"')
	}
	return ''
}

fn is_compile_time_const(s string) bool {
	s = s.trim_space()
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

fn (p &Parser) building_v() bool {
	cur_dir := os.getwd()
	return p.file_path.contains('v/compiler') || cur_dir.contains('v/compiler') 
}

fn (p mut Parser) attribute() {
	p.check(.lsbr)
	p.attr = p.check_name() 
	p.check(.rsbr) 
	if p.tok == .func {
		p.fn_decl() 
		p.attr = '' 
		return 
	} 
	else if p.tok == .key_struct {
		p.struct_decl() 
		p.attr = '' 
		return 
	} 
	p.error('bad attribute usage') 
} 

fn (p mut Parser) defer_st() {
	p.check(.key_defer)
	// Wrap everything inside the defer block in /**/ comments, and save it in 
	// `defer_text`. It will be inserted before every `return`. 
	p.genln('/*') 
	pos := p.cgen.lines.len 
	p.check(.lcbr) 
	p.genln('{') 
	p.statements() 
	p.cur_fn.defer_text = p.cgen.lines.right(pos).join('\n') 
	p.genln('*/') 
}  


///////////////////////////////////////////////////////////////////////////////

// fmt helpers
fn (scanner mut Scanner) fgen(s string) {
	if scanner.fmt_line_empty {
		s = strings.repeat(`\t`, scanner.fmt_indent) + s
	}
	scanner.fmt_out.write(s)
	scanner.fmt_line_empty = false
}

fn (scanner mut Scanner) fgenln(s string) {
	if scanner.fmt_line_empty {
		s = strings.repeat(`\t`, scanner.fmt_indent) + s
	}
	scanner.fmt_out.writeln(s)
	scanner.fmt_line_empty = true
}

fn (p mut Parser) fgen(s string) {
	p.scanner.fgen(s)
}

fn (p mut Parser) fspace() {
	p.fgen(' ')
}

fn (p mut Parser) fgenln(s string) {
	p.scanner.fgenln(s)
}
