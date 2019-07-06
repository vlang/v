// Copyright (c) 2019 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.

module main

import rand

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
	pkg             string // module where this var is stored  TODO rename to `mod`
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
	run            Pass // TODO rename `run` to `pass`
	os             Os
	pkg            string
	inside_const   bool
	expr_var       Var
	assigned_type  string
	left_type  string
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
}

const (
	EmptyFn = &Fn { }
	MainFn= &Fn{name:'main'} 
)

fn (c mut V) new_parser(path string, run Pass) Parser {
	c.log('new_parser("$path")')
	c.cgen.run = run
	mut p := Parser {
		file_path: path
		file_name: path.all_after('/')
		scanner: new_scanner(path)
		table: c.table
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
		p.pkg = 'main'
		// User may still specify `module main`
		if p.tok == PACKAGE {
			p.next()
			p.fgen('module ')
			p.pkg = p.check_name()
		}
	}
	else {
		p.check(PACKAGE)
		p.fspace() 
		p.pkg = p.check_name()
	}
	p.fgenln('\n')
	p.builtin_pkg = p.pkg == 'builtin'
	p.can_chash = p.pkg == 'gg' || p.pkg == 'glm' || p.pkg == 'gl' || 
		p.pkg == 'http' ||  p.pkg == 'glfw' || p.pkg=='ui' // TODO tmp remove
	// Import pass - the first and the smallest pass that only analyzes imports
	p.table.register_package(p.pkg)
	if p.run == RUN_IMPORTS {
		for p.tok == IMPORT && p.peek() != CONST {
			p.import_statement()
		}
		if p.table.imports.contains('builtin') {
			p.error('module `builtin` cannot be imported') 
		} 
		return
	}
	// Go through every top level token or throw a compilation error if a non-top level token is met
	for {
		switch p.tok {
		case IMPORT:
			if p.peek() == CONST {
				p.const_decl()
			}
			else {
				// TODO remove imported consts from the language
				p.import_statement()
			}
		case ENUM:
			p.next()
			if p.tok == NAME {
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
				p.check(NAME)
			}
		case PUB:
			if p.peek() == FUNC {
				p.fn_decl()
			} else if p.peek() == STRUCT {
				p.error('structs can\'t be declared public *yet*')
				// TODO public structs
			} else {
				p.error('wrong pub keyword usage')
			}
		case FUNC:
			p.fn_decl()
		case TIP:
			p.type_decl()
		case STRUCT, INTERFACE, UNION, LSBR:// `[` can only mean an [attribute] before the struct definition
			p.struct_decl()
		case CONST:
			p.const_decl()
		case HASH:
			// insert C code, TODO this is going to be removed ASAP
			// some libraries (like UI) still have lots of C code
			// # puts("hello");
			p.chash()
		case DOLLAR:
			// $if, $else
			p.comp_time()
		case GLOBAL:
			if !p.pref.translated && !p.builtin_pkg && !p.building_v() {
				p.error('__global is only allowed in translated code')
			}
			p.next()
			name := p.check_name()
			typ := p.get_type()
			p.register_global(name, typ)
			// p.genln(p.table.cgen_name_type_pair(name, typ))
			mut g := p.table.cgen_name_type_pair(name, typ)
			if p.tok == ASSIGN {
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
		case EOF:
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
				p.cgen.cur_line = ''
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

fn (p mut Parser) import_statement() {
	p.check(IMPORT)
	// `import ()`
	if p.tok == LPAR {
		p.check(LPAR)
		for p.tok != RPAR && p.tok != EOF {
			pkg := p.lit.trim_space()
			p.next()
			if p.table.imports.contains(pkg) {
				continue
			}
			p.table.imports << pkg
			p.table.register_package(pkg)
		}
		p.check(RPAR)
		return
	}
	// `import foo`
	if p.tok != NAME {
		p.error('bad import format')
	}
	mut pkg := p.lit.trim_space()
	// submodule support
	// limit depth to 4 for now
	max_module_depth := 4
	mut depth := 1
	for p.peek() == DOT {
		p.next() // SKIP DOT
		p.next() // SUBMODULE
		submodule := p.lit.trim_space()
		pkg = pkg + '.' + submodule
		depth++
		if depth > max_module_depth {
			panic('Sorry. Module depth of $max_module_depth exceeded: $pkg ($submodule is too deep).')
		}
	}
	p.next()
	p.fgenln(' ' + pkg)
	// Make sure there are no duplicate imports
	if p.table.imports.contains(pkg) {
		return
	}
	p.log('adding import $pkg')
	p.table.imports << pkg
	p.table.register_package(pkg)
}

fn (p mut Parser) const_decl() {
	is_import := p.tok == IMPORT
	p.inside_const = true
	if is_import {
		p.next()
	}
	p.check(CONST)
	p.fspace()
	p.check(LPAR)
	p.fgenln('')
	p.fmt_inc() 
	for p.tok == NAME {
		// `Age = 20`
		mut name := p.check_name()
		if p.pref.is_play && ! (name[0] >= `A` && name[0] <= `Z`) {
			p.error('const name must be capitalized')
		}
		// Imported consts (like GL_TRIANGLES) dont need pkg prepended (gl__GL_TRIANGLES)
		if !is_import {
			name = p.prepend_pkg(name)
		}
		mut typ := 'int'
		if !is_import {
			p.check_space(ASSIGN)
			typ = p.expression()
		}
		if p.first_run() && !is_import && p.table.known_const(name) {
			p.error('redefinition of `$name`')
		}
		p.table.register_const(name, typ, p.pkg, is_import)
		if p.run == RUN_MAIN && !is_import {
			// TODO hack
			// cur_line has const's value right now. if it's just a number, then optimize generation:
			// output a #define so that we don't pollute the binary with unnecessary global vars
			if is_compile_time_const(p.cgen.cur_line) {
				p.cgen.consts << '#define $name $p.cgen.cur_line'
				p.cgen.cur_line = ''
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
			p.cgen.cur_line = ''
		}
		p.fgenln('')
	}
	p.fmt_dec() 
	p.check(RPAR)
	p.fgenln('\n')
	p.inside_const = false
}

// `type myint int`
// `type onclickfn fn(voidptr) int`
fn (p mut Parser) type_decl() {
	p.check(TIP)
	name := p.check_name()
	parent := p.get_type()
	nt_pair := p.table.cgen_name_type_pair(name, parent)
	// TODO dirty C typedef hacks for DOOM
	// Unknown type probably means it's a struct, and it's used before the struct is defined,
	// so specify "struct"
	_struct := if !parent.contains('[') && !parent.starts_with('fn ') && !p.table.known_type(parent){'struct'} else { ''}
	p.gen_typedef('typedef $_struct $nt_pair; // type alias name="$name" parent="$parent"')
	p.table.register_type_with_parent(name, parent)
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
	if p.tok == LSBR {
		p.check(LSBR)
		// `[interface:ParentInterface]`
		is_objc = p.tok == INTERFACE
		p.next()
		if is_objc {
			p.check(COLON)
			objc_parent = p.check_name()
		}
		p.check(RSBR)
	}
	is_interface := p.tok == INTERFACE
	is_union := p.tok == UNION
	is_struct := p.tok == STRUCT
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
	is_c := name == 'C' && p.tok == DOT
	if is_c {
		p.check(DOT)
		name = p.check_name()
	}
	// Specify full type name
	if !is_c && !p.builtin_pkg && p.pkg != 'main' {
		name = p.prepend_pkg(name)
	}
	if p.run == RUN_DECLS && p.table.known_type(name) {
		p.error('`$name` redeclared')
	}
	// Generate type definitions
	if is_objc {
		p.gen_type('@interface $name : $objc_parent { @public')
	}
	else {
		// type alias is generated later
		if !is_c {
			kind := if is_union{'union'} else { 'struct'}
			p.gen_typedef('typedef $kind $name $name;')
			p.gen_type('$kind /*kind*/ $name {')
		}
	}
	// V used to have 'type Foo struct', many Go users might use this syntax
	if !is_c && p.tok == STRUCT {
		p.error('use `struct $name {` instead of `type $name struct {`')
	}
	// Register the type
	mut typ := p.table.find_type(name)
	mut is_ph := false
	if typ.is_placeholder {
		is_ph = true
		typ.name = name
		typ.pkg = p.pkg
		typ.is_c = is_c
		typ.is_placeholder = false
	}
	else {
		typ = &Type {
			name: name
			pkg: p.pkg
			is_c: is_c
			is_interface: is_interface
		}
	}
	// Struct `C.Foo` declaration, no body
	// println('EEEE $is_c $is_struct')
	if is_c && is_struct && p.tok != LCBR {
		// println('skipping struct header $name')
		p.table.register_type2(typ)
		return
	}
	p.fgen(' ')
	p.check(LCBR)
	// Struct fields
	mut is_pub := false
	mut is_mut := false
	mut names := []string// to avoid dup names TODO alloc perf
	// mut is_mut_mut := false
	for p.tok != RCBR {
		if p.tok == PUB {
			if is_pub {
				p.error('structs can only have one `pub:`, all public fields have to be grouped')
			}
			is_pub = true
			p.fmt_dec() 
			p.check(PUB)
			if p.tok != MUT {
				p.check(COLON)
			}
			p.fmt_inc() 
			p.fgenln('')
		}
		if p.tok == MUT {
			if is_mut {
				p.error('structs can only have one `mut:`, all private mutable fields have to be grouped')
			}
			is_mut = true
			p.fmt_dec() 
			p.check(MUT)
			if p.tok != MUT {
				p.check(COLON)
			}
			p.fmt_inc() 
			p.fgenln('')
		}
		// if is_pub {
		// }
		// (mut) user *User
		// if p.tok == PLUS {
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
		access_mod := if is_pub{PUBLIC} else { PRIVATE}
		p.fgen(' ')
		field_type := p.get_type()
		is_atomic := p.tok == ATOMIC
		if is_atomic {
			p.next()
			p.gen_type('_Atomic ')
		}
		if !is_c {
			p.gen_type(p.table.cgen_name_type_pair(field_name, field_type) + ';')
		}
		// [ATTR]
		mut attr := ''
		if p.tok == LSBR {
			p.next()
			attr = p.check_name()
			p.check(RSBR)
		}
		typ.add_field(field_name, field_type, is_mut, attr, access_mod)
		p.fgenln('')
	}
	if !is_ph && p.first_run() {
		p.table.register_type2(typ)
	}
	p.check(RCBR)
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
	if !p.builtin_pkg && p.pkg != 'main' {
		enum_name = p.prepend_pkg(enum_name)
	}
	p.table.register_type2(&Type {
		name: enum_name
		pkg: p.pkg
		parent: 'int'
		is_enum: true
	})
	// Skip empty enums
	if enum_name != 'int' {
		p.cgen.typedefs << 'typedef int $enum_name ;\n'
	}
	p.check(LCBR)
	mut val := 0
	for p.tok == NAME {
		field := p.check_name()
		mut name := '$field'
if enum_name == 'BuildMode' { 
		 name = '${p.pkg}__${enum_name}_$field'
} 
		p.fgenln('')
		if p.run == RUN_MAIN {
			p.cgen.consts << '#define $name $val \n'
		}
		if p.tok == COMMA {
			p.next()
		}
		p.table.register_const(name, enum_name, p.pkg, false)
		val++
	}
	p.check(RCBR)
	p.fgenln('\n')
}

// check_name checks for a name token and returns its literal
fn (p mut Parser) check_name() string {
	name := p.lit
	p.check(NAME)
	return name
}

fn (p mut Parser) check_string() string {
	s := p.lit
	p.check(STRING)
	return s
}

fn (p &Parser) strtok() string {
	if p.tok == NAME {
		return p.lit
	}
	if p.tok == STRING {
		return '"$p.lit"'
	}
	res := p.tok.str()
	if res == '' {
		n := int(p.tok)
		return n.str()
	}
	return res
}

// same as check(), but addes a space to the formatter output
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
	if expected == RCBR {
		p.fmt_dec() 
	}
	p.fgen(p.strtok())
	// vfmt: increase indentation on `{` unless it's `{}`
	if expected == LCBR && p.scanner.text[p.scanner.pos + 1] != `}` {
		p.fgenln('')
		p.fmt_inc() 
	}
	p.next()
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
	return p.run == RUN_DECLS
}

// TODO return Type instead of string?
fn (p mut Parser) get_type() string {
	debug := p.fileis('fn_test') && false
	mut mul := false
	mut nr_muls := 0
	mut typ := ''
	// fn type
	if p.tok == FUNC {
		if debug {
			println('\nget_type() GOT FN TYP line=$p.scanner.line_nr')
		}
		mut f := Fn{name: '_', pkg: p.pkg}
		p.next()
		line_nr := p.scanner.line_nr
		p.fn_args(mut f)
		// Same line, it's a return type
		if p.scanner.line_nr == line_nr {
			if debug {
				println('same line getting type')
			}
			f.typ = p.get_type()
			// println('fn return typ=$f.typ')
		}
		else {
			f.typ = 'void'
		}
		// Register anon fn type
		fn_typ := Type {
			name: f.typ_str()// 'fn (int, int) string'
			pkg: p.pkg
			func: f
		}
		p.table.register_type2(fn_typ)
		return f.typ_str()
	}
	// arrays ([]int)
	mut is_arr := false
	mut is_arr2 := false// [][]int TODO remove this and allow unlimited levels of arrays
	is_question := p.tok == QUESTION
	if is_question {
		p.check(QUESTION)
	}
	if p.tok == LSBR {
		p.check(LSBR)
		// [10]int
		if p.tok == INT {
			typ = '[$p.lit]'
			p.next()
		}
		else {
			is_arr = true
		}
		p.check(RSBR)
		// [10][3]int
		if p.tok == LSBR {
			p.next()
			if p.tok == INT {
				typ += '[$p.lit]'
				p.check(INT)
			}
			else {
				is_arr2 = true
			}
			p.check(RSBR)
		}
	}
	// map[string]int 
	if !p.builtin_pkg && p.tok == NAME && p.lit == 'map' {
		p.next()
		p.check(LSBR) 
		key_type := p.check_name() 
		if key_type != 'string' {
			p.error('maps only support string keys for now') 
		} 
		p.check(RSBR) 
		val_type := p.check_name() 
		typ= 'map_$val_type' 
		p.register_map(typ)
		return typ 
	} 
	//  
	for p.tok == MUL {
		mul = true
		nr_muls++
		p.check(MUL) 
	}
	if p.tok == AMP {
		mul = true
		nr_muls++
		p.check(AMP) 
	}
	typ += p.lit
	if !p.is_struct_init {
		// Otherwise we get `foo := FooFoo{` because `Foo` was already generated in name_expr()
		p.fgen(p.lit)
	}
	// C.Struct import
	if p.lit == 'C' && p.peek() == DOT {
		p.next()
		p.check(DOT)
		typ = p.lit
	}
	else {
		// Package specified? (e.g. gx.Image)
		if p.peek() == DOT {
			p.next()
			p.check(DOT)
			typ += '__$p.lit'
		}
		mut t := p.table.find_type(typ)
		// "typ" not found? try "pkg__typ"
		if t.name == '' && !p.builtin_pkg {
			// && !p.first_run() {
			if !typ.contains('array_') && p.pkg != 'main' && !typ.contains('__') &&
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
	if p.tok == QUESTION || is_question {
		typ = 'Option_$typ'
		p.table.register_type_with_parent(typ, 'Option')
		if p.tok == QUESTION {
			p.next()
		}
	}
	// Because the code uses * to see if it's a pointer
	if typ == 'byteptr' {
		return 'byte*'
	}
	if typ == 'voidptr' {
		//if !p.builtin_pkg && p.pkg != 'os' && p.pkg != 'gx' && p.pkg != 'gg' && !p.pref.translated {
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
	if p.tok == NAME {
		println(p.lit)
		return
	}
	if p.tok == STRING {
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
	for p.tok != RCBR && p.tok != EOF && p.tok != CASE && p.tok != DEFAULT {
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
	if p.tok != CASE && p.tok != DEFAULT {
		// p.next()
		p.check(RCBR)
	}
	else {
		// p.check(RCBR)
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
	case NAME:
		next := p.peek()
		if p.pref.is_verbose {
			println(next.str())
		}
		// goto_label:
		if p.peek() == COLON {
			p.fmt_dec()
			label := p.check_name()
			p.fmt_inc()
			p.genln(label + ':')
			p.check(COLON)
			return ''
		}
		// `a := 777` 
		else if p.peek() == DECL_ASSIGN {
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
	case GOTO:
		p.check(GOTO)
		p.fgen(' ')
		label := p.check_name()
		p.genln('goto $label;')
		return ''
	case HASH:
		p.chash()
		return ''
	case DOLLAR:
		p.comp_time()
	case IF:
		p.if_st(false)
	case FOR:
		p.for_st()
	case SWITCH, MATCH:
		p.switch_statement()
	case MUT, STATIC:
		p.var_decl()
	case RETURN:
		p.return_st()
	case LCBR:// {} block
		p.next()
		p.genln('{')
		p.statements()
		return ''
	case CONTINUE:
		if p.for_expr_cnt == 0 {
			p.error('`continue` statement outside `for`')
		}
		p.genln('continue')
		p.check(CONTINUE) 
	case BREAK:
		if p.for_expr_cnt == 0 {
			p.error('`break` statement outside `for`')
		}
		p.genln('break')
		p.check(BREAK) 
	case GO:
		p.go_statement()
	case ASSERT:
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
	if p.inside_if_expr && p.tok != RCBR {
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
	case ASSIGN:
		if !is_map {
			p.gen(' = ')
		}
	case PLUS_ASSIGN:
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
		p.cgen.cur_line = left + 'opt_ok($expr, sizeof($expr_type))'
	}
	else if !p.builtin_pkg && !p.check_types_no_throw(expr_type, p.assigned_type) {
		p.scanner.line_nr--
		p.error('cannot use type `$expr_type` as type `$p.assigned_type` in assignment')
	}
	if is_str && tok == PLUS_ASSIGN {
		p.gen(')')
	}
	// p.assigned_var = ''
	p.assigned_type = ''
	if !v.is_used {
		p.cur_fn.mark_var_used(v)
	}
}

fn (p mut Parser) var_decl() {
	is_mut := p.tok == MUT || p.prev_tok == FOR
	is_static := p.tok == STATIC
	if p.tok == MUT {
		p.check(MUT)
		p.fspace()
	}
	if p.tok == STATIC {
		p.check(STATIC)
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
	p.check_space(DECL_ASSIGN) // := 
	// Generate expression to tmp because we need its type first
	// [TYP NAME =] bool_expression()
	pos := p.cgen.add_placeholder()
	mut typ := p.bool_expression()
	// Option check ? or {
	or_else := p.tok == OR_ELSE
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
		p.check(LCBR)
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
		if !p.returns && p.prev_tok2 != CONTINUE && p.prev_tok2 != BREAK {
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
	for p.tok == AND || p.tok == OR {
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
	p.left_type = typ 
	is_str := typ=='string' 
	tok := p.tok
	// if tok in [ EQ, GT, LT, LE, GE, NE] {
	if tok == EQ || tok == GT || tok == LT || tok == LE || tok == GE || tok == NE {
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
			case EQ: p.cgen.set_placeholder(ph, 'string_eq(')
			case NE: p.cgen.set_placeholder(ph, 'string_ne(')
			case LE: p.cgen.set_placeholder(ph, 'string_le(')
			case GE: p.cgen.set_placeholder(ph, 'string_ge(')
			case GT: p.cgen.set_placeholder(ph, 'string_gt(')
			case LT: p.cgen.set_placeholder(ph, 'string_lt(')
			}
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
	 
	ptr := p.tok == AMP
	deref := p.tok == MUL
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
	is_c := name == 'C' && p.peek() == DOT
	mut is_c_struct_init := is_c && ptr// a := &C.mycstruct{}
	if is_c {
		p.next()
		p.check(DOT)
		name = p.lit
		p.fgen(name)
		// Currently struct init is set to true only we have `&C.Foo{}`, handle `C.Foo{}`:
		if !is_c_struct_init && p.peek() == LCBR {
			is_c_struct_init = true
		}
	}
	// enum value? (`color == .green`) 
	if p.tok == DOT {
		//println('got enum dot val $p.left_type pass=$p.run $p.scanner.line_nr left=$p.left_type') 
		T := p.find_type(p.left_type) 
		if T.is_enum {
			p.check(DOT) 
			val := p.check_name() 
			p.gen(p.pkg + '__' + p.left_type + '_' + val) 
		} 
		return p.left_type 
	} 
	// //////////////////////////
	// module ?
	// Allow shadowing (gg = gg.newcontext(); gg.draw_triangle())
	if p.table.known_pkg(name) && !p.cur_fn.known_var(name) && !is_c {
		// println('"$name" is a known pkg')
		pkg := name
		p.next()
		p.check(DOT)
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
	// if known_type || is_c_struct_init || (p.first_run() && p.peek() == LCBR) {
	// known type? int(4.5) or Color.green (enum)
	if p.table.known_type(name) {
		// float(5), byte(0), (*int)(ptr) etc
		if !is_c && ( p.peek() == LPAR || (deref && p.peek() == RPAR) ) {
			if deref {
				name += '*'
			}
			else if ptr {
				name += '*'
			}
			p.gen('(')
			mut typ := p.cast(name)
			p.gen(')')
			for p.tok == DOT {
				typ = p.dot(typ, ph)
			}
			return typ
		}
		// Color.green
		else if p.peek() == DOT {
			enum_type := p.table.find_type(name)
			if !enum_type.is_enum {
				p.error('`$name` is not an enum')
			}
			p.next()
			p.check(DOT)
			val := p.lit
			// println('enum val $val')
			p.gen(p.pkg + '__' + enum_type.name + '_' + val)// `color = main__Color_green`
			p.next()
			return enum_type.name
		}
		else if p.peek() == LCBR {
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
	f := p.table.find_fn(name)
	if f.name == '' {
		// We are in a second pass, that means this function was not defined, throw an error. 
		if !p.first_run() {
			// println('name_expr():')
			// If orig_name is a pkg, then printing undefined: `pkg` tells us nothing
			if p.table.known_pkg(orig_name) {
				name = name.replace('__', '.')
				p.error('undefined: `$name`')
			}
			else {
				p.error('undefined: `$orig_name`')
			}
		}
		p.next()
		return 'void'
	}
	// no () after func, so func is an argument, just gen its name
	// TODO verify this and handle errors
	if p.peek() != LPAR {
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
	if p.tok == DOT {
		mut typ := ''
		for p.tok == DOT {
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
	// p.gen('VAR EXPR ')
	p.cur_fn.mark_var_used(v)
	fn_ph := p.cgen.add_placeholder()
	p.expr_var = v
	p.gen(p.table.var_cgen_name(v.name))
	p.next()
	mut typ := v.typ
	// fn_pointer()
	if typ.starts_with('fn ') {
		println('CALLING FN PTR')
		p.print_tok()
		T := p.table.find_type(typ)
		p.gen('(')
		p.fn_call_args(T.func)
		p.gen(')')
		typ = T.func.typ
	}
	// users[0] before dot so that we can have
	// users[0].name
	if p.tok == LSBR {
		typ = p.index_expr(typ, fn_ph)
		// ////println('QQQQ KEK $typ')
	}
	// a.b.c().d chain
	// mut dc := 0
	for p.tok == DOT {
		// println('dot #$dc')
		typ = p.dot(typ, fn_ph)
		p.log('typ after dot=$typ')
		// print('tok after dot()')
		// p.print_tok()
		// dc++
		if p.tok == LSBR {
			// typ = p.index_expr(typ, fn_ph, v)
		}
	}
	// a++ and a--
	if p.tok == INC || p.tok == DEC {
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
	if p.tok == DOT { 
		for p.tok == DOT {
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
	p.check(DOT)
	field_name := p.lit
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
		// println(typ.str())
		if typ.name.starts_with('Option_') {
			opt_type := typ.name.substr(7, typ.name.len)
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
		modifying := next.is_assign() || next == INC || next == DEC
		is_vi := p.fileis('vi')
		if !p.builtin_pkg && !p.pref.translated && modifying && !field.is_mut && !is_vi {
			p.error('cannot modify immutable field `$field_name` (type `$typ.name`)')
		}
		if !p.builtin_pkg && p.pkg != typ.pkg {
		}
		// if p.pref.is_play && field.access_mod == PRIVATE && !p.builtin_pkg && p.pkg != typ.pkg {
		// Don't allow `arr.data`
		if field.access_mod == PRIVATE && !p.builtin_pkg && !p.pref.translated && p.pkg != typ.pkg {
			// println('$typ.name :: $field.name ')
			// println(field.access_mod)
			p.error('cannot refer to unexported field `$field_name` (type `$typ.name`)')
		}
		// if field.access_mod == PUBLIC && p.peek() == ASSIGN && !p.builtin_pkg && p.pkg != typ.pkg {
		// Don't allow `str.len = 0`
		if field.access_mod == PUBLIC && !p.builtin_pkg && p.pkg != typ.pkg {
			// if field.name == 'age' {
			// println('HOHOH')
			// println(next.str())
			// }
			if !field.is_mut && !p.pref.translated && modifying {
				p.error('cannot modify public immutable field `$field_name` (type `$typ.name`)')
			}
		}
		p.gen('${dot}${field_name}')
		// p.gen(dot + field_name)
		p.next()
		return field.typ
	}
	// method
	// mut method := typ.find_method(field_name)
	mut method := p.table.find_method(typ, field_name)
	p.fn_call(method, method_ph, '', str_typ)
	// Methods returning "array" (like slice_fast) should return "array_string"
	if method.typ == 'array' && typ.name.starts_with('array_') {
		return typ.name
	}
	// Array Methods returning `voidptr` (like `last()`) should return element type
	if method.typ == 'void*' && typ.name.starts_with('array_') {
		// return typ.name.replace('array_', '')
		return typ.name.right(6)
	}
	//if false && p.tok == LSBR {
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
	is_indexer := p.tok == LSBR
	mut close_bracket := false
	if is_indexer {
		is_fixed_arr := typ[0] == `[`
		if !is_str && !is_arr && !is_map && !is_ptr && !is_fixed_arr {
			p.error('Cant [] non-array/string/map. Got type "$typ"')
		}
		p.check(LSBR)
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
			if p.pref.translated {
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
		p.check(RSBR)
		// if (is_str && p.builtin_pkg) || is_ptr || is_fixed_arr && ! (is_ptr && is_arr) {
		if close_bracket {
			p.gen(']/*r$typ $v.is_mut*/')
		}
	}
	// TODO if p.tok in ...
	// if p.tok in [ASSIGN, PLUS_ASSIGN, MINUS_ASSIGN]
	if p.tok == ASSIGN || p.tok == PLUS_ASSIGN || p.tok == MINUS_ASSIGN ||
	p.tok == MULT_ASSIGN || p.tok == DIV_ASSIGN || p.tok == XOR_ASSIGN || p.tok == MOD_ASSIGN ||
	p.tok == OR_ASSIGN || p.tok == AND_ASSIGN || p.tok == RIGHT_SHIFT_ASSIGN ||
	p.tok == LEFT_SHIFT_ASSIGN {
		if is_indexer && is_str && !p.builtin_pkg {
			p.error('strings are immutable')
		}
		// println('111 "$p.cgen.cur_line"')
		assign_pos := p.cgen.cur_line.len
		p.assigned_type = typ
		p.left_type = typ
		p.assign_statement(v, fn_ph, is_indexer && (is_map || is_arr))
		// m[key] = val
		if is_indexer && (is_map || is_arr) {
			// a[0] = 7
			// curline right now: "a , 0  =  7"
			// println('222 "$p.cgen.cur_line"')
			// Cant have &7, so need a tmp
			tmp := p.get_tmp()
			tmp_val := p.cgen.cur_line.right(assign_pos)
			p.cgen.cur_line = p.cgen.cur_line.left(assign_pos)
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
		p.cgen.cur_line = p.cgen.cur_line.left(fn_ph)
		// Can't pass integer literal, because map_get() requires a void*
		tmp := p.get_tmp()
		tmp_ok := p.get_tmp()
		if is_map {
			p.gen('$tmp')
			def := type_default(typ)
			p.cgen.insert_before('$typ $tmp = $def; bool $tmp_ok = map_get($index_expr, & $tmp);')
		}
		else if is_arr {
			if p.pref.translated {
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
		println('epxression() pass=$p.run tok=')
		p.print_tok()
	}
	p.cgen('/* expr start*/')
	ph := p.cgen.add_placeholder()
	mut typ := p.term()
	is_str := typ=='string' 
	// a << b ==> array2_push(&a, b)
	if p.tok == LEFT_SHIFT {
		if typ.contains('array_') {
			// Can't pass integer literal, because push requires a void*
			// a << 7 => int tmp = 7; array_push(&a, &tmp);
			// _PUSH(&a, expression(), tmp, string)
			tmp := p.get_tmp()
			tmp_typ := typ.right(6)// skip "array_"
			p.check_space(LEFT_SHIFT) 
			// Get the value we are pushing
			p.gen(', (')
			// Immutable? Can we push?
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
				// Don't dereference if it's already a mutable array argument  (`fn foo(mut []int)`)
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
	if p.tok == IN {
		p.fgen(' ')
		p.check(IN)
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
	if p.tok == RIGHT_SHIFT {
		p.next()
		p.gen(' >> ')
		p.check_types(p.expression(), typ)
		return 'int'
	}
	if p.tok == DOT  { 
		for p.tok == DOT {
			typ = p.dot(typ, ph)
		}
	}
	// + - |
	for p.tok == PLUS || p.tok == MINUS || p.tok == PIPE || p.tok == AMP || p.tok == XOR {
		// for p.tok in [PLUS, MINUS, PIPE, AMP, XOR] {
		tok_op := p.tok
		is_num := typ == 'void*' || typ == 'byte*' || is_number_type(typ)
		p.check_space(p.tok) 
		if is_str && tok_op == PLUS {
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
		if is_str && tok_op == PLUS {
			p.gen(')')
		}
		// Make sure operators are used with correct types
		if !p.pref.translated && !is_str && !is_num {
			T := p.table.find_type(typ)
			if tok_op == PLUS {
				if T.has_method('+') {
					p.cgen.set_placeholder(ph, typ + '_plus(')
					p.gen(')')
				}
				else {
					p.error('operator + not defined on `$typ`')
				}
			}
			else if tok_op == MINUS {
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
	if p.tok == MUL && line_nr != p.scanner.line_nr {
		return typ
	}
	for p.tok == MUL || p.tok == DIV || p.tok == MOD {
		tok := p.tok
		is_div := tok == DIV
		is_mod := tok == MOD
		// is_mul := tok == MOD
		p.next()
		p.gen(tok.str())// + ' /*op2*/ ')
		p.fgen(' ' + tok.str() + ' ')
		if is_div && p.tok == INT && p.lit == '0' {
			p.error('division by zero')
		}
		if is_mod && (is_float_type(typ) || !is_number_type(typ)) {
			p.error('operator MOD requires integer types')
		}
		p.check_types(p.unary(), typ)
	}
	return typ
}

fn (p mut Parser) unary() string {
	mut typ := ''
	tok := p.tok
	switch tok {
	case NOT:
		p.gen('!')
		p.check(NOT) 
		typ = 'bool'
		p.bool_expression()
	case BIT_NOT:
		p.gen('~')
		p.check(BIT_NOT) 
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
	case INT:
		typ = 'int'
		// Check if float (`1.0`, `1e+3`) but not if is hexa
		if (p.lit.contains('.') || p.lit.contains('e')) && 
	 		!(p.lit[0] == `0` && p.lit[1] == `x`) {
			typ = 'f32'
			// typ = 'f64' // TODO 
		}
		p.gen(p.lit)
		p.fgen(p.lit)
	case MINUS:
		p.gen('-')
		p.fgen('-')
		p.next()
		return p.factor()
		// Variable
	case SIZEOF:
		p.gen('sizeof(')
		p.fgen('sizeof(')
		p.next()
		p.check(LPAR)
		mut sizeof_typ := p.get_type()
		if sizeof_typ.ends_with('*') {
			// Move * from the end to the beginning, as C requires
			sizeof_typ = '*' + sizeof_typ.left(sizeof_typ.len - 1)
		}
		p.check(RPAR)
		p.gen('$sizeof_typ)')
		p.fgen('$sizeof_typ)')
		return 'int'
	case AMP:
		return p.name_expr()
	case DOT:
		return p.name_expr()// `.green` (enum)
	case MUL:
		return p.name_expr()
	case NAME:
		// map[string]int
		if p.lit == 'map' && p.peek() == LSBR {
			return p.map_init()
		}
		if p.lit == 'json' && p.peek() == DOT {
			return p.js_decode()
		}
		typ = p.name_expr()
		return typ
	case DEFAULT:
		p.next()
		p.next()
		name := p.check_name()
		if name != 'T' {
			p.error('default needs T')
		}
		p.gen('default(T)')
		p.next()
		return 'T'
	case LPAR:
		p.gen('(/*lpar*/')
		p.check(LPAR) 
		typ = p.bool_expression()
		// Hack. If this `)` referes to a ptr cast `(*int__)__`, it was already checked
		// TODO: fix parser so that it doesn't think it's a par expression when it sees `(` in
		// __(__*int)(
		if !p.ptr_cast {
			p.check(RPAR)
		}
		p.ptr_cast = false
		p.gen(')')
		return typ
	case CHAR:
		p.char_expr()
		typ = 'byte'
		return typ
	case STRING:
		p.string_expr()
		typ = 'string'
		return typ
	case FALSE:
		typ = 'bool'
		p.gen('0')
		p.fgen('false')
	case TRUE:
		typ = 'bool'
		p.gen('1')
		p.fgen('true')
	case LSBR:
		// `[1,2,3]` or `[]` or `[20]byte`
		// TODO have to return because arrayInit does next()
		// everything should do next()
		return p.array_init()
	case LCBR:
		// { user | name :'new name' }
		return p.assoc()
	case IF:
		typ = p.if_st(true)
		return typ
	default:
		next := p.peek()
		println('PREV=${p.prev_tok.str()}')
		println('NEXT=${next.str()}')
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
	p.check(PIPE)
	p.gen('($var.typ){')
	mut fields := []string// track the fields user is setting, the rest will be copied from the old object
	for p.tok != RCBR {
		field := p.check_name()
		fields << field
		p.gen('.$field = ')
		p.check(COLON)
		p.bool_expression()
		p.gen(',')
		if p.tok != RCBR {
			p.check(COMMA)
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
	p.check(RCBR)
	p.gen('}')
	return var.typ
}

fn (p mut Parser) char_expr() {
	p.gen('\'$p.lit\'')
	p.next()
}


fn (p mut Parser) typ_to_fmt(typ string) string {
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
	// println('STRING EXPR')
	str := p.lit
	p.fgen('\'$str\'')
	// No ${}, just return simple string
	if p.peek() != DOLLAR {
		// println('before format: "$str"')
		f := format_str(str)
		// println('after format: "$str"')
		if p.calling_c || p.pref.translated {
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
	for p.tok == STRING {
		// Add the string between %d's
		format += format_str(p.lit)
		p.next()// skip $
		if p.tok != DOLLAR {
			continue
		}
		// Handle DOLLAR
		p.next()
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
		custom := p.tok == COLON
		if custom {
			format += '%'
			p.next()
			if p.tok == DOT {
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
			f := p.typ_to_fmt(typ) 
			if f == '' { 
				p.error('unhandled sprintf format "$typ" ')
			} 
			format += f 
		}
	}
	// println("hello %d", num) optimization.
	if p.cgen.nogen {
		return
	}
	// println: don't allocate a new string, just print	it. 
	cur_line := p.cgen.cur_line.trim_space()
	if cur_line.contains('println (') && p.tok != PLUS && 
		!cur_line.contains('string_add') && !cur_line.contains('eprintln') {
		p.cgen.cur_line = cur_line.replace('println (', 'printf(')
		p.gen('$format\\n$args')
		return
	}
	// '$age'! means the user wants this to be a tmp string (uses global buffer, no allocation,
	// won't be used	again)
	if p.tok == NOT {
		p.next()
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
	p.check(LSBR)
	key_type := p.check_name()
	if key_type != 'string' {
		p.error('only string key maps allowed for now')
	}
	p.check(RSBR)
	val_type := p.check_name()
	if !p.table.known_type(val_type) {
		p.error('map init unknown type "$val_type"')
	}
	p.gen('new_map(1, sizeof($val_type))')
	p.check(LCBR)
	p.check(RCBR)
	return 'map_$val_type'
}

// [1,2,3]
fn (p mut Parser) array_init() string {
	p.check(LSBR)
	is_integer := p.tok == INT
	lit := p.lit
	mut typ := ''
	new_arr_ph := p.cgen.add_placeholder()
	mut i := 0
	pos := p.cgen.cur_line.len// remember cur line to fetch first number in cgen  for [0; 10]
	for p.tok != RSBR {
		val_typ := p.bool_expression()
		// Get type of the first expression
		if i == 0 {
			typ = val_typ
			// fixed width array initialization? (`arr := [20]byte`)
			if is_integer && p.tok == RSBR && p.peek() == NAME {
				nextc := p.scanner.text[p.scanner.pos + 1]
				// TODO whitespace hack
				// Make sure there's no space in `[10]byte`
				if !nextc.is_space() {
					p.check(RSBR)
					name := p.check_name()
					if p.table.known_type(name) {
						p.cgen.cur_line = ''
						p.gen('{} /* arkek init*/')
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
		if p.tok != RSBR && p.tok != SEMICOLON {
			p.gen(', ')
			p.check(COMMA)
			p.fspace() 
		}
		i++
		// Repeat (a = [0;5] )
		if i == 1 && p.tok == SEMICOLON {
			p.check_space(SEMICOLON)
			val := p.cgen.cur_line.right(pos)
			// p.cgen.cur_line = ''
			p.cgen.cur_line = p.cgen.cur_line.left(pos)
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
			p.check(RSBR)
			return 'array_$typ'
		}
	}
	p.check(RSBR)
	// type after `]`? (e.g. "[]string")
	if p.tok != NAME && i == 0 {
		p.error('specify array type: `[]typ` instead of `[]`')
	}
	if p.tok == NAME && i == 0 {
		// vals.len == 0 {
		typ = p.get_type()
		// println('GOT TYP after [] $typ')
	}
	// ! after array => no malloc and no copy
	no_alloc := p.tok == NOT
	if no_alloc {
		p.next()
	}
	// [1,2,3]!! => [3]int{1,2,3}
	is_fixed_size := p.tok == NOT
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
	p.check(LCBR)
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
		if p.tok == NOT {
			p.next()
			p.gen('0')
			p.check(RCBR)
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
	if peek == COLON || p.tok == RCBR {
		t := p.table.find_type(typ)
		for p.tok != RCBR {
			field := p.check_name()
			if !t.has_field(field) {
				p.error('`$t.name` has no field `$field`')
			}
			f := t.find_field(field) 
			inited_fields << field
			p.gen('.$field = ')
			p.check(COLON)
			p.fspace()
			p.check_types(p.bool_expression(),  f.typ) 
			if p.tok == COMMA {
				p.next()
			}
			if p.tok != RCBR {
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
				if p.tok != COMMA {
					p.error('too few values in `$typ` literal (${i+1} instead of $T.fields.len)')
				}
				p.gen(',')
				p.next()
			}
		}
		// Allow `user := User{1,2,3,}`
		// The final comma will be removed by vfmt, since we are not calling `p.fgen()`
		if p.tok == COMMA {
			p.next()
		}
		if p.tok != RCBR {
			p.error('too many fields initialized: `$typ` has $T.fields.len field(s)')
		}
	}
	p.gen('}')
	if ptr {
		p.gen(')')
	}
	p.check(RCBR)
	p.is_struct_init = false
	return typ
}

// `f32(3)`
// tok is `f32` or `)` if `(*int)(ptr)`
fn (p mut Parser) cast(typ string) string {
	p.next()
	pos := p.cgen.add_placeholder()
	if p.tok == RPAR {
		// skip `)` if it's `(*int)(ptr)`, not `int(a)`
		p.ptr_cast = true
		p.next()
	}
	p.check(LPAR)
	expr_typ := p.bool_expression()
	p.check(RPAR)
	// `string(buffer)` => `tos2(buffer)`
	if typ == 'string' && (expr_typ == 'byte*' || expr_typ == 'byteptr') {
		p.cgen.set_placeholder(pos, 'tos2(')
	}
	// `string(234)` => error
	else if typ == 'string' && expr_typ == 'int' {
		p.error('cannot cast `$expr_typ` to `$typ`, use `str()` method instead')
	} 
	else {
		p.cgen.set_placeholder(pos, '($typ)(')
		// p.fgen(typ)
	}
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
	} 
	panic('bad os ifdef name "$name"') 
	return '' 
} 

fn (p mut Parser) comp_time() {
	p.check(DOLLAR) 
	if p.tok == IF {
		p.check(IF) 
		p.fspace() 
		not := p.tok == NOT
		if not {
			p.check(NOT) 
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
			p.check(LCBR)
			p.statements_no_curly_end()
			if ! (p.tok == DOLLAR && p.peek() == ELSE) {
				p.genln('#endif')
			}
		}
		else {
			println('Supported platforms:')
			println(SupportedPlatforms)
			p.error('unknown platform `$name`')
		}
	}
	else if p.tok == FOR {
		p.next()
		name := p.check_name()
		if name != 'field' {
			p.error('for field only')
		}
		p.check(IN)
		p.check_name()
		p.check(DOT)
		p.check_name()// fields
		p.check(LCBR)
		// for p.tok != RCBR && p.tok != EOF {
		res_name := p.check_name()
		println(res_name)
		p.check(DOT)
		p.check(DOLLAR)
		p.check(NAME)
		p.check(ASSIGN)
		p.cgen.start_tmp()
		p.bool_expression()
		val := p.cgen.end_tmp()
		println(val)
		p.check(RCBR)
		// }
	}
	else if p.tok == ELSE {
		p.next()
		p.check(LCBR)
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
	if hash == 'live' {
		if p.pref.is_so {
			return
		}
		p.pref.is_live = true
		return
	}
	if hash.starts_with('flag ') {
		mut flag := hash.right(5)
		// No the right os? Skip!
		// mut ok := true
		if hash.contains('linux') && p.os != LINUX {
			return
		}
		else if hash.contains('darwin') && p.os != MAC {
			return
		}
		else if hash.contains('windows') && p.os != WINDOWS {
			return
		}
		// Remove "linux" etc from flag
		if flag.contains('linux') || flag.contains('darwin') || flag.contains('windows') {
			pos := flag.index(' ')
			flag = flag.right(pos)
		}
		flag = flag.trim_space().replace('@VROOT', p.vroot)
		if p.table.flags.contains(flag) {
			return
		}
		p.log('adding flag "$flag"')
		p.table.flags << flag// .all_after(' '))
		// }
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
	else {
		if !p.can_chash {
			p.error('bad token `#` (embedding C code is no longer supported)')
		}
		p.genln(hash)
	}
}

fn (p mut Parser) if_st(is_expr bool) string {
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
		p.genln('/*if*/')
	}
	p.fgen(' ')
	p.check(LCBR)
	mut typ := ''
	// if { if hack
	if p.tok == IF && p.inside_if_expr {
		println('AAAWWFAFAF')
		typ = p.factor()
		println('QWEWQE typ=$typ')
		p.next()
	}
	else {
		typ = p.statements()
	}
	// println('IF TYp=$typ')
	if p.tok == ELSE {
		p.fgenln('') 
		p.check(ELSE)  
		p.fspace() 
		if p.tok == IF {
			p.gen(' else ')
			return p.if_st(is_expr)
			// return ''
		}
		if is_expr {
			p.gen(') : (')
		}
		else {
			p.genln(' else { ')
			p.genln('/*else if*/')
		}
		p.check(LCBR)
		// statements() returns the type of the last statement
		typ = p.statements()
		p.inside_if_expr = false
		if is_expr {
			p.gen(')')
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
	p.check(FOR)
	p.fgen(' ')
	p.for_expr_cnt++
	next_tok := p.peek()
	debug := p.scanner.file_path.contains('r_draw')
	if debug {
		println('\n\nFOR {')
	}
	p.cur_fn.open_scope()
	if p.tok == LCBR {
		// Infinite loop
		p.gen('while (1) {')
	}
	else if p.tok == MUT {
		p.error('`mut` is not required in for loops')
	}
	// for i := 0; i < 10; i++ {
	else if next_tok == DECL_ASSIGN || next_tok == ASSIGN || p.tok == SEMICOLON {
		if debug {
			println('for 1')
		}
		p.genln('for (')
		if next_tok == DECL_ASSIGN {
			p.var_decl()
		}
		else if p.tok != SEMICOLON {
			// allow `for ;; i++ {`
			// Allow `for i = 0; i < ...`
			p.statement(false)
		}
		if debug {
			println('for 2')
		}
		p.check(SEMICOLON)
		p.gen(' ; ')
		p.fgen(' ')
		if p.tok != SEMICOLON {
			p.bool_expression()
		}
		if debug {
			println('for 3')
		}
		p.check(SEMICOLON)
		p.gen(' ; ')
		p.fgen(' ')
		if p.tok != LCBR {
			p.statement(false)
		}
		if debug {
			println('for 4')
		}
		p.genln(') { ')
	}
	// for i, val in array
	else if p.peek() == COMMA {
		// for i, val in array { ==>
		// 
		// array_int tmp = array;
		// for (int i = 0; i < tmp.len; i++) {
		// int val = tmp[i];
		i := p.check_name()
		p.check(COMMA)
		val := p.check_name()
		p.fgen(' ')
		p.check(IN)
		p.fgen(' ')
		tmp := p.get_tmp()
		p.cgen.start_tmp()
		typ := p.bool_expression()
		expr := p.cgen.end_tmp()
		p.genln('$typ $tmp = $expr ;')
		var_typ := typ.right(6)
		// typ = strings.Replace(typ, "_ptr", "*", -1)
		// Register temp var
		val_var := Var {
			name: val
			typ: var_typ
			// parent_fn: p.cur_fn
			ptr: typ.contains('*')
		}
		p.register_var(val_var)
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
	// `for val in vals`
	else if p.peek() == IN {
		val := p.check_name()
		p.fgen(' ')
		p.check(IN)
		p.fspace()
		tmp := p.get_tmp()
		p.cgen.start_tmp()
		typ := p.bool_expression()
		expr := p.cgen.end_tmp()
		// println('if in:')
		// println(p.strtok())
		is_range := p.tok == DOTDOT
		mut range_end := ''
		if is_range {
			p.check_types(typ, 'int')
			p.check_space(DOTDOT)
			p.cgen.start_tmp()
			p.check_types(p.bool_expression(), 'int')
			range_end = p.cgen.end_tmp()
		}
		is_arr := typ.contains('array')
		is_str := typ == 'string'
		// ////if !typ.contains('array') && typ != 'string' {
		if !is_arr && !is_str && !is_range {
			p.error('`for in` requires an array or a string but got `$typ`')
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
	p.check(LCBR)
	p.statements()
	p.cur_fn.close_scope()
	p.for_expr_cnt--
}

fn (p mut Parser) switch_statement() {
	p.check(SWITCH) 
	p.cgen.start_tmp()
	typ := p.bool_expression()
	expr := p.cgen.end_tmp()
	p.check(LCBR)
	mut i := 0
	for p.tok == CASE || p.tok == DEFAULT {
		if p.tok == DEFAULT {
			p.genln('else  { // default:')
			p.check(DEFAULT) 
			p.check(COLON)
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
			if p.tok == CASE || p.tok == DEFAULT {
				p.check(p.tok) 
			}
			p.bool_expression()
			if p.tok != COMMA {
				break
			}
			p.check(COMMA)
			got_comma = true
		}
		p.check(COLON)
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
	p.check(ASSERT)
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
	p.cgen.insert_before(p.cur_fn.defer)
	p.gen('return ')
	if p.cur_fn.name == 'main' {
		p.gen(' 0')
	}
	p.check(RETURN)
	p.fgen(' ')
	fn_returns := p.cur_fn.typ != 'void'
	if fn_returns {
		if p.tok == RCBR {
			p.error('`$p.cur_fn.name` needs to return `$p.cur_fn.typ`')
		}
		else {
			ph := p.cgen.add_placeholder()
			expr_type := p.bool_expression()
			// Automatically wrap an object inside an option if the function returns an option
			if p.cur_fn.typ.ends_with(expr_type) && p.cur_fn.typ.starts_with('Option_') {
				//p.cgen.set_placeholder(ph, 'opt_ok(& ')
				p.cgen.set_placeholder(ph, 'opt_ok(& ')
				p.gen(', sizeof($expr_type))')
			}
			p.check_types(expr_type, p.cur_fn.typ)
		}
	}
	else {
		// Don't allow `return val` in functions that don't return anything
		// if p.tok != RCBR && p.tok != HASH {
		if false && p.tok == NAME || p.tok == INT {
			p.error('function `$p.cur_fn.name` does not return a value')
		}
	}
	p.returns = true
}

fn prepend_pkg(pkg, name string) string {
	return '${pkg}__${name}'
}

fn (p &Parser) prepend_pkg(name string) string {
	return prepend_pkg(p.pkg, name)
}

fn (p mut Parser) go_statement() {
	p.check(GO)
	// TODO copypasta of name_expr() ?
	// Method
	if p.peek() == DOT {
		var_name := p.lit
		v := p.cur_fn.find_var(var_name)
		p.cur_fn.mark_var_used(v)
		p.next()
		p.check(DOT)
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
	p.check(NAME)// json
	p.check(DOT)
	op := p.check_name()
	if op == 'decode' {
		// User tmp2; tmp2.foo = 0; tmp2.bar = 0;// I forgot to zero vals before => huge bug
		// Option_User tmp3 =  jsdecode_User(json_parse( s), &tmp2); ;
		// if (!tmp3 .ok) {
		// return
		// }
		// User u = *(User*) tmp3 . data;  // TODO remove this (generated in or {} block handler)
		p.check(LPAR)
		typ := p.get_type()
		p.check(COMMA)
		p.cgen.start_tmp()
		p.check_types(p.bool_expression(), 'string')
		expr := p.cgen.end_tmp()
		p.check(RPAR)
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
		p.check(LPAR)
		p.cgen.start_tmp()
		typ := p.bool_expression()
		T := p.table.find_type(typ)
		p.gen_json_for_type(T)
		expr := p.cgen.end_tmp()
		p.check(RPAR)
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
