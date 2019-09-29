// Copyright (c) 2019 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.

module main

import(
	strings
)

const (
	MaxLocalVars = 50
)

struct Fn {
	// addr int
mut:
	name          string
	mod           string
	//local_vars    []Var
	//var_idx       int
	args          []Var
	is_interface  bool
	// called_fns    []string
	// idx           int
	scope_level   int
	typ           string // return type
	is_c          bool
	receiver_typ  string
	is_public     bool
	is_method     bool
	returns_error bool
	is_decl       bool // type myfn fn(int, int)
	defer_text    []string
	//gen_types []string
	fn_name_token Tok
}

fn (p &Parser) find_var(name string) ?Var {
	for i in 0 .. p.var_idx {
		if p.local_vars[i].name == name {
			return p.local_vars[i]
		}
	}
	return none
}

fn (p &Parser) find_var_check_new_var(name string) ?Var {
	for i in 0 .. p.var_idx {
		if p.local_vars[i].name == name {
			return p.local_vars[i]
		}
	}
	// A hack to allow `newvar := Foo{ field: newvar }`
	// Declare the variable so that it can be used in the initialization
	if name == 'main__' + p.var_decl_name {
		return Var{
			name : p.var_decl_name
			typ : 'voidptr'
			is_mut : true
		}
	}
	return none
}

fn (p mut Parser) open_scope() {
	p.cur_fn.defer_text << ''
	p.cur_fn.scope_level++
}

fn (p mut Parser) mark_var_used(v Var) {
	if v.idx == -1 || v.idx >= p.local_vars.len {
		return
	}	
	p.local_vars[v.idx].is_used = true
}

fn (p mut Parser) mark_var_returned(v Var) {
	if v.idx == -1 || v.idx >= p.local_vars.len {
		return
	}	
	p.local_vars[v.idx].is_returned = true
}

fn (p mut Parser) mark_var_changed(v Var) {
	if v.idx == -1 || v.idx >= p.local_vars.len {
		return
	}	
	p.local_vars[v.idx].is_changed = true
}

fn (p mut Parser) mark_arg_moved(v Var) {
	for i, arg in p.cur_fn.args {
		if arg.name == v.name {
			//println('setting f $p.cur_fn.name arg $arg.name to is_mut')
			p.cur_fn.args[i].is_moved = true
			break
		}	
	}	
	p.table.fns[p.cur_fn.name] = p.cur_fn
}

fn (p mut Parser) known_var(name string) bool {
	_ = p.find_var(name) or {
		return false
	}	
	return true
}

fn (p mut Parser) register_var(v Var) {
	mut new_var := {v | idx: p.var_idx, scope_level: p.cur_fn.scope_level}
	if v.line_nr == 0 {
		new_var.token = p.cur_tok()
		new_var.line_nr = new_var.token.line_nr
	}
	// Expand the array
	if p.var_idx >= p.local_vars.len {
		p.local_vars << new_var
	}
	else {
		p.local_vars[p.var_idx] = new_var
	}
	p.var_idx++
}

fn (p mut Parser) clear_vars() {
	// shared a := [1, 2, 3]
	p.var_idx = 0
	if p.local_vars.len > 0 {
		if p.pref.autofree {
			p.local_vars.free()
		}
		p.local_vars = []Var
	}
}

// vlib header file?
fn (p mut Parser) is_sig() bool {
	return (p.pref.build_mode == .default_mode || p.pref.build_mode == .build_module) &&
	(p.file_path.contains(v_modules_path))
}

// Function signatures are added to the top of the .c file in the first run.
fn (p mut Parser) fn_decl() {
	p.clear_vars() // clear local vars every time a new fn is started
	p.fgen('fn ')
	//defer { p.fgenln('\n') }
	// If we are in the first pass, create a new function.
	// In the second pass fetch the one we created.
	/*
	mut f := if p.first_pass {
		Fn{
			mod: p.mod
			is_public: p.tok == .key_pub
		}
	else {
	}	
	*/
	mut f := Fn{
		mod: p.mod
		is_public: p.tok == .key_pub
	}
	is_live := p.attr == 'live' && !p.pref.is_so  && p.pref.is_live
	if p.attr == 'live' &&  p.first_pass() && !p.pref.is_live && !p.pref.is_so {
		println('INFO: run `v -live program.v` if you want to use [live] functions')
	}
	if f.is_public {
		p.next()
	}
	p.returns = false
	//p.gen('/* returns $p.returns */')
	p.next()
	// Method receiver
	mut receiver_typ := ''
	if p.tok == .lpar {
		f.is_method = true
		p.check(.lpar)
		receiver_name := p.check_name()
		is_mut := p.tok == .key_mut
		is_amp := p.tok == .amp
		if is_mut || is_amp {
			p.check_space(p.tok)
		}
		receiver_typ = p.get_type()
		T := p.table.find_type(receiver_typ)
		if T.cat == .interface_ {
			p.error('invalid receiver type `$receiver_typ` (`$receiver_typ` is an interface)')
		}
		// Don't allow modifying types from a different module
		if !p.first_pass() && !p.builtin_mod && T.mod != p.mod &&
			p.id != 'vgen' { // allow .str() on builtin arrays
			println('T.mod=$T.mod')
			println('p.mod=$p.mod')
			p.error('cannot define new methods on non-local type `$receiver_typ`')
		}
		// `(f *Foo)` instead of `(f mut Foo)` is a common mistake
		//if !p.builtin_mod && receiver_typ.contains('*') {
		if receiver_typ.ends_with('*') {
			t := receiver_typ.replace('*', '')
			p.error('use `($receiver_name mut $t)` instead of `($receiver_name *$t)`')
		}
		f.receiver_typ = receiver_typ
		if is_mut || is_amp {
			receiver_typ += '*'
		}
		p.check(.rpar)
		p.fspace()
		receiver := Var {
			name: receiver_name
			is_arg: true
			typ: receiver_typ
			is_mut: is_mut
			ref: is_amp
			ptr: is_mut
			line_nr: p.scanner.line_nr
			token: p.cur_tok()
		}
		f.args << receiver
		p.register_var(receiver)
	}
	// +-/* methods
	if p.tok == .plus || p.tok == .minus || p.tok == .mul {
		f.name = p.tok.str()
		p.next()
	}
	else {
		f.name = p.check_name()
	}
	f.fn_name_token = p.cur_tok()
	// C function header def? (fn C.NSMakeRect(int,int,int,int))
	is_c := f.name == 'C' && p.tok == .dot
	// Just fn signature? only builtin.v + default build mode
	// is_sig := p.builtin_mod && p.pref.build_mode == default_mode
	// is_sig := p.pref.build_mode == default_mode && (p.builtin_mod || p.file.contains(LANG_TMP))
	is_sig := p.is_sig()
	// println('\n\nfn_decl() name=$f.name receiver_typ=$receiver_typ')
	if is_c {
		p.check(.dot)
		f.name = p.check_name()
		f.is_c = true
	}
	else if !p.pref.translated && !p.file_path.contains('view.v') {
		if contains_capital(f.name) {
			p.error('function names cannot contain uppercase letters, use snake_case instead')
		}
		if f.name.contains('__') {
			p.error('function names cannot contain double underscores, use single underscores instead')
		}
	}
	// simple_name := f.name
	// println('!SIMP.le=$simple_name')
	// user.register() => User_register()
	has_receiver := receiver_typ.len > 0
	if receiver_typ != '' {
		// f.name = '${receiver_typ}_${f.name}'
	}
	// full mod function name
	// os.exit ==> os__exit()
	if !is_c && !p.builtin_mod && receiver_typ.len == 0 {
		f.name = p.prepend_mod(f.name)
	}
	if p.first_pass() && receiver_typ.len == 0 {
		for {
		existing_fn := p.table.find_fn(f.name) or { break }
		// This existing function could be defined as C decl before (no body), then we don't need to throw an erro
		if !existing_fn.is_decl {
			p.error('redefinition of `$f.name`')
		}
		break
		}
	}
	// Generic?
	mut is_generic := false
	if p.tok == .lt {
		is_generic = true
		p.next()
		gen_type := p.check_name()
		if gen_type != 'T' {
			p.error('only `T` is allowed as a generic type for now')
		}
		p.check(.gt)
		if p.first_pass() {
			p.table.register_generic_fn(f.name)
		}  else {
			//gen_types := p.table.fn_gen_types(f.name)
			//println(gen_types)
		}
	}
	// Args (...)
	p.fn_args(mut f)
	// Returns an error?
	if p.tok == .not {
		p.next()
		f.returns_error = true
	}
	// Returns a type?
	mut typ := 'void'
	if p.tok == .name || p.tok == .mul || p.tok == .amp || p.tok == .lsbr ||
	p.tok == .question || p.tok == .lpar {
		p.fgen(' ')
		// TODO In
		// if p.tok in [ .name, .mul, .amp, .lsbr ] {
		typ = p.get_type()
	}
	// multiple returns
	if typ.starts_with('_V_MulRet_') && p.first_pass() && !p.table.known_type(typ) {
		p.table.register_type2(Type{
			cat: TypeCategory.struct_,
			name: typ,
			mod: p.mod
		})
		for i, t in typ.replace('_V_MulRet_', '').replace('_PTR_', '*').split('_V_') {
			p.table.add_field(typ, 'var_$i', t, false, '', .public)
		}
		p.cgen.typedefs << 'typedef struct $typ $typ;'
	}
	// Translated C code can have empty functions (just definitions)
	is_fn_header := !is_c && !is_sig && (p.pref.translated || p.pref.is_test) &&	p.tok != .lcbr
	if is_fn_header {
		f.is_decl = true
	}
	// { required only in normal function declarations
	if !is_c && !is_sig && !is_fn_header {
		p.fgen(' ')
		p.check(.lcbr)
	}
	// Register ?option type
	if typ.starts_with('Option_') {
		p.cgen.typedefs << 'typedef Option $typ;'
	}
	// Register function
	f.typ = typ
	str_args := f.str_args(p.table)
	// Special case for main() args
	if f.name == 'main__main' && !has_receiver {
		if str_args != '' || typ != 'void' {
			p.error_with_tok('fn main must have no arguments and no return values', f.fn_name_token)
		}
	}
	dll_export_linkage := if p.os == .msvc && p.attr == 'live' && p.pref.is_so {
		'__declspec(dllexport) '
	} else if p.attr == 'inline' {
		'static inline '
	} else {
		''
	}
	if !p.is_vweb {
		p.set_current_fn( f )
	}
	// Generate `User_register()` instead of `register()`
	// Internally it's still stored as "register" in type User
	mut fn_name_cgen := p.table.fn_gen_name(f)
	// Start generation of the function body
	skip_main_in_test := false
	if !is_c && !is_live && !is_sig && !is_fn_header && !skip_main_in_test {
		if p.pref.obfuscate {
			p.genln('; // $f.name')
		}
		// Generate this function's body for all generic types
		if is_generic {
			gen_types := p.table.fn_gen_types(f.name)
			// Remember current scanner position, go back here for each type
			// TODO remove this once tokens are cached in `new_parser()`
			cur_pos := p.scanner.pos
			cur_tok := p.tok
			cur_lit := p.lit
			for gen_type in gen_types {
				p.genln('$dll_export_linkage$typ ${fn_name_cgen}_$gen_type($str_args) {')
				p.genln('// T start $p.pass ${p.strtok()}')
				p.cur_gen_type = gen_type // TODO support more than T
				p.statements()
				p.scanner.pos = cur_pos
				p.tok  = cur_tok
				p.lit = cur_lit
			}
		}
		else {
			p.gen_fn_decl(f, typ, str_args)
		}
	}
	if is_fn_header {
		p.genln('$typ $fn_name_cgen($str_args);')
		p.fgenln('')
	}
	if is_c {
		p.fgenln('\n')
	}
	// Register the method
	if receiver_typ != '' {
		mut receiver_t := p.table.find_type(receiver_typ)
		// No such type yet? It could be defined later. Create a new type.
		// struct declaration later will modify it instead of creating a new one.
		if p.first_pass() && receiver_t.name == '' {
			//println('fn decl ! registering placeholder $receiver_typ')
			receiver_t = Type {
				name: receiver_typ.replace('*', '')
				mod: p.mod
				is_placeholder: true
			}
			p.table.register_type2(receiver_t)
		}
		p.add_method(receiver_t.name, f)
	}
	else {
		// println('register_fn typ=$typ isg=$is_generic')
		p.table.register_fn(f)
	}
	if is_sig || p.first_pass() || is_live || is_fn_header || skip_main_in_test {
		// First pass? Skip the body for now
		// Look for generic calls.
		if !is_sig && !is_fn_header {
			p.skip_fn_body()
		}
		// Live code reloading? Load all fns from .so
		if is_live && p.first_pass() && p.mod == 'main' {
			//println('ADDING SO FN $fn_name_cgen')
			p.cgen.so_fns << fn_name_cgen
			fn_name_cgen = '(* $fn_name_cgen )'
		}
		// Function definition that goes to the top of the C file.
		mut fn_decl := '$dll_export_linkage$typ $fn_name_cgen($str_args)'
		if p.pref.obfuscate {
			fn_decl += '; // $f.name'
		}
		// Add function definition to the top
		if !is_c && p.first_pass() {
			// TODO hack to make Volt compile without -embed_vlib
			if f.name == 'darwin__nsstring' && p.pref.build_mode == .default_mode {
				return
			}
			p.cgen.fns << fn_decl + ';'
		}
		return
	}
	if p.attr == 'live' && p.pref.is_so {
		//p.genln('// live_function body start')
		p.genln('pthread_mutex_lock(&live_fn_mutex);')
	}

	if f.name == 'main__main' || f.name == 'main' || f.name == 'WinMain' {
		if p.pref.is_test && !p.scanner.file_path.contains('/volt') {
			p.error_with_tok('tests cannot have function `main`', f.fn_name_token)
		}
	}
	// println('is_c=$is_c name=$f.name')
	if is_c || is_sig || is_fn_header {
		// println('IS SIG .key_returnING tok=${p.strtok()}')
		return
	}
	// Profiling mode? Start counting at the beginning of the function (save current time).
	if p.pref.is_prof && f.name != 'time__ticks' {
		p.genln('double _PROF_START = time__ticks();//$f.name')
		cgen_name := p.table.fn_gen_name(f)
		if f.defer_text.len > f.scope_level {
			f.defer_text[f.scope_level] = '  ${cgen_name}_time += time__ticks() - _PROF_START;'
		}
	}
	if is_generic {
		// Don't need to generate body for the actual generic definition
		p.cgen.nogen = true
	}
	p.statements_no_rcbr()
	p.cgen.nogen = false
	// Print counting result after all statements in main
	if p.pref.is_prof && f.name == 'main' {
		p.genln(p.print_prof_counters())
	}
	// Counting or not, always need to add defer before the end
	if !p.is_vweb {
		if f.defer_text.len > f.scope_level {
		p.genln(f.defer_text[f.scope_level])
		}
	}
	if typ != 'void' && !p.returns {
		p.error_with_tok('$f.name must return "$typ"', f.fn_name_token)
	}
	if p.attr == 'live' && p.pref.is_so {
		//p.genln('// live_function body end')
		p.genln('pthread_mutex_unlock(&live_fn_mutex);')
	}
	// {} closed correctly? scope_level should be 0
	if p.mod == 'main' {
		// println(p.cur_fn.scope_level)
	}
	if p.cur_fn.scope_level > 2 {
		// p.error('unclosed {')
	}
	// Make sure all vars in this function are used (only in main for now)
	if p.mod != 'main' {
		if !is_generic {
			p.genln('}')
		}
		return
	}
	p.check_unused_variables()
	p.set_current_fn( EmptyFn )
	p.returns = false
	if !is_generic {
		p.genln('}')
	}
}

[inline]
// Skips the entire function's body in the first pass.
fn (p mut Parser) skip_fn_body() {
	mut opened_scopes := 0
	mut closed_scopes := 0
	for {
		if p.tok == .lcbr {
			opened_scopes++
		}
		if p.tok == .rcbr {
			closed_scopes++
		}
		// find `foo<Bar>()` in function bodies and register generic types
		// TODO
		// ...
		// Reached a declaration token? (fn, struct, const etc) Stop.
		if p.tok.is_decl() {
			break
		}
		// fn body ended, and a new fn attribute declaration like [live] is starting?
		if closed_scopes > opened_scopes && p.prev_tok == .rcbr {
			if p.tok == .lsbr {
				break
			}
		}
		p.next()
	}
}

fn (p mut Parser) check_unused_variables() {
	for var in p.local_vars {
		if var.name == '' {
			break
		}
		if !var.is_used && !p.pref.is_repl && !var.is_arg && !p.pref.translated {
			p.production_error_with_token('`$var.name` declared and not used', var.token )
		}
		if !var.is_changed && var.is_mut && !p.pref.is_repl && !p.pref.translated {
			p.error_with_tok( '`$var.name` is declared as mutable, but it was never changed', var.token )
		}
	}
}

// user.register() => "User_register(user)"
// method_ph - where to insert "user_register("
// receiver_var - "user" (needed for pthreads)
// receiver_type - "User"
fn (p mut Parser) async_fn_call(f Fn, method_ph int, receiver_var, receiver_type string) {
	// println('\nfn_call $f.name is_method=$f.is_method receiver_type=$f.receiver_type')
	// p.print_tok()
	mut thread_name := ''
	// Normal function => just its name, method => TYPE_FN.name
	mut fn_name := f.name
	if f.is_method {
		fn_name = receiver_type.replace('*', '') + '_' + f.name
		//fn_name = '${receiver_type}_${f.name}'
	}
	// Generate tmp struct with args
	arg_struct_name := 'thread_arg_$fn_name'
	tmp_struct := p.get_tmp()
	p.genln('$arg_struct_name * $tmp_struct = malloc(sizeof($arg_struct_name));')
	mut arg_struct := 'typedef struct  $arg_struct_name   { '
	p.next()
	p.check(.lpar)
	// str_args contains the args for the wrapper function:
	// wrapper(arg_struct * arg) { fn("arg->a, arg->b"); }
	mut str_args := ''
	mut did_gen_something := false
	for i, arg in f.args {
		arg_struct += '$arg.typ $arg.name ;'// Add another field (arg) to the tmp struct definition
		str_args += 'arg $dot_ptr $arg.name'
		if i == 0 && f.is_method {
			p.genln('$tmp_struct $dot_ptr $arg.name =  $receiver_var ;')
			if i < f.args.len - 1 {
				str_args += ','
			}
			did_gen_something = true
			continue
		}
		// Set the struct values (args)
		p.genln('$tmp_struct $dot_ptr $arg.name =  ')
		p.expression()
		p.genln(';')
		if i < f.args.len - 1 {
			p.check(.comma)
			str_args += ','
		}
		did_gen_something = true
	}

	if !did_gen_something {
		// Msvc doesnt like empty struct
		arg_struct += 'EMPTY_STRUCT_DECLARATION;'
	}

	arg_struct += '} $arg_struct_name ;'
	// Also register the wrapper, so we can use the original function without modifying it
	fn_name = p.table.fn_gen_name(f)
	wrapper_name := '${fn_name}_thread_wrapper'
	wrapper_text := 'void* $wrapper_name($arg_struct_name * arg) {$fn_name( /*f*/$str_args );  }'
	p.cgen.register_thread_fn(wrapper_name, wrapper_text, arg_struct)
	// Create thread object
	tmp_nr := p.get_tmp_counter()
	thread_name = '_thread$tmp_nr'
	if p.os != .windows && p.os != .msvc {
		p.genln('pthread_t $thread_name;')
	}
	tmp2 := p.get_tmp()
	mut parg := 'NULL'
	if f.args.len > 0 {
		parg = ' $tmp_struct'
	}
	// Call the wrapper
	if p.os == .windows || p.os == .msvc {
		p.genln(' CreateThread(0,0, $wrapper_name, $parg, 0,0);')
	}
	else {
		p.genln('int $tmp2 = pthread_create(& $thread_name, NULL, $wrapper_name, $parg);')
	}
	p.check(.rpar)
}

// p.tok == fn_name
fn (p mut Parser) fn_call(f Fn, method_ph int, receiver_var, receiver_type string) {
	if !f.is_public &&  !f.is_c && !p.pref.is_test && !f.is_interface && f.mod != p.mod  {
		if f.name == 'contains' {
			println('use `value in numbers` instead of `numbers.contains(value)`')
		}
		p.error('function `$f.name` is private')
	}
	p.calling_c = f.is_c
	if f.is_c && !p.builtin_mod {
		if f.name == 'free' {
			p.error('use `free()` instead of `C.free()`')
		} else if f.name == 'malloc' {
			p.error('use `malloc()` instead of `C.malloc()`')
		}
	}
	mut cgen_name := p.table.fn_gen_name(f)
	p.next()
	mut gen_type := ''
	if p.tok == .lt {
		p.check(.lt)
		gen_type = p.check_name()
		// run<T> => run_App
		if gen_type == 'T' && p.cur_gen_type != '' {
			gen_type = p.cur_gen_type
		}
		// `foo<Bar>()`
		// If we are in the first pass, we need to add `Bar` type to the generic function `foo`,
		// so that generic `foo`s body can be generated for each type in the second pass.
		if p.first_pass() {
			println('registering $gen_type in $f.name fname=$f.name')
			p.table.register_generic_fn_type(f.name, gen_type)
			// Function bodies are skipped in the first passed, we only need to register the generic type here.
			return
		}
		cgen_name += '_' + gen_type
		p.check(.gt)
	}
	// if p.pref.is_prof {
	// p.cur_fn.called_fns << cgen_name
	// }
	// Normal function call
	if !f.is_method {
		p.gen(cgen_name)
		p.gen('(')
		// p.fgen(f.name)
	}
	// If we have a method placeholder,
	// we need to preappend "method(receiver, ...)"
	else {
		receiver := f.args.first()
		//println('r=$receiver.typ RT=$receiver_type')
		if receiver.is_mut && !p.expr_var.is_mut {
			//println('$method_call  recv=$receiver.name recv_mut=$receiver.is_mut')
			p.error('`$p.expr_var.name` is immutable, declare it with `mut`')
		}
		if !p.expr_var.is_changed {
			p.mark_var_changed(p.expr_var)
		}
		p.gen_method_call(receiver_type, f.typ, cgen_name, receiver, method_ph)
	}
	// foo<Bar>()
	p.fn_call_args(mut f)
	p.gen(')')
	p.calling_c = false
	// println('end of fn call typ=$f.typ')
}

// for declaration
// return an updated Fn object with args[] field set
fn (p mut Parser) fn_args(f mut Fn) {
	p.check(.lpar)
	defer { p.check(.rpar) }
	if f.is_interface {
		int_arg := Var {
			typ: f.receiver_typ
			token: p.cur_tok()
		}
		f.args << int_arg
	}
	// `(int, string, int)`
	// Just register fn arg types
	types_only := p.tok == .mul || p.tok == .amp || (p.peek() == .comma && p.table.known_type(p.lit)) || p.peek() == .rpar// (int, string)
	if types_only {
		for p.tok != .rpar {
			typ := p.get_type()
			p.check_and_register_used_imported_type(typ)
			v := Var {
				typ: typ
				is_arg: true
				// is_mut: is_mut
				line_nr: p.scanner.line_nr
				token: p.cur_tok()
			}
			// f.register_var(v)
			f.args << v
			if p.tok == .comma {
				p.next()
			}
		}
	}
	// `(a int, b, c string)` syntax
	for p.tok != .rpar {
		mut names := [	p.check_name()	]
		// `a,b,c int` syntax
		for p.tok == .comma {
			p.check(.comma)
			p.fspace()
			names << p.check_name()
		}
		p.fspace()
		is_mut := p.tok == .key_mut
		if is_mut {
			p.next()
		}
		mut typ := p.get_type()
		p.check_and_register_used_imported_type(typ)
		if is_mut && is_primitive_type(typ) {
			p.error('mutable arguments are only allowed for arrays, maps, and structs.' +
			'\nreturn values instead: `foo(n mut int)` => `foo(n int) int`')
		}
		for name in names {
			if !p.first_pass() && !p.table.known_type(typ) {
				p.error('fn_args: unknown type $typ')
			}
			if is_mut {
				typ += '*'
			}
			v := Var{
				name: name
				typ: typ
				is_arg: true
				is_mut: is_mut
				ptr: is_mut
				line_nr: p.scanner.line_nr
				token: p.cur_tok()
			}
			p.register_var(v)
			f.args << v
		}
		if p.tok == .comma {
			p.next()
		}
		if p.tok == .dotdot {
			f.args << Var{
				name: '..'
			}
			p.next()
		}
	}
}

// foo *(1, 2, 3, mut bar)*
fn (p mut Parser) fn_call_args(f mut Fn) &Fn {
	// println('fn_call_args() name=$f.name args.len=$f.args.len')
	// C func. # of args is not known
	p.check(.lpar)
	if f.is_c {
		for p.tok != .rpar {
			//C.func(var1, var2.method())
			//If the parameter calls a function or method that is not C,
			//the value of p.calling_c is changed
			p.calling_c = true
			ph := p.cgen.add_placeholder()
			typ := p.bool_expression()
			// Cast V byteptr to C char* (byte is unsigned in V, that led to C warnings)
			if typ == 'byte*' {
				p.cgen.set_placeholder(ph, '(char*)')
			}	
			if p.tok == .comma {
				p.gen(', ')
				p.check(.comma)
			}
		}
		p.check(.rpar)
		return f
	}
	// add debug information to panic when -debug arg is passed
	if p.v.pref.is_debug && f.name == 'panic' && !p.is_js {
		mod_name := p.mod.replace('_dot_', '.')
		fn_name := p.cur_fn.name.replace('${p.mod}__', '')
		file_path := p.file_path.replace('\\', '\\\\') // escape \
		p.cgen.resetln(p.cgen.cur_line.replace(
			'v_panic (',
			'_panic_debug ($p.scanner.line_nr, tos2((byte *)"$file_path"), tos2((byte *)"$mod_name"), tos2((byte *)"$fn_name"), '
		))
	}
	for i, arg in f.args {
		// Receiver is the first arg
		// Skip the receiver, because it was already generated in the expression
		if i == 0 && f.is_method {
			if f.args.len > 1 && !p.is_js {
				p.gen(',')
			}
			continue
		}
		// Reached the final vararg? Quit
		if i == f.args.len - 1 && arg.name == '..' {
			break
		}
		ph := p.cgen.add_placeholder()
		// `)` here means that not enough args were provided
		if p.tok == .rpar {
			str_args := f.str_args(p.table)// TODO this is C args
			p.error('not enough arguments in call to `$f.name ($str_args)`')
		}
		// If `arg` is mutable, the caller needs to provide `mut`:
		// `mut numbers := [1,2,3]; reverse(mut numbers);`
		if arg.is_mut {
			if p.tok != .key_mut && p.tok == .name {
				mut dots_example :=  'mut $p.lit'
				if i > 0 {
					dots_example = '.., ' + dots_example
				}
				if i < f.args.len - 1 {
					dots_example = dots_example + ',..'
				}
				p.error('`$arg.name` is a mutable argument, you need to provide `mut`: `$f.name($dots_example)`')	
			}
			if p.peek() != .name {
				p.error('`$arg.name` is a mutable argument, you need to provide a variable to modify: `$f.name(... mut a...)`')
			}
			p.check(.key_mut)
			var_name := p.lit
			v := p.find_var(var_name) or {
				p.error('`$arg.name` is a mutable argument, you need to provide a variable to modify: `$f.name(... mut a...)`')
				exit(1)
			}
			if !v.is_changed {
				p.mark_var_changed(v)
			}
		}
		p.expected_type = arg.typ
		clone := p.pref.autofree && arg.typ == 'string' && arg.is_moved && p.mod != 'builtin'
		if clone {
			p.gen('/*YY f=$f.name arg=$arg.name is_moved=$arg.is_moved*/string_clone(')
		}	
		mut typ := p.bool_expression()
		if clone {
			p.gen(')')
		}
		// Optimize `println`: replace it with `printf` to avoid extra allocations and
		// function calls.
		// `println(777)` => `printf("%d\n", 777)`
		// (If we don't check for void, then V will compile `println(func())`)
		if i == 0 && (f.name == 'println' || f.name == 'print') && typ == 'ustring' {
			if typ == 'ustring' {
				p.gen('.s')
			}
			typ = 'string'
		}
		if i == 0 && (f.name == 'println' || f.name == 'print')  && typ != 'string' && typ != 'ustring' && typ != 'void' {
			T := p.table.find_type(typ)
			$if !windows {
			$if !js {
				fmt := p.typ_to_fmt(typ, 0)
				if fmt != '' {
					p.cgen.resetln(p.cgen.cur_line.replace(f.name + ' (', '/*opt*/printf ("' + fmt + '\\n", '))
					continue
				}
			}
			}
			if typ.ends_with('*') {
				p.cgen.set_placeholder(ph, 'ptr_str(')
				p.gen(')')
				continue
			}
			// Make sure this type has a `str()` method
			$if !js {
			if !T.has_method('str') {
				// Arrays have automatic `str()` methods
				if T.name.starts_with('array_') {
					p.gen_array_str(T)
					p.cgen.set_placeholder(ph, '${typ}_str(')
					p.gen(')')
					continue
				} else if T.cat == .struct_ {
					p.gen_struct_str(T)
					p.cgen.set_placeholder(ph, '${typ}_str(')
					p.gen(')')
					continue
				}	
				error_msg := ('`$typ` needs to have method `str() string` to be printable')
				p.error(error_msg)
			}
			p.cgen.set_placeholder(ph, '${typ}_str(')
			p.gen(')')
			}
			continue
		}
		got := typ
		expected := arg.typ
		got_ptr := got.ends_with('*')
		exp_ptr := expected.ends_with('*')
		// println('fn arg got="$got" exp="$expected"')
		if !p.check_types_no_throw(got, expected) {
			mut j := i
			if f.is_method {
				j--
			}	
			mut nr := '${i+1}th'
			if j == 0 {
				nr = 'first'
			} else if j == 1 {
				nr = 'second'
			}	 else if j == 2 {
				nr = 'third'
			}	
			p.error('cannot use type `$typ` as type `$arg.typ` in $nr ' +
				'argument to `$f.name()`')
		}
		is_interface := p.table.is_interface(arg.typ)
		// Automatically add `&` or `*` before an argument.
		// V, unlike C and Go, simplifies this aspect:
		// `foo(bar)` is allowed where `foo(&bar)` is expected.
		// The argument is not mutable, so it won't be changed by the function.
		// It doesn't matter whether it's passed by referencee or by value
		// to the end user.
		if !is_interface {
			// Dereference
			if got_ptr && !exp_ptr {
				p.cgen.set_placeholder(ph, '*')
			}
			// Reference
			// TODO ptr hacks. DOOM hacks, fix please.
			if !got_ptr && exp_ptr && got != 'voidptr' {
				// Special case for mutable arrays. We can't `&` function
				// results,
				// have to use `(array[]){ expr }` hack.
				if expected.starts_with('array_') && exp_ptr { //&& !arg.is_mut{
					p.cgen.set_placeholder(ph, '& /*111*/ (array[]){')
					p.gen('}[0] ')
				}
				// println('\ne:"$expected" got:"$got"')
				else if ! (expected == 'void*' && got == 'int') &&
				! (expected == 'byte*' && got.contains(']byte')) &&
				! (expected == 'byte*' && got == 'string') &&
				//! (expected == 'void*' && got == 'array_int') {
				! (expected == 'byte*' && got == 'byteptr') {
					p.cgen.set_placeholder(ph, '& /*112 EXP:"$expected" GOT:"$got" */')
				}
			}
		}
		else if is_interface {
			if !got_ptr {
				p.cgen.set_placeholder(ph, '&')
			}
			// Pass all interface methods
			interface_type := p.table.find_type(arg.typ)
			for method in interface_type.methods {
				p.gen(', ${typ}_${method.name} ')
			}
		}
		// Check for commas
		if i < f.args.len - 1 {
			// Handle 0 args passed to varargs
			is_vararg := i == f.args.len - 2 && f.args[i + 1].name == '..'
			if p.tok != .comma && !is_vararg {
				p.error('wrong number of arguments for $i,$arg.name fn `$f.name`: expected $f.args.len, but got less')
			}
			if p.tok == .comma {
				p.fgen(', ')
			}
			if !is_vararg {
				p.next()
				p.gen(',')
			}
		}
	}
	// varargs
	if f.args.len > 0 {
		last_arg := f.args.last()
		if last_arg.name == '..' {
			for p.tok != .rpar {
				if p.tok == .comma {
					p.gen(',')
					p.check(.comma)
				}
				p.bool_expression()
			}
		}
	}
	if p.tok == .comma {
		p.error('wrong number of arguments for fn `$f.name`: expected $f.args.len, but got more')
	}
	p.check(.rpar)
	return f // TODO is return f right?
}

// "fn (int, string) int"
fn (f &Fn) typ_str() string {
	mut sb := strings.new_builder(50)
	sb.write('fn (')
	for i, arg in f.args {
		sb.write(arg.typ)
		if i < f.args.len - 1 {
			sb.write(',')
		}
	}
	sb.write(')')
	if f.typ != 'void' {
		sb.write(' $f.typ')
	}
	return sb.str()
}

// "fn foo(a int) stirng", for .vh module headers
fn (f &Fn) v_definition() string {
	return 'fn '//$f.name(${f.str_args()})'
}

// f.args => "int a, string b"
fn (f &Fn) str_args(table &Table) string {
	mut s := ''
	for i, arg in f.args {
		// Interfaces are a special case. We need to pass the object + pointers
		// to all methods:
		// fn handle(r Runner) { =>
		// void handle(void *r, void (*Runner_run)(void*)) {
		if table.is_interface(arg.typ) {
			// First the object (same name as the interface argument)
			s += ' void* $arg.name'
			// Now  all methods
			interface_type := table.find_type(arg.typ)
			for method in interface_type.methods {
				s += ', $method.typ (*${arg.typ}_${method.name})(void*'
				if method.args.len > 1 {
					for a in method.args.right(1) {
						s += ', $a.typ'
					}
				}
				s += ')'
			}
		}
		else if arg.name == '..' {
			s += '...'
		}
		else {
			// s += '$arg.typ $arg.name'
			s += table.cgen_name_type_pair(arg.name, arg.typ)// '$arg.typ $arg.name'
		}
		if i < f.args.len - 1 {
			s += ', '
		}
	}
	return s
}

// find local function variable with closest name to `name`
fn (p &Parser) find_misspelled_local_var(name string, min_match f32) string {
	mut closest := f32(0)
	mut closest_var := ''
	for var in p.local_vars {
		if var.scope_level > p.cur_fn.scope_level {
			continue
		}
		n := name.all_after('.')
		if var.name == '' || (n.len - var.name.len > 2 || var.name.len - n.len > 2) { continue }
		coeff := strings.dice_coefficient(var.name, n)
		if coeff > closest {
			closest = coeff
			closest_var = var.name
		}
	}
	return if closest >= min_match { closest_var } else { '' }
}

fn (fns []Fn) contains(f Fn) bool {
	for ff in fns {
		if ff.name == f.name {
			return true
		}	
	}	
	return false
}	
