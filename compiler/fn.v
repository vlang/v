// Copyright (c) 2019 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.

module main

import strings 

const (
	MaxLocalVars = 50
)

struct Fn {
	// addr int
mut:
	pkg           string
	local_vars    []Var
	var_idx       int
	args          []Var
	is_interface  bool
	// called_fns    []string
	// idx           int
	scope_level   int
	typ           string // return type
	name          string
	is_c          bool
	receiver_typ  string
	is_public    bool
	is_method     bool
	returns_error bool
	is_decl       bool // type myfn fn(int, int)
	defer         string
}

fn (f &Fn) find_var(name string) Var {
	for i in 0 .. f.var_idx {
		if f.local_vars[i].name == name {
			return f.local_vars[i]
		}
	}
	return Var{}
}

fn (f mut Fn) open_scope() {
	f.scope_level++
}

fn (f mut Fn) close_scope() {
	// println('close_scope level=$f.scope_level var_idx=$f.var_idx')
	// Move back `var_idx` (pointer to the end of the array) till we reach the previous scope level.
	// This effectivly deletes (closes) current scope.
	mut i := f.var_idx - 1
	for; i >= 0; i-- {
		v := f.local_vars[i]
		if v.scope_level != f.scope_level {
			// println('breaking. "$v.name" v.scope_level=$v.scope_level')
			break
		}
	}
	f.var_idx = i + 1
	// println('close_scope new var_idx=$f.var_idx\n')
	f.scope_level--
}

fn (f &Fn) mark_var_used(v Var) {
	for i, vv in f.local_vars {
		if vv.name == v.name {
			mut ptr := &f.local_vars[i]
			ptr.is_used = true
			// / f.local_vars[i].is_used = true
			// return
		}
	}
}

fn (f &Fn) known_var(name string) bool {
	v := f.find_var(name)
	return v.name.len > 0
}

fn (f mut Fn) register_var(v Var) {
	new_var := {v | scope_level: f.scope_level}
	// Expand the array
	if f.var_idx >= f.local_vars.len {
		f.local_vars << new_var
	}
	else {
		f.local_vars[f.var_idx] = new_var
	}
	f.var_idx++
}

fn (f mut Fn) clear_vars() {
	f.var_idx = 0
	f.local_vars = []Var
}

// vlib header file?
fn (p mut Parser) is_sig() bool {
	return (p.pref.build_mode == .default_mode || p.pref.build_mode == .build) &&
	(p.file_path.contains(TmpPath))
}

fn new_fn(pkg string, is_public bool) *Fn {
	mut f := &Fn {
		pkg: pkg
		local_vars: [Var{}
		; MaxLocalVars]
		is_public: is_public
	}
	return f
}

// Function signatures are added to the top of the .c file in the first run.
fn (p mut Parser) fn_decl() {
	p.fgen('fn ')
	is_pub := p.tok == PUB
	if is_pub {
		p.next()
	}
	p.returns = false
	p.next()
	mut f := new_fn(p.pkg, is_pub)
	// Method receiver
	mut receiver_typ := ''
	if p.tok == LPAR {
		f.is_method = true
		p.check(LPAR)
		receiver_name := p.check_name()
		is_mut := p.tok == MUT
		is_amp := p.tok == AMP
		if is_mut || is_amp {
			p.check_space(p.tok) 
		}
		receiver_typ = p.get_type()
		T := p.table.find_type(receiver_typ)
		if T.is_interface {
			p.error('invalid receiver type `$receiver_typ` (`$receiver_typ` is an interface)')
		}
		// Don't allow modifying types from a different module
		if !p.first_run() && !p.builtin_pkg && T.pkg != p.pkg {
			println('T.pkg=$T.pkg')
			println('pkg=$p.pkg')
			p.error('cannot define new methods on non-local type `$receiver_typ`')
		}
		// (a *Foo) instead of (a mut Foo) is a common mistake
		if !p.builtin_pkg && receiver_typ.contains('*') {
			t := receiver_typ.replace('*', '')
			p.error('use `($receiver_name mut $t)` instead of `($receiver_name *$t)`')
		}
		f.receiver_typ = receiver_typ
		if is_mut || is_amp {
			receiver_typ += '*'
		}
		p.check(RPAR)
		p.fspace() 
		receiver := Var {
			name: receiver_name
			is_arg: true
			typ: receiver_typ
			is_mut: is_mut
			ref: is_amp
			ptr: is_mut
			line_nr: p.scanner.line_nr
		}
		f.args << receiver
		f.register_var(receiver)
	}
	if p.tok == PLUS || p.tok == MINUS || p.tok == MUL {
		f.name = p.tok.str()
		p.next()
	}
	else {
		f.name = p.check_name()
	}
	// C function header def? (fn C.NSMakeRect(int,int,int,int))
	is_c := f.name == 'C' && p.tok == DOT
	// Just fn signature? only builtin.v + default build mode
	// is_sig := p.builtin_pkg && p.pref.build_mode == default_mode
	// is_sig := p.pref.build_mode == default_mode && (p.builtin_pkg || p.file.contains(LANG_TMP))
	is_sig := p.is_sig()
	// println('\n\nfn decl !!is_sig=$is_sig name=$f.name $p.builtin_pkg')
	if is_c {
		p.check(DOT)
		f.name = p.check_name()
		f.is_c = true
	}
	else if !p.pref.translated && !p.file_path.contains('view.v') {
		if contains_capital(f.name) {
			p.error('function names cannot contain uppercase letters, use snake_case instead')
		}
		if f.name.contains('__') {
			p.error('function names cannot contain double underscores ("__"), use single underscores instead')
		}
	}
	// simple_name := f.name
	// println('!SIMPLE=$simple_name')
	// user.register() => User_register()
	has_receiver := receiver_typ.len > 0
	if receiver_typ != '' {
		// f.name = '${receiver_typ}_${f.name}'
	}
	// full pkg function name
	// os.exit ==> os__exit()
	if !is_c && !p.builtin_pkg && p.pkg != 'main' && receiver_typ.len == 0 {
		f.name = p.prepend_pkg(f.name)
	}
	if p.first_run() && p.table.known_fn(f.name) && receiver_typ.len == 0 {
		existing_fn := p.table.find_fn(f.name)
		// This existing function could be defined as C decl before (no body), then we don't need to throw an erro
		if !existing_fn.is_decl {
			p.error('redefinition of `$f.name`')
		}
	}
	// Generic?
	mut is_generic := false
	if p.tok == LT {
		p.next()
		gen_type := p.check_name()
		if gen_type != 'T' {
			p.error('only `T` is allowed as a generic type for now')
		}
		p.check(GT)
		is_generic = true
	}
	// Args (...)
	p.fn_args(mut f)
	// Returns an error?
	if p.tok == NOT {
		p.next()
		f.returns_error = true
	}
	// Returns a type?
	mut typ := 'void'
	if p.tok == NAME || p.tok == MUL || p.tok == AMP || p.tok == LSBR ||
	p.tok == QUESTION {
		p.fgen(' ')
		// TODO In
		// if p.tok in [ NAME, MUL, AMP, LSBR ] {
		typ = p.get_type()
	}
	// Translated C code can have empty functions (just definitions)
	is_fn_header := !is_c && !is_sig && (p.pref.translated || p.pref.is_test) &&
	(p.tok != LCBR)// || (p.tok == NAME && p.peek() != LCBR))
	if is_fn_header {
		f.is_decl = true
		// println('GOT fn header $f.name')
	}
	// { required only in normal function declarations
	if !is_c && !is_sig && !is_fn_header {
		p.fgen(' ')
		p.check(LCBR)
	}
	// Register option ? type
	if typ.starts_with('Option_') {
		p.cgen.typedefs << 'typedef Option $typ;'
	}
	// Register function
	f.typ = typ
	mut str_args := f.str_args(p.table)
	// println('FN DECL $f.name typ=$f.typ str_args="$str_args"')
	// Special case for main() args
	if f.name == 'main' && !has_receiver {
		if str_args != '' || typ != 'void' {
			p.error('fn main must have no arguments and no return values')
		}
		typ = 'int'
		str_args = 'int argc, char** argv'
	}
	// Only in C code generate User_register() instead of register()
	// Internally it's still stored as "register" in type User
	// mut fn_name_cgen := f.name
	// if receiver_typ != '' {
	// fn_name_cgen = '${receiver_typ}_$f.name'
	// fn_name_cgen = fn_name_cgen.replace(' ', '')
	// fn_name_cgen = fn_name_cgen.replace('*', '')
	// }
	mut fn_name_cgen := p.table.cgen_name(f)
	// Start generation of the function body
	is_live := p.pref.is_live && f.name != 'main' && f.name != 'reload_so'
	skip_main_in_test := f.name == 'main' && p.pref.is_test
	if !is_c && !is_live && !is_sig && !is_fn_header && !skip_main_in_test {
		if p.pref.obfuscate {
			p.genln('; // ${f.name}')
		}
		p.genln('$typ $fn_name_cgen($str_args) {')
		// if f.name == 'WinMain' {
		// typ = 'int'
		// }
	}
	if is_fn_header {
		p.genln('$typ $fn_name_cgen($str_args);')
		p.fgenln('')
	}
	if is_c {
		p.fgenln('\n')
	}
	p.cur_fn = f
	// Register the method
	if receiver_typ != '' {
		mut receiver_T := p.table.find_type(receiver_typ)
		// No such type yet? It could be defined later. Create a new type.
		// struct declaration later will modify it instead of creating a new one.
		if p.first_run() && receiver_T.name == '' {
			// println('fn decl !!!!!!! REG PH $receiver_typ')
			ttyp := Type {
				name: receiver_typ.replace('*', '')
				pkg: p.pkg
				is_placeholder: true
			}
			p.table.register_type2(ttyp)
		}
		// f.idx = p.table.fn_cnt
		receiver_T.add_method(f)
	}
	else {
		// println('register_fn typ=$typ isg=$is_generic')
		p.table.register_fn(f)
	}
	if is_sig || p.first_run() || is_live || is_fn_header || skip_main_in_test {
		// First pass? Skip the body for now [BIG]
		if !is_sig && !is_fn_header {
			for {
				p.next()
				if p.tok.is_decl() {
					break
				}
			}
		}
		// Live code reloading? Load all fns from .so
		if is_live && p.first_run() {
			// p.cgen.consts_init.push('$fn_name_cgen = dlsym(lib, "$fn_name_cgen");')
			p.cgen.so_fns << fn_name_cgen
			fn_name_cgen = '(* $fn_name_cgen )'
		}
		// Actual fn declaration!
		mut fn_decl := '$typ $fn_name_cgen($str_args)'
		if p.pref.obfuscate {
			fn_decl += '; // ${f.name}'
		}
		// Add function definition to the top
		if !is_c && f.name != 'main' && p.first_run() {
			// TODO hack to make Volt compile without -embed_vlib
			if f.name == 'darwin__nsstring' && p.pref.build_mode == .default_mode {
				return
			}
			p.cgen.fns << fn_decl + ';'
		}
		p.fgenln('\n')// TODO defer this instead of copy pasting
		return
	}
	if f.name == 'main' || f.name == 'WinMain' {
		p.genln('init_consts();')
		if p.table.imports.contains('os') {
			if f.name == 'main' {
				p.genln('os__args = os__init_os_args(argc, argv);')
			}
			else if f.name == 'WinMain' {
				p.genln('os__args = os__parse_windows_cmd_line(pCmdLine);')
			}
		}
		// We are in live code reload mode, call the .so loader in bg
		if p.pref.is_live {
			p.genln(' 
load_so("bounce.so"); 
pthread_t _thread_so;
pthread_create(&_thread_so , NULL, &reload_so, NULL); ')
		}
		if p.pref.is_test && !p.scanner.file_path.contains('/volt') {
			p.error('tests cannot have function `main`')
		}
	}
	// println('is_c=$is_c name=$f.name')
	if is_c || is_sig || is_fn_header {
		// println('IS SIG RETURNING tok=${p.strtok()}')
		p.fgenln('\n')
		return
	}
	// We are in profile mode? Start counting at the beginning of the function (save current time).
	if p.pref.is_prof && f.name != 'main' && f.name != 'time__ticks' {
		p.genln('double _PROF_START = time__ticks();//$f.name')
		cgen_name := p.table.cgen_name(f)
		f.defer = '  ${cgen_name}_time += time__ticks() - _PROF_START;'
	}
	p.statements_no_curly_end()
	// Print counting result after all statements in main
	if p.pref.is_prof && f.name == 'main' {
		p.genln(p.print_prof_counters())
	}
	// Counting or not, always need to add defer before the end
	p.genln(f.defer)
	if typ != 'void' && !p.returns && f.name != 'main' && f.name != 'WinMain' {
		p.error('$f.name must return "$typ"')
	}
	// {} closed correctly? scope_level should be 0
	if p.pkg == 'main' {
		// println(p.cur_fn.scope_level)
	}
	if p.cur_fn.scope_level > 2 {
		// p.error('unclosed {')
	}
	// Make sure all vars in this function are used (only in main for now)
	// if p.builtin_pkg || p.pkg == 'os' ||p.pkg=='http'{
	if p.pkg != 'main' {
		p.genln('}')
		p.fgenln('\n')
		return
	}
	p.check_unused_variables()
	p.cur_fn = EmptyFn
	p.fgenln('\n')
	p.genln('}')
}

fn (p mut Parser) check_unused_variables() {
	for var in p.cur_fn.local_vars {
		if var.name == '' {
			break
		}
		if !var.is_used && !p.pref.is_repl && !var.is_arg && !p.pref.translated && var.name != '_' {
			p.scanner.line_nr = var.line_nr - 1
			p.error('`$var.name` declared and not used')
		}
		// Very basic automatic memory management at the end of the function.
		// This is inserted right before the final `}`, so if the object is being returned,
		// the free method will not be called.
		if p.pref.is_test && var.typ.contains('array_') {
			// p.genln('v_${var.typ}_free($var.name); // !!!! XAXA')
			// p.genln('free(${var.name}.data); // !!!! XAXA')
		}
	}
}

// Important function with 5 args.
// user.say_hi() => "User_say_hi(user)"
// method_ph - where to insert "user_say_hi("
// receiver_var - "user" (needed for pthreads)
// receiver_type - "User"
fn (p mut Parser) async_fn_call(f Fn, method_ph int, receiver_var, receiver_type string) {
	// println('\nfn_call $f.name is_method=$f.is_method receiver_type=$f.receiver_type')
	// p.print_tok()
	mut thread_name := ''
	// Normal function => just its name, method => TYPE_FNNAME
	mut fn_name := f.name
	if f.is_method {
		receiver_type = receiver_type.replace('*', '')
		fn_name = '${receiver_type}_${f.name}'
	}
	// Generate tmp struct with args
	arg_struct_name := 'thread_arg_$fn_name'
	tmp_struct := p.get_tmp()
	p.genln('$arg_struct_name * $tmp_struct = malloc(sizeof($arg_struct_name));')
	mut arg_struct := 'typedef struct  $arg_struct_name   { '
	p.next()
	p.check(LPAR)
	// str_args contains the args for the wrapper function:
	// wrapper(arg_struct * arg) { fn("arg->a, arg->b"); }
	mut str_args := ''
	for i, arg in f.args {
		arg_struct += '$arg.typ $arg.name ;'// Add another field (arg) to the tmp struct definition
		str_args += 'arg->$arg.name'
		if i == 0 && f.is_method {
			p.genln('$tmp_struct -> $arg.name =  $receiver_var ;')
			if i < f.args.len - 1 {
				str_args += ','
			}
			continue
		}
		// Set the struct values (args)
		p.genln('$tmp_struct -> $arg.name =  ')
		p.expression()
		p.genln(';')
		if i < f.args.len - 1 {
			p.check(COMMA)
			str_args += ','
		}
	}
	arg_struct += '} $arg_struct_name ;'
	// Also register the wrapper, so we can use the original function without modifying it
	fn_name = p.table.cgen_name(f)
	wrapper_name := '${fn_name}_thread_wrapper'
	wrapper_text := 'void* $wrapper_name($arg_struct_name * arg) {$fn_name( /*f*/$str_args );  }'
	p.cgen.register_thread_fn(wrapper_name, wrapper_text, arg_struct)
	// Create thread object
	tmp_nr := p.get_tmp_counter()
	thread_name = '_thread$tmp_nr'
	if p.os != WINDOWS {
		p.genln('pthread_t $thread_name;')
	}
	tmp2 := p.get_tmp()
	mut parg := 'NULL'
	if f.args.len > 0 {
		parg = ' $tmp_struct'
	}
	// Call the wrapper
	if p.os == WINDOWS {
		p.genln(' CreateThread(0,0, $wrapper_name, $parg, 0,0);')
	}
	else {
		p.genln('int $tmp2 = pthread_create(& $thread_name, NULL, $wrapper_name, $parg);')
	}
	p.check(RPAR)
}

fn (p mut Parser) fn_call(f Fn, method_ph int, receiver_var, receiver_type string) {
	if !f.is_public &&  !f.is_c && !p.pref.is_test && !f.is_interface && f.pkg != p.pkg  { 
		p.error('function `$f.name` is private')
	}
	p.calling_c = f.is_c
	is_print := p.pref.is_prod &&// Hide prints only in prod
	!p.pref.is_test &&
	!p.builtin_pkg &&// Allow prints in builtin  pkgs
	f.is_c && f.name == 'printf'
	if !p.cgen.nogen {
		p.cgen.nogen = is_print
	}
	cgen_name := p.table.cgen_name(f)
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
		mut method_call := '${cgen_name}('
		receiver := f.args.first()
		if receiver.is_mut && !p.expr_var.is_mut {
			println('$method_call  recv=$receiver.name recv_mut=$receiver.is_mut')
			p.error('`$p.expr_var.name` is immutable')
		}
		// if receiver is mutable or a ref (&), generate & for the first arg
		if receiver.ref || (receiver.is_mut && !receiver_type.contains('*')) {
			method_call += '& /* ? */'
		}
		// generate deref (TODO copy pasta later in fn_call_args)
		if !receiver.is_mut && receiver_type.contains('*') {
			method_call += '*'
		}
		mut cast := ''
		// Method returns (void*) => cast it to int, string, user etc
		// number := *(int*)numbers.first()
		if f.typ == 'void*' {
			// array_int => int
			cast = receiver_type.all_after('_')
			cast = '*($cast*) '
		}
		p.cgen.set_placeholder(method_ph, '$cast $method_call')
	}
	p.next()
	p.fn_call_args(f)
	p.gen(')')
	p.calling_c = false
	if is_print {
		p.cgen.nogen = false
	}
	// println('end of fn call typ=$f.typ')
}

// for declaration
// return an updated Fn object with args[] field set
fn (p mut Parser) fn_args(f mut Fn) {
	p.check(LPAR)
	// TODO defer  	p.check(RPAR)
	if f.is_interface {
		int_arg := Var {
			typ: f.receiver_typ
		}
		f.args << int_arg
	}
	// Just register fn arg types
	types_only := p.tok == MUL || (p.peek() == COMMA && p.table.known_type(p.lit)) || p.peek() == RPAR// (int, string)
	if types_only {
		for p.tok != RPAR {
			typ := p.get_type()
			v := Var {
				typ: typ
				is_arg: true
				// is_mut: is_mut
				line_nr: p.scanner.line_nr
			}
			// f.register_var(v)
			f.args << v
			if p.tok == COMMA {
				p.next()
			}
		}
	}
	// (a int, b,c string) syntax
	for p.tok != RPAR {
		mut names := [
		p.check_name()
		]
		// a,b,c int syntax
		for p.tok == COMMA {
			p.check(COMMA)
			p.fspace()
			names << p.check_name()
		}
		p.fspace()
		is_mut := p.tok == MUT
		if is_mut {
			p.next()
		}
		mut typ2 := p.get_type()
		for name in names {
			if !p.first_run() && !p.table.known_type(typ2) {
				p.error('fn_args: unknown type $typ2')
			}
			if is_mut {
				// && !typ2.starts_with('array_') {
				typ2 += '*'
			}
			v := Var {
				name: name
				typ: typ2
				is_arg: true
				is_mut: is_mut
				ptr: is_mut
				line_nr: p.scanner.line_nr
			}
			f.register_var(v)
			f.args << v
		}
		if p.tok == COMMA {
			p.next()
		}
		if p.tok == DOTDOT {
			f.args << Var {
				name: '..'
			}
			p.next()
		}
	}
	p.check(RPAR)
}

fn (p mut Parser) fn_call_args(f *Fn) *Fn {
	// p.gen('(')
	// println('fn_call_args() name=$f.name args.len=$f.args.len')
	// C func. # of args is not known
	// if f.name.starts_with('c_') {
	p.check(LPAR)
	if f.is_c {
		for p.tok != RPAR {
			p.bool_expression()
			if p.tok == COMMA {
				p.gen(', ')
				p.check(COMMA)
			}
		}
		p.check(RPAR)
		return f
	}
	// Receiver - first arg
	for i, arg in f.args {
		// println('$i) arg=$arg.name')
		// Skip receiver, because it was already generated in the expression
		if i == 0 && f.is_method {
			if f.args.len > 1 {
				p.gen(',')
			}
			continue
		}
		// Reached the final vararg? Quit
		if i == f.args.len - 1 && arg.name == '..' {
			break
		}
		ph := p.cgen.add_placeholder()
		// ) here means not enough args were supplied
		if p.tok == RPAR {
			str_args := f.str_args(p.table)// TODO this is C args
			p.error('not enough arguments in call to `$f.name ($str_args)`')
		}
		// If `arg` is mutable, the caller needs to provide MUT:
		// `arr := [1,2,3]; reverse(mut arr);`
		if arg.is_mut {
			if p.tok != MUT {
				p.error('`$arg.name` is a mutable argument, you need to provide `mut`: `$f.name(...mut a...)`')
			}
			if p.peek() != NAME {
				p.error('`$arg.name` is a mutable argument, you need to provide a variable to modify: `$f.name(... mut a...)`')
			}
			p.check(MUT)
		}
		typ := p.bool_expression()
		// Optimize `println`: replace it with `printf` to avoid extra allocations and
		// function calls. `println(777)` => `printf("%d\n", 777)` 
		// (If we don't check for void, then V will compile `println(func())`) 
		if i == 0 && f.name == 'println' && typ != 'string' && typ != 'void' {
			T := p.table.find_type(typ)
			fmt := p.typ_to_fmt(typ) 
			if fmt != '' { 
				p.cgen.cur_line = p.cgen.cur_line.replace('println (', '/*opt*/printf ("' + fmt + '\\n", ')    
				continue 
			}  
			if typ.ends_with('*') {
				p.cgen.set_placeholder(ph, 'ptr_str(')
				p.gen(')')
				continue 
			}
			// Make sure this type has a `str()` method
			if !T.has_method('str') {
				if T.fields.len > 0 {
					mut index := p.cgen.cur_line.len - 1
					for index > 0 && p.cgen.cur_line[index] != ` ` { index-- }
					name := p.cgen.cur_line.right(index + 1)
					if name == '}' {
						p.error('`$typ` needs to have method `str() string` to be printable')
					}
					p.cgen.cur_line = p.cgen.cur_line.left(index)
					p.create_type_string(T, name)
					p.cgen.cur_line.replace(typ, '')
					p.next()
					return p.fn_call_args(f)
				}
				p.error('`$typ` needs to have method `str() string` to be printable')
			}
			p.cgen.set_placeholder(ph, '${typ}_str(')
			p.gen(')')
			continue
		}
		got := typ
		expected := arg.typ
		// println('fn arg got="$got" exp="$expected"')
		if !p.check_types_no_throw(got, expected) {
			mut err := 'Fn "$f.name" wrong arg #${i+1}. '
			err += 'Expected "$arg.typ" ($arg.name)  but got "$typ"'
			p.error(err)
		}
		is_interface := p.table.is_interface(arg.typ)
		// Add & or * before arg?
		if !is_interface {
			// Dereference
			if got.contains('*') && !expected.contains('*') {
				p.cgen.set_placeholder(ph, '*')
			}
			// Reference
			// TODO ptr hacks. DOOM hacks, fix please.
			if !got.contains('*') && expected.contains('*') && got != 'voidptr' {
				// println('\ne:"$expected" got:"$got"')
				if ! (expected == 'void*' && got == 'int') &&
				! (expected == 'byte*' && got.contains(']byte')) &&
				! (expected == 'byte*' && got == 'string') {
					p.cgen.set_placeholder(ph, '& /*11 EXP:"$expected" GOT:"$got" */')
				}
			}
		}
		// interface?
		if is_interface {
			if !got.contains('*') {
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
			if p.tok != COMMA && !is_vararg {
				p.error('wrong number of arguments for $i,$arg.name fn `$f.name`: expected $f.args.len, but got less')
			}
			if p.tok == COMMA {
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
			println('GOT VAR ARGS AFTER')
			for p.tok != RPAR {
				if p.tok == COMMA {
					p.gen(',')
					p.check(COMMA)
				}
				p.bool_expression()
			}
		}
	}
	if p.tok == COMMA {
		p.error('wrong number of arguments for fn `$f.name`: expected $f.args.len, but got more')
	}
	p.check(RPAR)
	// p.gen(')')
}

fn contains_capital(s string) bool {
	// for c in s {
	for i := 0; i < s.len; i++ {
		c := s[i]
		if c >= `A` && c <= `Z` {
			return true
		}
	}
	return false
}

// "fn (int, string) int"
fn (f Fn) typ_str() string {
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

// f.args => "int a, string b"
fn (f &Fn) str_args(table *Table) string {
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
				s += ', $method.typ (*${arg.typ}_${method.name})(void*) '
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
