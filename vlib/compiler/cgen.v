// Copyright (c) 2019-2020 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module compiler

import (
	os
	strings
	filepath
)

struct CGen {
	out             os.File
	out_path        string
	// types        []string
	thread_fns      []string
	// buf          strings.Builder
	is_user         bool
mut:
	lines           []string
	lines_extra     []string
	typedefs        []string
	type_aliases    []string
	includes        []string
	thread_args     []string
	consts          []string
	const_defines   []string
	fns             []string
	so_fns          []string
	consts_init     []string
	pass            Pass
	nogen           bool
	prev_tmps       []string
	tmp_line        string
	cur_line        string
	prev_line       string
	is_tmp          bool
	fn_main         string
	stash           string
	file            string
	line            int
	line_directives bool
	cut_pos         int
}

pub fn new_cgen(out_name_c string) &CGen {
	path := out_name_c
	out := os.create(path)or{
		println('failed to create $path')
		return &CGen{
		}
	}
	gen := &CGen{
		out_path: path
		out: out
		// buf: strings.new_builder(10000)

		lines: make(0, 1000, sizeof(string))
	}
	return gen
}

fn (g mut CGen) genln(s string) {
	if g.nogen || g.pass != .main {
		return
	}
	if g.is_tmp {
		g.tmp_line = '$g.tmp_line $s\n'
		return
	}
	g.cur_line = '$g.cur_line $s'
	if g.cur_line != '' {
		if g.line_directives && g.cur_line.trim_space() != '' {
			if g.file.len > 0 && g.line > 0 {
				g.lines << '\n#line $g.line "$g.file"'
			}
		}
		g.lines << g.cur_line
		g.prev_line = g.cur_line
		g.cur_line = ''
	}
}

// same as `set_placeholder(0, s)`, but faster
fn (g mut CGen) prepend_to_statement(s string) {
	if g.is_tmp {
		g.tmp_line = s + g.tmp_line
		return
	}
	g.lines << s
	g.prev_line = g.cur_line
}

fn (g mut CGen) gen(s string) {
	if g.nogen || g.pass != .main {
		return
	}
	if g.is_tmp {
		g.tmp_line = '$g.tmp_line $s'
	}
	else {
		g.cur_line = '$g.cur_line $s'
	}
}

fn (g mut CGen) resetln(s string) {
	if g.nogen || g.pass != .main {
		return
	}
	if g.is_tmp {
		g.tmp_line = s
	}
	else {
		g.cur_line = s
	}
}

fn (g mut CGen) save() {
	s := g.lines.join('\n')
	g.out.writeln(s)
	g.out.writeln(g.lines_extra.join('\n'))
	g.out.close()
}

// returns expression's type, and entire expression's string representation)
fn (p mut Parser) tmp_expr() (string,string) {
	// former start_tmp()
	if p.cgen.is_tmp {
		p.cgen.prev_tmps << p.cgen.tmp_line
	}
	// kg.tmp_lines_pos++
	p.cgen.tmp_line = ''
	p.cgen.is_tmp = true
	//
	typ := p.bool_expression()
	res := p.cgen.tmp_line
	if p.cgen.prev_tmps.len > 0 {
		p.cgen.tmp_line = p.cgen.prev_tmps.last()
		p.cgen.prev_tmps = p.cgen.prev_tmps[0..p.cgen.prev_tmps.len - 1]
	}
	else {
		p.cgen.tmp_line = ''
		p.cgen.is_tmp = false
	}
	return typ,res
}

fn (g &CGen) add_placeholder() int {
	if g.is_tmp {
		return g.tmp_line.len
	}
	return g.cur_line.len
}

fn (g mut CGen) start_cut() {
	g.cut_pos = g.add_placeholder()
}

fn (g mut CGen) cut() string {
	pos := g.cut_pos
	g.cut_pos = 0
	if g.is_tmp {
		res := g.tmp_line[pos..]
		g.tmp_line = g.tmp_line[..pos]
		return res
	}
	res := g.cur_line[pos..]
	g.cur_line = g.cur_line[..pos]
	return res
}

fn (g mut CGen) set_placeholder(pos int, val string) {
	if g.nogen || g.pass != .main {
		return
	}
	// if pos == 0 {
	// g.prepend_to_statement(val)
	// return
	// }
	// g.lines.set(pos, val)
	if g.is_tmp {
		left := g.tmp_line[..pos]
		right := g.tmp_line[pos..]
		g.tmp_line = '${left}${val}${right}'
		return
	}
	left := g.cur_line[..pos]
	right := g.cur_line[pos..]
	g.cur_line = '${left}${val}${right}'
	// g.genln('')
}

fn (g mut CGen) insert_before(val string) {
	if g.nogen {
		return
	}
	prev := g.lines[g.lines.len - 1]
	g.lines[g.lines.len - 1] = '$prev \n $val \n'
}

fn (g mut CGen) register_thread_fn(wrapper_name, wrapper_text, struct_text string) {
	for arg in g.thread_args {
		if arg.contains(wrapper_name) {
			return
		}
	}
	g.thread_args << struct_text
	g.thread_args << wrapper_text
}

fn (v &V) prof_counters() string {
	res := []string
	// Global fns
	// for f in c.table.fns {
	// res << 'double ${c.table.cgen_name(f)}_time;'
	// }
	// Methods
	/*
	for typ in c.table.types {
		// println('')
		for f in typ.methods {
			// res << f.cgen_name()
			res << 'double ${c.table.cgen_name(f)}_time;'
			// println(f.cgen_name())
		}
	}
	*/

	return res.join(';\n')
}

fn (p &Parser) print_prof_counters() string {
	res := []string
	// Global fns
	// for f in p.table.fns {
	// counter := '${p.table.cgen_name(f)}_time'
	// res << 'if ($counter) printf("%%f : $f.name \\n", $counter);'
	// }
	// Methods
	/*
	for typ in p.table.types {
		// println('')
		for f in typ.methods {
			counter := '${p.table.cgen_name(f)}_time'
			res << 'if ($counter) printf("%%f : ${p.table.cgen_name(f)} \\n", $counter);'
			// res << 'if ($counter) printf("$f.name : %%f\\n", $counter);'
			// res << f.cgen_name()
			// res << 'double ${f.cgen_name()}_time;'
			// println(f.cgen_name())
		}
	}
	*/

	return res.join(';\n')
}

fn (p mut Parser) gen_typedef(s string) {
	if !p.first_pass() {
		return
	}
	p.cgen.typedefs << s
}

fn (p mut Parser) gen_type_alias(s string) {
	if !p.first_pass() {
		return
	}
	p.cgen.type_aliases << s
}

fn (g mut CGen) add_to_main(s string) {
	g.fn_main = g.fn_main + s
}

fn (v &V) build_thirdparty_obj_file(path string, moduleflags []CFlag) {
	obj_path := os.realpath(path)
	if os.exists(obj_path) {
		return
	}
	println('$obj_path not found, building it...')
	parent := filepath.dir(obj_path)
	files := os.ls(parent)or{
		panic(err)
	}
	mut cfiles := ''
	for file in files {
		if file.ends_with('.c') {
			cfiles += '"' + os.realpath(parent + filepath.separator + file) + '" '
		}
	}
	btarget := moduleflags.c_options_before_target()
	atarget := moduleflags.c_options_after_target()
	cmd := '$v.pref.ccompiler $v.pref.third_party_option $btarget -c -o "$obj_path" $cfiles $atarget '
	res := os.exec(cmd)or{
		println('failed thirdparty object build cmd: $cmd')
		verror(err)
		return
	}
	if res.exit_code != 0 {
		println('failed thirdparty object build cmd: $cmd')
		verror(res.output)
		return
	}
	println(res.output)
}

fn os_name_to_ifdef(name string) string {
	match name {
		'windows' {
			return '_WIN32'
		}
		'mac' {
			return '__APPLE__'
		}
		'macos' {
			return '__APPLE__'
		}
		'linux' {
			return '__linux__'
		}
		'freebsd' {
			return '__FreeBSD__'
		}
		'openbsd' {
			return '__OpenBSD__'
		}
		'netbsd' {
			return '__NetBSD__'
		}
		'dragonfly' {
			return '__DragonFly__'
		}
		'msvc' {
			return '_MSC_VER'
		}
		'android' {
			return '__ANDROID__'
		}
		'js' {
			return '_VJS'
		}
		'solaris' {
			return '__sun'
		}
		'haiku' {
			return '__haiku__'
		}
		'linux_or_macos' {
			return ''
		}
		else {
			verror('bad os ifdef name "$name"')
		}}
	// verror('bad os ifdef name "$name"')
	return ''
}

fn (v &V) platform_postfix_to_ifdefguard(name string) string {
	if name.starts_with('custom '){
		cdefine := name.replace('custom ','')
		return '#ifdef CUSTOM_DEFINE_${cdefine}'
	}
	s := match name {
		'.v'{
			''
		} // no guard needed
		'_win.v', '_windows.v'{
			'#ifdef _WIN32'
		}
		'_nix.v'{
			'#ifndef _WIN32'
		}
		'_lin.v', '_linux.v'{
			'#ifdef __linux__'
		}
		'_mac.v', '_darwin.v'{
			'#ifdef __APPLE__'
		}
		'_bsd.v', '_freebsd.v '{
			'#ifdef __FreeBSD__'
		}
		'_solaris.v'{
			'#ifdef __sun'
		}
		'_haiku.v'{
			'#ifdef __haiku__'
		}
		else {
			// verror('bad platform_postfix "$name"')
			// TODO
			''}}
	if s == '' {
		verror('bad platform_postfix "$name"')
	}
	return s
}

// C struct definitions, ordered
// Sort the types, make sure types that are referenced by other types
// are added before them.
fn (v &V) type_definitions() string {
	mut types := []Type // structs that need to be sorted
	mut builtin_types := []Type // builtin types
	// builtin types need to be on top
	builtins := ['string', 'array', 'map', 'Option']
	for builtin in builtins {
		typ := v.table.typesmap[builtin]
		builtin_types << typ
	}
	// everything except builtin will get sorted
	for t_name, t in v.table.typesmap {
		if t_name in builtins || t.is_generic {
			continue
		}
		types << t
	}
	// sort structs
	types_sorted := sort_structs(types)
	// Generate C code
	res := types_to_c(builtin_types, v.table) + '\n//----\n' + types_to_c(types_sorted, v.table)
	return res
}

// sort structs by dependant fields
fn sort_structs(types []Type) []Type {
	mut dep_graph := new_dep_graph()
	// types name list
	mut type_names := []string
	for typ in types {
		type_names << typ.name
	}
	// loop over types
	for t in types {
		// create list of deps
		mut field_deps := []string
		for field in t.fields {
			// Need to handle fixed size arrays as well (`[10]Point`)
			ft := if field.typ.starts_with('[') { field.typ.all_after(']') } else { field.typ }
			// skip if not in types list or already in deps
			if !(ft in type_names) || ft in field_deps {
				continue
			}
			field_deps << ft // field.typ
		}
		// add type and dependant types to graph
		dep_graph.add(t.name, field_deps)
	}
	// sort graph
	dep_graph_sorted := dep_graph.resolve()
	if !dep_graph_sorted.acyclic {
		verror('cgen.sort_structs(): the following structs form a dependency cycle:\n' + dep_graph_sorted.display_cycles() + '\nyou can solve this by making one or both of the dependant struct fields references, eg: field &MyStruct' + '\nif you feel this is an error, please create a new issue here: https://github.com/vlang/v/issues and tag @joe-conigliaro')
	}
	// sort types
	mut types_sorted := []Type
	for node in dep_graph_sorted.nodes {
		for t in types {
			if t.name == node.name {
				types_sorted << t
				continue
			}
		}
	}
	return types_sorted
}

// Generates interface table and interface indexes
fn (v &V) interface_table() string {
	mut sb := strings.new_builder(100)
	for _, t in v.table.typesmap {
		if t.cat != .interface_ {
			continue
		}
		// interface_name is for example Speaker
		interface_name := t.name
		mut methods := ''
		mut generated_casting_functions := ''
		sb.writeln('// NR methods = $t.gen_types.len')
		for i, gen_type in t.gen_types {
			// ptr_ctype can be for example Cat OR Cat_ptr:
			ptr_ctype := gen_type.replace('*', '_ptr')
			// cctype is the Cleaned Concrete Type name, *without ptr*,
			// i.e. cctype is always just Cat, not Cat_ptr:
			cctype := gen_type.replace('*', '')

			// Speaker_Cat_index = 0
			interface_index_name := '_${interface_name}_${ptr_ctype}_index'

			generated_casting_functions += '
${interface_name} I_${cctype}_to_${interface_name}(${cctype} x) {
  return (${interface_name}){
           ._object = (void*) memdup(&x, sizeof(${cctype})),
           ._interface_idx = ${interface_index_name} };
}
'
			methods += '{\n'
			for j, method in t.methods {
				// Cat_speak
				methods += ' (void*)    ${cctype}_${method.name}'
				if j < t.methods.len - 1 {
					methods += ', \n'
				}
			}
			methods += '\n},\n\n'
			sb.writeln('int ${interface_index_name} = $i;')
		}
		if t.gen_types.len > 0 {
			// methods = '{TCCSKIP(0)}'
			// }
			sb.writeln('void* (* ${interface_name}_name_table[][$t.methods.len]) = ' + '{ \n $methods \n }; ')
		}
		else {
			// The line below is needed so that C compilation succeeds,
			// even if no interface methods are called.
			// See https://github.com/zenith391/vgtk3/issues/7
			sb.writeln('void* (* ${interface_name}_name_table[][1]) = ' + '{ {NULL} }; ')
		}
		if generated_casting_functions.len > 0 {
			sb.writeln('// Casting functions for interface "${interface_name}" :')
			sb.writeln( generated_casting_functions )
		}
	}
	return sb.str()
}
