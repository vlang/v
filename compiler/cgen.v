// Copyright (c) 2019 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.

module main

import os

struct CGen {
	out          os.File
	out_path     string
	//types        []string
	thread_fns   []string
	//buf          strings.Builder
	is_user      bool
mut:
	lines        []string
	typedefs     []string
	type_aliases []string
	includes     []string
	thread_args  []string
	consts       []string
	fns          []string
	so_fns       []string
	consts_init  []string
	pass         Pass
	nogen           bool
	tmp_line        string
	cur_line        string
	prev_line       string
	is_tmp          bool
	fn_main         string
	stash           string
	file            string
	line            int
	line_directives bool
	cut_pos int
}

fn new_cgen(out_name_c string) &CGen {
	path := out_name_c
	out := os.create(path) or {
		println('failed to create $path')
		return &CGen{}
	}
	gen := &CGen {
		out_path: path
		out: out
		//buf: strings.new_builder(10000)
		lines: _make(0, 1000, sizeof(string))
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
			g.lines << '\n#line $g.line "$g.file"'
		}
		g.lines << g.cur_line
		g.prev_line = g.cur_line
		g.cur_line = ''
	}
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
	g.out.close()
}

fn (g mut CGen) start_tmp() {
	if g.is_tmp {
		println(g.tmp_line)
		println('start_tmp() already started. cur_line="$g.cur_line"')
		exit(1)
	}
	// kg.tmp_lines_pos++
	g.tmp_line = ''
	g.is_tmp = true
}

fn (g mut CGen) end_tmp() string {
	g.is_tmp = false
	res := g.tmp_line
	g.tmp_line = ''
	return res
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
		res := g.tmp_line.right(pos)
		g.tmp_line = g.tmp_line.left(pos)
		return res
	}
	res := g.cur_line.right(pos)
	g.cur_line = g.cur_line.left(pos)
	return res
}

fn (g mut CGen) set_placeholder(pos int, val string) {
	if g.nogen || g.pass != .main {
		return
	}
	// g.lines.set(pos, val)
	if g.is_tmp {
		left := g.tmp_line.left(pos)
		right := g.tmp_line.right(pos)
		g.tmp_line = '${left}${val}${right}'
		return
	}
	left := g.cur_line.left(pos)
	right := g.cur_line.right(pos)
	g.cur_line = '${left}${val}${right}'
	// g.genln('')
}

fn (g mut CGen) insert_before(val string) {
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
	mut res := []string
	// Global fns
	//for f in c.table.fns {
		//res << 'double ${c.table.cgen_name(f)}_time;'
	//}
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
	mut res := []string
	// Global fns
	//for f in p.table.fns {
		//counter := '${p.table.cgen_name(f)}_time'
		//res << 'if ($counter) printf("%%f : $f.name \\n", $counter);'
	//}
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


fn build_thirdparty_obj_file(path string, moduleflags []CFlag) {
	obj_path := os.realpath(path)
	if os.file_exists(obj_path) {
		return
	}
	println('$obj_path not found, building it...')
	parent := os.dir(obj_path)
	files := os.ls(parent)
	mut cfiles := ''
	for file in files {
		if file.ends_with('.c') {
			cfiles += '"' + os.realpath( parent + os.PathSeparator + file ) + '" '
		}
	}
	cc := find_c_compiler()
	cc_thirdparty_options := find_c_compiler_thirdparty_options()
	btarget := moduleflags.c_options_before_target()
	atarget := moduleflags.c_options_after_target()
	cmd := '$cc $cc_thirdparty_options $btarget -c -o "$obj_path" $cfiles $atarget '
	res := os.exec(cmd) or {
		println('failed thirdparty object build cmd: $cmd')
		verror(err)
		return
	}
	println(res.output)
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
		case 'msvc': return '_MSC_VER'
		case 'android': return '__BIONIC__'
		case 'js': return '_VJS'
		case 'solaris': return '__sun'
	}
	verror('bad os ifdef name "$name"')
	return ''
}

fn platform_postfix_to_ifdefguard(name string) string {
	switch name {
		case '.v': return '' // no guard needed
		case '_win.v': return '#ifdef _WIN32'
		case '_nix.v': return '#ifndef _WIN32'
		case '_lin.v': return '#ifdef __linux__'
		case '_mac.v': return '#ifdef __APPLE__'
		case '_solaris.v': return '#ifdef __sun'
	}
	verror('bad platform_postfix "$name"')
	return ''
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
		if t_name in builtins {
			continue
		}
		types << t
	}
	// sort structs
	types_sorted := sort_structs(types)
	// Generate C code
	res := types_to_c(builtin_types,v.table) + '\n//----\n' +
			types_to_c(types_sorted, v.table)
	return res
}
	
// sort structs by dependant fields
fn sort_structs(types []Type) []Type {
	mut dep_graph := new_dep_graph()
	// types name list
	mut type_names := []string
	for t in types {
		type_names << t.name
	}
	// loop over types
	for t in types {
		// create list of deps
		mut field_deps := []string
		for field in t.fields {
			// skip if not in types list or already in deps
			if !(field.typ in type_names) || field.typ in field_deps {
				continue
			}
			field_deps << field.typ
		}
		// add type and dependant types to graph
		dep_graph.add(t.name, field_deps)
	}
	// sort graph
	dep_graph_sorted := dep_graph.resolve()
	if !dep_graph_sorted.acyclic {
		verror('error: cgen.sort_structs() DGNAC.\nplease create a new issue here: https://github.com/vlang/v/issues and tag @joe-conigliaro')
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
