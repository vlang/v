// Copyright (c) 2019 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.

module main

import os
import strings
import time

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
			g.lines << '#line $g.line "$g.file"'
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

fn (g mut CGen) add_placeholder() int {
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

fn (c mut V) prof_counters() string {
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

fn (p mut Parser) print_prof_counters() string {
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


fn build_thirdparty_obj_file(flag string) {
	obj_path := flag.all_after(' ')
	if os.file_exists(obj_path) {
		return
	}
	println('$obj_path not found, building it...')
	parent := os.dir( obj_path )
	files := os.ls(parent)
	mut cfiles := ''
	for file in files {
		if file.ends_with('.c') {
			cfiles += '"' + os.realpath( parent + os.PathSeparator + file ) + '" '
		}
	}
	cc := find_c_compiler()
	cc_thirdparty_options := find_c_compiler_thirdparty_options()
	cmd := '$cc $cc_thirdparty_options -c -o "$obj_path" $cfiles'
	res := os.exec(cmd) or {
		println('failed thirdparty object build cmd: $cmd')
		cerror(err)
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
	}
	cerror('bad os ifdef name "$name"')
	return ''
}

fn platform_postfix_to_ifdefguard(name string) string {
  switch name {
    case '.v': return '' // no guard needed
    case '_win.v': return '#ifdef _WIN32'
    case '_nix.v': return '#ifndef _WIN32'
    case '_lin.v': return '#ifdef __linux__'
    case '_mac.v': return '#ifdef __APPLE__'
  }
  cerror('bad platform_postfix "$name"')
  return ''
}

// C struct definitions, ordered
// Sort the types, make sure types that are referenced by other types
// are added before them.
fn (v mut V) c_type_definitions() string {
	mut types := []Type // structs that need to be sorted
	mut top_types := []Type // builtin types and types that only have primitive fields
	mut builtin_types := []Type
	// builtin types need to be on top
	builtins := ['string', 'array', 'map', 'Option']
	for builtin in builtins {
		typ := v.table.typesmap[builtin]
		builtin_types << typ
		v.table.typesmap.delete(builtin)
	}
	// split all types
	for _, t in v.table.typesmap {
		if !t.name[0].is_capital() {
			top_types << t
			continue
		}
		mut only_builtin_fields := true
		for field in t.fields {
			// user types start with a capital or contain __ (defined in another module)
			if field.typ[0].is_capital() || field.typ.contains('__') {
				only_builtin_fields = false
				break
			}
		}
		if only_builtin_fields {
			top_types << t
			continue
		}
		types << t
	}
	sort_structs(mut top_types)
	sort_structs(mut types)
	// Generate C code
	return types_to_c(builtin_types,v.table) + '\n//----\n' +
			types_to_c(top_types, v.table) + '\n/*----*/\n' +
			types_to_c(types, v.table)
}
	
fn types_to_c(types []Type, table &Table) string {
	mut sb := strings.new_builder(10)
	for t in types {
		if t.cat != .union_ && t.cat != .struct_ {
			continue
		}
		//if is_objc {
			//sb.writeln('@interface $name : $objc_parent { @public')
		//}
		//if is_atomic {
			//sb.write('_Atomic ')
		//}
		kind := if t.cat == .union_ {'union'} else {'struct'}
		sb.writeln('$kind $t.name {')
		for field in t.fields {
			sb.writeln(table.cgen_name_type_pair(field.name,
				field.typ) + ';')
		}
		sb.writeln('};\n')
		//if is_objc {
			//sb.writeln('@end')
		//}
	}
	return sb.str()
}

// sort structs by dependant fields
fn sort_structs(types mut []Type) {
	mut graph := new_dag()
	// types list
	mut type_names := []string
	for i := 0; i < types.len; i++ {
		t := types[i]
		type_names << t.name
	}
	// create list of deps
	for i := 0; i < types.len; i++ {
		t := types[i]
		mut field_types := []string
		for field in t.fields {
			// skip if not in types list
			if !(field.typ in type_names) {
				continue
			}
			// add type to deps
			field_types << field.typ
		}
		// add type and dependant types to graph
		graph.add(t.name, field_types)
	}
	// sort
	sorted := graph.resolve()
	// reorder types
	old_types := types.clone()
	for i:=0; i<sorted.nodes.len; i++ {
		node := sorted.nodes[i]
		for j := 0; j < old_types.len; j++ {
			t := old_types[j]
			if t.name == node.name {
				types[i] = t
			}
		}
	}
}

