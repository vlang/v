// Copyright (c) 2019 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.

module main

struct CGen {
	out          os.File
	out_path     string
	typedefs     []string
	type_aliases []string
	includes     []string
	types        []string
	thread_args  []string
	thread_fns   []string
	consts       []string
	fns          []string
	so_fns       []string
	consts_init  []string
	lines        []string
	is_user      bool
mut:
	run          Pass
	nogen        bool
	tmp_line     string
	cur_line     string
	prev_line    string
	is_tmp       bool
	fn_main      string
	stash        string
}

fn new_cgen(out_name_c string) *CGen {
	path:='$TmpPath/$out_name_c'
	out := os.create(path) or {
		println('failed to create $path') 
		return &CGen{} 
} 
	 
	gen := &CGen {
		out_path: path 
		out: out 
	}
	return gen
}

fn (g mut CGen) genln(s string) {
	if g.nogen || g.run == RUN_DECLS {
		return
	}
	if g.is_tmp {
		g.tmp_line = '$g.tmp_line $s\n'
		return
	}
	g.cur_line = '$g.cur_line $s'
	if g.cur_line != '' {
		g.lines << g.cur_line
		g.prev_line = g.cur_line
		g.cur_line = ''
	}
}

fn (g mut CGen) gen(s string) {
	if g.nogen || g.run == RUN_DECLS {
		return
	}
	if g.is_tmp {
		g.tmp_line = '$g.tmp_line $s'
	}
	else {
		g.cur_line = '$g.cur_line $s'
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

fn (g mut CGen) set_placeholder(pos int, val string) {
	if g.nogen {
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

fn (g mut CGen) add_placeholder2() int {
	if g.is_tmp {
		println('tmp in addp2')
		exit(1)
	}
	g.lines << ''
	return g.lines.len - 1
}

fn (g mut CGen) set_placeholder2(pos int, val string) {
	if g.nogen {
		return
	}
	if g.is_tmp {
		println('tmp in setp2')
		exit(1)
	}
	g.lines[pos] = val
}

fn (g mut CGen) insert_before(val string) {
	g.lines.insert(g.lines.len - 1, val)
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
	for typ in c.table.types {
		// println('')
		for f in typ.methods {
			// res << f.cgen_name()
			res << 'double ${c.table.cgen_name(f)}_time;'
			// println(f.cgen_name())
		}
	}
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
	return res.join(';\n')
}

fn (p mut Parser) gen_type(s string) {
	if !p.first_run() {
		return
	}
	p.cgen.types << s
}

fn (p mut Parser) gen_typedef(s string) {
	if !p.first_run() {
		return
	}
	p.cgen.typedefs << s
}

fn (p mut Parser) gen_type_alias(s string) {
	if !p.first_run() {
		return
	}
	p.cgen.type_aliases << s
}

fn (g mut CGen) add_to_main(s string) {
	println('add to main')
	g.fn_main = g.fn_main + s
}

