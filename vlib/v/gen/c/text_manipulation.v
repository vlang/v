// Copyright (c) 2019-2024 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module c

import v.util

const trace_gen_wanted_value = $d('trace_gen_wanted_value', '')

@[if trace_gen_wanted ?]
fn (mut g Gen) trace_gen_wanted_context(last_character_len int, s string) {
	last_n := g.out.last_n(last_character_len)
	eprintln('> trace_gen_wanted, last characters:\n${last_n}\n')
	eprintln('> trace_gen_wanted, found wanted cgen string `${trace_gen_wanted_value}` in generated string ${s}')
	print_backtrace()
}

@[if trace_gen_wanted ?]
fn (mut g Gen) trace_gen_wanted(s string) {
	if s.contains(trace_gen_wanted_value) {
		g.trace_gen_wanted_context(256, s)
	}
}

@[if trace_gen_wanted ?]
fn (mut g Gen) trace_gen_wanted2(s1 string, s2 string) {
	if s1.contains(trace_gen_wanted_value) || s2.contains(trace_gen_wanted_value) {
		g.trace_gen_wanted_context(256, s1 + s2)
	}
}

@[if trace_gen ?]
fn (mut g Gen) trace_gen(reason string, s string) {
	if g.file == unsafe { nil } {
		eprintln('gen file: <nil> | last_fn_c_name: ${g.last_fn_c_name:-45} | ${reason}: ${s}')
	} else {
		eprintln('gen file: ${g.file.path:-30} | last_fn_c_name: ${g.last_fn_c_name:-45} | ${reason}: ${s}')
	}
}

@[expand_simple_interpolation]
fn (mut g Gen) write(s string) {
	g.trace_gen_wanted(s)
	g.trace_gen('write', s)
	if g.indent > 0 && g.empty_line {
		g.out.write_string(util.tabs(g.indent))
	}
	g.out.write_string(s)
	g.empty_line = false
}

fn (mut g Gen) write2(s1 string, s2 string) {
	g.trace_gen_wanted2(s1, s2)
	g.trace_gen('write2 s1', s1)
	if g.indent > 0 && g.empty_line {
		g.out.write_string(util.tabs(g.indent))
	}
	g.out.write_string(s1)
	g.empty_line = false

	g.trace_gen('write2 s2', s2)
	if g.indent > 0 && g.empty_line {
		g.out.write_string(util.tabs(g.indent))
	}
	g.out.write_string(s2)
	g.empty_line = false
}

fn (mut g Gen) write_decimal(x i64) {
	g.trace_gen('write_decimal', x.str())
	if g.indent > 0 && g.empty_line {
		g.out.write_string(util.tabs(g.indent))
	}
	g.out.write_decimal(x)
	g.empty_line = false
}

fn (mut g Gen) writeln(s string) {
	g.trace_gen_wanted(s)
	g.trace_gen('writeln', s)
	if g.indent > 0 && g.empty_line {
		g.out.write_string(util.tabs(g.indent))
		// g.out_parallel[g.out_idx].write_string(util.tabs(g.indent))
	}
	// println('w len=$g.out_parallel.len')
	g.out.writeln(s)
	// g.out_parallel[g.out_idx].writeln(s)
	g.empty_line = true
	// g.line_nr++
}

fn (mut g Gen) writeln2(s1 string, s2 string) {
	g.trace_gen_wanted2(s1, s2)
	g.trace_gen('writeln2 s1', s1)
	// expansion for s1
	if g.indent > 0 && g.empty_line {
		g.out.write_string(util.tabs(g.indent))
	}
	g.out.writeln(s1)
	g.empty_line = true

	// expansion for s2
	g.trace_gen('writeln2 s2', s2)
	if g.indent > 0 && g.empty_line {
		g.out.write_string(util.tabs(g.indent))
	}
	g.out.writeln(s2)
	g.empty_line = true
}

// Below are hacks that should be removed at some point.

fn (mut g Gen) go_back(n int) {
	g.out.go_back(n)
	// g.out_parallel[g.out_idx].go_back(n)
}

fn (mut g Gen) go_back_to(n int) {
	g.out.go_back_to(n)
	// g.out_parallel[g.out_idx].go_back_to(n)
}

@[inline]
fn (g &Gen) nth_stmt_pos(n int) int {
	return g.stmt_path_pos[g.stmt_path_pos.len - (1 + n)]
}

@[inline]
fn (mut g Gen) set_current_pos_as_last_stmt_pos() {
	g.stmt_path_pos << g.out.len
}

@[inline]
fn (mut g Gen) go_before_last_stmt() string {
	return g.out.cut_to(g.nth_stmt_pos(0))
}

@[inline]
fn (mut g Gen) go_before_ternary() string {
	return g.out.cut_to(g.nth_stmt_pos(g.inside_ternary))
}

fn (mut g Gen) insert_before_stmt(s string) {
	cur_line := g.out.cut_to(g.nth_stmt_pos(g.inside_ternary))
	g.writeln(s)
	g.write(cur_line)
}

fn (mut g Gen) insert_at(pos int, s string) {
	cur_line := g.out.cut_to(pos)
	// g.out_parallel[g.out_idx].cut_to(pos)
	g.writeln(s)
	g.write(cur_line)

	// After modifying the code in the buffer, we need to adjust the positions of the statements
	// to account for the added line of code.
	// This is necessary to ensure that autofree can properly insert string declarations
	// in the correct positions, considering the surgically made changes.
	for index, stmt_pos in g.stmt_path_pos {
		if stmt_pos >= pos {
			g.stmt_path_pos[index] += s.len + 1
		}
	}
}
