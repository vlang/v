// Copyright (c) 2019-2024 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module c

import v.util

fn (mut g Gen) write(s string) {
	$if trace_gen ? {
		if g.file == unsafe { nil } {
			eprintln('gen file: <nil> | last_fn_c_name: ${g.last_fn_c_name:-45} | write: ${s}')
		} else {
			eprintln('gen file: ${g.file.path:-30} | last_fn_c_name: ${g.last_fn_c_name:-45} | write: ${s}')
		}
	}
	if g.indent > 0 && g.empty_line {
		g.out.write_string(util.tabs(g.indent))
		// g.out_parallel[g.out_idx].write_string(util.tabs(g.indent))
	}
	g.out.write_string(s)
	////g.out_parallel[g.out_idx].write_string(s)
	g.empty_line = false
}

fn (mut g Gen) writeln(s string) {
	$if trace_gen ? {
		if g.file == unsafe { nil } {
			eprintln('gen file: <nil> | last_fn_c_name: ${g.last_fn_c_name:-45} | writeln: ${s}')
		} else {
			eprintln('gen file: ${g.file.path:-30} | last_fn_c_name: ${g.last_fn_c_name:-45} | writeln: ${s}')
		}
	}
	if g.indent > 0 && g.empty_line {
		g.out.write_string(util.tabs(g.indent))
		// g.out_parallel[g.out_idx].write_string(util.tabs(g.indent))
	}
	// println('w len=$g.out_parallel.len')
	g.out.writeln(s)
	// g.out_parallel[g.out_idx].writeln(s)
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
