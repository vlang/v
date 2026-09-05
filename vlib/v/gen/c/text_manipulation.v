// Copyright (c) 2019-2024 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module c

import v.util

const trace_gen_wanted_value = $d('trace_gen_wanted_value', '')

// line_directive is the start of a `#line N "file"` C preprocessor directive.
const line_directive = '#line '

@[if trace_gen_wanted ?]
fn (mut g Gen) trace_gen_wanted_context(last_character_len int, s string) {
	last_n := g.out.last_n(last_character_len)
	eprintln('> trace_gen_wanted, last characters:\n${last_n}\n')
	eprintln("> trace_gen_wanted, found wanted cgen string `${trace_gen_wanted_value}` in generated string: \"${s}\"")
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

// ends_with_open_line_directive reports whether the last line of `s` is a `#line`
// directive that was not terminated by a newline.
// With `-g`, cgen routinely cuts an unfinished statement out of the output with
// go_before_last_stmt()/go_before_ternary(), trims it, and writes it back later.
// When that chunk ends with a `#line N "file"` directive, whatever is written
// next lands on the same line, and the C preprocessor swallows it as trailing
// tokens of the directive - `warning: extra tokens at end of #line directive`,
// followed by `error: expected expression` for the statement that just lost its
// value. Tracking it here terminates such a directive before the next write,
// instead of patching every one of the ~100 write back sites.
@[direct_array_access]
fn (g &Gen) ends_with_open_line_directive(s string) bool {
	if !g.pref.is_vlines || s.len == 0 || s[s.len - 1] == `\n` {
		return false
	}
	mut i := s.len - 1
	for i > 0 && s[i - 1] != `\n` {
		i--
	}
	for i < s.len && s[i] in [` `, `\t`, `\r`] {
		i++
	}
	if s.len - i < line_directive.len {
		return false
	}
	for j in 0 .. line_directive.len {
		if s[i + j] != line_directive[j] {
			return false
		}
	}
	return true
}

// terminate_open_line_directive closes a pending `#line` directive, so that the
// next generated code starts on its own line.
@[inline]
fn (mut g Gen) terminate_open_line_directive() {
	if g.vlines_pending_nl {
		g.vlines_pending_nl = false
		g.out.writeln('')
		g.empty_line = true
	}
}

@[expand_simple_interpolation]
fn (mut g Gen) write(s string) {
	g.trace_gen_wanted(s)
	g.trace_gen('write', s)
	g.terminate_open_line_directive()
	if g.indent > 0 && g.empty_line {
		g.out.write_string(util.tabs(g.indent))
	}
	g.out.write_string(s)
	g.empty_line = false
	g.vlines_pending_nl = g.ends_with_open_line_directive(s)
}

fn (mut g Gen) write2(s1 string, s2 string) {
	if g.pref.is_vlines {
		g.write(s1)
		g.write(s2)
		return
	}
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
	g.terminate_open_line_directive()
	if g.indent > 0 && g.empty_line {
		g.out.write_string(util.tabs(g.indent))
	}
	g.out.write_decimal(x)
	g.empty_line = false
}

fn (mut g Gen) writeln(s string) {
	g.trace_gen_wanted(s)
	g.trace_gen('writeln', s)
	g.terminate_open_line_directive()
	if g.indent > 0 && g.empty_line {
		g.out.write_string(util.tabs(g.indent))
		// g.out_parallel[g.out_idx].write_string(util.tabs(g.indent))
	}
	// println('w len=${g.out_parallel.len}')
	g.out.writeln(s)
	// g.out_parallel[g.out_idx].writeln(s)
	g.empty_line = true
	// g.line_nr++
}

fn (mut g Gen) writeln2(s1 string, s2 string) {
	if g.pref.is_vlines {
		g.writeln(s1)
		g.writeln(s2)
		return
	}
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
	g.vlines_pending_nl = false
	g.out.go_back(n)
	// g.out_parallel[g.out_idx].go_back(n)
}

fn (mut g Gen) go_back_to(n int) {
	g.vlines_pending_nl = false
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
	g.vlines_pending_nl = false
	return g.out.cut_to(g.nth_stmt_pos(0))
}

@[inline]
fn (mut g Gen) go_before_ternary() string {
	g.vlines_pending_nl = false
	return g.out.cut_to(g.nth_stmt_pos(g.inside_ternary))
}

fn (mut g Gen) insert_before_stmt(s string) {
	g.vlines_pending_nl = false
	cur_line := g.out.cut_to(g.nth_stmt_pos(g.inside_ternary))
	g.writeln(s)
	g.write(cur_line)
}

fn (mut g Gen) insert_at(pos int, s string) {
	g.vlines_pending_nl = false
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
