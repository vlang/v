module fastc

import os
import v.cmdexec
import v.pref

const member_smartcast_value_source = r'module main

enum Kind {
	one
	two
}

type Val = Kind | f64 | i64 | int | string | u8

struct Box {
	v Val
}

fn if_stmt(b Box) int {
	if b.v is int {
		return b.v + 4
	}
	return 0
}

fn if_expr(b Box) int {
	return if b.v is int { b.v * 2 } else { 0 }
}

fn conjunction(b Box) bool {
	return b.v is int && b.v + 1 > 3
}

fn if_conjunction(b Box) int {
	if b.v is int && b.v - 1 > 1 {
		return b.v - 1
	}
	return 0
}

fn match_stmt(b Box) int {
	match b.v {
		int {
			return b.v + 100
		}
		f64 {
			return int(b.v * 2)
		}
		u8, i64 {
			return int(b.v) + 50
		}
		else {}
	}
	return 0
}

fn match_expr(b Box) int {
	return match b.v {
		int { b.v + 1000 }
		u8 { int(b.v) + 1 }
		else { 0 }
	}
}

fn copied(b Box) int {
	if b.v is int {
		mut y := b.v
		y += 2
		y++
		return y + b.v
	}
	return 0
}

fn float_sum(b Box) f64 {
	if b.v is f64 {
		return b.v + 0.5
	}
	return 0
}

fn enum_value(b Box) int {
	if b.v is Kind {
		k := b.v
		if k == .two {
			return 2
		}
		return 1
	}
	return 0
}

fn string_len(b Box) int {
	if b.v is string {
		return b.v.len + 1
	}
	return 0
}

fn interpolated(b Box) string {
	if b.v is int {
		return "v=${b.v + 1}"
	}
	return ""
}

fn main() {
	b := Box{
		v: 3
	}
	println(if_stmt(b))
	println(if_expr(b))
	println(conjunction(b))
	println(if_conjunction(b))
	println(match_stmt(b))
	println(match_expr(b))
	println(copied(b))
	println(interpolated(b))
	println(if_stmt(b))
	println(float_sum(Box{ v: 1.25 }))
	println(match_stmt(Box{ v: 1.5 }))
	println(match_stmt(Box{ v: u8(7) }))
	println(match_expr(Box{ v: u8(7) }))
	println(enum_value(Box{ v: Kind.two }))
	println(string_len(Box{ v: "abc" }))
}
'

// A member narrowed to a number, bool, rune or enum variant (`if b.v is int`) reads as the
// value behind the payload pointer: arithmetic and declarations use the value, rather than
// doing pointer arithmetic (`*(__v1+4)`) or binding the pointer.
fn test_fastc_scalar_member_smartcasts_read_values() {
	$if !macos && !linux {
		return
	}
	root := os.join_path(os.vtmp_dir(), 'fastc_member_smartcast_value_${os.getpid()}')
	os.mkdir_all(root) or { panic(err) }
	defer { os.rmdir_all(root) or {} }
	os.write_file(os.join_path(root, 'main.v'), member_smartcast_value_source) or { panic(err) }
	mut prefs := pref.new_preferences()
	prefs.vroot = @VEXEROOT
	prefs.building_v = true
	prefs.backend = 'fastc'
	prefs.ccompiler = 'clang'
	prefs.user_defines = ['gc_none', 'no_backtrace', 'v3_backend']
	prefs.exclude = [os.join_path(@VEXEROOT, 'vlib/builtin/backtraces_nix.c.v')]
	c_source := generate_files([root], prefs) or { panic(err) }
	c_file := os.join_path(root, 'main.c')
	program := os.join_path(root, 'program')
	os.write_file(c_file, c_source) or { panic(err) }
	build := cmdexec.run('cc', ['-std=gnu11', '-w', '-pthread', '-o', program, c_file])
	assert build.exit_code == 0, build.output
	run := cmdexec.run(program, [])
	assert run.exit_code == 0, run.output
	assert run.output.trim_space().split_into_lines() == ['7', '6', 'true', '2', '103', '1003',
		'9', 'v=4', '7', '1.75', '3', '57', '8', '2', '4'], run.output
}
