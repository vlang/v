module fastc

import os
import v.cmdexec
import v.pref

// Indexing a pointer that is not an auto-deref `mut` local is pointer arithmetic:
// `base[i]` on `base &string` is a `string`, not a byte, so a method call on it must
// lower to the method function rather than to a C member access on the element.
fn test_fastc_method_calls_on_indexed_pointer_elements() {
	$if !macos && !linux {
		return
	}
	root := os.join_path(os.vtmp_dir(), 'fastc_pointer_index_method_${os.getpid()}')
	os.mkdir_all(root) or { panic(err) }
	defer { os.rmdir_all(root) or {} }
	os.write_file(os.join_path(root, 'main.v'), 'module main

struct Point {
	x int
	y int
}

fn (p Point) sum(q Point) int {
	return p.x + p.y + q.x + q.y
}

fn (p &Point) psum() int {
	return p.x + p.y
}

fn point_methods(base &Point, n int) int {
	a := unsafe { base[0].sum(base[1]) }
	mut b := 0
	unsafe {
		b = base[n - 1].psum()
	}
	return a + b
}

fn string_methods(base &string, n int) string {
	first := unsafe { base[0].replace(base[1], base[2]) }
	mut last := ""
	unsafe {
		last = base[n - 1].to_upper()
	}
	return first + last + unsafe { base[1].repeat(2) }
}

fn mut_array_methods(mut names []string) string {
	return names[0].to_upper() + names[1][0].ascii_str()
}

fn main() {
	points := [Point{1, 2}, Point{3, 4}]
	assert point_methods(&points[0], 2) == 17
	words := ["abc", "b", "x"]
	assert string_methods(&words[0], 3) == "axcXbb"
	mut names := ["ab", "cd"]
	assert mut_array_methods(mut names) == "ABc"
	assert words[0][1].ascii_str() == "b"
	assert points[1].sum(points[0]) == 10
	fixed_points := [Point{5, 6}, Point{7, 8}]!
	assert fixed_points[0].sum(fixed_points[1]) == 26
	fixed_words := ["xy", "y"]!
	assert fixed_words[0].replace(fixed_words[1], "z") == "xz"
	println("ok")
}
') or {
		panic(err)
	}
	mut prefs := pref.new_preferences()
	prefs.vroot = @VEXEROOT
	prefs.building_v = true
	prefs.backend = 'fastc'
	prefs.ccompiler = 'clang'
	prefs.user_defines = ['gc_none', 'no_backtrace', 'v3_backend']
	prefs.exclude = [os.join_path(@VEXEROOT, 'vlib/builtin/backtraces_nix.c.v')]
	c_source := generate_files([root], prefs) or { panic(err) }
	for member_call in ['.plus_two(', '.sum(', '.psum(', '.replace(', '.to_upper(', '.repeat('] {
		assert !c_source.contains(member_call), member_call
	}
	c_file := os.join_path(root, 'main.c')
	program := os.join_path(root, 'program')
	os.write_file(c_file, c_source) or { panic(err) }
	build := cmdexec.run('cc', ['-std=gnu11', '-w', '-o', program, c_file])
	assert build.exit_code == 0, build.output
	run := cmdexec.run(program, [])
	assert run.exit_code == 0, run.output
	assert run.output.trim_space() == 'ok', run.output
}
