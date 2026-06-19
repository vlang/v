// Copyright (c) 2020-2024 Joe Conigliaro. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
// vtest build: macos
module builder

import os
import v2.ast
import v2.parser
import v2.pref
import v2.token

fn parse_source_for_flat(src string) []ast.File {
	tmp := '/tmp/v2_flat_roundtrip_${os.getpid()}.v'
	os.write_file(tmp, src) or { panic('write_file: ${err}') }
	defer {
		os.rm(tmp) or {}
	}
	p := &pref.Preferences{}
	mut fs := token.FileSet.new()
	mut par := parser.Parser.new(p)
	return par.parse_files([tmp], mut fs)
}

fn assert_roundtrip(src string) {
	files := parse_source_for_flat(src)
	flat_a := ast.flatten_files(files)
	rt_files := flat_a.to_files()
	flat_b := ast.flatten_files(rt_files)
	sig_a := flat_a.signature()
	sig_b := flat_b.signature()
	if sig_a != sig_b {
		mut diff_at := 0
		for diff_at < sig_a.len && diff_at < sig_b.len && sig_a[diff_at] == sig_b[diff_at] {
			diff_at++
		}
		from := if diff_at > 80 { diff_at - 80 } else { 0 }
		end_a := if diff_at + 200 < sig_a.len { diff_at + 200 } else { sig_a.len }
		end_b := if diff_at + 200 < sig_b.len { diff_at + 200 } else { sig_b.len }
		eprintln('signature mismatch at offset ${diff_at}')
		eprintln('A: ${sig_a[from..end_a]}')
		eprintln('B: ${sig_b[from..end_b]}')
	}
	assert sig_a == sig_b, 'round-trip signature mismatch for source:\n${src}'
}

fn test_roundtrip_module_only() {
	assert_roundtrip('module foo\n')
}

fn test_roundtrip_simple_fn() {
	src := 'module main

fn add(a int, b int) int {
	return a + b
}
'
	assert_roundtrip(src)
}

fn test_roundtrip_struct_and_const() {
	src := "module main

pub const greeting = 'hello'

pub struct Point {
pub mut:
	x int
	y int
}

fn (p Point) sum() int {
	return p.x + p.y
}
"
	assert_roundtrip(src)
}

fn test_roundtrip_control_flow() {
	src := 'module main

fn run(n int) int {
	mut total := 0
	for i := 0; i < n; i++ {
		if i % 2 == 0 {
			total += i
		} else {
			total -= i
		}
	}
	return total
}
'
	assert_roundtrip(src)
}

fn test_roundtrip_string_inter() {
	src := "module main

fn show(name string, age int) string {
	return 'name=\${name} age=\${age:04d}'
}
"
	assert_roundtrip(src)
}

fn test_roundtrip_match_and_array() {
	src := 'module main

fn classify(x int) string {
	return match x {
		0 { "zero" }
		1, 2, 3 { "small" }
		else { "big" }
	}
}

fn sum_array() int {
	nums := [1, 2, 3, 4, 5]
	mut total := 0
	for n in nums {
		total += n
	}
	return total
}
'
	assert_roundtrip(src)
}

fn test_roundtrip_map_and_or() {
	src := "module main

fn lookup() int {
	m := {'a': 1, 'b': 2}
	v := m['a'] or { -1 }
	return v
}
"
	assert_roundtrip(src)
}

fn test_roundtrip_enum_and_attrs() {
	src := 'module main

@[flag]
pub enum Color {
	red
	green
	blue
}
'
	assert_roundtrip(src)
}

fn test_roundtrip_interface() {
	src := 'module main

pub interface Animal {
	name() string
mut:
	age int
}
'
	assert_roundtrip(src)
}

fn test_roundtrip_generic_fn() {
	src := 'module main

fn max[T](a T, b T) T {
	if a > b { return a }
	return b
}
'
	assert_roundtrip(src)
}

fn test_roundtrip_real_source_file() {
	// Round-trip a real V2 source file end-to-end. This exercises the full
	// surface of parser-produced AST variants.
	path := os.real_path('vlib/v2_toberemoved/ast/ast.v')
	if !os.exists(path) {
		return
	}
	src := os.read_file(path) or { return }
	files := parse_source_for_flat(src)
	flat_a := ast.flatten_files(files)
	rt_files := flat_a.to_files()
	flat_b := ast.flatten_files(rt_files)
	assert flat_a.signature() == flat_b.signature(), 'round-trip mismatch for ${path}'
}
