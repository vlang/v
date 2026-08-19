module v

import os
import v3.parser
import v3.pref
import v3.flat

// parse_source parses `src` on its own and returns the flat AST plus the id of
// its trailing `.file` node (the one carrying the top-level declarations).
fn parse_source(name string, src string) (&flat.FlatAst, flat.NodeId) {
	path := os.join_path(os.temp_dir(), 'v3_vfmt_${name}_${os.getpid()}.v')
	os.write_file(path, src) or { panic(err) }
	mut p := parser.Parser.new(pref.new_preferences())
	a := p.parse_file(path)
	os.rm(path) or {}
	mut fid := flat.empty_node
	for id in a.file_node_ids {
		n := a.nodes[id]
		if n.kind == .file && n.children_count > 0 {
			fid = flat.NodeId(id)
		}
	}
	return a, fid
}

// vfmt formats `src` and returns the generated V source.
fn vfmt(name string, src string) string {
	a, fid := parse_source(name, src)
	return format_file(a, fid)
}

// reparse_diagnostics reports how many parse diagnostics `src` produces.
fn reparse_diagnostics(name string, src string) int {
	path := os.join_path(os.temp_dir(), 'v3_vfmt_rp_${name}_${os.getpid()}.v')
	os.write_file(path, src) or { panic(err) }
	mut p := parser.Parser.new(pref.new_preferences())
	p.parse_file(path)
	os.rm(path) or {}
	return p.diagnostics.len
}

fn test_fn_and_method() {
	out := vfmt('method', 'module m\npub fn (mut p Point) inc(dx int) int {\n\tp.n += dx\n\treturn p.n\n}\n')
	assert out == 'module m

pub fn (mut p Point) inc(dx int) int {
	p.n += dx
	return p.n
}
'
}

fn test_struct_access_sections() {
	out := vfmt('struct', 'struct Point {\n\tid int\npub mut:\n\tx int\n\ty int = 3\n}\n')
	assert out == 'struct Point {
	id int
pub mut:
	x int
	y int = 3
}
'
}

fn test_attributes_and_pub() {
	out := vfmt('attrs', '@[heap]\npub struct Foo {\n\tx int\n}\n')
	assert out.starts_with('@[heap]\npub struct Foo {')
}

fn test_enum_and_types() {
	out := vfmt('enum', 'pub enum Color as u8 {\n\tred = 1\n\tgreen\n}\n\ntype MyInt = int\ntype Sum = Foo | Bar\n')
	assert out.contains('enum Color as u8 {')
	assert out.contains('red = 1')
	assert out.contains('type MyInt = int')
	assert out.contains('type Sum = Foo | Bar')
}

fn test_control_flow() {
	out := vfmt('flow', 'fn f(xs []int) {
	for i, x in xs {
		if x > 0 {
			println(x)
		} else {
			continue
		}
	}
	for j := 0; j < 3; j++ {
	}
}
')
	assert out.contains('for i, x in xs {')
	// C-style loop var must not gain a spurious `mut`
	assert out.contains('for j := 0; j < 3; j++ {')
	assert !out.contains('for mut j := 0')
	assert out.contains('} else {')
}

fn test_match() {
	out := vfmt('match', 'fn f(x int) string {\n\treturn match x {\n\t\t1, 2 { "a" }\n\t\telse { "b" }\n\t}\n}\n')
	assert out.contains('match x {')
	assert out.contains('1, 2 {')
	assert out.contains('else {')
}

fn test_string_escaping() {
	// a literal `$` must be escaped so it is not read back as interpolation
	out := vfmt('stresc', "fn f() {\n\ta := 'price: \$5'\n\tb := '\${x}y'\n}\n")
	assert out.contains("'price: \\\$5'")
	assert out.contains("'\${x}y'")
}

fn test_channel_ops() {
	out := vfmt('chan', 'fn f() {\n\tx := <-ch\n\tch <- 5\n}\n')
	assert out.contains('x := <-ch')
	assert out.contains('ch <- 5')
	assert !out.contains('->')
}

fn test_comptime_selector() {
	out := vfmt('ctsel', 'fn f[T](owned T) {\n\tdrop_owned(owned.\$(field.name))\n}\n')
	assert out.contains('owned.\$(field.name)')
}

fn test_select_compound_receive_preserves_operator() {
	// A compound receive (`x += <-ch`) must keep its operator; emitting `=` would
	// silently change program semantics.
	out := vfmt('selcompound', 'fn f(ch chan int) {
	mut x := 0
	select {
		x += <-ch {
			println(x)
		}
	}
}
')
	assert out.contains('x += <-ch'), out
	assert !out.contains('x = <-ch'), out
}

fn test_select_receive_forms() {
	out := vfmt('selforms', 'fn f(ch chan int) {
	mut x := 0
	select {
		y := <-ch {
			println(y)
		}
		x = <-ch {
			println(x)
		}
	}
}
')
	assert out.contains('y := <-ch'), out
	assert out.contains('x = <-ch'), out
}

fn test_generics_and_interface() {
	out := vfmt('gen', 'pub struct Stack[T] {\nmut:\n\tdata []T\n}\n\ninterface Reader {\n\tread(mut buf []u8) !int\nmut:\n\tpos int\n}\n')
	assert out.contains('struct Stack[T] {')
	assert out.contains('interface Reader {')
	assert out.contains('read(mut buf []u8) !int')
}

fn test_output_is_valid_and_idempotent() {
	src := 'module main

import os { join_path }

@[heap]
pub struct Node[T] {
pub mut:
	value T
	next  &Node[T] = unsafe { nil }
}

pub fn (mut n Node[T]) push(v T) {
	names := ["a", "b"]
	m := {"k": 1}
	for i, name in names {
		println("\${i}: \${name}")
	}
	x := if v == n.value { 1 } else { 2 }
	r := os.join_path("a", "b") or { panic(err) }
	match x {
		1, 2 { println("low") }
		else {}
	}
	assert x > 0, "positive"
}
'
	out1 := vfmt('rt', src)
	// the generated source must itself parse without diagnostics
	assert reparse_diagnostics('rt_valid', out1) == 0
	// and formatting is a fixed point
	out2 := vfmt('rt2', out1)
	assert out1 == out2
}
