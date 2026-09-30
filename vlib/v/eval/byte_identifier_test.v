module eval

import os
import v.parser

fn test_eval_match_byte_constant_uses_its_value() {
	mut e := create()
	e.run_text('
const byte = 8

fn classify(value int) string {
	return match value {
		byte { "matched" }
		else { "other" }
	}
}

fn main() {
	value := 9
	match value {
		byte { println("matched") }
		else { println("other") }
	}
	println(classify(8))
	println(classify(9))
}
') or { panic(err) }
	assert e.stdout() == 'other\nmatched\nother\n'
}

fn test_eval_sizeof_byte_constant_uses_its_declared_width() {
	mut e := create()
	e.run_text('
const byte = u8(1)

fn main() {
	println(sizeof(byte))
	println(sizeof(u8))
	println(sizeof(i32))
}
') or { panic(err) }
	assert e.stdout() == '1\n1\n4\n'
}

fn test_eval_sizeof_byte_local_uses_its_declared_width() {
	mut e := create()
	e.run_text('
fn main() {
	byte := i32(0)
	println(sizeof(byte))
}
') or { panic(err) }
	assert e.stdout() == '4\n'
}

fn test_eval_sizeof_byte_constant_ignores_caller_local_types() {
	mut e := create()
	e.run_text('
const narrow = u8(1)
const byte = narrow

fn main() {
	narrow := i32(0)
	println(sizeof(byte))
	println(sizeof(narrow))
}
') or { panic(err) }
	assert e.stdout() == '1\n4\n'
}

fn test_eval_sizeof_byte_alias_uses_its_underlying_width() {
	mut e := create()
	e.run_text('
type Small = u16
type Narrow = Small
const byte = Narrow(1)

fn main() {
	println(sizeof(byte))
	local := Narrow(0)
	println(sizeof(local))
	println(sizeof(Narrow))
}
') or { panic(err) }
	assert e.stdout() == '2\n2\n2\n'
}

fn test_eval_sizeof_byte_call_does_not_evaluate_the_constant() {
	mut e := create()
	e.run_text('
const byte = side_effect()

fn side_effect() u16 {
	println("evaluated")
	return u16(1)
}

fn main() {
	side_effect := fn () u8 { return u8(0) }
	println(sizeof(byte))
}
') or { panic(err) }
	assert e.stdout() == '2\n'
}

fn test_eval_sizeof_byte_local_call_uses_the_return_width() {
	mut e := create()
	e.run_text('
fn make_byte() u8 {
	println("initialized")
	return u8(1)
}

fn main() {
	byte := make_byte()
	println(sizeof(byte))
}
') or { panic(err) }
	assert e.stdout() == 'initialized\n1\n'
}

fn test_eval_sizeof_byte_fixed_arrays_use_recursive_widths() {
	mut e := create()
	e.run_text('
type Small = u16
type Row = [3]Small
type Matrix = [2]Row
const count = 1 + 2
type Counted = [count]Small
const byte = [3]u8{}

fn main() {
	println(sizeof(byte))
	nested := [2][3]u16{}
	println(sizeof(nested))
	println(sizeof(Matrix))
	println(sizeof([0]u8))
	println(sizeof(Counted))
	println(sizeof([3]&u8))
}
') or { panic(err) }
	assert e.stdout() == '3\n12\n12\n0\n6\n24\n'
}

fn test_eval_sizeof_byte_method_and_static_calls_use_return_widths() {
	mut e := create()
	e.run_text('
struct Maker {}
const byte = Maker.build()

fn (maker Maker) make() u8 {
	println("method")
	return u8(1)
}

fn Maker.build() u16 {
	println("static")
	return u16(1)
}

fn print_constant_width() {
	println(sizeof(byte))
}

fn main() {
	maker := Maker{}
	byte := maker.make()
	println(sizeof(byte))
	local := Maker.build()
	println(sizeof(local))
	print_constant_width()
}
') or { panic(err) }
	assert e.stdout() == 'method\n1\nstatic\n2\n2\n'
}

fn test_eval_sizeof_byte_function_value_call_uses_return_width() {
	mut e := create()
	e.run_text('
fn main() {
	make := fn () u8 {
		println("initialized")
		return u8(1)
	}
	byte := make()
	println(sizeof(byte))
}
') or { panic(err) }
	assert e.stdout() == 'initialized\n1\n'
}

fn test_eval_sizeof_byte_fixed_array_initializer_is_not_evaluated() {
	mut e := create()
	e.run_text('
const count = 1 + 2
const byte = [count]u8{init: side_effect()}

fn side_effect() u8 {
	println("evaluated")
	return u8(1)
}

fn main() {
	println(sizeof(byte))
}
') or { panic(err) }
	assert e.stdout() == '3\n'
}

fn test_eval_sizeof_byte_method_receiver_is_not_evaluated() {
	mut e := create()
	e.run_text('
struct Maker {}
const byte = make_maker().make()

fn make_maker() Maker {
	println("receiver")
	return Maker{}
}

fn (maker Maker) make() u8 {
	println("method")
	return u8(1)
}

fn main() {
	println(sizeof(byte))
}
') or { panic(err) }
	assert e.stdout() == '1\n'
}

fn test_eval_sizeof_byte_global_receiver_uses_the_method_signature() {
	mut e := create()
	e.run_text('
struct Maker {}
__global maker Maker

fn (maker Maker) make() u8 {
	println("method")
	return u8(1)
}

fn main() {
	byte := maker.make()
	println(sizeof(byte))
}
') or { panic(err) }
	assert e.stdout() == 'method\n1\n'
}

fn test_eval_sizeof_byte_qualified_calls_preserve_module_types() {
	dir := os.join_path(os.vtmp_dir(), 'eval_sizeof_qualified_${os.getpid()}')
	os.mkdir_all(dir) or { panic(err) }
	defer {
		os.rmdir_all(dir) or {}
	}
	module_file := os.join_path(dir, 'maker.v')
	main_file := os.join_path(dir, 'main.v')
	os.write_file(module_file, '
module maker

pub type Octet = u8
const count = 1 + 2
pub type Row = [count]Octet
pub struct Maker {}

pub fn Maker.build() Octet {
	println("static")
	return Octet(1)
}

pub fn make() Octet {
	println("qualified")
	return Octet(1)
}

pub fn rows() [2]Row {
	return [2]Row{}
}

pub fn counted() [count]Octet {
	return [count]Octet{}
}
') or { panic(err) }
	os.write_file(main_file, '
module main

import maker as m

const byte = m.Maker.build()

fn main() {
	println(sizeof(byte))
	local := m.make()
	println(sizeof(local))
	nested := m.rows()
	println(sizeof(nested))
	counted := m.counted()
	println(sizeof(counted))
}
') or { panic(err) }
	mut e := create()
	mut p := parser.Parser.new(&e.prefs)
	p.parse_files([module_file, main_file])
	e.run_files(p.a) or { panic(err) }
	assert e.stdout() == '1\nqualified\n1\n6\n3\n'
}
