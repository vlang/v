module fastc

import os
import v.cmdexec

// These constructs come from Vox and the vlib modules it imports. The C backend
// prints the same `ok`.
fn test_fastc_vox_constructs_match_the_c_backend() {
	$if !macos && !linux {
		return
	}
	root := os.join_path(os.vtmp_dir(), 'fastc_vox_constructs_${os.getpid()}')
	os.mkdir_all(os.join_path(root, 'm')) or { panic(err) }
	defer { os.rmdir_all(root) or {} }
	os.write_file(os.join_path(root, 'm', 'bridge.h'), '#define FASTC_BRIDGE_VALUE 7\n') or { panic(err) }
	os.write_file(os.join_path(root, 'm', 'm.v'), r'module m

@[json_as_number]
pub enum Level {
	low
	high
}

pub enum Color {
	none
	red   @[json: "Red"]
	green
}
') or { panic(err) }
	os.write_file(os.join_path(root, 'm', 'm.c.v'), r'module m

#preinclude "@DIR/bridge.h"

pub fn bridge_value() int {
	return C.FASTC_BRIDGE_VALUE
}
') or { panic(err) }
	os.write_file(os.join_path(root, 'main.v'), r'module main

import json2
import m

const enabled = "a" == "b"
	|| "c" == "c"

__global wake_count = 0
__global wake_ = fn () {
	wake_count++
}

enum Key {
	accept
	cache_control
}

struct Length {
	px f64
}

fn (l Length) is_absolute() bool {
	return l.px > 0
}

struct LengthValue {
	length_ Length
}

struct OtherValue {
	name string
}

type StyleValue = LengthValue | OtherValue

struct Box {
	value StyleValue
}

struct Rgb {
	r    []int
	name ?string
}

struct Kw {
	k int
}

type Value = Kw | Rgb

struct Holder {
	value Value
}

struct State {
mut:
	rerr ?IError
}

fn absolute(box &Box) ?f64 {
	if box.value !is LengthValue || !box.value.length_.is_absolute() {
		return none
	}
	return box.value.length_.px * 2
}

fn same(a &Holder, b &Holder) bool {
	return match a.value {
		Kw {
			if b.value is Kw { a.value.k == b.value.k } else { false }
		}
		Rgb {
			if b.value is Rgb { a.value == b.value } else { false }
		}
	}
}

fn headers(values map[Key]string) map[Key]string {
	return values
}

fn walk(mut out []int, values []int) {
	defer {
		for value in values {
			if value < 0 {
				break
			}
			out << value
		}
	}
}

fn main() {
	assert enabled
	wake_()
	wake_()
	assert wake_count == 2
	assert m.bridge_value() == 7
	assert json2.encode(m.Color.red) == "\"Red\""
	assert json2.encode(m.Color.green) == "\"green\""
	assert json2.encode(m.Level.high) == "1"
	color := m.Color.from_string("green") or { return }
	assert color == .green
	missing := m.Color.from_string("blue") or { m.Color.none }
	assert missing == .none
	counts := map[string]int{
		"a": 1
		"b": 2
	}
	assert counts["b"] == 2
	assert json2.encode(map[string]int{
		"x": 3
	}) == "{\"x\":3}"
	selected := headers({
		.accept:        "text/plain"
		.cache_control: "no-cache"
	})
	assert selected[.accept] == "text/plain"
	assert selected[.cache_control] == "no-cache"
	assert absolute(&Box{LengthValue{Length{3}}}) or { -1 } == 6
	assert absolute(&Box{OtherValue{"x"}}) or { -1 } == -1
	rgb := &Holder{Rgb{[1, 2], "n"}}
	assert same(rgb, &Holder{Rgb{[1, 2], "n"}})
	assert !same(rgb, &Holder{Rgb{[1, 3], "n"}})
	assert !same(rgb, &Holder{Kw{1}})
	mut out := []int{}
	walk(mut out, [1, 2, -1, 3])
	assert out == [1, 2]
	mut state := State{}
	assert state.rerr == none
	state.rerr = error("closed")
	if err := state.rerr {
		assert err.msg() == "closed"
	} else {
		assert false
	}
	println("ok")
}
') or { panic(err) }
	program := os.join_path(root, 'program')
	build := cmdexec.run(@VEXE, ['-b', 'fastc', '-nocache', '-enable-globals', '-o', program, root])
	assert build.exit_code == 0, build.output
	run := cmdexec.run(program, [])
	assert run.exit_code == 0, run.output
	assert run.output.trim_space() == 'ok'
}

// fastc_vox_construct_program builds the directory program `source` with FastC and
// checks that it prints `ok`.
fn fastc_vox_construct_program(name string, source string) {
	$if !macos && !linux {
		return
	}
	root := os.join_path(os.vtmp_dir(), 'fastc_vox_construct_${name}_${os.getpid()}')
	os.mkdir_all(root) or { panic(err) }
	defer { os.rmdir_all(root) or {} }
	os.write_file(os.join_path(root, 'main.v'), source) or { panic(err) }
	program := os.join_path(root, 'program')
	build := cmdexec.run(@VEXE, ['-b', 'fastc', '-nocache', '-enable-globals', '-o', program, root])
	assert build.exit_code == 0, build.output
	run := cmdexec.run(program, [])
	assert run.exit_code == 0, run.output
	assert run.output.trim_space() == 'ok'
}

// A member narrowed past `if x.f !is T { return }` is read again after the condition,
// so a path with a side effect, such as a call or an `if` in an index, is not narrowed:
// it runs once, as with the C backend.
fn test_fastc_vox_constructs_fallthrough_subject_is_evaluated_once() {
	fastc_vox_construct_program('fallthrough', r'module main

__global count = 0

struct First {
	x int
}

struct Second {
	x int
}

type Value = First | Second

struct Holder {
	value Value
}

fn next_holder() Holder {
	count++
	return Holder{First{1}}
}

fn next_index() int {
	count++
	return 0
}

fn main() {
	if next_holder().value !is First {
		return
	}
	holders := [Holder{First{2}}]
	if holders[next_index()].value !is First {
		return
	}
	assert count == 2
	if holders[if true {
		count++
		0
	} else {
		0
	}].value !is First {
		return
	}
	assert count == 3
	plain := Holder{First{3}}
	if plain.value !is First {
		return
	}
	assert plain.value.x == 3
	println("ok")
}
')
}

// A whole-string enum attribute is its decoded text without an argument, also when it
// contains a colon. Quoted text is decoded with V's escapes, including `\u`, `\U` and
// line continuations, as with the C backend.
fn test_fastc_vox_constructs_enum_attributes_are_decoded() {
	fastc_vox_construct_program('attributes', r'module main

@["baz"]
@["a: b"]
@[key: "v\x41"]
@[flag]
@["\u0062\U00000061z\$"]
@["c\
   d"]
enum Level {
	low
	high
}

fn main() {
	mut attributes := []string{}
	$for attr in Level.attributes {
		attributes << "${attr.name}|${attr.arg}|${attr.has_arg}"
	}
	assert attributes == ["baz||false", "a: b||false", "key|vA|true", "flag||false", "baz$||false",
		"cd||false"]
	println("ok")
}
')
}

// Collections as Vox's JPEG XL decoder spells them: an interface cast of an indexed
// operand, fixed arrays of dynamic arrays and the reverse, an option cast of a fixed array,
// an option cast that starts the line after a call, and a slice of a `mut` array parameter.
fn test_fastc_vox_constructs_collection_casts_and_initializers() {
	fastc_vox_construct_program('collections', r'module main

interface Shape {
	area() int
}

struct Square {
	side int
}

fn (s Square) area() int {
	return s.side * s.side
}

struct Info {
	spot ?[4]f32
	rows [3][]i32
}

struct Predictor {
mut:
	errors [][4]u32
}

struct Size {
	width  int
	height int
}

struct Frame {
	side int
}

fn (f Frame) size() Size {
	return Size{f.side, f.side * 2}
}

struct Cache {
	cells int
}

struct Holder {
	cache ?Cache
}

struct Channel {
	buffer int
}

fn shapes(squares []Square) []Shape {
	mut values := []Shape{cap: squares.len}
	for index in 0 .. squares.len {
		values << Shape(squares[index])
	}
	return values
}

fn info(kind int) Info {
	mut rows := [3][]i32{}
	for c in 0 .. 3 {
		mut row := []i32{len: c + 1}
		row[0] = i32(c * 5)
		rows[c] = row
	}
	mut spot := ?[4]f32(none)
	if kind == 2 {
		mut values := [4]f32{}
		for i in 0 .. 4 {
			values[i] = f32(i) * 0.5
		}
		spot = values
	}
	return Info{
		spot: spot
		rows: rows
	}
}

fn make_cache(width int, height int) !Cache {
	if width < 0 {
		return error("negative")
	}
	return Cache{width * height}
}

fn holder(frame Frame, curves ?int) !Holder {
	cache := if extra := curves {
		size := frame.size()
		// The comment keeps the option cast on a line of its own.
		?Cache(make_cache(size.width + extra, size.height)!)
	} else {
		?Cache(none)
	}
	return Holder{
		cache: cache
	}
}

fn buffers(mut channels []Channel, begin int, count int) []int {
	outputs := channels[begin..begin + count].map(it.buffer)
	channels << Channel{99}
	return outputs
}

fn main() {
	values := shapes([Square{2}, Square{3}])
	assert values.len == 2
	assert values[1].area() == 9
	with_spot := info(2)
	if spot := with_spot.spot {
		assert spot[3] == 1.5
	} else {
		assert false
	}
	assert with_spot.rows[2].len == 3
	assert with_spot.rows[1][0] == 5
	assert info(0).spot == none
	mut predictor := Predictor{
		errors: [][4]u32{len: 4}
	}
	predictor.errors[2][1] = 7
	mut local := [][4]u32{len: 2}
	local[1][3] = 9
	assert predictor.errors.len == 4
	assert predictor.errors[2][1] + predictor.errors[0][0] + local[1][3] == 16
	filled := holder(Frame{3}, ?int(1)) or { panic(err) }
	if cache := filled.cache {
		assert cache.cells == 24
	} else {
		assert false
	}
	empty := holder(Frame{3}, ?int(none)) or { panic(err) }
	assert empty.cache == none
	mut channels := [Channel{1}, Channel{3}, Channel{5}, Channel{7}]
	assert buffers(mut channels, 1, 2) == [3, 5]
	assert channels.len == 5
	println("ok")
}
')
}

// Match values as Vox's layout and CSS code spell them: a nested match whose subject holds
// an `unsafe` block, and arms that yield the values of an optional multi-return whose
// first component is an interface.
fn test_fastc_vox_constructs_match_values() {
	fastc_vox_construct_program('match_values', r'module main

enum Mode {
	loose
	normal
}

enum Language {
	japanese
	other
}

enum Break {
	loose_japanese
	loose
	normal
}

struct Tagged {
	tag string
}

fn language(node &Tagged) Language {
	return if node.tag == "ja" { Language.japanese } else { Language.other }
}

fn line_break(mode Mode, node Tagged) Break {
	return match mode {
		.loose {
			match language(unsafe { &Tagged(&node) }) {
				.japanese { Break.loose_japanese }
				.other { Break.loose }
			}
		}
		.normal {
			Break.normal
		}
	}
}

interface Value {
	text() string
}

struct Keyword {
	name string
}

fn (k Keyword) text() string {
	return "keyword ${k.name}"
}

struct Length {
	px int
}

fn (l Length) text() string {
	return "${l.px}px"
}

struct Leaf {
	px int
}

struct Sum {
	terms int
}

type Node = Leaf | Sum

enum Kind {
	keyword
	length
	prepared
}

fn node(value Value) ?Node {
	if value is Length {
		return Leaf{value.px}
	}
	return none
}

fn prepared(value Value) ?(Value, Node) {
	leaf := node(value) or { return none }
	return value, leaf
}

fn operand(value Value, kind Kind) ?(Value, Node) {
	return match kind {
		.keyword { value, Node(Sum{1}) }
		.length { Value(Keyword{"any"}), node(value)? }
		.prepared { prepared(value)? }
	}
}

fn describe(value Value, kind Kind) string {
	first, second := operand(value, kind) or { return "none" }
	return match second {
		Leaf { "${first.text()} leaf ${second.px}" }
		Sum { "${first.text()} sum ${second.terms}" }
	}
}

fn main() {
	assert line_break(.loose, Tagged{"ja"}) == .loose_japanese
	assert line_break(.loose, Tagged{"en"}) == .loose
	assert line_break(.normal, Tagged{"ja"}) == .normal
	assert describe(Length{3}, .keyword) == "3px sum 1"
	assert describe(Length{4}, .length) == "keyword any leaf 4"
	assert describe(Length{5}, .prepared) == "5px leaf 5"
	assert describe(Keyword{"x"}, .length) == "none"
	println("ok")
}
')
}
