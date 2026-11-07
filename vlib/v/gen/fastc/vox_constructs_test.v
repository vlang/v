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
