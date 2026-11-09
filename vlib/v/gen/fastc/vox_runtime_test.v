module fastc

import os
import v.cmdexec

// fastc_run_program builds the directory program `files` with FastC and returns its output.
fn fastc_run_program(name string, files map[string]string) string {
	root := os.join_path(os.vtmp_dir(), 'fastc_${name}_${os.getpid()}')
	os.mkdir_all(root) or { panic(err) }
	defer { os.rmdir_all(root) or {} }
	for file, text in files {
		os.write_file(os.join_path(root, file), text) or { panic(err) }
	}
	program := os.join_path(root, 'program')
	build := cmdexec.run(@VEXE, ['-b', 'fastc', '-nocache', '-o', program, root])
	assert build.exit_code == 0, build.output
	run := cmdexec.run(program, [])
	assert run.exit_code == 0, run.output
	return run.output.trim_space()
}

// A directory program runs on FastC's full runtime. It keeps the program's collector,
// as with the C backend; Vox frees its own pages with `GC_free`. HTTP/2's HPACK table
// is a constant built by a call that reads a constant of another file.
fn test_fastc_directory_program_runs_like_the_c_backend() {
	$if !macos && !linux {
		return
	}
	output := fastc_run_program('vox_runtime', {
		'main.v':       r'module main

import json2

$if gcboehm ? {
	@[c_extern]
	fn C.GC_free(voidptr)
}

enum Mode {
	light
	dark @[json: "Dark"]
}

struct Inner {
	x int
	y ?string
}

struct Settings {
	name   string
	mode   Mode
	tags   []string
	inner  Inner
	opt    ?Inner
	counts map[string]int
}

type Shape = Inner | int | string

struct Page {
	values [64]int
}

fn new_page() &Page {
	return &Page{}
}

fn release(page &Page) {
	$if gcboehm ? {
		C.GC_free(page)
	} $else {
		unsafe { free(page) }
	}
}

fn payload[T](value ?T) ?&T {
	snapshot := value or { return none }
	return &snapshot
}

fn clobber(n int) int {
	mut values := [64]int{}
	for i in 0 .. 64 {
		values[i] = n * i
	}
	return values[63]
}

fn scale(value f64) int {
	if value >= f64(2147483647) {
		return 2147483647
	}
	if value <= f64(-2147483648) {
		return -2147483648
	}
	return int(value)
}

// table is built by a call that reads `lengths`, a constant of a later file.
const table = build_table()

fn build_table() []int {
	return scaled(2)
}

fn scaled(factor int) []int {
	return lengths.map(it * factor)
}

struct BigPayload {
mut:
	data [4900]u8
	tag  int
}

struct SmallPayload {
	a string
	b string
	c string
	d string
}

// The copy of `small` lies past the copy of `big` in the outgoing arguments, more
// than 4096 bytes from the stack pointer.
fn take_payload(small SmallPayload, big BigPayload) int {
	return small.a.len + big.tag + big.data[10]
}

fn work(id int) int {
	mut total := 0
	for _ in 0 .. 20 {
		mut items := []&Inner{}
		for i in 0 .. 500 {
			items << &Inner{
				x: i
				y: "v${i}"
			}
		}
		for item in items {
			total += item.x + (item.y or { "" }).len
		}
	}
	return total + id
}

fn main() {
	$if prealloc {
		assert false
	}
	assert table == [2, 4, 6]
	mut big := BigPayload{
		tag: 40
	}
	big.data[10] = 2
	assert take_payload(SmallPayload{ a: "abc" }, big) == 45
	release(new_page())
	coordinates := payload(?Inner(Inner{ x: 7 })) or { panic("none") }
	clobber(3)
	assert coordinates.x == 7
	assert scale(16 * 65536) == 1048576
	assert scale(-3e9) == -2147483648
	negative := -5
	assert negative < 3000000000
	mut threads := []thread int{}
	for i in 0 .. 4 {
		threads << spawn work(i)
	}
	assert threads.wait() == [work(0), work(1), work(2), work(3)]
	settings := Settings{
		name:   "vox"
		mode:   .dark
		tags:   ["a", "b"]
		inner:  Inner{3, "why"}
		counts: {
			"k": 2
		}
	}
	encoded := json2.encode(settings)
	assert encoded == "{\"name\":\"vox\",\"mode\":\"Dark\",\"tags\":[\"a\",\"b\"],\"inner\":{\"x\":3,\"y\":\"why\"},\"counts\":{\"k\":2}}"
	decoded := json2.decode[Settings](encoded) or { panic(err) }
	assert decoded.mode == .dark
	assert decoded.inner.y or { "" } == "why"
	assert decoded.opt == none
	assert decoded.counts["k"] == 2
	text := "{\"a\":[1,2.5,\"x\",null,true],\"b\":{\"c\":\"d\"}}"
	values := json2.decode[map[string]json2.Any](text) or { panic(err) }
	assert json2.encode(values) == text
	any := json2.decode[json2.Any](text) or { panic(err) }
	assert json2.encode(any) == text
	shapes := [Shape(Inner{1, none}), Shape(2), Shape("three")]
	assert json2.encode(shapes) == "[{\"x\":1,\"_type\":\"Inner\"},2,\"three\"]"
	println("ok")
}
'
		'zz_lengths.v': 'module main

const lengths = [1, 2, 3]
'
	})
	assert output == 'ok'
}

// TinyCC does not lay out arm64 stack arguments as Apple's ABI does. A native call
// with more arguments than registers goes through the bridge's argument adapter.
fn test_fastc_native_call_passes_stack_arguments() {
	$if !macos {
		return
	}
	output := fastc_run_program('native_stack_arguments', {
		'native.m': '#include <stdio.h>
int sig(const float *data, int count, int flag, float a0, float a1, float a2, float a3, float a4,
	float a5, float a6, float a7, float a8, float a9, int stroke, float width, int cap, int join,
	float b0, float b1, float b2, float b3, int spread, const float *stops, int stop_count,
	float opacity) {
	printf("%d %d %d %d %d %d %d %g %g %g %g %g %d %d\\n", count, flag, stroke, cap, join,
		spread, stop_count, a0, a9, width, b3, opacity, (int)data[0], (int)stops[0]);
	return count + stop_count;
}
'
		'main.v':   r'module main

#include "@DIR/native.m"

fn C.sig(data &f32, count int, flag int, a0 f32, a1 f32, a2 f32, a3 f32, a4 f32, a5 f32, a6 f32, a7 f32, a8 f32, a9 f32, stroke int, width f32, cap int, join int, b0 f32, b1 f32, b2 f32, b3 f32, spread int, stops &f32, stop_count int, opacity f32) int

fn main() {
	data := [f32(7)]
	stops := [f32(42)]
	result := C.sig(&data[0], 3, 1, 1.5, 2, 3, 4, 5, 6, 7, 8, 9, 10.5, 11, 12.5, 13, 14, 15, 16, 17, 18.5, 19, &stops[0], 20, 0.75)
	assert result == 23
}
'
	})
	assert output == '3 1 11 13 14 19 20 1.5 10.5 12.5 18.5 0.75 7 42'
}
