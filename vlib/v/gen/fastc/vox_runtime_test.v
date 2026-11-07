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
