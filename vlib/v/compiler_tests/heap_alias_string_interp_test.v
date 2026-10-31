// A local moved to the heap is read as its value, but string interpolation took the
// `&Alias` of its storage for a pointer still to be dereferenced, and dereferenced the
// value again: `'${x}'` became `UUID__str(**x)`. For a fixed array that read through
// its first bytes and crashed; for a `@[heap]` struct the C did not compile.
// `println(x)` and `x.str()` already read it once.
import os

const vexe = @VEXE

const heap_program = 'type UUID = [4]u8

@[heap]
struct Node {
	v int
}

type Alias = Node

fn (u UUID) str() string {
	return "uuid\${u[0]}"
}

fn (a Alias) str() string {
	return "alias\${a.v}"
}

fn fill(mut b []u8) {
	b[0] = 7
}

fn main() {
	mut x := UUID{}
	fill(mut x[..])
	println(x)
	println("\${x}")
	println("[\${x}|\${x}]")
	println("\${x:6}|")
	mut a := Alias{
		v: 3
	}
	println("\${a}")
	println("<\${a}>")
}
'

fn test_interpolating_a_heap_alias_reads_it_once() {
	dir := os.join_path(os.vtmp_dir(), 'v3_heap_alias_interp_${os.getpid()}')
	os.rmdir_all(dir) or {}
	os.mkdir_all(dir) or { panic(err) }
	defer {
		os.rmdir_all(dir) or {}
	}
	src := os.join_path(dir, 'main.v')
	os.write_file(src, heap_program) or { panic(err) }
	exe := os.join_path(dir, 'main.exe')
	build := os.exec([vexe, '-new-compiler', '-o', exe, src])
	assert build.exit_code == 0, build.output
	run := os.exec([exe])
	assert run.exit_code == 0, run.output
	assert run.output.split_into_lines() == ['uuid7', 'uuid7', '[uuid7|uuid7]', ' uuid7|', 'alias3',
		'<alias3>']
}
