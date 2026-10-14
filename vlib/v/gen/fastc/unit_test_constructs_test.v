module fastc

import os
import v.cmdexec

// Constructs blobly_emb's unit tests use, printed against the C backend's output: a fn-typed
// field called with a method-call argument, a call through an array element, a raw fixed
// array assigned to a local, fixed arrays sized by constant arithmetic and by `$d(...)`,
// `.len` of a fixed array, chained indexes in arithmetic, `f()!.x[i].y`, struct update in an
// array literal, a field named `pub`, a generic with two type parameters, gated slices,
// a map literal assigned with `=`, and map, `.str()` and `${n:+}` interpolation.
fn test_fastc_unit_test_constructs_match_the_c_backend() {
	$if !macos && !linux {
		return
	}
	root := os.join_path(os.vtmp_dir(), 'fastc_unit_test_constructs_${os.getpid()}')
	os.mkdir_all(root) or { panic(err) }
	defer {
		os.rmdir_all(root) or {}
	}
	os.write_file(os.join_path(root, 'v.mod'), "Module { name: 'fastc_unit_test_constructs' }") or {
		panic(err)
	}
	os.write_file(os.join_path(root, 'main.v'), "module main

const base = 4
const size_a = 2 * base
const total = size_a + base + 1

type Handler = fn (x int) int

struct Sig {
	name  string
	width int
	mux   bool
}

struct Rec {
	pub  string
	sigs []Sig
}

struct Store {
mut:
	get fn (id u16, scale int) int
}

struct Unit {
mut:
	store    Store
	handlers [\$d('max_tasks', 3)]Handler
	last     [4]u8
	off      map[string]int
}

struct Srv {
mut:
	n int
}

fn (mut s Srv) handle() int {
	s.n++
	return s.n
}

struct Wire {
mut:
	sent int
}

fn (mut w Wire) send(x int) {
	w.sent += x
}

fn step[T, H](mut s T, mut w H) int {
	v := s.handle()
	w.send(v)
	return v
}

fn isig(name string, width int) Sig {
	return Sig{
		name:  name
		width: width
	}
}

fn rec(n int) !Rec {
	if n < 0 {
		return error('negative')
	}
	return Rec{
		pub:  'k\${n}'
		sigs: [Sig{
			...isig('a', 8)
			mux: true
		}, isig('b', 4)]
	}
}

fn twice(id u16, scale int) int {
	return int(id) * scale
}

fn (u &Unit) id_of(i int) u16 {
	return u16(i + 1)
}

fn double(x int) int {
	return x * 2
}

fn width_of(mut out [4]u8) int {
	return out.len
}

fn main() {
	mut u := Unit{
		store: Store{
			get: twice
		}
	}
	u.handlers[1] = double
	u.last[2] = 7
	u.off = {
		'x': 32
	}
	mut local := [4]u8{}
	local = u.last
	mut fixed := [total + 1]u8{}
	grid := [[1, 2], [3, 4]]
	mut s := Srv{}
	mut w := Wire{}
	step(mut s, mut w)
	step(mut s, mut w)
	r := rec(1)!
	m := {
		'a': 1
	}
	text := 'id_raw'
	n := -3
	println('\${u.store.get(3, u.id_of(1))} \${u.handlers[1](21)} \${local[2]} \${fixed.len} \${width_of(mut local)}')
	println('\${grid[1][0] - grid[0][1] + 1} \${rec(2)!.sigs[0].width} \${r.sigs[0].mux} \${r.pub} \${rec(3)!.sigs.len}')
	println('\${text#[..-4]} \${text#[-3..]} \${w.sent} \${m} \${n:+} \${u.off.str()} \${u.handlers.len}')
}
") or { panic(err) }
	expected := ['6 42 7 14 4', '2 8 true k1 2', "id raw 3 {'a': 1} -3 {'x': 32} 3"]
	for define in ['', 'max_tasks=5'] {
		program := os.join_path(root, 'program')
		mut args := ['-b', 'fastc']
		if define != '' {
			args << ['-d', define]
		}
		args << ['-o', program, root]
		build := cmdexec.run(@VEXE, args)
		assert build.exit_code == 0, build.output
		run := cmdexec.run(program, [])
		assert run.exit_code == 0, run.output
		mut want := expected.clone()
		if define != '' {
			want[2] = want[2].all_before_last(' ') + ' 5'
		}
		assert run.output.trim_space().split_into_lines() == want
	}
}
