module fastc

import os
import v.cmdexec

// Constructs blobly_emb's code generators use, each printed against the C backend's output:
// `'0x' + s[0..2]`, a fixed-array alias, a `mut` fixed-array parameter, a sum-type subject
// narrowed earlier in a condition (`e is string && e !in xs`, `e is i64 && e != 4`), an
// omitted option field, and a map type only a literal spells.
fn test_fastc_code_generator_constructs_match_the_c_backend() {
	$if !macos && !linux {
		return
	}
	root := os.join_path(os.vtmp_dir(), 'fastc_generator_constructs_${os.getpid()}')
	os.mkdir_all(root) or { panic(err) }
	defer {
		os.rmdir_all(root) or {}
	}
	os.write_file(os.join_path(root, 'v.mod'), "Module { name: 'fastc_generator_constructs' }") or {
		panic(err)
	}
	os.write_file(os.join_path(root, 'main.v'), "module main

type Gf = [4]i64

type Val = i64 | string

struct Opt {
	name string
	v    ?int
}

fn fill(mut out [4]u8) {
	for i in 0 .. 4 {
		out[i] = u8(i * 2)
	}
	out[3] += out[1]
}

fn main() {
	s := 'abcdef'
	println('0x' + s[0..2])
	g := Gf([4]i64{})
	mut h := g
	h[1] = 7
	println(h[1] + g[0])
	mut b := [4]u8{}
	fill(mut b)
	println(b[3])
	o := Opt{
		name: 'x'
	}
	println(o.v == none)
	vals := [Val(i64(3)), Val('q'), Val('z')]
	names := ['q', 'r']
	mut hits := 0
	for e in vals {
		if e is string && e !in names {
			hits += 10
		}
		if e is string && e in names {
			hits += 1
		}
		if e is i64 && e != 4 {
			hits += 100
		}
	}
	println(hits)
	m := {
		'k': ['a', 'b']
	}
	println(m['k'].len)
}
") or { panic(err) }
	program := os.join_path(root, 'program')
	build := cmdexec.run(@VEXE, ['-b', 'fastc', '-o', program, root])
	assert build.exit_code == 0, build.output
	run := cmdexec.run(program, [])
	assert run.exit_code == 0, run.output
	assert run.output.trim_space().split_into_lines() == ['0xab', '7', '8', 'true', '111', '2']
}
