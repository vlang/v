module parser

import os
import flat
import pref

const verifier_src = r'module main

import os

const answer = 42

struct Point {
	x int
	y int
}

enum Color {
	red
	green
}

type MyInt = int

fn (p Point) sum() int {
	return p.x + p.y
}

fn generic_id[T](v T) T {
	return v
}

fn main() {
	mut xs := [1, 2, 3]
	xs << 4
	m := {"a": 1, "b": 2}
	total := xs.map(|it| it * 2).filter(it > 2)
	c := Color.red
	p := Point{x: 1, y: 2}
	s := "hi ${p.sum()} ${answer}"
	arr := [5]int{}
	fixed := [1, 2, 3]!
	inferred := [..]int[1, 2, 3]
	inferred2 := [..][..]int[[1], [2, 3]]
	sl := xs[1..3]
	sl2 := xs[..2]
	sl3 := xs[1..]
	sl4 := xs[..]
	init := []int{len: 3, init: 0}
	if total.len > 0 && c == .red {
		println(s)
	} else {
		println("no")
	}
	for i, v in xs {
		println("${i}:${v}")
	}
	for i := 0; i < 3; i++ {
		println(i)
	}
	y := if answer > 0 { 1 } else { 2 }
	match c {
		.red { println("r") }
		.green { println("g") }
	}
	ptr := &p
	unsafe {
		println(ptr.x)
	}
	r := generic_id[int](7)
	_ = m
	_ = arr
	_ = fixed
	_ = inferred
	_ = inferred2
	_ = sl
	_ = sl2
	_ = sl3
	_ = sl4
	_ = init
	_ = y
	_ = r
}
'

struct SpanReport {
mut:
	valid        int
	total        int
	invalid      map[string]int
	out_of_range map[string]int
}

fn verify_one(name string, src string, mut rep SpanReport) {
	path := os.join_path(os.temp_dir(), 'v3_span_verify_${name}_${os.getpid()}.v')
	os.write_file(path, src) or { panic(err) }
	mut p := Parser.new(pref.new_preferences())
	ast := p.parse_file(path)
	os.rm(path) or {}
	for node in ast.nodes {
		if node.kind == .empty {
			continue
		}
		rep.total++
		if !node.pos.is_valid() {
			rep.invalid['${node.kind}']++
			continue
		}
		if node.pos.offset < 0 || node.pos.end > src.len || node.pos.end < node.pos.offset {
			rep.out_of_range['${node.kind}']++
			continue
		}
		rep.valid++
	}
}

// Every non-synthetic parsed node must carry a valid file id and an in-range,
// half-open span. This guards against new parser productions that build nodes
// directly on the flat AST without a position (which used to yield zero spans).
fn test_all_parsed_nodes_have_valid_spans() {
	mut rep := SpanReport{
		invalid:      map[string]int{}
		out_of_range: map[string]int{}
	}
	verify_one('synthetic', verifier_src, mut rep)
	vroot := os.dir(os.dir(os.dir(os.dir(@FILE))))
	real_files := [
		os.join_path(vroot, 'vlib', 'v3', 'parser', 'parser.v'),
		os.join_path(vroot, 'vlib', 'v3', 'types', 'checker.v'),
		os.join_path(vroot, 'vlib', 'v3', 'gen', 'c', 'cleanc.v'),
		os.join_path(vroot, 'vlib', 'v3', 'flat', 'flat.v'),
		os.join_path(vroot, 'vlib', 'v3', 'token', 'token.v'),
		os.join_path(vroot, 'vlib', 'builtin', 'array.v'),
		os.join_path(vroot, 'vlib', 'builtin', 'string.v'),
	]
	for f in real_files {
		src := os.read_file(f) or { continue }
		verify_one(os.base(f).replace('.', '_'), src, mut rep)
	}
	println('span verifier: ${rep.valid}/${rep.total} nodes have valid in-range spans')
	assert rep.invalid.len == 0, 'nodes with zero/invalid spans by kind: ${rep.invalid}'
	assert rep.out_of_range.len == 0, 'nodes with out-of-range spans by kind: ${rep.out_of_range}'
	assert rep.valid == rep.total
}
