module fastc

import os
import v.cmdexec
import v.pref

// A compound assignment on a map entry (`m[k] += v`, `m[k].x *= 2`) reads the entry,
// or the value type's zero when the key is absent, applies the operator and stores the
// result back. A map read is not a C lvalue, so it must not be emitted as the target of
// a C compound assignment (`({ ...map read... })+=v`).
fn test_fastc_compound_assignment_on_map_entries() {
	$if !macos && !linux {
		return
	}
	root := os.join_path(os.vtmp_dir(), 'fastc_map_compound_assignment_${os.getpid()}')
	os.mkdir_all(root) or { panic(err) }
	defer { os.rmdir_all(root) or {} }
	os.write_file(os.join_path(root, 'main.v'), 'module main

struct Point {
mut:
	x    int
	y    int
	name string
}

struct Counter {
mut:
	counts map[string]int
}

enum Color {
	red
	green
}

struct Vec {
	x int
	y int
}

fn (a Vec) + (b Vec) Vec {
	return Vec{a.x + b.x, a.y + b.y}
}

type Name = string

fn add(mut m map[string]int, key string, value int) {
	m[key] += value
	m[key] *= 2
}

fn next_key(mut calls int) string {
	calls++
	return "k" + calls.str()
}

fn maybe(ok bool) ?int {
	if ok {
		return 4
	}
	return none
}

fn add_maybe(mut m map[string]int, mut p map[string]Point, ok bool) {
	m["r"] += maybe(ok) or { return }
	p["r"].x -= maybe(ok) or { return }
	p["r"].y = maybe(ok) or { return }
}

fn main() {
	mut m := map[string]int{}
	m["a"] += 5
	m["a"] -= 2
	m["a"] *= 4
	m["a"] /= 3
	m["a"] %= 3
	assert m["a"] == 1
	m["b"] |= 6
	m["b"] &= 3
	m["b"] ^= 1
	m["b"] <<= 3
	m["b"] >>= 1
	assert m["b"] == 12
	m["c"] += m["b"] + m["a"]
	assert m["c"] == 13
	assert m.len == 3
	add(mut m, "d", 4)
	add(mut m, "d", 1)
	assert m["d"] == 18
	mut calls := 0
	m[next_key(mut calls)] += 7
	assert calls == 1
	assert m["k1"] == 7
	mut s := map[string]string{}
	s["k"] += "hello"
	s["k"] += " world"
	assert s["k"] == "hello world"
	mut f := map[int]f64{}
	f[1] += 1.5
	f[1] *= 2.0
	assert f[1] == 3.0
	mut vectors := map[string]Vec{}
	vectors["v"] += Vec{1, 2}
	vectors["v"] += Vec{3, 4}
	assert vectors["v"].x == 4 && vectors["v"].y == 6
	mut names := map[int]Name{}
	names[1] += "ab"
	names[1] += "c"
	assert names[1] == "abc"
	mut big := map[string]u64{}
	big["x"] += 9
	big["x"] >>>= 1
	assert big["x"] == 4
	mut colors := map[Color]u8{}
	colors[.green] += 200
	colors[.green] += 100
	assert colors[.green] == 44
	mut c := Counter{}
	c.counts["x"] += 3
	c.counts["x"] -= 1
	assert c.counts["x"] == 2
	mut nested := map[string]map[string]int{}
	nested["o"]["i"] += 9
	nested["o"]["i"] -= 1
	assert nested["o"]["i"] == 8
	mut p := map[string]Point{}
	p["z"] = Point{
		x: 1
	}
	p["z"].x += 7
	p["z"].y -= 2
	p["w"].x *= 5
	p["w"].x += 3
	p["z"].name += "ab"
	p["z"].name += "c"
	assert p["z"].x == 8
	assert p["z"].y == -2
	assert p["z"].name == "abc"
	assert p["w"].x == 3
	m["o"] += maybe(true) or { 1 }
	m["o"] += maybe(false) or { 10 }
	assert m["o"] == 14
	add_maybe(mut m, mut p, true)
	add_maybe(mut m, mut p, false)
	assert m["r"] == 4
	assert p["r"].x == -4
	assert p["r"].y == 4
	println("ok")
}
') or {
		panic(err)
	}
	mut prefs := pref.new_preferences()
	prefs.vroot = @VEXEROOT
	prefs.building_v = true
	prefs.backend = 'fastc'
	prefs.ccompiler = 'clang'
	prefs.user_defines = ['gc_none', 'no_backtrace', 'v3_backend']
	prefs.exclude = [os.join_path(@VEXEROOT, 'vlib/builtin/backtraces_nix.c.v')]
	c_source := generate_files([root], prefs) or { panic(err) }
	for rvalue_target in ['})+=', '})-=', '})*=', '})|=', '})<<=', '})=builtin__string_plus', '}).x+=',
		'}).x-=', '}).x*=', '}).y-=', '}).y=({', '}).name=builtin__string_plus'] {
		assert !c_source.contains(rvalue_target), rvalue_target
	}
	c_file := os.join_path(root, 'main.c')
	program := os.join_path(root, 'program')
	os.write_file(c_file, c_source) or { panic(err) }
	build := cmdexec.run('cc', ['-std=gnu11', '-w', '-o', program, c_file])
	assert build.exit_code == 0, build.output
	run := cmdexec.run(program, [])
	assert run.exit_code == 0, run.output
	assert run.output.trim_space() == 'ok', run.output
}
