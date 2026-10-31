module fastc

import os
import v.cmdexec
import v.pref

// A map behind a type alias (`type Counts = map[string]int`) is still a map: `Counts{}`
// makes an empty map, indexing reads, writes or updates an entry, and `in`, `.len`,
// `.delete()`, `.keys()` and `or` reads use the map runtime, the same as for the map type
// itself. C has no `[]` on the map struct. Methods declared on the alias still apply.
fn test_fastc_map_operations_through_a_type_alias() {
	$if !macos && !linux {
		return
	}
	root := os.join_path(os.vtmp_dir(), 'fastc_map_alias_${os.getpid()}')
	os.mkdir_all(root) or { panic(err) }
	defer { os.rmdir_all(root) or {} }
	os.write_file(os.join_path(root, 'main.v'), 'module main

type Counts = map[string]int

type Grid = map[int]map[string]f64

type Board = Counts

struct Point {
mut:
	x int
	y int
}

type Points = map[string]Point

struct Tally {
mut:
	counts Counts
}

fn bump(mut c Counts, key string) {
	c[key] += 10
	c[key]++
}

fn total(c Counts) int {
	mut sum := 0
	for _, v in c {
		sum += v
	}
	return sum
}

fn (c Counts) sum() int {
	return total(c)
}

fn lookup(c Counts, key string) int {
	return c[key] or { -1 }
}

fn main() {
	mut c := Counts{}
	c["a"] = 1
	assert c["a"] == 1
	assert c["missing"] == 0
	c["a"] += 2
	c["a"] *= 3
	c["a"] -= 1
	assert c["a"] == 8
	c["b"]++
	c["b"]++
	c["b"]--
	assert c["b"] == 1
	assert "a" in c
	assert "z" !in c
	assert c.len == 2
	v := c["zz"] or { -1 }
	assert v == -1
	w := c["a"] or { -1 }
	assert w == 8
	assert lookup(c, "a") == 8
	assert lookup(c, "q") == -1
	bump(mut c, "a")
	bump(mut c, "n")
	assert c["a"] == 19
	assert c["n"] == 11
	assert total(c) == 31
	assert c.sum() == 31
	assert c.keys().len == 3
	c.delete("b")
	assert c.len == 2
	assert "b" !in c
	mut b := Board{}
	b["x"] += 4
	assert b["x"] == 4
	assert b.len == 1
	mut g := Grid{}
	g[1]["p"] += 1.5
	g[1]["p"] += 1.0
	assert g[1]["p"] == 2.5
	assert g.len == 1
	mut p := Points{}
	p["o"] = Point{
		x: 1
	}
	p["o"].x += 4
	p["o"].y = 7
	assert p["o"].x == 5
	assert p["o"].y == 7
	mut t := Tally{}
	t.counts["k"] += 2
	t.counts["k"] += 3
	assert t.counts["k"] == 5
	assert t.counts.len == 1
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
	c_file := os.join_path(root, 'main.c')
	program := os.join_path(root, 'program')
	os.write_file(c_file, c_source) or { panic(err) }
	build := cmdexec.run('cc', ['-std=gnu11', '-w', '-o', program, c_file])
	assert build.exit_code == 0, build.output
	run := cmdexec.run(program, [])
	assert run.exit_code == 0, run.output
	assert run.output.trim_space() == 'ok', run.output
}
