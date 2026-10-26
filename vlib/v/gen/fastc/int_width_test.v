module fastc

import os
import v.cmdexec
import v.pref

// A V `int` is 64 bits wide, and FastC spells it as the 64-bit C type wherever it declares
// one. Values that pass through a box or a slot (a multi-return value, a sum type payload,
// an option payload, an interface dispatcher) must be read back as the same C type, or the
// upper half of the value is lost.
fn test_fastc_int_keeps_64_bits_through_slots_and_boxes() {
	$if !macos && !linux {
		return
	}
	root := os.join_path(os.vtmp_dir(), 'fastc_int_width_${os.getpid()}')
	os.mkdir_all(root) or { panic(err) }
	defer { os.rmdir_all(root) or {} }
	os.write_file(os.join_path(root, 'main.v'), 'module main

type Value = int | string | bool

type Num = int | i64

interface Scaler {
	scale(x int) int
}

struct Doubler {
	k int
}

fn (d Doubler) scale(x int) int {
	return x * d.k
}

struct Holder {
	n int
}

fn (h Holder) pair() (int, string) {
	return h.n, "x"
}

fn pair() (int, int) {
	return 3000000000, -5000000000
}

fn maybe_pair(ok bool) ?(int, int) {
	if ok {
		return 4000000000, 7
	}
	return none
}

fn apply(s Scaler, x int) int {
	return s.scale(x)
}

fn describe(v Value) int {
	match v {
		int { return v + 1 }
		string { return v.len }
		bool { return 0 }
	}
}

fn narrowed(v Value) int {
	if v is int {
		w := v
		return w + 2
	}
	return 0
}

fn fallthrough(v Value) int {
	if v !is int {
		return 0
	}
	return v + 3
}

fn both(a Value, b Value) int {
	if a is int && b is int {
		return a + b
	}
	return 0
}

fn numeric(n Num) i64 {
	match n {
		int, i64 { return n }
	}
}

fn main() {
	a, b := pair()
	assert a == 3000000000
	assert b == -5000000000
	if c, d := maybe_pair(true) {
		assert c == 4000000000
		assert d == 7
	} else {
		assert false
	}
	h := Holder{
		n: 5000000000
	}
	e, f := h.pair()
	assert e == 5000000000
	assert f == "x"
	x, y := maybe_pair(true) or { 0, 0 }
	assert x == 4000000000
	assert y == 7
	z := if p, _ := maybe_pair(true) { p } else { 0 }
	assert z == 4000000000
	big := Value(5000000000)
	assert describe(big) == 5000000001
	assert narrowed(big) == 5000000002
	assert fallthrough(big) == 5000000003
	assert both(big, Value(1)) == 5000000001
	matched := match big {
		int { big + 10 }
		else { 0 }
	}
	assert matched == 5000000010
	assert numeric(Num(6000000000)) == 6000000000
	assert apply(Doubler{2}, 3000000000) == 6000000000
	m := {
		"a": 7000000000
	}
	mut flag := 0
	missing := m["zzz"] or {
		flag = 9
		8000000000
	}
	assert missing == 8000000000
	assert flag == 9
	found := m["a"] or {
		flag = 1
		0
	}
	assert found == 7000000000
	assert flag == 9
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
	build := cmdexec.run('cc', ['-std=gnu11', '-w', '-pthread', '-o', program, c_file])
	assert build.exit_code == 0, build.output
	run := cmdexec.run(program, [])
	assert run.exit_code == 0, run.output
	assert run.output.trim_space() == 'ok', run.output
}
