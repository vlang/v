import os

// Regression test for the `x = f(mut x)` use-after-free.
//
// When a call takes `x` by mutable reference, mutates its owned (heap) storage
// in place, and returns (a copy of) `x`, the assignment `x = f(mut x)` must NOT
// drop `x` before the assignment: the returned value aliases `x`'s heap buffer,
// so dropping first frees storage the result still points at. This reproduced as
// corrupted array contents (e.g. `2043 0 3 4` instead of `1 2 3 4`) and, in the
// ripgrep port's literal extractor (`seq = ex.cross(seq, ...)`), as a crash.

const vexe = @VEXE
const tests_dir = os.dir(@FILE)
const v3_dir = os.dir(tests_dir)
const vlib_dir = os.dir(v3_dir)
const v3_src = os.join_path(v3_dir, 'v3.v')

fn self_reassign_build_v3() string {
	v3_bin := os.join_path(os.temp_dir(), 'v3_self_reassign_call_codegen_test_${os.getpid()}')
	os.rm(v3_bin) or {}
	build :=
		os.execute('${vexe} -gc none -path "${vlib_dir}|@vlib|@vmodules" -o ${v3_bin} ${v3_src}')
	assert build.exit_code == 0, build.output
	return v3_bin
}

fn self_reassign_run_good(v3_bin string, name string, source string) string {
	src := os.join_path(os.temp_dir(), 'v3_${name}_${os.getpid()}.v')
	os.write_file(src, source) or { panic(err) }
	bin := os.join_path(os.temp_dir(), 'v3_${name}_${os.getpid()}')
	compile := os.execute('${v3_bin} ${src} -b c -o ${bin}')
	assert compile.exit_code == 0, compile.output
	assert !compile.output.contains('C compilation failed'), compile.output
	run := os.execute(bin)
	assert run.exit_code == 0, run.output
	return run.output.trim_space()
}

// `acc = combine(mut acc, mut other)` where `combine` returns its mutated-by-ref
// first argument. The heap array field must survive intact.
fn test_self_reassign_via_mut_ref_call_preserves_owned_field() {
	v3_bin := self_reassign_build_v3()
	out := self_reassign_run_good(v3_bin, 'self_reassign_bag', 'struct Bag implements IClone {
mut:
	items []int
}

fn (b &Bag) clone() Bag {
	return Bag{ items: b.items.clone() }
}

fn combine(mut a Bag, mut b Bag) Bag {
	for x in b.items {
		a.items << x
	}
	return a
}

fn main() {
	mut acc := Bag{ items: [1, 2] }
	mut other := Bag{ items: [3, 4] }
	acc = combine(mut acc, mut other)
	assert acc.items.len == 4
	assert acc.items[0] == 1
	assert acc.items[1] == 2
	assert acc.items[2] == 3
	assert acc.items[3] == 4
	println("ok")
}
')
	assert out == 'ok'
}

// Nested-owned variant closer to the extractor: a struct holds an array of
// structs that each own a `[]u8`. `seq = grow(mut seq, ...)` must not free the
// inner buffers early.
fn test_self_reassign_preserves_nested_owned_arrays() {
	v3_bin := self_reassign_build_v3()
	out := self_reassign_run_good(v3_bin, 'self_reassign_nested', 'struct Lit implements IClone {
mut:
	bytes []u8
}

fn (l &Lit) clone() Lit {
	return Lit{ bytes: l.bytes.clone() }
}

struct Seq implements IClone {
mut:
	lits []Lit
}

fn (s &Seq) clone() Seq {
	mut out := []Lit{cap: s.lits.len}
	for l in s.lits {
		out << l.clone()
	}
	return Seq{ lits: out }
}

fn grow(mut s Seq, b u8) Seq {
	mut fresh := []u8{}
	fresh << b
	s.lits << Lit{ bytes: fresh }
	return s
}

fn main() {
	mut seq := Seq{ lits: [] }
	seq = grow(mut seq, `a`)
	seq = grow(mut seq, `b`)
	seq = grow(mut seq, `c`)
	assert seq.lits.len == 3
	assert seq.lits[0].bytes[0] == `a`
	assert seq.lits[1].bytes[0] == `b`
	assert seq.lits[2].bytes[0] == `c`
	println("ok")
}
')
	assert out == 'ok'
}
