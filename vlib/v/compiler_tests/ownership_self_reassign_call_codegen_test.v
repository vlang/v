import os
import v.cmdexec
import v.tempname

// Regression test for the `x = f(mut x)` use-after-free.
//
// When a call takes `x` by mutable reference, mutates its owned (heap) storage
// in place, and returns (a copy of) `x`, the assignment `x = f(mut x)` must NOT
// drop `x` before the assignment: the returned value aliases `x`'s heap buffer,
// so dropping first frees storage the result still points at. This reproduced as
// corrupted array contents (e.g. `2043 0 3 4` instead of `1 2 3 4`) and, in the
// ripgrep port's literal extractor (`seq = ex.cross(seq, ...)`), as a crash.

fn test_self_reassign_preserves_owned_storage() {
	shared := os.getenv('V3_TEST_UNIT_SHARED_V3_OWNERSHIP')
	v3_src := os.join_path(os.dir(@FILE), '../v.v')
	compiler := if os.is_file(shared) && os.is_executable(shared)
		&& os.real_path(os.getenv('V3_TEST_UNIT_V3_SRC')) == os.real_path(v3_src) {
		os.real_path(shared)
	} else {
		@VEXE
	}
	root := os.join_path(os.vtmp_dir(), 'v3_self_reassign_${tempname.unique_token()}')
	os.mkdir(root) or { panic(err) }
	defer { os.rmdir_all(root) or {} }
	src := os.join_path(root, 'v3_self_reassign.v')
	os.write_file(src, 'struct Bag implements IClone {
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

fn check_bag() {
	mut acc := Bag{ items: [1, 2] }
	mut other := Bag{ items: [3, 4] }
	acc = combine(mut acc, mut other)
	assert acc.items.len == 4
	assert acc.items[0] == 1
	assert acc.items[1] == 2
	assert acc.items[2] == 3
	assert acc.items[3] == 4
	println("bag")
}

struct Lit implements IClone {
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

fn check_nested() {
	mut seq := Seq{ lits: [] }
	seq = grow(mut seq, `a`)
	seq = grow(mut seq, `b`)
	seq = grow(mut seq, `c`)
	assert seq.lits.len == 3
	assert seq.lits[0].bytes[0] == `a`
	assert seq.lits[1].bytes[0] == `b`
	assert seq.lits[2].bytes[0] == `c`
	println("nested")
}

fn main() {
	check_bag()
	check_nested()
}
') or { panic(err) }
	bin := os.join_path(root, 'v3_self_reassign')
	compile := cmdexec.run_in_merged_with_env(compiler, ['-ownership', '-d', 'ownership', '-b',
		'c', '-o', bin, src], '', {
		'VFLAGS':               ''
		'VTOOLS_CACHE_VERBOSE': ''
	})
	assert compile.exit_code == 0, compile.output
	assert !compile.output.contains('C compilation failed'), compile.output
	run := os.exec([bin])
	assert run.exit_code == 0, run.output
	assert run.output.trim_space().split_into_lines() == ['bag', 'nested'], run.output
}
