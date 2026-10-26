module fastc

import os
import v.cmdexec
import v.pref

const generic_multi_return_util_source = r'module util

pub fn pair[T](x T) (T, T) {
	return x, x + x
}

pub fn mixed[T](x T) (T, string, f64) {
	return x, "v${x}", 1.5
}

pub struct Maker {
pub:
	base int
}

pub fn (m Maker) two[T](x T) (T, int) {
	return x, m.base
}
'

const generic_multi_return_main_source = r'module main

import util

struct LocalMaker {
	k int
}

fn (m LocalMaker) three[T](x T) (T, int, string) {
	return x, m.k, "lm"
}

fn local_pair[T](x T) (T, int) {
	return x, 7
}

fn main() {
	a, b := util.pair[int](1)
	println(a + b)
	c, d := util.pair[f64](1.25)
	println(c + d)
	e, s, f := util.mixed[i64](9)
	println("${e} ${s.len} ${s} ${f}")
	g, h := local_pair[u8](3)
	println(int(g) + h)
	mut i, mut j := util.pair[int](5)
	i++
	j++
	println(i + j)
	m := util.Maker{
		base: 4
	}
	p, q := m.two[f64](0.5)
	println("${p} ${q}")
	lm := LocalMaker{
		k: 6
	}
	r, t, u := lm.three[u16](2)
	println("${r} ${t} ${u}")
}
'

// The locals of `a, b := mod.f[T](x)`, an explicit generic call into another module, are
// declared as the specialized function's return types. FastC used to find no types for the
// call, which keeps its type arguments, and declared every local as `uint`.
fn test_fastc_explicit_generic_multi_return_call_declares_component_types() {
	$if !macos && !linux {
		return
	}
	root := os.join_path(os.vtmp_dir(), 'fastc_generic_multi_return_call_${os.getpid()}')
	os.mkdir_all(os.join_path(root, 'util')) or { panic(err) }
	defer { os.rmdir_all(root) or {} }
	os.write_file(os.join_path(root, 'v.mod'), "Module {\n\tname: 'app'\n}\n") or { panic(err) }
	os.write_file(os.join_path(root, 'util', 'util.v'), generic_multi_return_util_source) or {
		panic(err)
	}
	os.write_file(os.join_path(root, 'main.v'), generic_multi_return_main_source) or {
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
	assert run.output.trim_space().split_into_lines() == ['3', '3.75', '9 2 v9 1.5', '10', '17',
		'0.5 4', '2 6 lm'], run.output
}
