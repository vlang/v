module fastc

import os
import v.cmdexec

// An unused function containing a capturing closure is pruned: the closure's lowering keeps the
// enclosing function's C name, and the closure prototype no longer lives in its span.
fn test_fastc_prunes_unused_function_with_closure() {
	$if !macos && !linux {
		return
	}
	root := os.join_path(os.vtmp_dir(), 'fastc_prunes_unused_function_with_closure_${os.getpid()}')
	os.mkdir_all(os.join_path(root, 'xev')) or { panic(err) }
	defer {
		os.rmdir_all(root) or {}
	}
	os.write_file(os.join_path(root, 'xev', 'xev.v'), "module xev

fn C.__fastc_missing_clock(id u32) u16

pub struct Timer {
	n int
}

fn apply(f fn (u64) u64, n u64) u64 {
	return f(n)
}

// run is never called; its native call has no prototype, so it must be pruned together with
// the closure inside it.
pub fn (t &Timer) run(n u64) u64 {
	if C.__fastc_missing_clock(1) != 0 {
		panic('x')
	}
	k := t.n
	return apply(fn [k] (x u64) u64 {
		return x + u64(k)
	}, n)
}

pub fn used() int {
	k := 2
	return int(apply(fn [k] (x u64) u64 {
		return x * u64(k)
	}, 3))
}
") or { panic(err) }
	os.write_file(os.join_path(root, 'main.v'), 'module main

import xev

fn main() {
	println(xev.used())
}
') or { panic(err) }
	program := os.join_path(root, 'program')
	build := cmdexec.run_in_merged_with_env(@VEXE, ['-b', 'fastc', '-nocache', '-gc', 'none', '-o',
		program, root], os.real_path(os.join_path(@DIR, '../../../..')), {
		'VEXE':                   @VEXE
		'VFLAGS':                 ''
		'VOSARGS':                ''
		'V_MACOS_V3_NO_FALLBACK': '1'
	})
	assert build.exit_code == 0, build.output
	run := cmdexec.run(program, [])
	assert run.exit_code == 0, run.output
	assert run.output.trim_space() == '6', run.output
}
