// A private fn that its own module calls only in a `$if` branch this build does
// not take is not unused. Two dotted imports that share the short name `util`
// make the loader rename a module declaration to its import path after parsing,
// which used to lose the record of that call, so the fn was reported as unused
// and `-N` rejected the build.
//
// This is not a checker/tests/modules fixture: those run as `-prod run` with the
// module cache on, which does not show notices from imported modules.
import os

const vexe = @VEXE

fn util_source(m string) string {
	return 'module util

fn used_only_in_skipped_branch_${m}() int {
	return 1
}

fn unused_${m}() int {
	return 2
}

pub fn value() int {
	\$if never_defined ? {
		return used_only_in_skipped_branch_${m}()
	}
	return 3
}
'
}

fn test_skipped_comptime_use_survives_module_canonicalization() {
	dir := os.join_path(os.vtmp_dir(), 'v3_unused_fn_skipped_comptime_same_short_module_${os.getpid()}')
	os.rmdir_all(dir) or {}
	defer {
		os.rmdir_all(dir) or {}
	}
	for m in ['xa', 'xb'] {
		os.mkdir_all(os.join_path(dir, m, 'util'))!
		os.write_file(os.join_path(dir, m, 'util', 'util.v'), util_source(m))!
	}
	os.write_file(os.join_path(dir, 'main.v'), 'module main

import xa.util as ua
import xb.util as ub

fn main() {
	println(ua.value() + ub.value())
}
')!
	out := os.join_path(dir, 'app.exe')
	// `-new-compiler` keeps a V 0.5.2 fallback retry from answering for V3.
	res := os.execute('${os.quoted_path(vexe)} -new-compiler -nocache -o ${os.quoted_path(out)} ${os.quoted_path(dir)}')
	assert res.exit_code == 0, res.output
	for m in ['xa', 'xb'] {
		assert res.output.contains('unused function: `unused_${m}`'), res.output
		assert !res.output.contains('unused function: `used_only_in_skipped_branch_${m}`'), res.output
	}
}
