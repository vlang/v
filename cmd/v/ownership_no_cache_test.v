// Copyright (c) 2019-2026 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module main

import os
import v.cmdexec

// Regression test for https://github.com/vlang/v/issues/30677: `VTOOLS_NO_CACHE=1` was ignored by
// `-ownership`/`-autofree`, which still required a usable tool cache for the ownership compiler,
// unlike every other tool that `v` builds on demand.

fn test_ownership_compilations_honour_vtools_no_cache() {
	root := os.join_path(os.vtmp_dir(), 'v_ownership_no_cache_${os.getpid()}')
	os.rmdir_all(root) or {}
	os.mkdir_all(root)!
	os.chmod(root, 0o700)!
	defer {
		os.chmod(os.join_path(root, 'shared'), 0o700) or {}
		os.rmdir_all(root) or {}
	}
	// Both default tool cache folders are below a folder that everybody can write to, so the
	// cache cannot be used at all, as on the host from the issue.
	shared := os.join_path(root, 'shared')
	xdg := os.join_path(shared, 'xdg')
	vtmp := os.join_path(shared, 'vtmp')
	os.mkdir(shared)!
	os.chmod(shared, 0o777)!
	os.mkdir(xdg, mode: 0o700)!
	os.mkdir(vtmp, mode: 0o700)!
	moved := os.join_path(root, 'moved.v')
	// Only the ownership checker rejects this program, so its diagnostic shows that the
	// ownership compiler was built and ran.
	os.write_file(moved, 'fn consume(values []int) int {\n\treturn values.len\n}\n\nfn main() {\n\tvalues := [1, 2, 3]\n\tprintln(consume(values))\n\tprintln(values.len)\n}\n')!
	clean := os.join_path(root, 'clean.v')
	os.write_file(clean, 'fn main() {\n\tvalues := [1, 2, 3]\n\tprintln(values.len)\n}\n')!
	environment := {
		'XDG_CACHE_HOME':       xdg
		'VTMP':                 vtmp
		'VFLAGS':               ''
		tool_cache_dir_env:     ''
		tool_cache_disable_env: '1'
		tool_cache_verbose_env: ''
	}
	rejected := cmdexec.run_in_merged_with_env(@VEXE, ['-ownership', '-check', moved], root,
		environment)
	assert rejected.exit_code == 1, rejected.output
	assert rejected.output.contains('use of moved value: `values`'), rejected.output
	assert !rejected.output.contains('cannot find a writable cache'), rejected.output
	accepted := cmdexec.run_in_merged_with_env(@VEXE, ['-ownership', '-check', clean], root,
		environment)
	assert accepted.exit_code == 0, accepted.output
	assert !accepted.output.contains('cannot find a writable cache'), accepted.output
	// The compiler is built for the one compilation and removed again, and the tool cache is
	// not touched.
	leftovers := (os.ls(vtmp) or { [] }).filter(it.starts_with(tool_cache_stage_prefix))
	assert leftovers == [], 'the temporary ownership compiler was left in `${vtmp}`'
	assert !os.exists(os.join_path(xdg, 'v')), 'the tool cache must not be prepared'
	assert !os.exists(os.join_path(vtmp, 'tools')), 'the tool cache must not be prepared'
}
