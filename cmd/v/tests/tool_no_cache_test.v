// Copyright (c) 2019-2026 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module main

import os
import time
import v.cmdexec

const uncached_tool_stage_prefix = '.v-toolcache-stage-'

fn test_uncached_directory_tools_have_private_executables() ! {
	root := os.join_path(os.vtmp_dir(), 'v_uncached_tool_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	tmp := os.join_path(root, 'tmp')
	os.mkdir_all(tmp)!
	// Only the dispatcher uses this small source tree; the compiler keeps its baked vlib.
	os.mkdir_all(os.join_path(root, 'vlib', 'v'))!
	os.write_file(os.join_path(root, 'GNUmakefile'), '')!
	exe_suffix := $if windows { '.exe' } $else { '' }
	launcher := os.join_path(root, 'v' + exe_suffix)
	os.cp(@VEXE, launcher)!
	os.chmod(launcher, 0o755)!
	environment := {
		'VTMP':                   tmp
		'VFLAGS':                 '-gc none -d uncached_tool_parent'
		'VTOOLS_NO_CACHE':        '1'
		'V_MACOS_V3_NO_FALLBACK': '1'
	}
	source := os.join_path(root, 'cmd', 'tools', 'vvet')
	os.mkdir_all(source)!
	os.write_file(os.join_path(source, 'main.v'), 'import os\nimport time\n\nfn main() {\n\tos.write_file(os.args[2], os.executable() + "\\n" + os.getenv("VFLAGS"))!\n\tfor _ in 0 .. 6000 {\n\t\tif os.exists(os.args[3]) { exit(os.args[4].int()) }\n\t\ttime.sleep(10 * time.millisecond)\n\t}\n\texit(99)\n}\n')!
	// A directory input used to replace this shared output even with VTOOLS_NO_CACHE.
	shared := os.join_path(source, 'vvet' + exe_suffix)
	os.write_file(shared, 'existing shared tool')!
	first_ready := os.join_path(root, 'first')
	second_ready := os.join_path(root, 'second')
	release := os.join_path(root, 'release')
	first := spawn cmdexec.run_in_merged_with_env(launcher, ['vet', first_ready, release, '0'], root,
		environment)
	second := spawn cmdexec.run_in_merged_with_env(launcher, ['vet', second_ready, release, '7'], root,
		environment)
	mut overlap := false
	for _ in 0 .. 6000 {
		if os.is_file(first_ready) && os.is_file(second_ready) {
			overlap = true
			break
		}
		time.sleep(10 * time.millisecond)
	}
	// Release both processes before assertions so failures cannot leave waiting children.
	os.write_file(release, '')!
	first_result := first.wait()
	second_result := second.wait()
	assert overlap, 'both tool processes must run at the same time: ${first_result.output} ${second_result.output}'
	assert first_result.exit_code == 0, first_result.output
	assert second_result.exit_code == 7, second_result.output
	first_lines := os.read_file(first_ready)!.split_into_lines()
	second_lines := os.read_file(second_ready)!.split_into_lines()
	assert first_lines.len == 2 && second_lines.len == 2
	assert first_lines[1] == '-gc none -d uncached_tool_parent'
	assert second_lines[1] == '-gc none -d uncached_tool_parent'
	assert first_lines[0] != second_lines[0]
	for executable in [first_lines[0], second_lines[0]] {
		assert os.dir(os.dir(executable)) == os.real_path(tmp), executable
		assert !os.exists(os.dir(executable)), 'temporary tool was not removed: ${executable}'
	}
	assert os.read_file(shared)! == 'existing shared tool'
	assert (os.ls(tmp) or { [] }).filter(it.starts_with(uncached_tool_stage_prefix)) == []
	// Checking or emitting C must not suppress the private executable build. Like
	// cached tool launches, uncached launches still execute the requested command.
	for mode in ['-check', '-check-syntax', '-c'] {
		ready := os.join_path(root, mode.trim_left('-'))
		result := cmdexec.run_in_merged_with_env(launcher, [mode, 'vet', ready, release, '0'],
			root, environment)
		assert result.exit_code == 0, result.output
		assert os.is_file(ready), 'the requested tool did not run with ${mode}'
		executable := os.read_file(ready)!.all_before('\n')
		assert os.dir(os.dir(executable)) == os.real_path(tmp), executable
		assert !os.exists(os.dir(executable)), 'temporary tool was not removed: ${executable}'
		assert os.read_file(shared)! == 'existing shared tool'
		assert (os.ls(tmp) or { [] }).filter(it.starts_with(uncached_tool_stage_prefix)) == []
	}
	// A failed build must remove its private directory too.
	os.write_file(os.join_path(source, 'main.v'), 'fn main() { missing_function() }\n')!
	failed := cmdexec.run_in_merged_with_env(launcher, ['vet'], root, environment)
	assert failed.exit_code != 0, failed.output
	assert os.read_file(shared)! == 'existing shared tool'
	assert (os.ls(tmp) or { [] }).filter(it.starts_with(uncached_tool_stage_prefix)) == []
}

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
	// Every default tool cache folder, `~/.vtools` included, is below a folder that everybody can
	// write to, so the cache cannot be used at all, as on the host from the issue.
	shared := os.join_path(root, 'shared')
	xdg := os.join_path(shared, 'xdg')
	vtmp := os.join_path(shared, 'vtmp')
	home := os.join_path(shared, 'home')
	os.mkdir(shared)!
	os.chmod(shared, 0o777)!
	os.mkdir(xdg, mode: 0o700)!
	os.mkdir(vtmp, mode: 0o700)!
	os.mkdir(home, mode: 0o700)!
	moved := os.join_path(root, 'moved.v')
	// Only the ownership checker rejects this program, so its diagnostic shows that the
	// ownership compiler was built and ran.
	os.write_file(moved, 'fn consume(values []int) int {\n\treturn values.len\n}\n\nfn main() {\n\tvalues := [1, 2, 3]\n\tprintln(consume(values))\n\tprintln(values.len)\n}\n')!
	clean := os.join_path(root, 'clean.v')
	os.write_file(clean, 'fn main() {\n\tvalues := [1, 2, 3]\n\tprintln(values.len)\n}\n')!
	environment := {
		'XDG_CACHE_HOME':       xdg
		'HOME':                 home
		'VTMP':                 vtmp
		'VFLAGS':               ''
		'VTOOLS_CACHE_DIR':     ''
		'VTOOLS_NO_CACHE':      '1'
		'VTOOLS_CACHE_VERBOSE': ''
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
	leftovers := (os.ls(vtmp) or { [] }).filter(it.starts_with(uncached_tool_stage_prefix))
	assert leftovers == [], 'the temporary ownership compiler was left in `${vtmp}`'
	assert !os.exists(os.join_path(xdg, 'v')), 'the tool cache must not be prepared'
	assert !os.exists(os.join_path(vtmp, 'tools')), 'the tool cache must not be prepared'
	assert !os.exists(os.join_path(home, '.vtools')), 'the tool cache must not be prepared'
}
