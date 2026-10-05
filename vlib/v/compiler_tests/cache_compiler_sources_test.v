import os
import time
import v.cmdexec

fn test_module_cache_reuses_artifacts_after_unbuilt_compiler_source_changes() {
	$if windows {
		return
	}
	root := os.join_path(os.vtmp_dir(), 'v cache compiler sources ${os.getpid()}_${time.now().unix_nano()}')
	os.mkdir_all(os.join_path(root, 'vlib', 'v'))!
	defer {
		os.rmdir_all(root) or { panic(err) }
	}
	compiler := os.join_path(root, 'v')
	os.cp(@VEXE, compiler)!
	os.cp(os.join_path(@VMODROOT, 'v.mod'), os.join_path(root, 'v.mod'))!
	for name in os.ls(os.join_path(@VMODROOT, 'vlib'))! {
		if name == 'v' {
			continue
		}
		from := os.join_path(@VMODROOT, 'vlib', name)
		to := os.join_path(root, 'vlib', name)
		if name == 'builtin' {
			// Keep builtin's @VEXEROOT in the isolated installation.
			os.cp_all(from, to, false)!
		} else {
			os.symlink(from, to)!
		}
	}
	os.symlink(os.join_path(@VMODROOT, 'thirdparty'), os.join_path(root, 'thirdparty'))!
	compiler_source := os.join_path(root, 'vlib', 'v', 'unused_compiler_source.v')
	os.write_file(compiler_source, 'module main\nfn compiler_placeholder() {}\n')!
	source := os.join_path(root, 'main.v')
	os.write_file(source, 'import strings\nfn main() { println(strings.repeat(120, 3)) }\n')!
	cache := os.join_path(root, 'cache')
	args := ['-u', 'VEXE', '-u', 'VFLAGS', 'V3CACHE=${cache}', compiler, '-new-compiler', '-cc',
		'cc', 'run', source]
	first := cmdexec.run_in('env', args, root)
	assert first.exit_code == 0, first.output
	assert first.output.trim_space() == 'xxx', first.output
	cache_roots := os.ls(cache)!.filter(it.starts_with('v3_module_cache_'))
	assert cache_roots.len == 1, cache_roots.str()
	cache_root := os.join_path(cache, cache_roots[0])
	before := os.ls(cache_root)!
	assert before.len == 1, before.str()

	os.write_file(compiler_source, 'module main\nfn changed_compiler_placeholder() {}\n')!
	second := cmdexec.run_in('env', args, root)
	assert second.exit_code == 0, second.output
	assert second.output.trim_space() == 'xxx', second.output
	assert os.ls(cache_root)! == before

	// Program source changes still invalidate the program's cached artifacts.
	os.write_file(source, 'import strings\nfn main() { println(strings.repeat(121, 3)) }\n')!
	changed := cmdexec.run_in('env', args, root)
	assert changed.exit_code == 0, changed.output
	assert changed.output.trim_space() == 'yyy', changed.output
}
