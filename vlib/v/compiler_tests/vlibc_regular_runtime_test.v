import os
import time

fn regular_vlibc_run(command []string, seconds int) (int, string) {
	mut child := os.new_process(command[0])
	child.set_args(command[1..])
	child.use_pgroup = true
	child.set_redirect_stdio_merged()
	child.run()
	deadline := time.sys_mono_now() + i64(seconds) * i64(time.second)
	for child.is_alive() && time.sys_mono_now() < deadline { time.sleep(time.millisecond) }
	if child.is_alive() { child.signal_pgkill() }
	child.wait()
	code := child.code
	output := child.stdout_slurp()
	child.close()
	return code, output
}

fn test_vlibc_preserves_regular_runtime_and_collector() ! {
	$if !linux || (!amd64 && !arm64) {
		return
	}
	root := os.join_path(os.vtmp_dir(), 'vlibc_regular_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	source := os.join_path(@VEXEROOT, 'vlib/builtin/testdata/vlibc_regular_runtime/main.c.v')
	for production in [false, true] {
		mut default_gc := ''
		for index, flags in [[]string{}, ['-vlibc'], ['-d', 'vlibc'], ['-vlibc', '-gc', 'boehm'],
			['-vlibc', '-gc', 'none'], ['-shim-libc'], ['-shim-libc', '-gc', 'boehm'],
			['-vlibc', '-no-system-libc', '-gc', 'none']] {
			binary := os.join_path(root, '${production}_${index}')
			mut command := [@VEXE, '-new-compiler', '-no-retry-compilation', '-cc', 'gcc']
			if production { command << '-prod' }
			command << flags
			command << ['-o', binary, source]
			built_code, built_output := regular_vlibc_run(command, 120)
			assert built_code == 0, '${flags}: ${built_output}'
			result_code, result_output := regular_vlibc_run([binary], 30)
			assert result_code == 0, '${flags}: ${result_code}: ${result_output}'
			gc := result_output.split_into_lines()[0]
			if index == 0 {
				default_gc = gc
			} else if index <= 2 || index == 5 {
				assert gc == default_gc, '${flags}: changed default GC: ${result_output}'
			} else {
				assert gc == if index in [3, 6] { 'gc=boehm' } else { 'gc=none' }, result_output
			}
			if index > 0 {
				assert result_output.ends_with('vlibc regular-runtime threads passed\n'), result_output
			}
		}
	}
}
