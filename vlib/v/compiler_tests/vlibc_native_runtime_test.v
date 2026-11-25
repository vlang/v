import os
import time

fn native_runtime_command(command []string) (int, string) {
	mut process := os.new_process(command[0])
	process.set_args(command[1..])
	process.use_pgroup = true
	process.set_redirect_stdio()
	process.run()
	deadline := time.sys_mono_now() + 90 * i64(time.second)
	for process.is_alive() && time.sys_mono_now() < deadline {
		time.sleep(10 * time.millisecond)
	}
	if process.is_alive() { process.signal_pgkill() }
	process.wait()
	output := process.stdout_slurp() + process.stderr_slurp()
	code := process.code
	process.close()
	return code, output
}

fn test_vlibc_native_runtime_against_hosted_oracle() ! {
	$if !linux || (!amd64 && !arm64) {
		return
	}
	root := os.join_path(os.vtmp_dir(), 'vlibc_native_runtime_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	fixture := os.join_path(@VEXEROOT, 'vlib/builtin/testdata/freestanding_compat')
	reference := os.join_path(root, 'hosted')
	code, output := native_runtime_command(['gcc', '-O2', '-DV_RUNTIME_HOSTED_ORACLE',
		'-Wl,--wrap=connect', os.join_path(fixture, 'vpac_runtime_probe.c'), '-pthread', '-lm',
		'-o', reference])
	assert code == 0, output
	reference_code, reference_output := native_runtime_command([reference])
	assert reference_code == 0, reference_output
	for production in [false, true] {
		binary := os.join_path(root, 'replacement_${production}')
		mut command := [@VEXE, '-no-retry-compilation', '-syscalls', '-vlibc', '-no-system-libc',
			'-cc', 'gcc', '-o', binary]
		if production { command << '-prod' }
		command << os.join_path(fixture, 'vpac_runtime.c.v')
		built_code, built_output := native_runtime_command(command)
		assert built_code == 0, built_output
		result_code, result_output := native_runtime_command([binary])
		assert result_code == 0, result_output
		assert result_output == reference_output + 'syslog private socket, errno formatting and filtering passed\n', result_output
		elf_code, elf_output := native_runtime_command(['readelf', '-d', binary])
		assert elf_code == 0, elf_output
		for line in elf_output.split_into_lines() {
			if line.contains('(NEEDED)') {
				assert !line.contains('[libc.') && !line.contains('[libpthread.'), line
			}
		}
	}
}
