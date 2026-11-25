import os
import time

fn test_vlibc_startup_dynamic_lookup() ! {
	$if !linux || (!amd64 && !arm64) {
		return
	}
	fixture := os.join_path(@VEXEROOT, 'vlib/builtin/testdata/freestanding_compat')
	root := os.join_path(os.vtmp_dir(), 'vlibc_dynamic_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	for library in ['leaf', 'root'] {
		mut command := ['gcc', '-fPIC', '-shared', '-nostdlib', '-funwind-tables',
			'-fno-optimize-sibling-calls', '-Wl,--eh-frame-hdr',
			'-Wl,--hash-style=${if library == 'leaf' { 'sysv' } else { 'gnu' }}',
			'-Wl,-soname,${if library == 'root' { './' } else { '' }}libv_dynamic_${library}.so',
			'-o', os.join_path(root, 'libv_dynamic_${library}.so'),
			os.join_path(fixture, 'dynamic_${library}.c')]
		if library == 'root' { command << ['-L${root}', '-lv_dynamic_leaf', r'-Wl,-rpath,$ORIGIN'] }
		dynamic_command(command)
	}
	os.symlink('libv_dynamic_root.so', os.join_path(root, 'libv_dynamic_alias.so'))!
	for production in [false, true] {
		binary := os.join_path(root, 'dynamic_${production}')
		mut command := [@VEXE, '-syscalls', '-vlibc', '-no-system-libc', '-cc', 'gcc',
			'-no-retry-compilation', '-cflags', '-L${root}', '-ldflags', '-Wl,-rpath,${root}',
			'-o', binary]
		if production { command << '-prod' }
		command << os.join_path(fixture, 'dynamic_runtime.c.v')
		dynamic_command(command)
		output := dynamic_command_in([binary], root)
		assert output.contains('GNU/SysV ELF lookup'), output
		elf := dynamic_command(['readelf', '-d', binary])
		assert elf.contains('[./libv_dynamic_root.so]'), elf
		assert !elf.contains('[libc.so') && !elf.contains('[libpthread.so'), elf
	}
}

fn test_vlibc_static_object_metadata_and_unwinding() ! {
	$if !linux || (!amd64 && !arm64) {
		return
	}
	root := os.join_path(os.vtmp_dir(), 'vlibc_static_unwind_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	fixture := os.join_path(@VEXEROOT, 'vlib/builtin/testdata/freestanding_compat/unwind_static.c.v')
	for production in [false, true] {
		binary := os.join_path(root, 'unwind_${production}')
		mut command := [@VEXE, '-syscalls', '-vlibc', '-no-system-libc', '-cc', 'gcc',
			'-no-retry-compilation', '-ldflags', '-static', '-o', binary]
		if production { command << '-prod' }
		command << fixture
		dynamic_command(command)
		assert dynamic_command([binary]).contains('static ELF metadata and native unwinding passed')
		elf := dynamic_command(['readelf', '-l', binary])
		assert !elf.contains('INTERP') && !elf.contains('DYNAMIC'), elf
	}
}

fn dynamic_command(command []string) string {
	return dynamic_command_in(command, '')
}

fn dynamic_command_in(command []string, folder string) string {
	mut process := os.new_process(command[0])
	process.set_args(command[1..])
	if folder.len > 0 { process.set_work_folder(folder) }
	process.use_pgroup = true
	process.set_redirect_stdio()
	process.run()
	deadline := time.sys_mono_now() + 240 * i64(time.second)
	for process.is_alive() && time.sys_mono_now() < deadline { time.sleep(10 * time.millisecond) }
	if process.is_alive() { process.signal_pgkill() }
	process.wait()
	output := process.stdout_slurp() + process.stderr_slurp()
	code := process.code
	process.close()
	assert code == 0, '${command}\n${output}'
	return output
}
