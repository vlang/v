import encoding.binary
import os
import time

fn runtime_selection_run(command []string) (int, string) {
	mut child := os.new_process(command[0])
	child.set_args(command[1..])
	child.set_redirect_stdio_merged()
	child.run()
	deadline := time.sys_mono_now() + i64(30 * time.second)
	for child.is_alive() && time.sys_mono_now() < deadline { time.sleep(time.millisecond) }
	if child.is_alive() { child.signal_kill() }
	child.wait()
	code := child.code
	output := child.stdout_slurp()
	child.close()
	return code, output
}

fn runtime_selection_is_dynamic(path string) !bool {
	data := os.read_bytes(path)!
	assert data[..4] == [u8(127), `E`, `L`, `F`]
	assert data[4] == 2 && data[5] == 1
	start := binary.little_endian_u64_at(data, 32)
	stride := binary.little_endian_u16_at(data, 54)
	count := binary.little_endian_u16_at(data, 56)
	for i in 0 .. count {
		kind := binary.little_endian_u32_at(data, int(start + u64(i) * stride))
		if kind == 2 || kind == 3 { return true }
	}
	return false
}

fn test_no_system_libc_preserves_caller_supplied_startup() ! {
	$if !linux || (!amd64 && !arm64) {
		return
	}
	work := os.join_path(os.vtmp_dir(), 'caller_startup_${os.getpid()}')
	os.mkdir_all(work)!
	defer { os.rmdir_all(work) or {} }
	source := os.join_path(work, 'main.v')
	os.write_file(source, "fn main() { println('caller startup ok') }")!
	startup := os.exec(['gcc', '-print-file-name=Scrt1.o'])
	assert startup.exit_code == 0 && os.is_file(startup.output.trim_space()), startup.output
	// Model a C toolchain that can link the supplied CRT but has no GNU
	// loader to offer V's replacement TLS startup.
	wrapper := os.join_path(work, 'gcc')
	os.write_file(wrapper, '#!/bin/sh\n' +
		r'for arg do case "$arg" in -print-file-name=ld-linux*) exit 1;; esac; done' + '\n' +
		r'exec gcc "$@"' + '\n')!
	os.chmod(wrapper, 0o755)!
	program := os.join_path(work, 'probe')
	linked := os.exec([@VEXE, '-no-retry-compilation', '-cc', wrapper, '-gc', 'none', '-no-system-libc',
		'-ldflags', os.quoted_path(startup.output.trim_space()) + ' -lc', '-o', program, source])
	assert linked.exit_code == 0, linked.output
	result := os.exec([program])
	assert result.exit_code == 0 && result.output.trim_space() == 'caller startup ok', result.output
}

fn test_runtime_aliases_and_flag_order_select_the_expected_abi() ! {
	$if !linux || (!amd64 && !arm64) {
		return
	}
	work := os.join_path(os.vtmp_dir(), 'runtime_aliases_${os.getpid()}')
	os.mkdir_all(work)!
	defer { os.rmdir_all(work) or {} }
	source := os.join_path(work, 'selection.v')
	os.write_file(source, "fn main() {
	\$if expected_bare ? {
		\$if !freestanding { \$compile_error('missing bare runtime') }
	} \$else {
		\$if freestanding { \$compile_error('unexpected bare runtime') }
	}
	\$if expected_syscalls ? {
		\$if !syscalls ? { \$compile_error('missing syscall alias define') }
	} \$else {
		\$if syscalls ? { \$compile_error('unexpected syscall alias define') }
	}
	\$if expected_libc ? {
		\$if !v_bare_libc ? { \$compile_error('missing libc ABI') }
	} \$else {
		\$if v_bare_libc ? { \$compile_error('unexpected libc ABI') }
	}
	\$if expected_vlibc ? {
		_ = vlibc_sys_getpid()
	} \$else {
		\$if vlibc ? { \$compile_error('unexpected vlibc API') }
	}
}
")!
	// Alias/order checks need frontend selection, not repeated optimization of
	// identical C programs. The runtime matrix below checks the resulting ABIs.
	for flags in [
		['-syscalls'],
		['-bare'],
		['-bare', '-vlibc'],
		['-vlibc', '-bare'],
		['-bare', '-no-system-libc'],
		['-no-system-libc', '-bare'],
		['-vlibc'],
		['-d', 'vlibc'],
		['-syscalls', '-vlibc'],
		['-vlibc', '-syscalls'],
		['-syscalls', '-d', 'vlibc'],
		['-d', 'vlibc', '-syscalls'],
		['-no-system-libc'],
		['-vlibc', '-no-system-libc'],
		['-no-system-libc', '-vlibc'],
		['-syscalls', '-no-system-libc'],
		['-no-system-libc', '-syscalls'],
		['-d', 'vlibc', '-no-system-libc'],
		['-no-system-libc', '-d', 'vlibc'],
		['-shim-libc'],
		['-syscalls', '-shim-libc'],
		['-shim-libc', '-syscalls'],
		['-syscalls', '-freestanding'],
		['-freestanding', '-syscalls'],
		['-freestanding', '-vlibc'],
		['-vlibc', '-freestanding'],
	] {
		mut command := [@VEXE, '-new-compiler', '-no-retry-compilation', '-o',
			os.join_path(work, 'selection.c')]
		bare := '-syscalls' in flags || '-bare' in flags || '-freestanding' in flags
		if bare { command << ['-d', 'expected_bare'] }
		if bare && '-freestanding' !in flags && '-no-system-libc' !in flags && '-shim-libc' !in flags {
			command << ['-d', 'expected_libc']
		}
		if '-vlibc' in flags || 'vlibc' in flags || '-shim-libc' in flags {
			command << ['-d', 'expected_vlibc']
		}
		if '-syscalls' in flags || '-bare' in flags {
			command << ['-d', 'expected_syscalls']
		}
		command << flags
		command << source
		result := os.exec(command)
		assert result.exit_code == 0, '${flags}: ${result.output}'
		assert os.is_file(os.join_path(work, 'selection.c'))
	}
}

fn test_syscalls_and_vlibc_runtime_selection_and_c_interoperability() ! {
	$if !linux || (!amd64 && !arm64) {
		return
	}
	work := os.join_path(os.vtmp_dir(), 'runtime_selection_${os.getpid()}')
	os.mkdir_all(work)!
	defer { os.rmdir_all(work) or {} }
	source := os.join_path(@VEXEROOT, 'vlib/builtin/testdata/runtime_selection/main.c.v')
	cases := [
		['-syscalls'],
		['-bare'],
		['-vlibc'],
		['-d', 'vlibc'],
		['-bare', '-no-system-libc'],
		['-shim-libc'],
		['-freestanding'],
		['-shim-libc', '-ldflags', '-static'],
	]
	for compiler in ['gcc', 'clang'] {
		for production in [false, true] {
			for index, flags in cases {
				with_libc := '-freestanding' !in flags && '-no-system-libc' !in flags && '-shim-libc' !in flags
				name := '${compiler}_${production}_${index}'
				program := os.join_path(work, name)
				mut command := [@VEXE, '-new-compiler', '-no-retry-compilation', '-cc', compiler,
					'-cstrict', '-dump-c-flags', '${program}.flags', '-o', program]
				if production { command << '-prod' }
				command << flags
				command << source
				built := os.exec(command)
				assert built.exit_code == 0, '${name}: ${built.output}'
				assert runtime_selection_is_dynamic(program)! == ('-static' !in flags), name
				if with_libc {
					defined := os.exec(['nm', '--defined-only', program])
					assert defined.exit_code == 0, defined.output
					for line in defined.output.split_into_lines() {
						symbol := line.all_after_last(' ')
						assert symbol !in ['malloc', 'calloc', 'realloc', 'free', '__errno_location'], '${name}: overrides libc ${symbol}'
					}
				}
				cflags := os.read_file('${program}.flags')!
				assert cflags.contains('-nostdlib') == !with_libc, '${name}: ${cflags}'
				dependencies := os.exec(['readelf', '-d', program])
				assert dependencies.exit_code == 0, dependencies.output
				assert dependencies.output.contains('[libc.so') == with_libc, '${name}: ${dependencies.output}'
				status, output := runtime_selection_run([program])
				assert status == 0, '${name}: ${output}'
				assert output == 'runtime interop passed\n', '${name}: ${output}'
				for mode in ['return', 'exit'] {
					flushed_status, flushed_output := runtime_selection_run([program, mode])
					assert flushed_status == 0 && flushed_output == 'buffered C output\n', '${name}/${mode}: ${flushed_output}'
				}
			}
		}
	}
}
