import os
import v.cmdexec
import v.pref

fn test_dynamic_c_libraries_tls_initializers_and_detached_threads_without_libc() ! {
	$if !linux || (!amd64 && !arm64) {
		return
	}
	fixture := os.join_path(@VEXEROOT, 'vlib/builtin/testdata/freestanding_dynamic')
	root := os.join_path(os.vtmp_dir(), 'vlibc_dynamic_${os.getpid()}')
	prefix := os.join_path(root, 'prefix with spaces')
	lib := os.join_path(prefix, 'lib')
	os.mkdir_all(os.join_path(lib, 'pkgconfig'))!
	os.mkdir_all(os.join_path(prefix, 'include'))!
	defer { os.rmdir_all(root) or {} }
	os.write_file(os.join_path(prefix, 'vlibc-target'), 'vlibc-native-v1\nlinux\n${pref.host_arch()}\n')!
	os.write_file(os.join_path(lib, 'pkgconfig/vlibc-dynamic-probe.pc'), 'Name: vlibc-dynamic-probe\nDescription: libc-free shared TLS fixture\nVersion: 1\nLibs: -L"${lib}" -lprobe\nLibs.private: -lintentionally_missing_static_only_dependency\n')!
	old_prefix := os.getenv_opt('VLIBC_PREFIX')
	old_test := os.getenv_opt('VLIBC_DYNAMIC_TEST')
	defer {
		if value := old_prefix {
			os.setenv('VLIBC_PREFIX', value, true)
		} else {
			os.unsetenv('VLIBC_PREFIX')
		}
		if value := old_test {
			os.setenv('VLIBC_DYNAMIC_TEST', value, true)
		} else {
			os.unsetenv('VLIBC_DYNAMIC_TEST')
		}
	}
	os.setenv('VLIBC_PREFIX', prefix, true)
	os.setenv('VLIBC_DYNAMIC_TEST', 'from-loader-environment', true)
	for compiler in ['gcc', 'clang'] {
		os.find_abs_path_of_executable(compiler) or { continue }
		for name in ['base', 'probe'] {
			mut arguments := ['-fPIC', '-shared', '-nostdlib', '-fno-stack-protector',
				'-Wl,-soname,lib${name}.so', r'-Wl,-rpath,$ORIGIN', '-o',
				os.join_path(lib, 'lib${name}.so'), os.join_path(fixture, '${name}.c')]
			if name == 'probe' { arguments << ['-L${lib}', '-lbase'] }
			if name == 'base' { arguments << ['-O2', '-D_FORTIFY_SOURCE=2'] }
			built := cmdexec.run(compiler, arguments)
			assert built.exit_code == 0, built.output
			elf := cmdexec.run('readelf', ['-d', os.join_path(lib, 'lib${name}.so')])
			assert elf.exit_code == 0 && !elf.output.contains('libc.so'), elf.output
		}
		for production in [false, true] {
			for mode in [['-vlibc', '-no-system-libc'],
				['-syscalls', '-d', 'vlibc', '-no-system-libc'], ['-freestanding'],
				['-shim-libc', '-cflags', '-std=gnu23']] {
				output := os.join_path(root, '${compiler}_${production}_${mode.join('_')}')
				mut arguments := ['-no-retry-compilation', '-cc', compiler, '-o', output]
				arguments << mode
				if production { arguments << '-prod' }
				arguments << os.join_path(fixture, 'main.c.v')
				built := cmdexec.run(@VEXE, arguments)
				assert built.exit_code == 0, built.output
				elf := cmdexec.run('readelf', ['-d', output])
				assert elf.exit_code == 0 && elf.output.contains('libprobe.so') && !elf.output.contains('libc.so'), elf.output
				for run in 0 .. 4 {
					result := cmdexec.run(output, if run % 2 == 0 { []string{} } else { ['exit'] })
					assert result.exit_code == 0, '${output}: ${result.exit_code} ${result.output}'
					assert result.output == 'dynamic TLS ok\nshared finalized\n', result.output
				}
			}
		}
	}
}
