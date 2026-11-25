import os
import v.cmdexec

const compat_fixture = os.join_path(@VMODROOT, 'vlib/builtin/testdata/freestanding_compat')

fn compat_work_directory(label string) !string {
	root := os.join_path(os.vtmp_dir(), 'freestanding_compat_${label}_${os.getpid()}')
	os.mkdir_all(root)!
	return root
}

fn compat_run(command []string) (int, string) {
	result := cmdexec.run_with_timeout(command[0], command[1..], 60_000)
	return result.exit_code, result.output
}

fn compat_elf_word(data []u8, offset int, width int) u64 {
	assert offset >= 0 && offset + width <= data.len
	mut value := u64(0)
	for i in 0 .. width {
		value |= u64(data[offset + i]) << u32(i * 8)
	}
	return value
}

fn compat_assert_static_elf(path string) ! {
	data := os.read_bytes(path)!
	assert data.len >= 64 && data[..4] == [u8(127), `E`, `L`, `F`]
	assert data[4] == 2 && data[5] == 1
	programs := int(compat_elf_word(data, 32, 8))
	width := int(compat_elf_word(data, 54, 2))
	count := int(compat_elf_word(data, 56, 2))
	assert width >= 56
	for i in 0 .. count {
		kind := compat_elf_word(data, programs + i * width, 4)
		assert kind != 2 && kind != 3, 'freestanding binary has PT_DYNAMIC or PT_INTERP: ${path}'
	}
}

fn compat_assert_oracle(reference []u8, actual []u8, label string) {
	assert actual.len == reference.len, '${label}: different oracle sizes'
	assert actual.len > 30000 * 276 && actual.len % 276 == 0
	for i in 0 .. reference.len {
		if reference[i] != actual[i] {
			start := i / 276 * 276
			end := start + 276
			assert false, '${label}: record ${i / 276}, byte ${i % 276}\nexpected: ${reference[start..end]}\nactual: ${actual[start..end]}'
		}
	}
}

fn test_freestanding_c_library_compatibility_matrix() ! {
	$if !linux || (!amd64 && !arm64) {
		return
	}
	root := compat_work_directory('matrix')!
	defer { os.rmdir_all(root) or {} }
	for compiler in ['gcc', 'clang'] {
		for with_vlibc in [false, true] {
			for production in [false, true] {
				for headers in [false, true] {
					role := '${compiler}_vlibc_${with_vlibc}_prod_${production}_headers_${headers}'
					output := os.join_path(root, role)
					flags_file := '${output}.flags'
					mut command := [@VEXE, '-new-compiler', '-no-retry-compilation', '-freestanding',
						'-ldflags', '-static', '-cstrict', '-cc', compiler, '-dump-c-flags',
						flags_file, '-o', output]
					if with_vlibc {
						command << ['-d', 'vlibc']
					}
					if production {
						command << '-prod'
					}
					if headers {
						command << '-target-libc-headers'
					}
					command << os.join_path(compat_fixture, 'main.c.v')
					built := os.exec(command)
					assert built.exit_code == 0, '${role}: ${built.output}'
					flags := os.read_file(flags_file)!
					assert '-lpthread' !in flags.split_into_lines(), '${role}: hosted pthread dependency'
					compat_assert_static_elf(output)!
					code, result := compat_run([output, root])
					assert code == 0, '${role}: ${result}'
					assert result.trim_space() == 'freestanding C compatibility ok', '${role}: ${result}'
					for mode in ['return', 'exit'] {
						exit_code, exit_output := compat_run([output, root, mode])
						assert exit_code == 0, '${role}/${mode}: ${exit_output}'
						assert exit_output == 'buffered stdout on exit\natexit second\natexit first\nELF finalizer\n', '${role}/${mode}: ${exit_output}'
					}
				}
			}
		}
	}
}

fn test_freestanding_formatting_matches_hosted_c_library() ! {
	$if !linux || (!amd64 && !arm64) {
		return
	}
	root := compat_work_directory('format')!
	defer { os.rmdir_all(root) or {} }
	source := os.join_path(compat_fixture, 'oracle.c.v')
	for compiler in ['gcc', 'clang'] {
		hosted := os.join_path(root, '${compiler}_hosted')
		built := os.exec([@VEXE, '-new-compiler', '-no-retry-compilation', '-gc', 'none', '-cc',
			compiler, '-o', hosted, source])
		assert built.exit_code == 0, built.output
		reference_file := '${hosted}.bin'
		code, output := compat_run([hosted, reference_file])
		assert code == 0, output
		reference := os.read_bytes(reference_file)!
		for production in [false, true] {
			binary := os.join_path(root, '${compiler}_bare_${production}')
			mut command := [@VEXE, '-new-compiler', '-no-retry-compilation', '-freestanding', '-ldflags',
				'-static', '-cc', compiler, '-o', binary]
			if production {
				command << '-prod'
			}
			command << source
			compiled := os.exec(command)
			assert compiled.exit_code == 0, compiled.output
			compat_assert_static_elf(binary)!
			data_file := '${binary}.bin'
			status, result := compat_run([binary, data_file])
			assert status == 0, result
			compat_assert_oracle(reference, os.read_bytes(data_file)!, '${compiler}/${production}')
		}
	}
}

fn test_freestanding_resolver_udp_tcp_hosts_services_and_failures() ! {
	$if !linux || (!amd64 && !arm64) {
		return
	}
	root := compat_work_directory('resolver')!
	defer { os.rmdir_all(root) or {} }
	for compiler in ['gcc', 'clang'] {
		for production in [false, true] {
			binary := os.join_path(root, '${compiler}_${production}')
			mut command := [@VEXE, '-new-compiler', '-no-retry-compilation', '-freestanding', '-ldflags',
				'-static', '-cc', compiler, '-o', binary]
			if production {
				command << ['-prod', '-d', 'vlibc']
			}
			command << os.join_path(compat_fixture, 'resolver.c.v')
			compiled := os.exec(command)
			assert compiled.exit_code == 0, compiled.output
			compat_assert_static_elf(binary)!
			status, output := compat_run(['python3', os.join_path(compat_fixture, 'dns_fixture.py'),
				os.join_path(root, 'config_${compiler}_${production}'), binary])
			assert status == 0, output
			assert output.contains('DNS UDP/TCP fixture passed'), output
		}
	}
}

fn test_shim_libc_with_musl_headers() ! {
	$if !linux || (!amd64 && !arm64) {
		return
	}
	compiler := os.find_abs_path_of_executable('musl-gcc') or { return }
	root := compat_work_directory('musl')!
	defer { os.rmdir_all(root) or {} }
	output := os.join_path(root, 'musl')
	built := os.exec([@VEXE, '-new-compiler', '-no-retry-compilation', '-gc', 'none', '-cc', compiler,
		'-shim-libc', '-ldflags', '-static', '-o', output,
		os.join_path(compat_fixture, 'musl_headers.c.v')])
	assert built.exit_code == 0, built.output
	compat_assert_static_elf(output)!
	code, result := compat_run([output])
	assert code == 0, result
}
