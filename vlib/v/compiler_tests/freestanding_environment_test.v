import encoding.binary as endian
import os
import time

fn run_environment_fixture(command []string) os.Result {
	mut child := os.new_process(command[0])
	child.set_args(command[1..])
	child.set_redirect_stdio_merged()
	child.run()
	defer { child.close() }
	deadline := time.sys_mono_now() + u64(30 * time.second)
	for child.is_alive() && time.sys_mono_now() < deadline { time.sleep(time.millisecond) }
	timed_out := child.is_alive()
	if timed_out { child.signal_kill() }
	child.wait()
	output := child.stdout_slurp()
	assert !timed_out, '${command}: environment fixture timed out: ${output}'
	return os.Result{ exit_code: child.code, output: output }
}

fn assert_environment_standalone_binary(path string) ! {
	$if macos && arm64 {
		return
	} // This target deliberately retains libSystem.
	bytes := os.read_bytes(path)!
	$if macos {
		assert bytes.len >= 32 && endian.little_endian_u32_at(bytes, 0) == 0xfeedfacf
		assert endian.little_endian_u32_at(bytes, 24) & 4 == 0
		commands := endian.little_endian_u32_at(bytes, 16)
		limit := u64(32) + endian.little_endian_u32_at(bytes, 20)
		assert limit <= u64(bytes.len)
		mut offset := 32
		mut entries := 0
		for _ in 0 .. commands {
			assert u64(offset) + 8 <= limit
			kind := endian.little_endian_u32_at(bytes, offset)
			size := endian.little_endian_u32_at(bytes, offset + 4)
			assert size >= 8 && u64(offset) + size <= limit
			assert kind !in [u32(0xc), 0xe, 0x80000018, 0x8000001f, 0x20, 0x80000023]
			if kind == 5 { entries++ }
			offset += int(size)
		}
		assert u64(offset) == limit && entries == 1
	} $else {
		assert bytes.len >= 64 && bytes[..4] == [u8(0x7f), `E`, `L`, `F`]
		assert bytes[4] == 2 && bytes[5] == 1
		start := endian.little_endian_u64_at(bytes, 32)
		stride := endian.little_endian_u16_at(bytes, 54)
		count := endian.little_endian_u16_at(bytes, 56)
		assert stride >= 56 && start + u64(stride) * count <= u64(bytes.len)
		for index in 0 .. count {
			entry := int(start + u64(stride) * index)
			kind := endian.little_endian_u32_at(bytes, entry)
			assert kind != 3, 'freestanding output has an ELF interpreter'
			if kind == 2 {
				offset := endian.little_endian_u64_at(bytes, entry + 8)
				size := endian.little_endian_u64_at(bytes, entry + 32)
				assert offset + size <= u64(bytes.len)
				for pos := u64(0); pos + 16 <= size; pos += 16 {
					assert endian.little_endian_u64_at(bytes, int(offset + pos)) != 1, 'freestanding output loads a dynamic library'
				}
			}
		}
	}
	undefined := os.exec(['nm', '-u', path])
	assert undefined.exit_code == 0 && undefined.output.trim_space() == '', undefined.output
}

fn run_environment_matrix(autofree bool) ! {
	$if (!linux && !macos) || (!amd64 && !arm64) {
		return
	}
	clang := os.find_abs_path_of_executable('clang') or { return }
	root := os.join_path(@VMODROOT, 'vlib/os/testdata/freestanding_environment')
	work := os.join_path(os.vtmp_dir(), 'freestanding_environment_${os.getpid()}_${autofree}')
	os.mkdir_all(work)!
	defer { os.rmdir_all(work) or {} }
	launcher := os.join_path(work, 'launcher')
	compiled_launcher := os.exec([clang, '-Wall', '-Wextra', '-Werror', os.join_path(root, 'launch.c'),
		'-o', launcher])
	assert compiled_launcher.exit_code == 0, compiled_launcher.output
	target := $if amd64 { 'amd64' } $else { 'arm64' }
	for production in [false, true] {
		for bare in [false, true] {
			mut flags := [@VEXE, '-nocache', '-no-retry-compilation', '-arch', target, '-gc', 'none',
				'-cc', clang]
			if production { flags << '-prod' }
			if autofree { flags << '-autofree' }
			if bare { flags << '-freestanding' }
			binary := os.join_path(work, 'environment_${bare}_${production}')
			mut command := flags.clone()
			command << ['-o', binary, os.join_path(root, 'main.v')]
			built := os.exec(command)
			assert built.exit_code == 0, built.output
			if bare { assert_environment_standalone_binary(binary)! }
			mut modes := ['startup', 'mutation', 'arena', 'clear']
			$if linux || amd64 {
				if bare { modes << 'threads' }
			}
			for mode in modes {
				ran := run_environment_fixture([launcher, binary, mode])
				assert ran.exit_code == 0 && ran.output == 'environment ${mode} passed\n', ran.output
			}
			// Include the live V store when Process snapshots the child's environment.
			spawn_binary := os.join_path(work, 'spawn_${bare}_${production}')
			mut spawn_command := flags.clone()
			spawn_command << ['-o', spawn_binary, os.join_path(root, 'spawn/main.v')]
			spawn_built := os.exec(spawn_command)
			assert spawn_built.exit_code == 0, spawn_built.output
			if bare { assert_environment_standalone_binary(spawn_binary)! }
			spawn_run := run_environment_fixture([launcher, spawn_binary, 'check'])
			assert spawn_run.exit_code == 0 && spawn_run.output == 'environment spawn passed\n', spawn_run.output

			$if linux || amd64 {
				if bare {
					tracking := os.join_path(work, 'tracking_${production}')
					mut tracking_command := flags.clone()
					tracking_command << ['-d', 'track_heap', '-o', tracking,
						os.join_path(root, 'tracking/main.c.v')]
					tracking_built := os.exec(tracking_command)
					assert tracking_built.exit_code == 0, tracking_built.output
					assert_environment_standalone_binary(tracking)!
					tracking_run := run_environment_fixture([launcher, tracking, 'check'])
					assert tracking_run.exit_code == 0 && tracking_run.output == 'environment allocation checks passed\n', tracking_run.output
				}
			}
			$if linux {
				if bare {
					oom := os.join_path(work, 'oom_${production}')
					mut oom_command := flags.clone()
					oom_command << ['-o', oom, os.join_path(root, 'oom/main.c.v')]
					oom_built := os.exec(oom_command)
					assert oom_built.exit_code == 0, oom_built.output
					assert_environment_standalone_binary(oom)!
					oom_run := run_environment_fixture([launcher, oom, 'check'])
					assert oom_run.exit_code == 0 && oom_run.output == 'environment allocation failure passed\n', oom_run.output
				}
			}
		}
	}
}

fn test_freestanding_process_environment_ownership_and_mutation() ! {
	run_environment_matrix(false)!
	run_environment_matrix(true)!
}

fn test_freestanding_embedded_environment_without_process_arguments() ! {
	$if !linux || (!amd64 && !arm64) {
		return
	}
	clang := os.find_abs_path_of_executable('clang') or { return }
	root := os.join_path(@VMODROOT, 'vlib/os/testdata/freestanding_environment')
	work := os.join_path(os.vtmp_dir(), 'embedded_environment_${os.getpid()}')
	os.mkdir_all(work)!
	defer { os.rmdir_all(work) or {} }
	launcher := os.join_path(work, 'launcher')
	compiled_launcher := os.exec([clang, '-Wall', '-Wextra', '-Werror', os.join_path(root, 'launch.c'),
		'-o', launcher])
	assert compiled_launcher.exit_code == 0, compiled_launcher.output
	for object in [false, true] {
		for autofree in [false, true] {
			library := os.join_path(work, if object { 'environment.o' } else { 'libenvironment.so' })
			mut command := [@VEXE, '-nocache', '-no-retry-compilation', '-freestanding', '-gc',
				'none', '-cc', clang, '-o', library]
			if autofree { command << '-autofree' }
			if object {
				command << ['-is_o', '-d', 'no_main']
			} else {
				command << '-shared'
			}
			command << os.join_path(root, 'embedded/library.v')
			built := os.exec(command)
			assert built.exit_code == 0, built.output
			host := os.join_path(work, 'host')
			mut host_command := [clang, '-Wall', '-Wextra', '-Werror',
				os.join_path(root, 'embedded/host.c'), '-o', host]
			if object {
				host_command << ['-DENV_OBJECT', library, '-lm', '-lpthread']
			} else {
				host_command << '-ldl'
			}
			linked := os.exec(host_command)
			assert linked.exit_code == 0, linked.output
			ran := run_environment_fixture([launcher, host, library])
			assert ran.exit_code == 0 && ran.output == 'embedded environment passed\n', ran.output
		}
	}
}
