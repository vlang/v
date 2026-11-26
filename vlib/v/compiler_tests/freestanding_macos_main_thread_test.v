import os
import time

const macos_main_thread_program = 'import os

struct Identity {
	id u64
	main bool
}

fn identity() Identity {
	return Identity{bf_gettid(), os.is_main_thread()}
}

fn require_identity(ok bool) {
	if !ok { exit(95) }
}

fn main() {
	main_id := bf_gettid()
	require_identity(main_id != 0 && os.is_main_thread())
	for _ in 0 .. 4 {
		task := spawn identity()
		worker := task.wait()
		require_identity(worker.id != 0 && worker.id != main_id && !worker.main)
		require_identity(bf_gettid() == main_id && os.is_main_thread())
	}
	println("Darwin main-thread identity ok")
}
'

fn test_macos_signal_identity_selects_kernel_only_for_raw_amd64() ! {
	root := os.join_path(os.vtmp_dir(), 'macos_signal_identity_selection_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	source := os.join_path(root, 'main.v')
	os.write_file(source, 'import os\nfn main() { println(os.is_main_thread()) }\n')!
	for target_os in ['macos', 'ios'] {
		for arch in ['amd64', 'arm64'] {
			for freestanding in [false, true] {
				output := os.join_path(root, '${target_os}_${arch}_${freestanding}.c')
				mut command := [@VEXE, '-no-retry-compilation', '-nocache', '-gc', 'none', '-os',
					target_os, '-arch', arch]
				if freestanding { command << '-freestanding' }
				command << ['-o', output, source]
				built := os.exec(command)
				assert built.exit_code == 0, '${target_os}, ${arch}, freestanding=${freestanding}: ${built.output}'
				code := os.read_file(output)!
				// iOS currently selects signal_default.c.v, rather than Darwin's
				// helper. Preserve that backend's existing identity selection too.
				definition := if target_os == 'ios' {
					'bool os__is_main_thread(void) {'
				} else {
					'u64 os__signal_thread_id(void) {'
				}
				start := code.index(definition) or {
					return error('missing Darwin thread identity definition in ${output}')
				}
				body := code[start..].all_before('\n}')
				if target_os == 'ios' && freestanding {
					assert !code.contains('u64 os__signal_thread_id(void) {'), target_os
					assert body.contains('return os__g_main_thread_id == bf_gettid();'), body
				} else if target_os == 'macos' && freestanding && arch == 'amd64' {
					assert body.contains('bf_gettid()'), body
					assert !body.contains('pthread_self()'), body
				} else {
					assert body.contains('pthread_self()'), body
					assert !body.contains('bf_gettid()'), body
				}
			}
		}
	}
}

fn test_macos_raw_main_thread_identity_survives_worker_lifetimes() ! {
	$if !macos || !amd64 {
		return
	}
	root := os.join_path(os.vtmp_dir(), 'macos_raw_main_thread_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	source := os.join_path(root, 'main.v')
	os.write_file(source, macos_main_thread_program)!
	for production in [false, true] {
		binary := os.join_path(root, if production { 'prod' } else { 'debug' })
		mut command := [@VEXE, '-no-retry-compilation', '-cc', 'clang', '-freestanding', '-gc',
			'none', '-o', binary]
		if production { command << '-prod' }
		command << source
		built := os.exec(command)
		assert built.exit_code == 0, built.output
		mut process := os.new_process(binary)
		process.set_redirect_stdio()
		process.run()
		watch := time.new_stopwatch()
		for process.is_alive() && watch.elapsed() < 15 * time.second {
			time.sleep(time.millisecond)
		}
		expired := process.is_alive()
		if expired { process.signal_kill() }
		process.wait()
		output := process.stdout_slurp() + process.stderr_slurp()
		code := process.code
		process.close()
		assert !expired && code == 0 && output.trim_space() == 'Darwin main-thread identity ok', output
		loads := os.exec(['otool', '-l', binary])
		assert loads.exit_code == 0, loads.output
		for forbidden in ['LC_LOAD_DYLINKER', 'LC_LOAD_DYLIB', 'LC_LOAD_WEAK_DYLIB', 'LC_REEXPORT_DYLIB',
			'LC_LOAD_UPWARD_DYLIB', 'LC_LAZY_LOAD_DYLIB'] {
			assert !loads.output.contains('cmd ${forbidden}'), loads.output
		}
		undefined := os.exec(['nm', '-u', binary])
		assert undefined.exit_code == 0 && undefined.output.trim_space() == '', undefined.output
	}
}
