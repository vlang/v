import os
import time

fn test_macos_v_clock_policy_with_raw_dispatcher_doubles() ! {
	cc := os.find_abs_path_of_executable('clang') or { return }
	$if !amd64 {
		return
	}
	root := os.join_path(os.vtmp_dir(), 'macos_v_clock_policy_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	clock_dir := os.join_path(@VMODROOT, 'vlib/builtin/macos_bare')
	fixture := os.join_path(@VMODROOT, 'vlib/builtin/testdata/macos_clocks')
	// Reuse the actual V policy with only its module name changed. SDK ABI
	// declarations and export wrappers are compiled against the host headers.
	for name in ['clocks_d_freestanding.c.v', 'clock_exports_d_freestanding.c.v'] {
		source := os.read_file(os.join_path(clock_dir, name))!
		os.write_file(os.join_path(root, name.replace('_d_freestanding', '')),
			source.replace_once('module builtin', 'module main'))!
	}
	os.cp(os.join_path(clock_dir, 'clock_abi.h'), os.join_path(root, 'clock_abi.h'))!
	for name in ['mock.c.v', 'vectors.v', 'abi_consumer.c', 'abi_consumer.h'] {
		os.cp(os.join_path(fixture, name), os.join_path(root, name))!
	}
	for prod in [false, true] {
		output := os.join_path(root, 'clock_${prod}')
		mut command := [@VEXE, '-no-retry-compilation', '-gc', 'none', '-enable-globals', '-cc',
			cc, '-cflags',
			'-Werror=incompatible-pointer-types -Werror=incompatible-function-pointer-types -Werror=incompatible-pointer-types-discards-qualifiers',
			'-o', output]
		if prod { command << '-prod' }
		command << root
		built := os.exec(command)
		assert built.exit_code == 0, built.output
		ran := os.exec([output])
		assert ran.exit_code == 0 && ran.output.trim_space() == 'V clock policy and ABI ok', ran.output
	}
}

fn test_macos_amd64_v_clocks_use_no_dynamic_libraries() ! {
	$if !macos || !amd64 {
		return
	}
	root := os.join_path(os.vtmp_dir(), 'macos_native_v_clocks_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	fixture := os.join_path(@VMODROOT, 'vlib/builtin/testdata/macos_clocks/native/main.c.v')
	for prod in [false, true] {
		for mode in ['plain', 'profile', 'trace'] {
			binary := os.join_path(root, 'clock_${prod}_${mode}')
			mut command := [@VEXE, '-no-retry-compilation', '-freestanding', '-gc', 'none', '-cc',
				'clang', '-o', binary]
			if prod { command << '-prod' }
			report := os.join_path(root, 'profile_${prod}.txt')
			if mode == 'profile' { command << ['-profile', report] }
			if mode == 'trace' { command << '-trace-calls' }
			command << fixture
			built := os.exec(command)
			assert built.exit_code == 0, built.output
			ran := macos_v_clock_bounded_run(binary)
			assert ran.exit_code == 0, ran.output
			assert ran.output.contains('raw macOS clock ABI ok'), ran.output
			assert ran.output.contains('raw macOS BF clock ok'), ran.output
			loads := os.exec(['otool', '-l', binary])
			assert loads.exit_code == 0, loads.output
			assert !loads.output.contains('LC_LOAD_DYLIB') && !loads.output.contains('LC_LOAD_DYLINKER'), loads.output
			undefined := os.exec(['nm', '-u', binary])
			assert undefined.exit_code == 0 && undefined.output.trim_space() == '', undefined.output
			if mode == 'profile' {
				contents := os.read_file(report)!
				assert contents.contains('main__main'), contents
				assert !contents.contains('mac_clock_'), contents
				assert !contents.contains('mac_kernel_'), contents
			}
		}
	}
}

fn macos_v_clock_bounded_run(binary string) os.Result {
	mut process := os.new_process(binary)
	process.set_redirect_stdio()
	process.run()
	watch := time.new_stopwatch()
	mut chunks := []string{}
	for process.is_alive() && watch.elapsed() < 20 * time.second {
		// Drain both pipes while the child runs: full tracing may exceed pipe
		// capacity, and must not turn the timeout guard into a false deadlock.
		if data := process.pipe_read(.stdout) { chunks << data }
		if data := process.pipe_read(.stderr) { chunks << data }
		time.sleep(time.millisecond)
	}
	expired := process.is_alive()
	if expired { process.signal_kill() }
	process.wait()
	chunks << process.stdout_slurp()
	chunks << process.stderr_slurp()
	code := if expired { -1 } else { process.code }
	process.close()
	return os.Result{ exit_code: code, output: chunks.join('') }
}

fn test_macos_amd64_v_nanosleep_reports_real_signal_interruption() ! {
	$if !macos || !amd64 {
		return
	}
	root := os.join_path(os.vtmp_dir(), 'macos_v_clock_signal_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	fixture := os.join_path(@VMODROOT, 'vlib/builtin/testdata/macos_clocks/signal/main.c.v')
	for prod in [false, true] {
		binary := os.join_path(root, 'signal_${prod}')
		mut command := [@VEXE, '-no-retry-compilation', '-freestanding', '-enable-globals', '-gc',
			'none', '-cc', 'clang', '-o', binary]
		if prod { command << '-prod' }
		command << fixture
		built := os.exec(command)
		assert built.exit_code == 0, built.output
		ran := macos_v_clock_bounded_run(binary)
		assert ran.exit_code == 0 && ran.output.trim_space() == 'raw macOS nanosleep interruption ok', ran.output
		loads := os.exec(['otool', '-l', binary])
		assert loads.exit_code == 0 && !loads.output.contains('LC_LOAD_DYLIB')
			&& !loads.output.contains('LC_LOAD_DYLINKER'), loads.output
		undefined := os.exec(['nm', '-u', binary])
		assert undefined.exit_code == 0 && undefined.output.trim_space() == '', undefined.output
	}
}
