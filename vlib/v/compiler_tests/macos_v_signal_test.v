import os
import time

fn test_macos_signal_backend_selection_and_instrumentation_boundary() ! {
	root := os.join_path(os.vtmp_dir(), 'macos_v_signal_selection_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	source := os.join_path(root, 'main.v')
	os.write_file(source, 'import os\nfn main() { os.signal_ignore(.pipe) }\n')!
	for role in ['raw', 'arm64', 'hosted'] {
		output := os.join_path(root, '${role}.c')
		mut command := [@VEXE, '-no-retry-compilation', '-gc', 'none', '-os', 'macos', '-arch',
			if role == 'arm64' { 'arm64' } else { 'amd64' }]
		if role != 'hosted' { command << '-freestanding' }
		if role == 'raw' { command << ['-profile', '-trace-calls'] }
		command << ['-o', output, source]
		build := os.exec(command)
		assert build.exit_code == 0, build.output
		code := os.read_file(output)!
		assert code.contains('bare_macos_signal(') == (role == 'raw'), role
		mut found_mask := false
		for line in code.split_into_lines() {
			if line.contains(' os__signal_ignore_internal(') && line.ends_with(' {') {
				found_mask = true
				body := code.all_after(line).all_before('\n}')
				assert body.contains('v_macos_kernel_bsd_call6(') == (role == 'raw'), body
				assert body.contains('sigprocmask(') == (role != 'raw'), body
			}
		}
		assert found_mask, role
		if role == 'raw' {
			mut found := false
			for line in code.split_into_lines() {
				if !line.starts_with('\t') && line.contains(' bare_macos_signal(')
					&& line.ends_with(' {') {
					found = true
					body := code.all_after(line).all_before('\n}\n')
					assert !body.contains('_PROF_FN_START'), body
					assert !body.contains('trace_calls__on_call'), body
					assert !body.contains('_vinit'), body
				}
			}
			assert found
		}
	}
}

fn test_macos_signal_policy_and_c_abi_without_darwin_kernel() ! {
	$if !amd64 {
		return
	}
	clang := os.find_abs_path_of_executable('clang') or { return }
	root := os.join_path(os.vtmp_dir(), 'macos_v_signal_policy_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	policy := os.join_path(@VEXEROOT, 'vlib/builtin/macos_bare/signal_d_freestanding.c.v')
	os.write_file(os.join_path(root, 'signal.c.v'),
		os.read_file(policy)!.replace_once('module builtin', 'module main'))!
	fixture := os.join_path(@VEXEROOT, 'vlib/builtin/testdata/macos_signal')
	os.cp(os.join_path(fixture, 'mock.c.v'), os.join_path(root, 'main.c.v'))!
	for production in [false, true] {
		object := os.join_path(root, 'policy.o')
		// Match V's hosted feature selection: strict glibc C otherwise redirects
		// signal to __sysv_signal instead of exercising this exported symbol.
		compile_c := os.exec([clang, '-std=c11', '-D_GNU_SOURCE', '-Wall', '-Wextra', '-Werror',
			if production { '-O2' } else { '-O0' }, '-DVMAC_SIGNAL_POLICY_MOCK', '-c',
			os.join_path(fixture, 'policy.c'), '-o', object])
		assert compile_c.exit_code == 0, compile_c.output
		binary := os.join_path(root, if production { 'prod' } else { 'debug' })
		mut command := [@VEXE, '-no-retry-compilation', '-gc', 'none', '-cc', clang, '-o', binary]
		if production { command << '-prod' }
		command << root
		build := os.exec(command)
		assert build.exit_code == 0, build.output
		run := os.exec([binary])
		assert run.exit_code == 0 && run.output.trim_space() == 'V Darwin signal policy and C ABI ok', run.output
	}
}

fn test_macos_raw_signal_delivery_returns_through_validated_trampoline() ! {
	$if !macos || !amd64 {
		return
	}
	root := os.join_path(os.vtmp_dir(), 'macos_v_signal_native_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	source := os.join_path(@VEXEROOT, 'vlib/builtin/testdata/macos_signal/native/main.c.v')
	for production in [false, true] {
		binary := os.join_path(root, if production { 'prod' } else { 'debug' })
		mut command := [@VEXE, '-no-retry-compilation', '-freestanding', '-gc', 'none', '-cc',
			'clang', '-o', binary]
		if production { command << '-prod' }
		command << source
		build := os.exec(command)
		assert build.exit_code == 0, build.output
		for inherited_ignore in [false, true] {
			mut process := os.new_process(if inherited_ignore { '/bin/sh' } else { binary })
			if inherited_ignore {
				process.set_args(['-c', 'trap "" USR1; exec "$@"', 'sh', binary])
			}
			process.set_redirect_stdio()
			process.run()
			watch := time.new_stopwatch()
			for process.is_alive() && watch.elapsed() < 10 * time.second {
				time.sleep(time.millisecond)
			}
			expired := process.is_alive()
			if expired { process.signal_kill() }
			process.wait()
			output := process.stdout_slurp() + process.stderr_slurp()
			code := process.code
			process.close()
			assert !expired && code == 0, 'expired=${expired}, code=${code}, inherited_ignore=${inherited_ignore}: ${output}'
			assert output.trim_space() == 'V Darwin signal delivery and C ABI ok', output
		}
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
