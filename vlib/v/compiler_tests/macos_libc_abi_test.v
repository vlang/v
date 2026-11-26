import os
import time

const macos_libc_helpers = ['bare_macos_posix_io_result', 'bare_macos_posix_read',
	'bare_macos_posix_write', 'bare_macos_posix_isatty', 'bare_macos_memchr', 'bare_macos_abort_mask',
	'bare_macos_abort_initial_mask', 'bare_macos_abort_delivery_wait', 'bare_macos_abort_trap',
	'bare_macos_abort', 'bare_macos_assert_write', 'bare_macos_assert_text', 'bare_macos_assert_line',
	'bare_macos_assert_rtn', 'read', 'write', 'isatty', 'memchr', 'abort', '__assert_rtn']

// Run the actual Darwin-generated V policy through independent SDK prototypes.
// Only the kernel/clock transport is doubled; the native test below covers it.
fn test_macos_libc_generated_policy_and_c_abi() ! {
	$if !linux || !amd64 {
		return
	}
	clang := os.find_abs_path_of_executable('clang') or { return }
	work := os.join_path(os.vtmp_dir(), 'macos_libc_policy_${os.getpid()}')
	os.mkdir_all(work)!
	defer { os.rmdir_all(work) or {} }
	source := os.join_path(work, 'main.v')
	// Import the real termios module as well: the file-local opaque SDK view
	// must coexist with its richer V declaration without changing C's layout.
	os.write_file(source, 'import term.termios\nfn main() { t := termios.Termios{}; println(t.c_iflag) }\n')!
	for autofree in [false, true] {
		for optimized in [false, true] {
			output := os.join_path(work, 'generated.c')
			mut command := [@VEXE, '-no-retry-compilation', '-gc', 'none', '-os', 'macos', '-arch',
				'amd64', '-freestanding', '-d', 'no_main', '-o', output]
			if optimized { command << '-prod' }
			if autofree { command << '-autofree' }
			command << source
			build := os.exec(command)
			assert build.exit_code == 0, build.output
			code := os.read_file(output)!
			os.write_file(os.join_path(work, 'generated_libc.h'), macos_libc_header(code))!
			binary := os.join_path(work, 'oracle')
			fixture := os.join_path(@VEXEROOT, 'vlib/builtin/testdata/macos_libc_abi/policy.c')
			compiled := os.exec([clang, '-std=gnu11', '-ffreestanding', '-fno-builtin',
				'-fno-stack-protector', '-Wall', '-Wextra', '-Werror',
				if optimized { '-O3' } else { '-O0' }, '-I', work, fixture, '-pthread', '-o', binary])
			assert compiled.exit_code == 0, compiled.output
			run := os.exec([binary])
			assert run.exit_code == 0 && run.output.trim_space() == 'V Darwin libc ABI policy ok', 'autofree=${autofree} optimized=${optimized}: ${run.output}'
		}
	}
}

fn macos_libc_definition(code string, name string) string {
	for line in code.split_into_lines() {
		if !line.starts_with('\t') && line.ends_with(' {')
			&& (line.contains(' ${name}(') || line.contains(' (${name})(')) {
			return line + code.all_after(line).all_before('\n}\n') + '\n}\n'
		}
	}
	assert false, 'missing generated function ${name}'
	return ''
}

fn macos_libc_header(code string) string {
	mut header := ''
	for line in code.split_into_lines() {
		if line.starts_with('typedef void (*_fn_ptr_')
			&& (line.ends_with(')(i32);') || line.ends_with(')(void);')) {
			header += line + '\n'
		}
		if line.starts_with('#define builtin__mac_nr_')
			&& (line.contains('_read ') || line.contains('_write ') || line.contains('_ioctl ')) {
			header += line + '\n'
		}
	}
	for name in ['BareMacSignalAction', 'FSysClockStamp'] {
		header += 'typedef struct ${name} ${name};\nstruct ${name} {'
		header += code.all_after('struct ${name} {').all_before('\n};') + '\n};\n'
	}
	header += 'u32 mac_kernel_thread_self(void);\ni32 mac_kernel_bsd_call6(u32, u64, u64, u64, u64, u64, u64, u64 *);\ni64 mac_clock_sleep_native(FSysClockStamp, FSysClockStamp *);\n'
	// Preserve V's real forward declarations, including external linkage for
	// inline helpers and SDK-selected const/ptrdiff_t/size_t export signatures.
	for name in macos_libc_helpers {
		mut found := false
		for line in code.split_into_lines() {
			if !line.starts_with('\t') && line.ends_with(';')
				&& (line.contains(' ${name}(') || line.contains(' (${name})('))
				&& !line.starts_with('V_CRT_LINKAGE ') {
				header += line + '\n'
				found = true
				break
			}
		}
		assert found, 'missing generated declaration ${name}'
	}
	for name in macos_libc_helpers {
		definition := macos_libc_definition(code, name)
		for forbidden in ['_vinit', '_vno_main_init_caller', 'builtin___v_panic', 'v_fixed_index',
			'_PROF_FN_START', 'trace_calls__on_call', 'builtin__free(', 'builtin__malloc('] {
			assert !definition.contains(forbidden), definition
		}
		header += definition
	}
	return header
}

fn test_macos_libc_platform_and_instrumentation_selection() ! {
	work := os.join_path(os.vtmp_dir(), 'macos_libc_selection_${os.getpid()}')
	os.mkdir_all(work)!
	defer { os.rmdir_all(work) or {} }
	source := os.join_path(work, 'main.v')
	os.write_file(source, 'fn main() {}\n')!
	for role in ['raw', 'arm64', 'hosted', 'syscalls', 'object', 'shared', 'explicit_host'] {
		output := os.join_path(work, '${role}.c')
		mut command := [@VEXE, '-no-retry-compilation', '-gc', 'none', '-os', 'macos', '-arch',
			if role == 'arm64' { 'arm64' } else { 'amd64' }]
		if role == 'syscalls' {
			command << '-syscalls'
		} else if role != 'hosted' {
			command << '-freestanding'
		}
		if role == 'raw' { command << ['-profile', '-trace-calls', '-d', 'no_main'] }
		if role == 'object' { command << '-is_o' }
		if role == 'shared' { command << '-shared' }
		if role == 'explicit_host' { command << ['-d', 'v_bare_no_startup'] }
		command << ['-o', output, source]
		build := os.exec(command)
		assert build.exit_code == 0, build.output
		code := os.read_file(output)!
		assert code.contains('bare_macos_posix_read(') == (role == 'raw'), role
		assert code.contains('bare_macos_assert_rtn(') == (role == 'raw'), role
		assert code.contains('bare_macos_memchr(') == (role in ['raw', 'object', 'shared',
			'explicit_host']), role
		if role == 'raw' { macos_libc_header(code) }
	}
}

fn test_macos_native_libc_and_abnormal_termination() ! {
	$if !macos || !amd64 {
		return
	}
	clang := os.find_abs_path_of_executable('clang') or { return }
	work := os.join_path(os.vtmp_dir(), 'macos_libc_native_${os.getpid()}')
	os.mkdir_all(work)!
	defer { os.rmdir_all(work) or {} }
	fixture := os.join_path(@VEXEROOT, 'vlib/builtin/testdata/macos_libc_abi/native')
	controller := os.join_path(work, 'controller')
	compiled := os.exec([clang, '-std=gnu11', '-Wall', '-Wextra', '-Werror',
		os.join_path(fixture, 'controller.c'), '-o', controller])
	assert compiled.exit_code == 0, compiled.output
	for optimized in [false, true] {
		binary := os.join_path(work, if optimized { 'prod' } else { 'debug' })
		mut command := [@VEXE, '-no-retry-compilation', '-gc', 'none', '-freestanding', '-cc',
			clang, '-o', binary]
		if optimized { command << '-prod' }
		command << os.join_path(fixture, 'main.c.v')
		build := os.exec(command)
		assert build.exit_code == 0, build.output
		mut process := os.new_process(controller)
		process.set_args([binary])
		process.set_redirect_stdio()
		process.run()
		watch := time.new_stopwatch()
		for process.is_alive() && watch.elapsed() < 30 * time.second {
			time.sleep(time.millisecond)
		}
		expired := process.is_alive()
		if expired { process.signal_kill() }
		process.wait()
		output := process.stdout_slurp() + process.stderr_slurp()
		status := process.code
		process.close()
		assert !expired && status == 0, output
		assert output.trim_space() == 'V Darwin native libc ABI and abnormal termination ok', output
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
