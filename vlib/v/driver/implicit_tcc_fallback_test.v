module driver

import os
import v.cmdexec

fn test_implicit_tcc_fallback_warning_cites_the_error_line_of_the_failure() {
	warning := v3_implicit_tcc_fallback_warning('gcc', "\n  tcc: error: unresolved reference to 'GetThreadId'\ntcc: error: second\n")
	assert warning == "warning: implicit tcc could not be used for this build (tcc: error: unresolved reference to 'GetThreadId'), regenerating it with gcc"
	assert v3_implicit_tcc_fallback_warning('cc', ' \n\t\n') == 'warning: implicit tcc could not be used for this build, regenerating it with cc'
	// tcc names the including file first when the error is in a header.
	assert v3_implicit_tcc_fallback_warning('gcc', 'In file included from src.c:3:\n/usr/include/x.h:7: error: bad\n') == 'warning: implicit tcc could not be used for this build (/usr/include/x.h:7: error: bad), regenerating it with gcc'
	assert v3_implicit_tcc_fallback_warning('gcc', 'tcc exited with code 1') == 'warning: implicit tcc could not be used for this build (tcc exited with code 1), regenerating it with gcc'
}

// A program that the bundled tcc cannot link must still build through the
// platform C compiler, and the switch must be reported without -v: it is
// often several times slower, and a silent one hid such a tcc failure behind
// a 10x slower Windows self-build.
fn test_failed_implicit_tcc_build_reports_the_fallback() {
	$if !windows {
		return
	}
	vexe := if os.base(@VEXE) == 'v1_fallback.exe' {
		os.join_path(os.dir(@VEXE), 'v.exe')
	} else {
		@VEXE
	}
	vroot := os.dir(vexe)
	kernel32_def := os.join_path(vroot, 'thirdparty', 'tcc', 'lib', 'kernel32.def')
	if !os.is_file(kernel32_def) {
		eprintln('skipping: ${vexe} has no bundled tcc')
		return
	}
	// The program relies on tcc's kernel32 import list lacking GetThreadId.
	if os.read_lines(kernel32_def)!.any(it.trim_space() == 'GetThreadId') {
		eprintln('skipping: the bundled tcc can link GetThreadId now')
		return
	}
	os.find_abs_path_of_executable('gcc') or {
		eprintln('skipping: no gcc to fall back to')
		return
	}
	dir := os.join_path(os.vtmp_dir(), 'v_implicit_tcc_fallback_${os.getpid()}')
	os.mkdir_all(dir)!
	defer {
		os.rmdir_all(dir) or {}
	}
	source := os.join_path(dir, 'thread_id.v')
	os.write_file(source, 'fn C.GetThreadId(voidptr) u32\nfn C.GetCurrentThread() voidptr\n\nfn main() {\n\tprintln(C.GetThreadId(C.GetCurrentThread()) != 0)\n}\n')!
	exe := os.join_path(dir, 'thread_id.exe')
	// CI sets VFLAGS to `-cc gcc`, `-cc msvc` or `-cc tcc`, which would bypass
	// the implicit tcc build under test.
	old_vflags := os.getenv_opt('VFLAGS')
	old_vosargs := os.getenv_opt('VOSARGS')
	os.unsetenv('VFLAGS')
	os.unsetenv('VOSARGS')
	defer {
		if value := old_vflags {
			os.setenv('VFLAGS', value, true)
		}
		if value := old_vosargs {
			os.setenv('VOSARGS', value, true)
		}
	}
	build := cmdexec.run(vexe, ['-nocache', '-o', exe, source])
	assert build.exit_code == 0, build.output
	assert build.output.contains('warning: implicit tcc could not be used for this build ('), build.output
	assert build.output.contains('GetThreadId'), build.output
	run := cmdexec.run(exe, [])
	assert run.exit_code == 0, run.output
	assert run.output.trim_space() == 'true'
	// -silent builds keep their output clean.
	quiet := cmdexec.run(vexe, ['-silent', '-nocache', '-o', exe, source])
	assert quiet.exit_code == 0, quiet.output
	assert !quiet.output.contains('implicit tcc could not be used'), quiet.output
}

// A deterministic implicit-tcc failure, independent of any specific missing
// symbol in the bundled tcc: unlike the GetThreadId-based test above, a
// toolchain update can never make this one skip, so it stays as coverage of
// the driver's own call site (and the -silent guard on it) on every platform.
fn test_forced_implicit_tcc_failure_reports_the_fallback() {
	vexe := if os.base(@VEXE) == 'v1_fallback.exe' {
		os.join_path(os.dir(@VEXE), 'v.exe')
	} else {
		@VEXE
	}
	vroot := os.dir(vexe)
	bundled_tcc := os.join_path(vroot, 'thirdparty', 'tcc', 'tcc.exe')
	mut has_tcc := os.is_file(bundled_tcc)
	if !has_tcc {
		if _ := os.find_abs_path_of_executable('tcc') {
			has_tcc = true
		}
	}
	if !has_tcc {
		eprintln('skipping: no bundled or system tcc available for implicit tcc selection')
		return
	}
	fallback := v3_platform_c_compiler_command(os.user_os())
	os.find_abs_path_of_executable(fallback) or {
		eprintln('skipping: no ${fallback} to fall back to')
		return
	}
	dir := os.join_path(os.vtmp_dir(), 'v_forced_implicit_tcc_failure_${os.getpid()}')
	os.mkdir_all(dir)!
	defer {
		os.rmdir_all(dir) or {}
	}
	source := os.join_path(dir, 'hello.v')
	os.write_file(source, "fn main() {\n\tprintln('ok')\n}\n")!
	exe := os.join_path(dir, 'hello.exe')
	old_vflags := os.getenv_opt('VFLAGS')
	old_vosargs := os.getenv_opt('VOSARGS')
	old_forced_failure := os.getenv_opt('V3_TEST_FORCE_IMPLICIT_TCC_FAILURE')
	os.unsetenv('VFLAGS')
	os.unsetenv('VOSARGS')
	os.setenv('V3_TEST_FORCE_IMPLICIT_TCC_FAILURE', 'tcc: error: injected for test coverage',
		true)
	defer {
		if value := old_vflags {
			os.setenv('VFLAGS', value, true)
		}
		if value := old_vosargs {
			os.setenv('VOSARGS', value, true)
		}
		if value := old_forced_failure {
			os.setenv('V3_TEST_FORCE_IMPLICIT_TCC_FAILURE', value, true)
		} else {
			os.unsetenv('V3_TEST_FORCE_IMPLICIT_TCC_FAILURE')
		}
	}
	build := cmdexec.run(vexe, ['-nocache', '-o', exe, source])
	assert build.exit_code == 0, build.output
	assert build.output.contains('warning: implicit tcc could not be used for this build (tcc: error: injected for test coverage)'), build.output
	run := cmdexec.run(exe, [])
	assert run.exit_code == 0, run.output
	assert run.output.trim_space() == 'ok'
	// -silent builds keep their output clean even for an injected failure.
	quiet := cmdexec.run(vexe, ['-silent', '-nocache', '-o', exe, source])
	assert quiet.exit_code == 0, quiet.output
	assert !quiet.output.contains('implicit tcc could not be used'), quiet.output
}

// TCC has no `-framework`/`-F` support, but links a framework's `.tbd` stub or
// binary when it is given as a file.
fn test_tcc_macos_framework_flags_are_replaced_by_link_files() {
	dir := os.join_path(os.vtmp_dir(), 'v_tcc_macos_frameworks_${os.getpid()}')
	sdk := os.join_path(dir, 'MacOSX.sdk')
	sdk_frameworks := os.join_path(sdk, 'System', 'Library', 'Frameworks')
	custom_frameworks := os.join_path(dir, 'custom')
	os.mkdir_all(os.join_path(sdk_frameworks, 'Foundation.framework'))!
	os.mkdir_all(os.join_path(custom_frameworks, 'Custom.framework'))!
	defer {
		os.rmdir_all(dir) or {}
	}
	foundation := os.join_path(sdk_frameworks, 'Foundation.framework', 'Foundation.tbd')
	custom := os.join_path(custom_frameworks, 'Custom.framework', 'Custom')
	os.write_file(foundation, '')!
	os.write_file(custom, '')!
	flags := ['-o', 'out', 'src.c', '-F', custom_frameworks, '-framework', 'Foundation',
		'-F${custom_frameworks}', '-framework', 'Custom', '-framework', 'V3NoSuchFramework',
		'-weak_framework', 'Foundation', '-lobjc']
	assert v3_tcc_macos_framework_flags(flags, 'macos', sdk) == ['-o', 'out', 'src.c', foundation,
		custom, '-framework', 'V3NoSuchFramework', '-weak_framework', 'Foundation', '-lobjc']
	assert v3_tcc_macos_framework_flags(flags, 'linux', sdk) == flags
}

// The macos module's Objective-C bridge must not stop the bundled tcc from
// building programs that include the SDK runtime headers themselves.
fn test_implicit_tcc_builds_programs_using_the_macos_module() {
	$if !macos {
		return
	}
	vexe := @VEXE
	bundled_tcc := os.join_path(os.dir(vexe), 'thirdparty', 'tcc', 'tcc.exe')
	if !os.is_file(bundled_tcc) {
		eprintln('skipping: ${vexe} has no bundled tcc')
		return
	}
	sdk := macos_sdk_root()
	if sdk == '' || !os.is_file(os.join_path(sdk, 'System', 'Library', 'Frameworks',
		'Foundation.framework', 'Foundation.tbd')) {
		eprintln('skipping: no macOS SDK with a Foundation.tbd stub')
		return
	}
	dir := os.join_path(os.vtmp_dir(), 'v_implicit_tcc_macos_${os.getpid()}')
	os.mkdir_all(dir)!
	defer {
		os.rmdir_all(dir) or {}
	}
	source := os.join_path(dir, 'objc_class.v')
	os.write_file(source, "import macos\n\n#include <objc/runtime.h>\n\nfn C.class_getName(voidptr) &char\n\nfn main() {\n\tprintln(unsafe { cstring_to_vstring(C.class_getName(macos.get_class('NSString'))) })\n}\n")!
	exe := os.join_path(dir, 'objc_class')
	old_vflags := os.getenv_opt('VFLAGS')
	old_vosargs := os.getenv_opt('VOSARGS')
	os.unsetenv('VFLAGS')
	os.unsetenv('VOSARGS')
	defer {
		if value := old_vflags {
			os.setenv('VFLAGS', value, true)
		}
		if value := old_vosargs {
			os.setenv('VOSARGS', value, true)
		}
	}
	build := cmdexec.run(vexe, ['-nocache', '-o', exe, source])
	assert build.exit_code == 0, build.output
	assert !build.output.contains('implicit tcc could not be used'), build.output
	run := cmdexec.run(exe, [])
	assert run.exit_code == 0, run.output
	assert run.output.trim_space() == 'NSString'
}
