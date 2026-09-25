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
