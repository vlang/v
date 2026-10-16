module c

import os
import time
import v.cmdexec
import v.flat
import v.pref

fn windows_tcc_test_gen(ccompiler string, output_cross_c bool) FlatGen {
	mut g := FlatGen.new()
	g.a = &flat.FlatAst{}
	g.target = pref.target_from('windows', 'amd64') or { panic(err) }
	g.set_ccompiler(ccompiler)
	g.set_output_cross_c(output_cross_c)
	return g
}

// A unit generated for Windows TCC includes V's WinAPI atomic compatibility
// header, which includes <windows.h> itself, so it must use the system libc
// preamble: the standalone preamble's `typedef u32 DWORD;` and its SDK records
// are not compatible redeclarations of the SDK's own (vlang/tccbin windef.h:94,
// "incompatible redefinition of 'DWORD'").
fn test_windows_tcc_atomic_header_selects_the_system_preamble() {
	for ccompiler in ['tinyc', 'tcc', 'C:/v/thirdparty/tcc/tcc.exe', 'S:\\tools\\TCC\\tcc.exe'] {
		g := windows_tcc_test_gen(ccompiler, false)
		assert g.uses_windows_tcc_atomic_header(), ccompiler
		assert g.c_directives_use_system_libc(), ccompiler
	}
	// Other compilers take the atomics from builtins and stay headerless.
	for ccompiler in ['', 'gcc', 'clang', 'msvc', 'C:/mingw64/bin/gcc.exe'] {
		g := windows_tcc_test_gen(ccompiler, false)
		assert !g.uses_windows_tcc_atomic_header(), ccompiler
		assert !g.c_directives_use_system_libc(), ccompiler
	}
	// A portable snapshot leaves the choice to the C preprocessor.
	assert !windows_tcc_test_gen('tcc', true).uses_windows_tcc_atomic_header()
	// Only Windows units include the WinAPI header.
	mut linux := windows_tcc_test_gen('tcc', false)
	linux.target = pref.target_from('linux', 'amd64') or { panic(err) }
	assert !linux.uses_windows_tcc_atomic_header()
}

// The failing shape: a program that uses the builtin (so it is not a literal
// output program) and has no header of its own, built with the bundled TCC.
fn test_windows_tcc_builds_a_program_without_its_own_headers() ! {
	$if !windows {
		return
	}
	tcc := os.join_path(@VEXEROOT, 'thirdparty', 'tcc', 'tcc.exe')
	if !os.is_file(tcc) {
		eprintln('skipping: no bundled tcc at ${tcc}')
		return
	}
	root := os.join_path(os.vtmp_dir(), 'windows_tcc_atomic_preamble_${os.getpid()}_${time.now().unix_nano()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	source := os.join_path(root, 'hello.v')
	os.write_file(source, "fn main() { message := 'Hello, World!'; println(message) }\n")!
	c_file := os.join_path(root, 'hello.c')
	generated := cmdexec.run(@VEXE, ['-nocache', '-no-retry-compilation', '-cc', tcc, '-o', c_file,
		source])
	assert generated.exit_code == 0, generated.output
	code := os.read_file(c_file)!
	atomic_header := code.index('/thirdparty/stdatomic/win/atomic.h"') or {
		assert false, 'the Windows TCC unit must include the WinAPI atomic header'
		return
	}
	windows_header := code.index('#include <windows.h>') or {
		assert false, 'the Windows TCC unit must include <windows.h> itself'
		return
	}
	assert windows_header < atomic_header, 'windows.h must precede the atomic header'
	assert !code.contains('typedef u32 DWORD;'), 'the standalone SDK types must not be emitted'
	exe := os.join_path(root, 'hello.exe')
	compiled := cmdexec.run(@VEXE, ['-nocache', '-no-retry-compilation', '-cc', tcc, '-o', exe,
		source])
	assert compiled.exit_code == 0, compiled.output
	assert !compiled.output.contains('incompatible redefinition'), compiled.output
	run := cmdexec.run(exe, [])
	assert run.exit_code == 0, run.output
	assert run.output.trim_space() == 'Hello, World!', run.output
}
