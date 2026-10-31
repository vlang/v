module c

import os
import time
import v.cmdexec
import v.flat
import v.pref
import v.types

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
		assert g.uses_windows_sdk_atomic_header(), ccompiler
		assert g.c_directives_use_system_libc(), ccompiler
	}
	// MSVC names `LONG` and the Interlocked functions in the prealloc helpers and
	// takes the WinAPI header behind a preprocessor choice, so it needs <windows.h>
	// first as well, but never the unconditional TCC include.
	msvc := windows_tcc_test_gen('msvc', false)
	assert !msvc.uses_windows_tcc_atomic_header()
	assert msvc.uses_windows_sdk_atomic_header()
	assert msvc.c_directives_use_system_libc()
	// Other compilers take the atomics from builtins and stay headerless.
	for ccompiler in ['', 'gcc', 'clang', 'C:/mingw64/bin/gcc.exe'] {
		g := windows_tcc_test_gen(ccompiler, false)
		assert !g.uses_windows_tcc_atomic_header(), ccompiler
		assert !g.uses_windows_sdk_atomic_header(), ccompiler
		assert !g.c_directives_use_system_libc(), ccompiler
	}
	// A portable snapshot leaves the choice to the C preprocessor.
	assert !windows_tcc_test_gen('tcc', true).uses_windows_sdk_atomic_header()
	assert !windows_tcc_test_gen('msvc', true).uses_windows_sdk_atomic_header()
	// Only Windows units include the WinAPI header.
	for ccompiler in ['tcc', 'msvc'] {
		mut linux := windows_tcc_test_gen(ccompiler, false)
		linux.target = pref.target_from('linux', 'amd64') or { panic(err) }
		assert !linux.uses_windows_sdk_atomic_header(), ccompiler
	}
}

// The driver's damaged-manual-header probe builds its own generator before C
// generation starts, so it has to reach the same preamble decision.
fn test_manual_header_probe_follows_the_windows_tcc_preamble_choice() {
	mut ast := flat.FlatAst.new()
	mut tc := types.TypeChecker.new(&ast)
	mut prefs := pref.new_preferences()
	prefs.target = pref.target_from('windows', 'amd64') or { panic(err) }
	for ccompiler in ['tinyc', 'tcc', 'C:/v/thirdparty/tcc/tcc.exe', 'msvc'] {
		prefs.ccompiler = ccompiler
		assert uses_manual_stdlib_c_headers(&ast, &tc, prefs, [], false), ccompiler
	}
	for ccompiler in ['', 'gcc', 'clang'] {
		prefs.ccompiler = ccompiler
		assert !uses_manual_stdlib_c_headers(&ast, &tc, prefs, [], false), ccompiler
	}
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

// The MSVC side of the same preamble: a headerless unit named `LONG` and
// `InterlockedExchangeAdd` in the prealloc helpers with no <windows.h> in scope
// (C4013, C2065).
fn test_windows_msvc_builds_a_program_without_its_own_headers() ! {
	$if !windows {
		return
	}
	vswhere := os.join_path(os.getenv('ProgramFiles(x86)'), 'Microsoft Visual Studio', 'Installer',
		'vswhere.exe')
	if !os.is_file(vswhere) {
		eprintln('skipping: no Visual Studio installer at ${vswhere}')
		return
	}
	root := os.join_path(os.vtmp_dir(), 'windows_msvc_atomic_preamble_${os.getpid()}_${time.now().unix_nano()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	source := os.join_path(root, 'bare.v')
	os.write_file(source, 'fn main() {}\n')!
	// Build first: an installed but unusable Visual Studio is reported by the
	// driver before any C is compiled, and is a reason to skip, not to fail.
	exe := os.join_path(root, 'bare.exe')
	compiled := cmdexec.run(@VEXE, ['-nocache', '-no-retry-compilation', '-cc', 'msvc', '-o', exe,
		source])
	if compiled.exit_code != 0 && compiled.output.contains('MSVC')
		&& !compiled.output.contains('src.c(') {
		eprintln('skipping: MSVC is installed but not usable here: ${compiled.output}')
		return
	}
	assert compiled.exit_code == 0, compiled.output
	assert !compiled.output.contains('C4013'), compiled.output
	run := cmdexec.run(exe, [])
	assert run.exit_code == 0, run.output
	c_file := os.join_path(root, 'bare.c')
	generated := cmdexec.run(@VEXE, ['-nocache', '-no-retry-compilation', '-cc', 'msvc', '-o',
		c_file, source])
	assert generated.exit_code == 0, generated.output
	code := os.read_file(c_file)!
	windows_header := code.index('#include <windows.h>') or {
		assert false, 'the Windows MSVC unit must include <windows.h> itself'
		return
	}
	interlocked := code.index('InterlockedExchangeAdd(') or {
		assert false, 'the MSVC prealloc helpers must still be emitted'
		return
	}
	assert windows_header < interlocked, 'windows.h must precede the Interlocked helpers'
	assert !code.contains('typedef u32 DWORD;'), 'the standalone SDK types must not be emitted'
}
