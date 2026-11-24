import os

// Compile the production V helpers for Darwin, then execute those exact helper
// definitions against an independent Linux ABI oracle. This checks portable
// startup logic without claiming to execute Darwin's syscall or Mach-O loader.
fn test_macos_v_startup_helpers_on_linux() ! {
	$if !linux || !amd64 {
		return
	}
	clang := os.find_abs_path_of_executable('clang') or { return }
	root := os.join_path(os.vtmp_dir(), 'macos_v_startup_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	source := os.join_path(root, 'main.v')
	generated_path := os.join_path(root, 'main.c')
	os.write_file(source, 'fn finish() {}\nfn main() { at_exit(finish) or { panic(err) } }\n')!
	vexe := os.getenv_opt('VEXE') or { @VEXE }
	generated := os.exec([vexe, '-os', 'macos', '-arch', 'amd64', '-freestanding', '-gc', 'none',
		'-o', generated_path, source])
	assert generated.exit_code == 0, generated.output
	c_source := os.read_file(generated_path)!
	assert macos_public_at_exit_body(c_source).contains('atexit(')
	// The complete runtime also has memory/thread helpers with this prefix.
	// Select declarations owned by the startup source, not an entire namespace.
	mut startup_names := []string{}
	for file in ['startup_d_freestanding.c.v', 'atexit_d_freestanding.c.v'] {
		startup_path := os.join_path(@VEXEROOT, 'vlib', 'builtin', 'macos_bare', file)
		for raw_line in os.read_file(startup_path)!.split_into_lines() {
			line := raw_line.trim_space()
			if line.starts_with('fn bare_macos_') {
				startup_names << line.all_after('fn ').all_before('(')
			}
		}
	}
	assert startup_names.len > 0
	// Exclusions on a caller do not propagate to the syscall transport that
	// runs before GS is installed. Check the actual instrumented leaf chain.
	instrumented_path := os.join_path(root, 'instrumented.c')
	instrumented := os.exec([vexe, '-os', 'macos', '-arch', 'amd64', '-freestanding', '-gc', 'none',
		'-profile', '-trace-calls', '-o', instrumented_path, source])
	assert instrumented.exit_code == 0, instrumented.output
	instrumented_c := os.read_file(instrumented_path)!
	mut early_functions := []string{}
	for line in instrumented_c.split_into_lines() {
		if !line.starts_with('\t') && (startup_names.any(line.contains(' ${it}(')) || line.contains(' sys_call')) && line.ends_with(' {') {
			early_functions << line
			body := instrumented_c.all_after(line).all_before('\n}\n')
			assert !body.contains('_PROF_FN_START'), line
			assert !body.contains('trace_calls__on_call'), line
		}
	}
	assert early_functions.any(it.contains('sys_call2('))
	assert early_functions.any(it.contains('sys_call6('))
	// ARM64 and host-loaded libraries must retain their existing startup.
	for control in ['arm64', 'shared', 'object', 'hosted'] {
		control_path := os.join_path(root, '${control}.c')
		mut args := [vexe, '-os', 'macos', '-arch', if control == 'arm64' {
			'arm64'
		} else {
			'amd64'
		}, '-gc', 'none', '-o', control_path]
		if control != 'hosted' { args << '-freestanding' }
		if control == 'shared' { args << '-shared' }
		if control == 'object' { args << '-is_o' }
		args << source
		build := os.exec(args)
		assert build.exit_code == 0, build.output
		control_source := os.read_file(control_path)!
		assert !control_source.contains('bare_macos_prepare_start('), control
		assert !control_source.contains('"_start:'), control
		assert !control_source.contains('bare_macos_atexit('), control
		assert macos_public_at_exit_body(control_source).contains('atexit(') == (control == 'hosted'), control
	}
	assert_macos_startup_oracle(c_source, startup_names, root, clang)!
	// Escaping registry nodes must survive compiler-managed local cleanup too.
	autofree_path := os.join_path(root, 'autofree.c')
	autofree := os.exec([vexe, '-os', 'macos', '-arch', 'amd64', '-freestanding', '-gc', 'none',
		'-autofree', '-o', autofree_path, source])
	assert autofree.exit_code == 0, autofree.output
	assert_macos_startup_oracle(os.read_file(autofree_path)!, startup_names, root, clang)!
	// Assemble the actual entry/thunk templates for the Mach-O target. No SDK
	// or Mac host is needed for this syntax/relocation check.
	asm_marker := '__asm__ (\n\t".section __TEXT,__text,regular,pure_instructions'
	assert c_source.contains(asm_marker)
	asm_definition := asm_marker + c_source.all_after(asm_marker).all_before('\n);') + '\n);\n'
	asm_path := os.join_path(root, 'entry.c')
	os.write_file(asm_path, asm_definition)!
	assembly := os.exec([clang, '-target', 'x86_64-apple-macos10.13', '-c', asm_path, '-o',
		os.join_path(root, 'entry.o')])
	assert assembly.exit_code == 0, assembly.output
}

fn assert_macos_startup_oracle(c_source string, startup_names []string, root string, clang string) ! {
	mut declarations := ''
	struct_names := ['BareMacProgramVars', 'BareMacTlsImage', 'BareMacTlvDescriptor', 'BareMacExitEntry']
	for name in struct_names { declarations += 'typedef struct ${name} ${name};\n' }
	for line in c_source.split_into_lines() {
		if line.starts_with('typedef void (*_fn_ptr_')
			&& (line.contains('BareMacProgramVars*') || line.ends_with('(void*);') || line.ends_with('(void);')) {
			declarations += line + '\n'
		}
	}
	for name in struct_names {
		marker := 'struct ${name} {'
		assert c_source.contains(marker)
		declarations += marker + c_source.all_after(marker).all_before('\n};') + '\n};\n'
	}
	for line in c_source.split_into_lines() {
		if !line.starts_with('\t') && line.contains(' g_bare_macos_') && line.ends_with(';') {
			declarations += line + '\n'
		}
		if line == 'i32 NXArgc;' || line == 'char** NXArgv;' || line == 'char** environ;'
			|| line == 'char* __progname;' {
			declarations += line + '\n'
		}
	}
	mut bodies := ''
	mut names := []string{}
	for line in c_source.split_into_lines() {
		name := macos_startup_c_definition_name(line, startup_names)
		if name.len > 0 {
			names << name
			declarations += line.trim_string_right(' {') + ';\n'
			body := c_source.all_after(line).all_before('\n}\n')
			if name != 'bare_macos_atexit' {
				assert !body.contains('malloc('), name
			}
			assert !body.contains('trace_call('), name
			bodies += line + body + '\n}\n'
		}
	}
	assert 'bare_macos_tls_prepare' in names && 'bare_macos_prepare_start' in names
	assert 'bare_macos_run_initializers' in names && 'bare_macos_run_finalizers' in names
	assert 'bare_macos_normal_exit' in names, names.str()
	message_symbol := bodies.all_after('string message = ').all_before(';')
	mut literal := ''
	for line in c_source.split_into_lines() {
		if line.starts_with('_SL(${message_symbol},') { literal = line + '\n' }
	}
	assert literal.len > 0, message_symbol
	os.write_file(os.join_path(root, 'generated_startup.h'), declarations + literal + bodies)!
	oracle := os.join_path(@VEXEROOT, 'vlib', 'builtin', 'testdata', 'macos_v_startup', 'oracle.c')
	for optimization in ['-O0', '-O3'] {
		executable := os.join_path(root, 'oracle')
		build := os.exec([clang, '-std=gnu11', optimization, '-Wall', '-Wextra', '-Werror', '-fno-pie',
			'-no-pie', '-pthread', '-Wl,--section-start=.oracle_header=0x200000', '-I', root, oracle,
			'-o', executable])
		assert build.exit_code == 0, build.output
		run := os.exec([executable])
		assert run.exit_code == 0, run.output
	}
}

fn test_at_exit_hosted_api_keeps_callbacks_and_captured_values() ! {
	$if !linux {
		return
	}
	root := os.join_path(os.vtmp_dir(), 'public_at_exit_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	vexe := os.getenv_opt('VEXE') or { @VEXE }
	source := os.join_path(root, 'main.v')
	os.write_file(source, 'import os
fn first() { println("first") }
fn second() { println("second") }
fn third() { println("third") }
fn main() {
	at_exit(first) or { panic(err) }
	at_exit(second) or { panic(err) }
	at_exit(third) or { panic(err) }
	at_exit(first) or { panic(err) }
	println("main")
	if os.args.len > 1 { exit(7) }
}
')!
	binary := os.join_path(root, 'main')
	built := os.exec([vexe, '-gc', 'none', '-cc', 'clang', '-o', binary, source])
	assert built.exit_code == 0, built.output
	for args in [[binary], [binary, 'explicit_exit']] {
		run := os.exec(args)
		assert run.exit_code == if args.len == 1 { 0 } else { 7 }, run.output
		assert run.output == 'main\nfirst\nthird\nsecond\nfirst\n', run.output
	}
	os.write_file(source, 'fn register(value int) {
	at_exit(fn [value] () { println(value) }) or { panic(err) }
}
fn main() {
	register(11)
	register(22)
	println("main")
}
')!
	captured := os.exec([vexe, '-gc', 'none', '-cc', 'clang', '-o', binary, source])
	assert captured.exit_code == 0, captured.output
	run := os.exec([binary])
	assert run.exit_code == 0, run.output
	assert run.output == 'main\n22\n11\n', run.output
}

fn macos_public_at_exit_body(source string) string {
	for line in source.split_into_lines() {
		if !line.starts_with('\t') && line.contains(' at_exit(') && line.ends_with(' {') {
			return source.all_after(line).all_before('\n}\n')
		}
	}
	assert false, 'missing public V at_exit implementation'
	return ''
}

// Exported declarators may be parenthesized to avoid SDK function-like macros.
fn macos_startup_c_definition_name(line string, startup_names []string) string {
	if line.starts_with('\t') || !line.ends_with(' {') { return '' }
	for name in startup_names {
		if line.contains(' ${name}(') || line.contains(' (${name})(') { return name }
	}
	for name in ['_NSGetArgc', '_NSGetArgv', '_NSGetEnviron', '_NSGetProgname',
		'_NSGetMachExecuteHeader', 'atexit'] {
		if line.contains(' ${name}(') || line.contains(' (${name})(') { return name }
	}
	return ''
}
