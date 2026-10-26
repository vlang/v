module c

import os
import v.cmdexec
import v.flat
import v.parser
import v.pref
import v.types

fn windows_literal_output_source(program string) ?string {
	return platform_literal_output_source(program, 'windows')
}

fn platform_literal_output_source(program string, target_os string) ?string {
	root := os.join_path(os.vtmp_dir(), '${target_os}_literal_gen_${os.getpid()}')
	os.mkdir_all(root) or { panic(err) }
	defer { os.rmdir_all(root) or {} }
	bindings := os.join_path(root, 'bindings.c.v')
	source := os.join_path(root, 'main.v')
	os.write_file(bindings, 'module builtin
fn C.puts(const_msg &char) i32
fn C.write(fd i32, const_buf voidptr, count u32) i32
fn C._setmode(int, int) int
fn C.__android_log_print(priority i32, const_tag &char, const_format &char, opt ...voidptr) i32
fn C.WrappedNSLog(const_format &char, opt ...voidptr)
') or { panic(err) }
	os.write_file(source, program) or { panic(err) }
	mut prefs := pref.new_preferences()
	prefs.target = pref.target_from(target_os, 'amd64') or { panic(err) }
	mut p := parser.Parser.new(prefs)
	p.parse_file(bindings)
	p.parse_file(source)
	assert p.diagnostics.len == 0, p.diagnostics.str()
	mut a := p.a
	mut tc := types.TypeChecker.new(a)
	tc.diagnostic_files[source] = true
	mut g := FlatGen.new()
	g.a = a
	g.tc = &tc
	g.set_target(prefs.target)
	g.set_ccompiler('msvc')
	g.set_skip_enum_autostr(true)
	return g.literal_output_program()
}

fn test_windows_literal_hello_has_no_headers_or_gc() {
	code := windows_literal_output_source('fn main() { println("hello world") }') or {
		panic('Windows ASCII hello must use minimal output')
	}
	assert code.len < 256, code
	// The driver links Windows executables with -municode, so the CRT calls wmain.
	assert code.contains('int wmain(void) {'), code
	assert !code.contains('int main('), code
	assert code.contains('int puts(const char* const_msg);'), code
	assert code.contains('_setmode(1, 0x8000);'), code
	assert !code.contains('#include') && !code.contains('GC_') && !code.contains('malloc'), code
	assert !code.contains('__SIZE_TYPE__') && !code.contains('__PTRDIFF_TYPE__'), code
	if _ := windows_literal_output_source('fn main() { println("привет") }') {
		assert false, 'Unicode console output needs the ordinary Windows runtime'
	}
}

fn test_windows_literal_output_retries_writes_and_preserves_bytes_and_order() ! {
	code := windows_literal_output_source('fn main() { print("a\\x00b"); eprintln("c"); print("d") }') or {
		panic('Windows ASCII stream output must use minimal output')
	}
	assert code.contains('int _write(int fd, const void* const_buf, unsigned int count);'), code
	assert code.contains('int wmain(void) {'), code
	assert !code.contains('#include') && !code.contains('GC_'), code
	assert !code.contains('__SIZE_TYPE__') && !code.contains('__PTRDIFF_TYPE__'), code
	stub := '#include <assert.h>
#include <string.h>
int literal_main(void);
static int modes;
static int calls;
static const unsigned char expected[] = {97, 0, 98, 99, 10, 100};
static const int fds[] = {1, 1, 2, 1};
static const unsigned int counts[] = {3, 1, 2, 1};
static unsigned int consumed;
int _setmode(int fd, int mode) { assert(fd == ++modes && mode == 0x8000); return 0; }
int _write(int fd, const void* data, unsigned int count) {
 assert(calls < 4 && fd == fds[calls] && count == counts[calls]);
 unsigned int size = count > 2 ? 2 : count;
 assert(memcmp(data, expected + consumed, size) == 0);
 consumed += size;
 calls++;
 return (int)size;
}
int main(void) {
 assert(literal_main() == 0);
 assert(modes == 2 && calls == 4 && consumed == sizeof(expected));
 return 0;
}
'
	run_literal_native_stub('ordered', code, stub, [])!
}

fn test_windows_literal_output_caps_crt_count_at_int_max() ! {
	code := windows_literal_output_source('print("x")') or { panic('missing minimal output') }
	// Inject a large remaining length without allocating a multi-gigabyte literal.
	// The failing sink must see the bounded CRT count before the loop exits.
	large := code.replace('unsigned long long n = 1;', 'unsigned long long n = 2147483648ULL;')
	assert large != code
	stub := '#include <assert.h>
int literal_main(void);
static int calls;
int _setmode(int fd, int mode) { assert(fd == 1 && mode == 0x8000); return 0; }
int _write(int fd, const void* data, unsigned int count) {
 assert(fd == 1 && data && count == 2147483647U);
 calls++;
 return -1;
}
int main(void) { assert(literal_main() == 0 && calls == 1); return 0; }
'
	run_literal_native_stub('large', large, stub, [])!
}

fn run_literal_native_stub(name string, code string, stub string, extra_flags []string) ! {
	root := os.join_path(os.vtmp_dir(), 'literal_native_stub_${name}_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	generated := os.join_path(root, 'generated.c')
	sink := os.join_path(root, 'sink.c')
	binary := os.join_path(root, 'probe' + $if windows { '.exe' } $else { '' })
	// Windows literal programs enter through wmain; the stub supplies its own main.
	os.write_file(generated, code.replace_each(['int wmain(void)', 'int literal_main(void)',
		'int main(void)', 'int literal_main(void)']))!
	os.write_file(sink, stub)!
	compiler := $if windows { 'gcc' } $else { 'cc' }
	compiled := cmdexec.run(compiler, ['-std=c11', '-Werror', '-Wall', '-Wextra', ...extra_flags,
		'-o', binary, generated, sink])
	assert compiled.exit_code == 0, compiled.output
	run := cmdexec.run(binary, [])
	assert run.exit_code == 0, run.output
}

fn test_android_literal_output_preserves_log_priority_format_tag_and_order() ! {
	code := platform_literal_output_source('fn main() { print("100%"); println("a\\x00b"); eprint("c"); eprintln("d") }', 'android') or {
		panic('Android literals must use the native logger')
	}
	assert code.contains('int __android_log_print('), code
	assert !code.contains('#include') && !code.contains('GC_') && !code.contains('puts('), code
	stub := '#include <assert.h>
#include <stdarg.h>
#include <stdio.h>
#include <string.h>
#ifndef EXPECTED_TAG
#define EXPECTED_TAG "\\\"V\\\""
#endif
int literal_main(void);
static int calls;
static const int priorities[] = {4, 4, 6, 6};
static const char* formats[] = {"%.*s\\n", "%.*s\\n", "%.*s", "%.*s\\n"};
static const char* messages[] = {"100%\\n", "a\\n", "c", "d\\n"};
int __android_log_print(int priority, const char* tag, const char* format, ...) {
 assert(calls < 4 && priority == priorities[calls]);
 assert(strcmp(tag, EXPECTED_TAG) == 0 && strcmp(format, formats[calls]) == 0);
 char output[32];
 va_list args;
 va_start(args, format);
 int result = vsnprintf(output, sizeof(output), format, args);
 va_end(args);
 assert(strcmp(output, messages[calls]) == 0);
 calls++;
 return result;
}
int main(void) { assert(literal_main() == 0 && calls == 4); return 0; }
'
	for flags in [([]string{}), ['-DAPPNAME=sample', '-DEXPECTED_TAG="sample"'],
		['-DV_ANDROID_LOG_TAG=chosen', '-DEXPECTED_TAG="chosen"']] {
		run_literal_native_stub('android', code, stub, flags)!
	}
}

fn test_ios_literal_output_preserves_native_logger_and_order() ! {
	code := platform_literal_output_source('fn main() { print("a"); eprint("b"); println("c"); eprintln("привет") }', 'ios') or {
		panic('iOS literals must use the native logger')
	}
	assert code.contains('void WrappedNSLog(const char* const_format, ...);'), code
	assert !code.contains('#include') && !code.contains('GC_') && !code.contains('puts('), code
	if _ := platform_literal_output_source('println("100%")', 'ios') {
		assert false, 'format directives need the ordinary iOS runtime'
	}
	stub := '#include <assert.h>
#include <string.h>
int literal_main(void);
static int calls;
static const char* messages[] = {"a", "b", "c", "привет"};
void WrappedNSLog(const char* format, ...) {
 assert(calls < 4 && strcmp(format, messages[calls]) == 0);
 calls++;
}
int main(void) { assert(literal_main() == 0 && calls == 4); return 0; }
'
	run_literal_native_stub('ios', code, stub, [])!
}

fn test_literal_platform_loggers_link_only_their_native_dependencies() {
	mut g := FlatGen.new()
	g.compiler_vroot = @VEXEROOT
	g.set_target(pref.target_from('android', 'arm64')!)
	assert g.literal_output_c_flags() == ['-llog']
	g.set_target(pref.target_from('ios', 'arm64')!)
	assert g.literal_output_c_flags() == [
		os.join_path(@VEXEROOT, 'thirdparty', 'ios', 'ios.o'),
		'-framework',
		'Foundation',
	]
	for source in [os.join_path(@VEXEROOT, 'vlib', 'builtin', 'cfns.c.v'), '/cache/builtin.vh'] {
		mut a := flat.FlatAst.new()
		a.add_node(flat.Node{ kind: .file, value: source })
		a.add_node(flat.Node{ kind: .c_fn_decl, value: 'WrappedNSLog' })
		if source.ends_with('.vh') {
			a.cached_header_sources[source] = os.join_path(@VEXEROOT, 'vlib', 'builtin', 'array.v')
		}
		g.a = &a
		g.compiler_vroot = ''
		assert g.literal_output_c_flags()[0] == os.join_path(@VEXEROOT, 'thirdparty', 'ios', 'ios.o')
	}
}

fn test_windows_literal_cli_output_matches_native_crt_declarations() ! {
	root := os.join_path(os.vtmp_dir(), 'windows_literal_cli_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	source := os.join_path(root, 'main.v')
	c_file := os.join_path(root, 'main.c')
	oracle := os.join_path(root, 'oracle.c')
	object := os.join_path(root, 'main.o')
	mingw := os.find_abs_path_of_executable('x86_64-w64-mingw32-gcc') or { '' }
	for program in ['fn main() { println("hello world") }',
		'fn main() { print("a\\x00b"); eprintln("c"); print("d") }'] {
		os.write_file(source, program)!
		for cc in ['clang', 'msvc'] {
			generated := cmdexec.run(@VEXE, ['-new-compiler', '-nocache', '-os', 'windows', '-arch',
				'amd64', '-cc', cc, '-o', c_file, source])
			assert generated.exit_code == 0, generated.output
			code := os.read_file(c_file)!
			assert code.len < 2048, code
			assert !code.contains('#include') && !code.contains('GC_') && !code.contains('malloc'), code
			assert !code.contains('__SIZE_TYPE__') && !code.contains('__PTRDIFF_TYPE__'), code
			if mingw.len > 0 {
				// Rename only the oracle's declarations: SDK dllimport attributes concern
				// linkage, while typed pointer assignments check the generated ABI.
				renamed := code.replace_each(['_setmode(', 'oracle_setmode(', '_write(', 'oracle_write(',
					'puts(', 'oracle_puts('])
				mut checks := '\n__typeof__(&oracle_setmode) abi_setmode = &_setmode;\n'
				if code.contains('_write(') {
					checks += '__typeof__(&oracle_write) abi_write = &_write;\n'
				}
				if code.contains('puts(') {
					checks += '__typeof__(&oracle_puts) abi_puts = &puts;\n'
				}
				os.write_file(oracle, '#include <stdio.h>\n#include <io.h>\n#include <fcntl.h>\n#if _O_BINARY != 0x8000\n#error unexpected binary mode\n#endif\n' + renamed + checks)!
				for file in [c_file, oracle] {
					compiled := cmdexec.run(mingw, ['-std=c11', '-Werror', '-Wall', '-Wextra',
						'-c', file, '-o', object])
					assert compiled.exit_code == 0, compiled.output
				}
			}
		}
	}
	os.write_file(source, 'fn main() { println("привет") }')!
	generated := cmdexec.run(@VEXE, ['-new-compiler', '-nocache', '-gc', 'none', '-os', 'windows',
		'-arch', 'amd64', '-o', c_file, source])
	assert generated.exit_code == 0, generated.output
	assert os.read_file(c_file)!.contains('MultiByteToWideChar'), 'Unicode console support must remain reachable'
}

fn test_portable_literal_output_retains_snapshot_runtime_and_metadata() {
	$if !macos && !linux && !freebsd && !openbsd {
		return
	}
	root := os.join_path(os.vtmp_dir(), 'literal_cross_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	source := os.join_path(root, 'main.v')
	code_file := os.join_path(root, 'main.c')
	os.write_file(source, "fn main() { println('portable literal') }\n")!
	for flags in [['-os', 'cross'], ['-cross']] {
		mut args := flags.clone()
		args << ['-o', code_file, source]
		generated := cmdexec.run(@VEXE, args)
		assert generated.exit_code == 0, generated.output
		code := os.read_file(code_file)!
		// The C compiler's target decides the width of `int`.
		assert !code.contains('#error "this C was generated by `v -os cross`'), flags.str()
		assert code.contains('typedef i32 __v_int;'), flags.str()
		assert code.contains('#define CUSTOM_DEFINE_cross'), flags.str()
		assert code.contains('#define CUSTOM_DEFINE_no_backtrace'), flags.str()
		assert code.contains('static inline byte atomic_fetch_add_byte('), flags.str()
		assert code.contains('portable literal'), flags.str()
	}
	// Ordinary host output remains allocation-free after snapshot generation.
	generated := cmdexec.run(@VEXE, ['-o', code_file, source])
	assert generated.exit_code == 0, generated.output
	code := os.read_file(code_file)!
	assert code.len < 256 && code.contains('puts("portable literal")'), code
	assert !code.contains('atomic_') && !code.contains('GC_'), code
}

fn test_literal_output_keeps_bytes_streams_and_call_order() {
	$if !macos && !linux && !freebsd && !openbsd {
		return
	}
	root := os.join_path(os.vtmp_dir(), 'literal_output_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	source := os.join_path(root, 'main.v')
	code_file := os.join_path(root, 'main.c')
	binary := os.join_path(root, 'main')
	os.write_file(source, 'fn main() {
	print("100% ")
	println("one\\x00two")
	println(r"\\n\\t")
	eprint("error ")
	eprintln("привет")
	println("quoted \\"text\\" \\\\")
}
')!
	generated := cmdexec.run(@VEXE, ['-o', code_file, source])
	assert generated.exit_code == 0, generated.output
	code := os.read_file(code_file)!
	assert code.contains('__PTRDIFF_TYPE__ write('), code
	assert !code.contains('GC_') && !code.contains('malloc') && !code.contains('#include'), code
	for cc in ['clang', 'tcc'] {
		compiled := cmdexec.run(@VEXE, ['-cc', cc, '-showcc', '-no-retry-compilation', '-o', binary,
			source])
		assert compiled.exit_code == 0, compiled.output
		assert !compiled.output.contains('atomic_') && !compiled.output.contains('libgc'), compiled.output
		mut process := os.new_process(binary)
		process.set_redirect_stdio()
		process.run()
		stdout := process.stdout_slurp()
		stderr := process.stderr_slurp()
		process.wait()
		assert process.code == 0, process.err
		process.close()
		assert stdout == '100% one\x00two\n\\n\\t\nquoted "text" \\\n', stdout.bytes().str()
		assert stderr == 'error привет\n', stderr
	}
	// The emitted C needs no runtime objects, collector, or extra library flags.
	compiled_c := cmdexec.run('clang', ['-Werror', '-Wall', '-Wextra', '-o', binary, code_file])
	assert compiled_c.exit_code == 0, compiled_c.output
}

fn test_literal_output_with_empty_strings_and_explicit_main() {
	$if !macos && !linux && !freebsd && !openbsd {
		return
	}
	root := os.join_path(os.vtmp_dir(), 'literal_empty_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	source := os.join_path(root, 'main.v')
	code_file := os.join_path(root, 'main.c')
	for program in ['fn main() { println("") }', 'print("")\neprint("")'] {
		os.write_file(source, program)!
		generated := cmdexec.run(@VEXE, ['-o', code_file, source])
		assert generated.exit_code == 0, generated.output
		code := os.read_file(code_file)!
		assert code.len < 256 && !code.contains('gc_'), code
		run := cmdexec.run(@VEXE, ['-no-retry-compilation', 'run', source])
		assert run.exit_code == 0, run.output
		assert run.output == if program.contains('println') { '\n' } else { '' }, run.output
	}
}

fn test_literal_output_with_file_list_preserves_closure_initialization() {
	$if !macos && !linux && !freebsd && !openbsd {
		return
	}
	root := os.join_path(os.vtmp_dir(), 'literal_file_list_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	source := os.join_path(root, 'main.v')
	listed_source := os.join_path(root, 'extra.v')
	os.write_file(source, 'println("main literal")\n')!
	os.write_file(listed_source, 'module main
fn init() {
	value := "closure init"
	captured := fn [value] () { println(value) }
	captured()
}
')!
	run := cmdexec.run(@VEXE, ['-nocache', '-cc', 'clang', '-gc', 'none', '-no-retry-compilation',
		'-file-list', listed_source, 'run', source])
	assert run.exit_code == 0, run.output
	assert run.output == 'closure init\nmain literal\n', run.output
}

fn test_literal_output_preserves_freestanding_runtime() {
	$if !macos && !linux && !freebsd && !openbsd {
		return
	}
	root := os.join_path(os.vtmp_dir(), 'literal_freestanding_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	source := os.join_path(root, 'main.v')
	code_file := os.join_path(root, 'main.c')
	os.write_file(source, 'fn main() { println("freestanding literal") }\n')!
	for flags in [['-freestanding'], ['-d', 'freestanding']] {
		mut args := ['-nocache', '-gc', 'none']
		args << flags
		args << ['-o', code_file, source]
		generated := cmdexec.run(@VEXE, args)
		assert generated.exit_code == 0, generated.output
		code := os.read_file(code_file)!
		assert code.contains('void println(string s) {'), code
		assert code.contains('bare_print(s.str,'), code
	}
}

fn test_nonliteral_program_preserves_gc_and_initialization() {
	$if !macos && !linux && !freebsd && !openbsd {
		return
	}
	root := os.join_path(os.vtmp_dir(), 'literal_gc_guard_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	source := os.join_path(root, 'main.v')
	code_file := os.join_path(root, 'main.c')
	os.write_file(source, 'fn init() { println("init") }
fn cleanup() { println("cleanup") }
fn main() {
	mut values := []string{}
	values << "hello"
	gc_collect()
	println(values.join(" "))
}
')!
	generated := cmdexec.run(@VEXE, ['-gc', 'boehm', '-o', code_file, source])
	assert generated.exit_code == 0, generated.output
	code := os.read_file(code_file)!
	assert code.contains('gc_runtime_init'), code
	assert code.contains('v_gc_malloc'), code
	run := cmdexec.run(@VEXE, ['-gc', 'boehm', '-no-retry-compilation', 'run', source])
	assert run.exit_code == 0, run.output
	assert run.output == 'init\nhello\ncleanup\n', run.output
}

fn test_literal_output_retries_partial_writes() {
	$if !macos && !linux && !freebsd && !openbsd {
		return
	}
	root := os.join_path(os.vtmp_dir(), 'literal_partial_write_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	source := os.join_path(root, 'main.v')
	code_file := os.join_path(root, 'main.c')
	stub_file := os.join_path(root, 'partial.c')
	binary := os.join_path(root, 'main')
	os.write_file(source, 'println("abcd\\x00e")')!
	generated := cmdexec.run(@VEXE, ['-o', code_file, source])
	assert generated.exit_code == 0, generated.output
	// A short-writing sink independently checks every byte and the remaining
	// length on each call, including the NUL byte and appended newline.
	os.write_file(stub_file, '#include <assert.h>
#include <stddef.h>
#include <string.h>
static const unsigned char expected[] = {97, 98, 99, 100, 0, 101, 10};
static size_t consumed;
static int calls;
long write(int fd, const void* buffer, size_t count) {
 assert(fd == 1 && count == sizeof(expected) - consumed);
 size_t size = count > 2 ? 2 : count;
 assert(memcmp(buffer, expected + consumed, size) == 0);
 consumed += size;
 calls++;
 return (long)size;
}
__attribute__((destructor)) static void verify(void) {
 assert(consumed == sizeof(expected) && calls == 4);
}
')!
	compiled := cmdexec.run('clang', ['-Werror', '-Wall', '-Wextra', '-o', binary, code_file,
		stub_file])
	assert compiled.exit_code == 0, compiled.output
	run := cmdexec.run(binary, [])
	assert run.exit_code == 0, run.output
}
