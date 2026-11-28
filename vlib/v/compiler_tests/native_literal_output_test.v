module main

import os
import v.cmdexec
import v.pref

fn supports_native_literal_output_tests() bool {
	return (os.user_os() == 'linux' && pref.host_arch() in ['amd64', 'arm64'])
		|| (os.user_os() == 'macos' && pref.host_arch() == 'arm64')
}

fn test_native_literal_output_preserves_bytes_streams_and_reduced_runtime() {
	if !supports_native_literal_output_tests() {
		return
	}
	root := os.join_path(os.vtmp_dir(), 'native literal output ${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	programs := [
		"println('Hello, World!')",
		"fn main() { print(''); eprint('') }",
		"fn main() { print(''); print('100% λ\\n'); eprint('err\\x00or'); println('a\\x00b'); eprintln('done'); println('') }",
		"module main\nfn main() { println(r'raw \\n'); print('\\t\\r\\n'); eprintln('привет') }",
	]
	stdout := ['Hello, World!\n', '', '100% λ\na\x00b\n\n', 'raw \\n\n\t\r\n']
	stderr := ['', '', 'err\x00ordone\n', 'привет\n']
	for index, program in programs {
		source := os.join_path(root, 'main.v')
		os.write_file(source, program)!
		for mode in ['', '-prod'] {
			output := os.join_path(root, 'hello_${index}_${mode}')
			mut args := ['-b', 'native', '-gc', 'none', '-o', output]
			if mode != '' { args << mode }
			args << source
			compiled := cmdexec.run(@VEXE, args)
			assert compiled.exit_code == 0, compiled.output
			out := os.join_path(root, 'stdout')
			err := os.join_path(root, 'stderr')
			run := cmdexec.run('sh', ['-c',
				'${os.quoted_path(output)} >${os.quoted_path(out)} 2>${os.quoted_path(err)}'])
			assert run.exit_code == 0, run.output
			assert os.read_file(out)! == stdout[index], '${index}: ${mode}'
			assert os.read_file(err)! == stderr[index], '${index}: ${mode}'
			assert os.file_size(output) < 65536, '${index}: ${mode}: runtime grew'
			symbols := cmdexec.run('nm', [output])
			assert symbols.exit_code == 0, symbols.output
			assert !symbols.output.contains('array_get')
			assert !symbols.output.contains('__autostr')
			assert !symbols.output.contains('print_backtrace')
		}
	}
}

fn test_native_nonliteral_and_debug_programs_keep_ordinary_runtime() {
	if !supports_native_literal_output_tests() {
		return
	}
	root := os.join_path(os.vtmp_dir(), 'native literal fallback ${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	source := os.join_path(root, 'main.v')
	for index, program in [
		"fn main() { text := 'hello'; println(text) }",
		"fn init() { print('init:') }\nfn main() { println('hello') }",
		"fn main() { println('hello') }",
	] {
		os.write_file(source, program)!
		output := os.join_path(root, 'hello_${index}')
		mut args := ['-b', 'native', '-o', output]
		if index == 2 { args << '-g' }
		args << source
		compiled := cmdexec.run(@VEXE, args)
		assert compiled.exit_code == 0, compiled.output
		run := cmdexec.run(output, []string{})
		assert run.exit_code == 0, run.output
		assert run.output == if index == 1 { 'init:hello\n' } else { 'hello\n' }
		symbols := cmdexec.run('nm', [output])
		assert symbols.exit_code == 0, symbols.output
		assert symbols.output.contains('print_backtrace')
	}
	object := os.join_path(root, 'hello.o')
	compiled := cmdexec.run(@VEXE, ['-b', 'native', '-o', object, source])
	assert compiled.exit_code == 0, compiled.output
	symbols := cmdexec.run('nm', [object])
	assert symbols.exit_code == 0, symbols.output
	assert symbols.output.contains('print_backtrace')
}

fn test_native_literal_output_retries_short_writes_and_stops_on_error() {
	if !supports_native_literal_output_tests() {
		return
	}
	root := os.join_path(os.vtmp_dir(), 'native literal short writes ${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	hook := os.join_path(root, 'short_write' + $if macos { '.dylib' } $else { '.so' })
	c_source := os.join_path(root, 'short_write.c')
	mut hook_source := '#include <unistd.h>
static ssize_t short_write(int fd, const void *p, size_t n) {
    static int failed;
    if (fd == 1 && !failed++) return -1;
    return write(fd, p, n > 2 ? 2 : n);
}
__attribute__((used, section("__DATA,__interpose")))
static const struct { const void *replacement; const void *original; } hook = {
    (const void *)short_write, (const void *)write
};
'
	mut link_flags := ['-dynamiclib']
	$if linux {
		hook_source = '#include <unistd.h>
#include <sys/syscall.h>
ssize_t write(int fd, const void *p, size_t n) {
    static int failed;
    if (fd == 1 && !failed++) return -1;
    return syscall(SYS_write, fd, p, n > 2 ? 2 : n);
}
'
		link_flags = ['-shared', '-fPIC']
	}
	os.write_file(c_source, hook_source)!
	linked := cmdexec.run('cc', [...link_flags, '-o', hook, c_source])
	assert linked.exit_code == 0, linked.output
	source := os.join_path(root, 'main.v')
	os.write_file(source, "fn main() { print('discard'); print('a\\x00b λ'); eprintln('stderr') }")!
	preload := $if macos { 'DYLD_INSERT_LIBRARIES' } $else { 'LD_PRELOAD' }
	// Linux splits LD_PRELOAD on spaces before expanding $ORIGIN.
	preload_path := $if macos { hook } $else { r'$ORIGIN/short_write.so' }
	for mode in ['', '-prod'] {
		output := os.join_path(root, 'hello_${mode}')
		mut args := ['-b', 'native', '-o', output]
		if mode != '' { args << mode }
		args << source
		compiled := cmdexec.run(@VEXE, args)
		assert compiled.exit_code == 0, compiled.output
		out := os.join_path(root, 'stdout')
		err := os.join_path(root, 'stderr')
		run := cmdexec.run_with_timeout('sh', ['-c',
			'${preload}=${os.quoted_path(preload_path)} ${os.quoted_path(output)} >${os.quoted_path(out)} 2>${os.quoted_path(err)}'], 5000)
		assert run.exit_code == 0, run.output
		assert os.read_file(out)! == 'a\x00b λ', mode
		assert os.read_file(err)! == 'stderr\n', mode
	}
}
