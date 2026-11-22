module main

import os
import v.cmdexec

fn test_arm64_literal_output_preserves_bytes_streams_and_reduced_runtime() {
	$if !macos || !arm64 {
		return
	}
	root := os.join_path(os.vtmp_dir(), 'arm64 literal output ${os.getpid()}')
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
			mut args := ['-b', 'arm64', '-gc', 'none', '-o', output]
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
			assert !symbols.output.contains('_array_get')
			assert !symbols.output.contains('__autostr')
			assert !symbols.output.contains('_print_backtrace')
		}
	}
}

fn test_arm64_nonliteral_and_debug_programs_keep_ordinary_runtime() {
	$if !macos || !arm64 {
		return
	}
	root := os.join_path(os.vtmp_dir(), 'arm64 literal fallback ${os.getpid()}')
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
		mut args := ['-b', 'arm64', '-o', output]
		if index == 2 { args << '-g' }
		args << source
		compiled := cmdexec.run(@VEXE, args)
		assert compiled.exit_code == 0, compiled.output
		run := cmdexec.run(output, []string{})
		assert run.exit_code == 0, run.output
		assert run.output == if index == 1 { 'init:hello\n' } else { 'hello\n' }
		symbols := cmdexec.run('nm', [output])
		assert symbols.exit_code == 0, symbols.output
		assert symbols.output.contains('_print_backtrace')
	}
}

fn test_arm64_literal_output_retries_short_writes_and_stops_on_error() {
	$if !macos || !arm64 {
		return
	}
	root := os.join_path(os.vtmp_dir(), 'arm64 literal short writes ${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	hook := os.join_path(root, 'short_write.dylib')
	c_source := os.join_path(root, 'short_write.c')
	os.write_file(c_source, '#include <unistd.h>
static ssize_t short_write(int fd, const void *p, size_t n) {
    static int failed;
    if (fd == 1 && !failed++) return -1;
    return write(fd, p, n > 2 ? 2 : n);
}
__attribute__((used, section("__DATA,__interpose")))
static const struct { const void *replacement; const void *original; } hook = {
    (const void *)short_write, (const void *)write
};
')!
	linked := cmdexec.run('cc', ['-dynamiclib', '-o', hook, c_source])
	assert linked.exit_code == 0, linked.output
	source := os.join_path(root, 'main.v')
	os.write_file(source, "fn main() { print('discard'); print('a\\x00b λ'); eprintln('stderr') }")!
	for mode in ['', '-prod'] {
		output := os.join_path(root, 'hello_${mode}')
		mut args := ['-b', 'arm64', '-o', output]
		if mode != '' { args << mode }
		args << source
		compiled := cmdexec.run(@VEXE, args)
		assert compiled.exit_code == 0, compiled.output
		out := os.join_path(root, 'stdout')
		err := os.join_path(root, 'stderr')
		run := cmdexec.run_with_timeout('sh', ['-c',
			'DYLD_INSERT_LIBRARIES=${os.quoted_path(hook)} ${os.quoted_path(output)} >${os.quoted_path(out)} 2>${os.quoted_path(err)}'], 5000)
		assert run.exit_code == 0, run.output
		assert os.read_file(out)! == 'a\x00b λ', mode
		assert os.read_file(err)! == 'stderr\n', mode
	}
}
