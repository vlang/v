import os
import v.cmdexec

fn test_c23_boolean_keywords_compile_and_run() {
	$if !linux && !macos {
		return
	}
	test_dir := os.join_path(os.vtmp_dir(), 'compiler_c23_boolean_${os.getpid()}')
	os.mkdir_all(test_dir)!
	defer {
		os.rmdir_all(test_dir) or {}
	}
	probe := os.join_path(test_dir, 'keywords.c')
	// Some compilers accept a C23 draft flag without implementing its keywords.
	os.write_file(probe, 'int main(void) { bool value = true; return value == false; }\n')!
	source := os.join_path(test_dir, 'main.v')
	os.write_file(source, 'fn negate(value bool) bool {
	return !value
}

fn main() {
	values := [true, false, true]
	mut flipped := []bool{}
	for value in values {
		flipped << negate(value)
	}
	assert flipped == [false, true, false]
	println(negate(true))
	println(negate(false))
	println(flipped)
}
')!
	for name in ['gcc', 'clang'] {
		compiler := os.find_abs_path_of_executable(name) or { continue }
		mut dialect := ''
		for candidate in ['-std=gnu23', '-std=gnu2x'] {
			available := cmdexec.run(compiler, [candidate, '-fsyntax-only', probe])
			if available.exit_code == 0 {
				dialect = candidate
				break
			}
		}
		if dialect.len == 0 {
			continue
		}
		version := cmdexec.run(compiler, ['--version'])
		flags := if version.output.contains('clang') {
			'${dialect} -Werror=keyword-macro'
		} else {
			dialect
		}
		binary := os.join_path(test_dir, name)
		compiled := cmdexec.run(@VEXE, ['-new-compiler', '-nocache', '-gc', 'none', '-cstrict',
			'-cc', compiler, '-cflags', flags, '-o', binary, source])
		assert compiled.exit_code == 0, '${name} ${dialect}: ${compiled.output}'
		executed := cmdexec.run(binary, [])
		assert executed.exit_code == 0, executed.output
		assert executed.output == 'false\ntrue\n[false, true, false]\n', executed.output
	}
}
