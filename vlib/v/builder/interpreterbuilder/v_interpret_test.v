import os
import rand
import term

const vexe = @VEXE

fn interpreter_wrap(a string) string {
	return 'fn main() {$a}'
}

fn interp_test(expression string, expected string) ? {
	tmpdir := os.join_path(os.temp_dir(), 'v_interpret_test_$rand.ulid()')
	os.mkdir_all(tmpdir) or {}
	defer {
		os.rmdir_all(tmpdir) or {}
	}
	//
	tmpfile := os.join_path(tmpdir, 'input.v')
	outfile := os.join_path(tmpdir, 'output.txt')
	os.write_file(tmpfile, interpreter_wrap(expression))?
	if os.system('${os.quoted_path(vexe)} interpret ${os.quoted_path(tmpfile)} > ${os.quoted_path(outfile)}') != 0 {
		eprintln('>>> Failed to interpret V expression: |$expression|')
		return error('v interp')
	}
	res := os.read_file(outfile)?
	output := res.trim_space()
	if output != expected {
		eprintln('>>> The output of the V expression, is not the same as the expected one')
		eprintln(' V expression: $expression')
		eprintln('       output: |$output|')
		eprintln('     expected: |$expected|')
		return error('test')
	}
	println('${term.colorize(term.green, 'OK')} ${term.colorize(term.bright_blue, expression.replace('\n',
		' '))}')
	println('   >> ${term.colorize(term.bright_yellow, output)}')
}

struct InterpTest {
	input  string
	output string
}

fn test_interpreter() ? {
	mut tests := []InterpTest{}
	tests << InterpTest{'println(3+3)', '6'}
	tests << InterpTest{'println(3)', '3'}
	tests << InterpTest{'println(3-4)', '-1'}
	tests << InterpTest{'println(3*3)', '9'}
	tests << InterpTest{'a := 3\nprintln(a*3)', '9'}
	for test in tests {
		interp_test(test.input, test.output)?
		assert true
	}
}
