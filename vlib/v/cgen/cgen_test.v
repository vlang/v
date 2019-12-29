import (
	os
	filepath
	v.parser
	v.ast
	v.cgen
	v.table
	term
)

const (
	nr_tests = 2
)

fn test_c_files() {
	println('Running V => C tests')
	vexe := os.getenv('VEXE')
	vroot := filepath.dir(vexe)
	for i in 1 .. nr_tests + 1 {
		text := os.read_file('$vroot/vlib/v/cgen/tests/${i}.v') or {
			panic(err)
		}
		ctext := os.read_file('$vroot/vlib/v/cgen/tests/${i}.c') or {
			panic(err)
		}
		table := &table.Table{}
		program := parser.parse_file(text, table)
		res := cgen.gen(program)
		if compare_texts(res, ctext) {
			eprintln('${i}... ' + term.green('OK'))
		}
		else {
			eprintln('${i}... ' + term.red('FAIL'))
			eprintln('expected:\n$ctext\ngot:\n$res')
		}
	}
}

fn compare_texts(a, b string) bool {
	lines_a := a.trim_space().split_into_lines()
	lines_b := b.trim_space().split_into_lines()
	if lines_a.len != lines_b.len {
		println('different len')
		return false
	}
	for i, line_a in lines_a {
		line_b := lines_b[i]
		if line_a.trim_space() != line_b.trim_space() {
			println('!' + line_a)
			return false
		}
	}
	return true
}
