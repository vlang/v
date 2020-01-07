import (
	os
	filepath
	v.parser
	v.ast
	v.gen
	v.table
	term
)

const (
	nr_tests = 3
)

fn test_c_files() {
	$if windows {
		return
	}
	println('Running V => C tests')
	vexe := os.getenv('VEXE')
	vroot := filepath.dir(vexe)
	for i in 1 .. nr_tests + 1 {
		path := '$vroot/vlib/v/gen/tests/${i}.vv'
		ctext := os.read_file('$vroot/vlib/v/gen/tests/${i}.c') or {
			panic(err)
		}
		table := &table.new_table()
		program := parser.parse_file(path, table)
		res := gen.cgen([program], table)
		if compare_texts(res, ctext) {
			eprintln('${i}... ' + term.green('OK'))
		}
		else {
			eprintln('${i}... ' + term.red('FAIL'))
			eprintln(path)
			eprintln('got:\n$res')
			assert false
		}
	}
}

fn compare_texts(a, b string) bool {
	lines_a_ := a.trim_space().split_into_lines()
	lines_b_ := b.trim_space().split_into_lines()
	lines_a := lines_a_.filter(it != '')
	lines_b := lines_b_.filter(it != '')
	if lines_a.len != lines_b.len {
		println(term.red('different len'))
		// return false
	}
	for i, line_a in lines_a {
		line_b := lines_b[i]
		if line_a.trim_space() != line_b.trim_space() {
			println(term.red('i=$i V="$line_a" C="$line_b"'))
			// exit(1)
			return false
		}
	}
	return true
}
