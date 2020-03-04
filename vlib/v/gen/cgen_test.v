import (
	os
	filepath
	v.pref
	v.builder
	term
)

const (
	nr_tests = 4
)

fn test_c_files() {
	println('Running V => C tests')
	vexe := os.getenv('VEXE')
	vroot := filepath.dir(vexe)
	term_ok := term.ok_message('OK')
	term_fail := term.fail_message('FAIL')
	for i in 1 .. (nr_tests + 1) {
		path := '$vroot/vlib/v/gen/tests/${i}.vv'
		mut ctext := os.read_file('$vroot/vlib/v/gen/tests/${i}.c') or {
			panic(err)
		}
		ctext = ctext // unused warn
		mut b := builder.new_builder(pref.Preferences{})
		b.module_search_paths = ['$vroot/vlib/v/gen/tests/']
		res := b.gen_c([path]).after('#endif')
		if compare_texts(res, ctext) {
			eprintln('${term_ok} ${i}')
		}
		else {
			eprintln('${term_fail} ${i}')
			eprintln('${path}: got\n$res')
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
