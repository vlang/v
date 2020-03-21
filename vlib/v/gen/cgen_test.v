import (
	os
	v.pref
	v.builder
	term
)

const (
	nr_tests = 4
	term_ok = term.ok_message('OK')
	term_fail = term.fail_message('FAIL')
)

fn test_c_files() {
	println('Running V => C tests')
	vexe := os.getenv('VEXE')
	vroot := os.dir(vexe)
	for i in 1 .. (nr_tests + 1) {
		path := '$vroot/vlib/v/gen/tests/${i}.vv'
		mut ctext := os.read_file('$vroot/vlib/v/gen/tests/${i}.c') or {
			panic(err)
		}
		ctext = ctext // unused warn
		mut b := builder.new_builder(pref.Preferences{})
		b.module_search_paths = ['$vroot/vlib/v/gen/tests/']
		mut res := b.gen_c([path]).after('#endbuiltin')
		if res.contains('string _STR') {
			pos := res.index('string _STR') or {
				-1
			}
			end := res.index_after('endof _STR_TMP', pos)
			res = res[..pos] + res[end + 15..]
		}
		if compare_texts(res, ctext, path) {
			println('${term_ok} ${i}')
		}
		else {
			assert false
		}
	}
}

fn compare_texts(a, b, path string) bool {
	lines_a_ := a.trim_space().split_into_lines()
	lines_b_ := b.trim_space().split_into_lines()
	lines_a := lines_a_.filter(it != '')
	lines_b := lines_b_.filter(it != '')
	/*
	if lines_a.len != lines_b.len {
		println(term.red('different len'))
		println('${path}: got\n$a')
		return false
	}
	*/

	for i, line_a in lines_a {
		if i >= lines_b.len {
			println(line_a)
			return false
		}
		line_b := lines_b[i]
		if line_a.trim_space() != line_b.trim_space() {
			println('${path}: got\n$a')
			println('${term_fail} ${i}')
			println(term.red('i=$i "$line_a" expected="$line_b"'))
			println(lines_b[i + 1])
			println(lines_b[i + 2])
			// exit(1)
			return false
		}
	}
	return true
}
