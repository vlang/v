import (
	os
	v.fmt
	filepath
	term
	v.table
	v.parser
)

const (
	nr_tests = 1
)

fn test_fmt() {
	println('Running vfmt tests')
	vexe := os.getenv('VEXE')
	vroot := filepath.dir(vexe)
	term_ok := term.ok_message('OK')
	term_fail := term.fail_message('FAIL')
	for i in 1 .. nr_tests + 1 {
		path := '$vroot/vlib/v/fmt/tests/${i}.vv'
		println(path)
		mut ctext := os.read_file('$vroot/vlib/v/fmt/tests/${i}_out.vv') or {
			panic(err)
		}
		ctext = ctext // unused warn
		table := table.new_table()
		file := parser.parse_file(path, table)
		fmt.fmt(file, table)
	}
}
