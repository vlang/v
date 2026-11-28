import os
import v.tempname

fn test_invalid_assignment_expression_is_rejected() {
	root := os.join_path(os.vtmp_dir(), 'v3_invalid_assignment_expression_${tempname.unique_token()}')
	os.mkdir(root)!
	defer { os.rmdir_all(root) or {} }
	cases := [
		'fn main() {
	a := 10
	b := 50
	x := (((((a & b = == 0)
	println(x)
}
',
		'fn main() {
	a := 10
	b := 50
	x := (a & b =) == 100
	println(x)
}
',
		'fn main() {
	a := 10
	b := 50
	x := a & b = == && println("hello")
	println(x)
}
',
		'fn main() {
	a := 10
	b := 50
	x := a = b
	println(x)
}
',
	]
	os.write_file(os.join_path(root, 'main.v'), 'module main\n\nfn main() {}\n')!
	for i, source in cases {
		name := 'invalid_${i}'
		fixture := 'module main\n\n' + source.replace_once('fn main()', 'fn ${name}()')
		os.write_file(os.join_path(root, '${name}.v'), fixture)!
	}
	bin := os.join_path(root, 'program')
	compile := os.exec([@VEXE, root, '-b', 'c', '-o', bin])
	assert compile.exit_code != 0, compile.output
	assert !compile.output.contains('C compilation failed'), compile.output
	mut rejected := map[string]bool{}
	for line in compile.output.split_into_lines() {
		location, message := line.split_once(': error: ') or { continue }
		if message == 'invalid expression: unexpected token `=`' {
			file := location.all_before_last(':').all_before_last(':')
			rejected[os.file_name(file)] = true
		}
	}
	for i in 0 .. cases.len {
		assert rejected['invalid_${i}.v'], 'invalid_${i}: ${compile.output}'
	}
}
