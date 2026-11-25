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
	for i, source in cases {
		bin := os.join_path(root, 'v3_invalid_assignment_expression_${i}')
		src := bin + '.v'
		os.write_file(src, source)!
		compile := os.exec([@VEXE, src, '-b', 'c', '-o', bin])
		assert compile.exit_code != 0, compile.output
		assert compile.output.contains('invalid expression: unexpected token `=`'), compile.output
		assert !compile.output.contains('C compilation failed'), compile.output
	}
}
