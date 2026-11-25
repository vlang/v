import os
import v.tempname

// Written as a raw string so the escapes reach the compiler under test as typed.
const nested_it_program = r"fn main() {
	b := 'B'
	tags := ['a', 'b']
	println('1:' + 'p ${tags.map('--\${it}').join(',')}')
	println('2:' + 'p ${tags.map('--${it}').join(',')}')
	println('3:' + 'p ${tags.map('--' + it).join(',')}')
	println('4:' + 'p ${tags.filter(it != 'b').map('<\${it}>').join(',')}')
	println('5:' + 'L ${'a ${b}'}')
}
"

// A `${it}` that reaches the transform still encoded in a string literal's text has
// no binding to rename: `it` belongs to the enclosing `map`/`filter`, whose lowering
// renames it while walking the parsed body. Expanding it anyway synthesized an `it`
// that no declaration reached, and the generated C failed to build with
// `use of undeclared identifier 'it'`, which made V fall back to the stable compiler.
fn test_escaped_it_interpolation_in_map_body_compiles() {
	dir := os.join_path(os.vtmp_dir(), 'v3_nested_escaped_map_it_interp_${tempname.unique_token()}')
	os.mkdir(dir)!
	defer { os.rmdir_all(dir) or {} }
	bin := os.join_path(dir, 'v3_nested_escaped_map_it_interp_input')
	source_file := bin + '.v'
	os.write_file(source_file, nested_it_program) or { panic(err) }
	compile := os.exec([@VEXE, '-nocache', source_file, '-o', bin])
	assert compile.exit_code == 0, compile.output
	run := os.exec([bin])
	assert run.exit_code == 0, run.output
	lines := run.output.trim_space().split_into_lines()
	assert lines.len == 5, run.output
	// the escaped form keeps the dollar the source asked to print
	assert lines[0] == r'1:p --${it},--${it}', run.output
	// an unescaped nested interpolation still resolves the `map` binding
	assert lines[1] == '2:p --a,--b', run.output
	assert lines[2] == '3:p --a,--b', run.output
	assert lines[3] == r'4:p <${it}>', run.output
	// deeper nesting keeps expanding
	assert lines[4] == '5:L a B', run.output
}
