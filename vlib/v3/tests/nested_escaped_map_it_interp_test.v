import os

const vexe = @VEXE
const tests_dir = os.dir(@FILE)
const v3_dir = os.dir(tests_dir)
const vlib_dir = os.dir(v3_dir)
const v3_src = os.join_path(v3_dir, 'v3.v')

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
	v3_bin := os.join_path(os.temp_dir(), 'v3_nested_escaped_map_it_interp_test')
	build := os.execute('${vexe} -gc none -path "${vlib_dir}|@vlib|@vmodules" -o ${v3_bin} ${v3_src}')
	assert build.exit_code == 0, build.output

	source_file := os.join_path(os.temp_dir(), 'v3_nested_escaped_map_it_interp_input.v')
	os.write_file(source_file, nested_it_program) or { panic(err) }
	bin := os.join_path(os.temp_dir(), 'v3_nested_escaped_map_it_interp_input')
	compile := os.execute('${v3_bin} -nocache ${source_file} -o ${bin}')
	assert compile.exit_code == 0, compile.output
	run := os.execute(bin)
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
