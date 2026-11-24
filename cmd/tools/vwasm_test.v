import os

fn test_compiler_generated_wasm_runs_with_v() ! {
	dir := os.join_path(os.vtmp_dir(), 'vwasm_${os.getpid()}')
	os.mkdir_all(dir)!
	defer { os.rmdir_all(dir) or {} }
	source := os.join_path(dir, 'guest.v')
	binary := os.join_path(dir, 'guest file.wasm')
	os.write_file(source, '
pub fn add(a int, b int) int { return a + b }
fn factorial(n i64) i64 { return if n < 2 { i64(1) } else { n * factorial(n - 1) } }
fn main() {
	println("hello from V wasm")
	println(add(20, 22))
	println(factorial(10))
	mut a := []int{}
	for i in 0..100 { a << i }
	mut sum := 0
	for n in a { sum += n }
	println(sum)
	exit(7)
}
')!
	for production in [false, true] {
		mut command := [@VEXE]
		if production { command << '-prod' }
		command << ['-b', 'wasm', '-o', binary, source]
		build := os.exec(command)
		assert build.exit_code == 0, build.output
		for prefix in [[@VEXE, 'run'], [@VEXE], [@VEXE, 'wasm']] {
			mut run := prefix.clone()
			run << binary
			result := os.exec(run)
			assert result.exit_code == 7, result.output
			assert result.output.trim_space() == 'hello from V wasm\n42\n3628800\n4950', result.output
		}
		result := os.exec([@VEXE, 'wasm', '--invoke', 'add', binary, '20', '22'])
		assert result.exit_code == 0, result.output
		assert result.output.trim_space() == '42', result.output
	}
	combined := os.exec([@VEXE, '-b', 'wasm', 'run', source])
	assert combined.exit_code == 7, combined.output
	assert combined.output.trim_space() == 'hello from V wasm\n42\n3628800\n4950', combined.output
	assert !os.exists(os.join_path(dir, 'guest.wasm'))
}
