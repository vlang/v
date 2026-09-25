import os

// V's test runner has no way to say "this program must panic", so these cases
// compile a small program with the compiler under test and run it. A division by
// zero has to stop the program rather than hand back a value, which is what the
// 128-bit helpers did before the guard was added.
fn run_probe(name string, body string) os.Result {
	dir := os.join_path(os.temp_dir(), 'v_int128_panic')
	os.mkdir_all(dir) or {}
	src := os.join_path(dir, '${name}.v')
	os.write_file(src, 'fn main() {\n${body}}\n') or { return os.Result{} }
	exe := os.join_path(dir, name)
	build := os.execute('${@VEXE} -o ${exe} ${src}')
	assert build.exit_code == 0, 'the probe did not compile: ${build.output}'
	return os.execute(exe)
}

fn test_wide_division_by_zero_panics() {
	result := run_probe('div0', '\ta := u128(10)\n\tb := u128(0)\n\tprintln(a / b)\n')
	assert result.exit_code != 0
	assert result.output.contains('by zero')
}

fn test_wide_modulo_by_zero_panics() {
	result := run_probe('mod0', '\ta := u128(10)\n\tb := u128(0)\n\tprintln(a % b)\n')
	assert result.exit_code != 0
	assert result.output.contains('by zero')
}

fn test_a_compound_division_by_zero_panics() {
	result := run_probe('div0_assign',
		'\tmut a := u128(10)\n\tb := u128(0)\n\ta /= b\n\tprintln(a)\n')
	assert result.exit_code != 0
	assert result.output.contains('by zero')
}

fn test_a_signed_division_by_zero_panics() {
	result := run_probe('idiv0', '	a := i128(-10)\n	b := i128(0)\n	println(a / b)\n')
	assert result.exit_code != 0
	assert result.output.contains('by zero')
}

fn test_an_array_element_division_by_zero_panics() {
	// The element path has to go through the same guard as the scalar one, which
	// it skipped: it handed back the maximum u128 where the native
	// representation raised SIGFPE instead.
	result := run_probe('arr_div0',
		'	mut a := [u128(10)]\n	b := u128(0)\n	a[0] /= b\n	println(a[0])\n')
	assert result.exit_code != 0
	assert result.output.contains('by zero')
}

fn test_an_array_element_modulo_by_zero_panics() {
	result := run_probe('arr_mod0',
		'	mut a := [u128(10)]\n	b := u128(0)\n	a[0] %= b\n	println(a[0])\n')
	assert result.exit_code != 0
	assert result.output.contains('by zero')
}
