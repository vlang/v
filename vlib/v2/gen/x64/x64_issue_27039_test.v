// vtest build: linux && amd64

module x64

import os

fn run_issue_27039_x64_program(name string, source string) string {
	tmp_dir := os.join_path(os.vtmp_dir(), 'v_issue_27039_x64_${name}_${os.getpid()}')
	os.mkdir_all(tmp_dir) or { panic(err) }
	defer {
		os.rmdir_all(tmp_dir) or {}
	}
	source_path := os.join_path(tmp_dir, '${name}.v')
	bin_path := os.join_path(tmp_dir, name)
	os.write_file(source_path, source) or { panic(err) }
	vexe := os.getenv_opt('VEXE') or { @VEXE }
	build :=
		os.execute('${os.quoted_path(vexe)} -v2 -b x64 ${os.quoted_path(source_path)} -o ${os.quoted_path(bin_path)}')
	assert build.exit_code == 0, build.output
	run :=
		os.execute('out=$(${os.quoted_path(bin_path)} 2>&1); code=$?; printf "%s\\n%s" "$code" "$out"')
	assert run.exit_code == 0, run.output
	lines := run.output.split_into_lines()
	assert lines.len >= 1, run.output
	assert lines[0] == '0', run.output
	return lines[1..].join('\n')
}

fn run_issue_27039_x64_program_redirected(name string, source string) (string, string) {
	tmp_dir := os.join_path(os.vtmp_dir(), 'v_issue_27039_x64_redir_${name}_${os.getpid()}')
	os.mkdir_all(tmp_dir) or { panic(err) }
	defer {
		os.rmdir_all(tmp_dir) or {}
	}
	source_path := os.join_path(tmp_dir, '${name}.v')
	bin_path := os.join_path(tmp_dir, name)
	stdout_path := os.join_path(tmp_dir, '${name}.out')
	stderr_path := os.join_path(tmp_dir, '${name}.err')
	os.write_file(source_path, source) or { panic(err) }
	vexe := os.getenv_opt('VEXE') or { @VEXE }
	build :=
		os.execute('${os.quoted_path(vexe)} -v2 -b x64 ${os.quoted_path(source_path)} -o ${os.quoted_path(bin_path)}')
	assert build.exit_code == 0, build.output
	run :=
		os.execute('${os.quoted_path(bin_path)} > ${os.quoted_path(stdout_path)} 2> ${os.quoted_path(stderr_path)}')
	assert run.exit_code == 0, run.output
	stdout := os.read_file(stdout_path) or { panic(err) }
	stderr := os.read_file(stderr_path) or { panic(err) }
	return stdout, stderr
}

fn test_issue_27039_x64_hello_world_runs() {
	output := run_issue_27039_x64_program('hello_world', "module main

fn main() {
	println('Hello World!')
}
")
	assert output == 'Hello World!'
}

fn test_issue_27039_x64_hello_world_runs_with_stdout_redirected() {
	stdout, stderr := run_issue_27039_x64_program_redirected('hello_world_redir', "module main

fn main() {
	println('Hello World!')
}
")
	assert stdout == 'Hello World!\n'
	assert stderr == ''
}

fn test_issue_27039_x64_const_init_is_not_skipped() {
	output := run_issue_27039_x64_program('const_string', "module main

const greeting = 'Const hello'

fn main() {
	println(greeting)
}
")
	assert output == 'Const hello'
}

fn test_issue_27039_x64_trunc_and_zext_mask_integer_values() {
	output := run_issue_27039_x64_program('int_cast_masks', "module main

fn narrow_and_widen(x u16) u64 {
	y := u8(x)
	return u64(y)
}

fn narrow_u32_to_u16(x u32) u64 {
	y := u16(x)
	return u64(y)
}

fn narrow_u64_to_u8(x u64) u64 {
	y := u8(x)
	return u64(y)
}

fn sign_extend_i8(x i8) i64 {
	return i64(x)
}

fn sign_extend_i16(x i16) i64 {
	return i64(x)
}

fn main() {
	if narrow_and_widen(u16(0x1234)) == 0x34
		&& narrow_u32_to_u16(u32(0x12345678)) == 0x5678
		&& narrow_u64_to_u8(u64(0x123456789ABCDEFF)) == 0xFF
		&& sign_extend_i8(i8(-42)) == -42
		&& sign_extend_i16(i16(-12345)) == -12345 {
		println('ok')
	} else {
		println('bad')
	}
}
")
	assert output == 'ok'
}

fn test_issue_27039_x64_float_width_casts_convert_values() {
	output := run_issue_27039_x64_program('float_width_casts', "module main

fn widen(x f32) f64 {
	return f64(x)
}

fn narrow(x f64) f32 {
	return f32(x)
}

fn main() {
	a := widen(f32(1.5))
	b := narrow(a + 0.25)
	if a > 1.49 && a < 1.51 && b > f32(1.74) && b < f32(1.76) {
		println('ok')
	} else {
		println('bad')
	}
}
")
	assert output == 'ok'
}

fn test_issue_27039_x64_unsigned_64_to_float_handles_high_bit() {
	output := run_issue_27039_x64_program('u64_to_float_high_bit', "module main

fn to_f64(x u64) f64 {
	return f64(x)
}

fn to_f32(x u64) f32 {
	return f32(x)
}

fn main() {
	a := to_f64(u64(9223372036854775808))
	b := to_f64(u64(18446744073709551615))
	c := to_f32(u64(9223372036854775808))
	if a == 9223372036854775808.0 && b > 18446744073709550000.0 && c > f32(9223371000000000000.0) {
		println('ok')
	} else {
		println('bad')
	}
}
")
	assert output == 'ok'
}

fn test_issue_27039_x64_float_to_unsigned_64_handles_high_bit() {
	output := run_issue_27039_x64_program('float_to_u64_high_bit', "module main

fn from_f64(x f64) u64 {
	return u64(x)
}

fn from_f32(x f32) u64 {
	return u64(x)
}

fn main() {
	a := from_f64(9223372036854775808.0)
	b := from_f64(9223372036854777856.0)
	c := from_f32(f32(9223372036854775808.0))
	if a == u64(9223372036854775808) && b == u64(9223372036854777856)
		&& c == u64(9223372036854775808) {
		println('ok')
	} else {
		println('bad')
	}
}
")
	assert output == 'ok'
}

fn test_issue_27039_x64_aggregate_arg_spills_to_stack() {
	output := run_issue_27039_x64_program('aggregate_arg_spill', "module main

struct Pair {
	a i64
	b i64
}

fn sum_after_five(a i64, b i64, c i64, d i64, e i64, p Pair) i64 {
	return a + b + c + d + e + p.a + p.b
}

fn main() {
	if sum_after_five(1, 2, 3, 4, 5, Pair{6, 7}) == 28 {
		println('ok')
	} else {
		println('bad')
	}
}
")
	assert output == 'ok'
}

fn test_issue_27039_x64_stack_arg_after_two_chunk_aggregate() {
	output := run_issue_27039_x64_program('stack_arg_after_pair', "module main

struct Pair {
	a i64
	b i64
}

fn sum_pair_then_scalars(p Pair, a i64, b i64, c i64, d i64, e i64) i64 {
	return p.a + p.b + a + b + c + d + e
}

fn main() {
	if sum_pair_then_scalars(Pair{10, 20}, 1, 2, 3, 4, 5) == 45 {
		println('ok')
	} else {
		println('bad')
	}
}
")
	assert output == 'ok'
}

fn test_issue_27039_x64_spilled_aggregate_does_not_consume_remaining_register() {
	output := run_issue_27039_x64_program('aggregate_spill_then_reg', "module main

struct Pair {
	a i64
	b i64
}

fn sum_spill_then_reg(a i64, b i64, c i64, d i64, e i64, p Pair, z i64) i64 {
	return p.a + p.b + z
}

fn main() {
	if sum_spill_then_reg(1, 2, 3, 4, 5, Pair{10, 20}, 6) == 36 {
		println('ok')
	} else {
		println('bad')
	}
}
")
	assert output == 'ok'
}
