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
	assert run.exit_code == 0, '${name}: ${run.output}'
	lines := run.output.split_into_lines()
	assert lines.len >= 1, '${name}: ${run.output}'
	assert lines[0] == '0', '${name}: ${run.output}'
	return lines[1..].join('\n')
}

fn run_issue_27039_x64_compile_error(name string, source string) string {
	tmp_dir := os.join_path(os.vtmp_dir(), 'v_issue_27039_x64_fail_${name}_${os.getpid()}')
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
	assert build.exit_code != 0, build.output
	return build.output
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

fn test_issue_27039_x64_signed_memory_loads_extend_values() {
	output := run_issue_27039_x64_program('signed_memory_loads', "module main

fn load_i8_from_ptr(p &i8) i64 {
	return i64(*p)
}

fn load_i16_from_ptr(p &i16) i64 {
	return i64(*p)
}

fn main() {
	mut a := i8(-42)
	mut b := i16(-12345)
	if load_i8_from_ptr(&a) == -42 && load_i16_from_ptr(&b) == -12345 {
		println('ok')
	} else {
		println('bad')
	}
}
")
	assert output == 'ok'
}

fn test_issue_27039_x64_raw_copy_tails_do_not_overwrite_sentinel() {
	output := run_issue_27039_x64_program('raw_copy_tails', "module main

struct Tiny3 {
mut:
	a u8
	b u8
	c u8
}

struct Wrap3 {
mut:
	data Tiny3
	sentinel u8
}

struct Tiny5 {
mut:
	a u8
	b u8
	c u8
	d u8
	e u8
}

struct Wrap5 {
mut:
	data Tiny5
	sentinel u8
}

struct Tiny6 {
mut:
	a u8
	b u8
	c u8
	d u8
	e u8
	f u8
}

struct Wrap6 {
mut:
	data Tiny6
	sentinel u8
}

struct Tiny7 {
mut:
	a u8
	b u8
	c u8
	d u8
	e u8
	f u8
	g u8
}

struct Wrap7 {
mut:
	data Tiny7
	sentinel u8
}

fn check3() bool {
	src := Tiny3{
		a: 1
		b: 2
		c: 3
	}
	mut w := Wrap3{
		sentinel: 0x5A
	}
	w.data = src
	return w.sentinel == 0x5A && w.data.a == 1 && w.data.b == 2 && w.data.c == 3
}

fn check5() bool {
	src := Tiny5{
		a: 1
		b: 2
		c: 3
		d: 4
		e: 5
	}
	mut w := Wrap5{
		sentinel: 0x5A
	}
	w.data = src
	return w.sentinel == 0x5A && w.data.a == 1 && w.data.b == 2 && w.data.c == 3
		&& w.data.d == 4 && w.data.e == 5
}

fn check6() bool {
	src := Tiny6{
		a: 1
		b: 2
		c: 3
		d: 4
		e: 5
		f: 6
	}
	mut w := Wrap6{
		sentinel: 0x5A
	}
	w.data = src
	return w.sentinel == 0x5A && w.data.a == 1 && w.data.b == 2 && w.data.c == 3
		&& w.data.d == 4 && w.data.e == 5 && w.data.f == 6
}

fn check7() bool {
	src := Tiny7{
		a: 1
		b: 2
		c: 3
		d: 4
		e: 5
		f: 6
		g: 7
	}
	mut w := Wrap7{
		sentinel: 0x5A
	}
	w.data = src
	return w.sentinel == 0x5A && w.data.a == 1 && w.data.b == 2 && w.data.c == 3
		&& w.data.d == 4 && w.data.e == 5 && w.data.f == 6 && w.data.g == 7
}

fn main() {
	if check3() && check5() && check6() && check7() {
		println('ok')
	} else {
		println('bad')
	}
}
")
	assert output == 'ok'
}

fn test_issue_27039_x64_small_aggregates_pass_by_value_in_registers() {
	output := run_issue_27039_x64_program('small_aggregate_reg_args', "module main

struct Tiny3 {
	a u8
	b u8
	c u8
}

struct Tiny5 {
	a u8
	b u8
	c u8
	d u8
	e u8
}

struct Tiny6 {
	a u8
	b u8
	c u8
	d u8
	e u8
	f u8
}

struct Tiny7 {
	a u8
	b u8
	c u8
	d u8
	e u8
	f u8
	g u8
}

fn sum3(t Tiny3) int {
	return int(t.a) + int(t.b) + int(t.c)
}

fn sum5(t Tiny5) int {
	return int(t.a) + int(t.b) + int(t.c) + int(t.d) + int(t.e)
}

fn sum6(t Tiny6) int {
	return int(t.a) + int(t.b) + int(t.c) + int(t.d) + int(t.e) + int(t.f)
}

fn sum7(t Tiny7) int {
	return int(t.a) + int(t.b) + int(t.c) + int(t.d) + int(t.e) + int(t.f) + int(t.g)
}

fn main() {
	a := Tiny3{
		a: 1
		b: 2
		c: 3
	}
	b := Tiny5{
		a: 1
		b: 2
		c: 3
		d: 4
		e: 5
	}
	c := Tiny6{
		a: 1
		b: 2
		c: 3
		d: 4
		e: 5
		f: 6
	}
	d := Tiny7{
		a: 1
		b: 2
		c: 3
		d: 4
		e: 5
		f: 6
		g: 7
	}
	if sum3(a) == 6 && sum5(b) == 15 && sum6(c) == 21 && sum7(d) == 28 {
		println('ok')
	} else {
		println('bad')
	}
}
")
	assert output == 'ok'
}

fn test_issue_27039_x64_raw_aggregate_arg_does_not_clobber_rcx_arg() {
	output := run_issue_27039_x64_program('small_aggregate_after_rcx_arg', "module main

struct Tiny3 {
	a u8
	b u8
	c u8
}

fn check(a i64, b i64, c i64, marker i64, t Tiny3) i64 {
	return marker + i64(t.a) + i64(t.b) + i64(t.c)
}

fn main() {
	t := Tiny3{
		a: 1
		b: 2
		c: 3
	}
	if check(10, 20, 30, 1000, t) == 1006 {
		println('ok')
	} else {
		println('bad')
	}
}
")
	assert output == 'ok'
}

fn test_issue_27039_x64_big17_sret_and_indirect_param_copy_exact_bytes() {
	output := run_issue_27039_x64_program('big17_exact_copies', "module main

struct Big17 {
mut:
	a u8
	b u8
	c u8
	d u8
	e u8
	f u8
	g u8
	h u8
	i u8
	j u8
	k u8
	l u8
	m u8
	n u8
	o u8
	p u8
	q u8
}

fn make_big() Big17 {
	return Big17{
		a: 1
		b: 2
		c: 3
		d: 4
		e: 5
		f: 6
		g: 7
		h: 8
		i: 9
		j: 10
		k: 11
		l: 12
		m: 13
		n: 14
		o: 15
		p: 16
		q: 17
	}
}

fn id_big(x Big17) Big17 {
	return x
}

fn sum_big(x Big17) int {
	return int(x.a) + int(x.b) + int(x.c) + int(x.d) + int(x.e) + int(x.f) + int(x.g)
		+ int(x.h) + int(x.i) + int(x.j) + int(x.k) + int(x.l) + int(x.m) + int(x.n)
		+ int(x.o) + int(x.p) + int(x.q)
}

fn main() {
	x := make_big()
	y := id_big(x)
	if sum_big(y) == 153 {
		println('ok')
	} else {
		println('bad')
	}
}
")
	assert output == 'ok'
}

fn test_issue_27039_x64_fixed_array_store_uses_raw_copy() {
	output := run_issue_27039_x64_program('fixed_array_store', "module main

fn main() {
	mut src := [3]u8{}
	src[0] = 1
	src[1] = 2
	src[2] = 3
	mut dst := [3]u8{}
	dst = src
	if dst[0] == 1 && dst[1] == 2 && dst[2] == 3 {
		println('ok')
	} else {
		println('bad')
	}
}
")
	assert output == 'ok'
}

fn test_issue_27039_x64_heap_sizing_allows_struct_access_past_eight_bytes() {
	output := run_issue_27039_x64_program('heap_struct_size', "module main

struct Heap17 {
mut:
	a u8
	b u8
	c u8
	d u8
	e u8
	f u8
	g u8
	h u8
	i u8
	j u8
	k u8
	l u8
	m u8
	n u8
	o u8
	p u8
	q u8
}

fn main() {
	mut p := &Heap17{}
	p.q = 17
	if p.q == 17 {
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

fn test_issue_27039_x64_scalar_float_abi_uses_xmm_registers() {
	output := run_issue_27039_x64_program('scalar_float_abi', "module main

fn from_f64_u32(x f64) u32 {
	return u32(x)
}

fn id_f64(x f64) f64 {
	return x
}

fn id_f32(x f32) f32 {
	return x
}

fn mixed(a i64, x f64, b i64, y f32, c i64) i64 {
	if x > 1.24 && x < 1.26 && y > f32(2.49) && y < f32(2.51) {
		return a + b + c
	}
	return -1
}

fn main() {
	x := 4000000000.0
	local := u32(x) == u32(4000000000)
	low := from_f64_u32(42.0) == u32(42)
	high := from_f64_u32(4000000000.0) == u32(4000000000)
	ret64 := id_f64(1.25) > 1.24 && id_f64(1.25) < 1.26
	ret32 := id_f32(f32(2.5)) > f32(2.49) && id_f32(f32(2.5)) < f32(2.51)
	mixed_ok := mixed(10, 1.25, 20, f32(2.5), 30) == 60
	if local && low && high && ret64 && ret32 && mixed_ok {
		println('ok')
	} else {
		println('bad')
	}
}
")
	assert output == 'ok'
}

fn test_issue_27039_x64_scalar_float_stack_args_fail_explicitly() {
	output := run_issue_27039_x64_compile_error('float_stack_arg_unsupported', 'module main

fn many(a f64, b f64, c f64, d f64, e f64, f f64, g f64, h f64, i f64) f64 {
	return a + b + c + d + e + f + g + h + i
}

fn main() {
	println(many(1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0, 9.0))
}
')
	assert output.contains('x64: unsupported float ABI stack parameter')
}

fn test_issue_27039_x64_float_comparisons_use_numeric_semantics() {
	output := run_issue_27039_x64_program('float_comparisons', "module main

import os

fn eq_f64(a f64, b f64) bool {
	return a == b
}

fn ne_f64(a f64, b f64) bool {
	return a != b
}

fn lt_f64(a f64, b f64) bool {
	return a < b
}

fn gt_f64(a f64, b f64) bool {
	return a > b
}

fn le_f64(a f64, b f64) bool {
	return a <= b
}

fn ge_f64(a f64, b f64) bool {
	return a >= b
}

fn le_f32(a f32, b f32) bool {
	return a <= b
}

fn gt_f32(a f32, b f32) bool {
	return a > b
}

fn ge_f32(a f32, b f32) bool {
	return a >= b
}

fn eq_f32(a f32, b f32) bool {
	return a == b
}

fn ne_f32(a f32, b f32) bool {
	return a != b
}

fn lt_f32(a f32, b f32) bool {
	return a < b
}

fn main() {
	argc := os.args.len
	z := f64(argc - argc)
	one := f64(argc)
	two := one + one
	neg_zero := -z
	nan := z / z
	ok64 := eq_f64(z, neg_zero) && ne_f64(nan, nan) && lt_f64(-two, -one)
		&& gt_f64(-one, -two) && le_f64(z, neg_zero) && ge_f64(z, neg_zero)
	nan64 := !eq_f64(nan, nan) && ne_f64(nan, nan) && !lt_f64(nan, one)
		&& !le_f64(nan, one) && !gt_f64(nan, one) && !ge_f64(nan, one)
	nanf := f32(nan)
	onef := f32(one)
	ok32 := eq_f32(f32(z), f32(neg_zero)) && lt_f32(f32(-two), f32(-one))
	nan32 := !eq_f32(nanf, nanf) && ne_f32(nanf, nanf) && !lt_f32(nanf, onef)
		&& !le_f32(nanf, onef) && !gt_f32(nanf, onef) && !ge_f32(nanf, onef)
	if ok64 && nan64 && ok32 && nan32 {
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
