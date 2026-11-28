import os
import v.tempname

const vexe = @VEXE
const tests_dir = os.dir(@FILE)
const v3_dir = os.dir(tests_dir)
const v3_src = os.join_path(v3_dir, 'v.v')
const wasm_suite_temp = os.join_path(os.vtmp_dir(), 'ssa_wasm_codegen_${tempname.unique_token()}')
const wasm_compiler = wasm_suite_temp + $if windows { '.exe' } $else { '' }

fn testsuite_begin() {
	if os.getenv('V3_TEST_WASM') != '1' {
		eprintln('> skipping v3 wasm backend tests; set V3_TEST_WASM=1 to run')
		exit(0)
	}
	// The launcher caches its WASM-capable compiler. Keep a suite-local build
	// for standalone drivers, custom flags, and runs with tool caching disabled.
	if os.getenv('VFLAGS') == ''
		&& os.getenv('VTOOLS_NO_CACHE').trim_space().to_lower() !in ['1', 'true', 'yes', 'on']
		&& running_compiler_supports_wasm() {
		return
	}
	build := os.exec([vexe, '-no-retry-compilation', '-cc', 'clang', '-gc', 'none', '-compile-backend',
		'wasm', '-o', wasm_compiler, v3_src])
	assert build.exit_code == 0, build.output
}

fn running_compiler_supports_wasm() bool {
	source := wasm_suite_temp + '.v'
	output := wasm_suite_temp + '.wasm'
	defer {
		os.rm(source) or {}
		os.rm(output) or {}
	}
	os.write_file(source, 'fn main() {}\n') or { panic(err) }
	probe := os.exec([vexe, '-b', 'wasm', '-o', output, source])
	if probe.exit_code != 0
		&& probe.output.contains('WebAssembly support is not compiled into this executable') {
		return false
	}
	assert probe.exit_code == 0, probe.output
	assert_valid_wasm(output)
	return true
}

fn testsuite_end() {
	os.rm(wasm_compiler) or {}
}

fn v3_binary() string {
	return if os.is_file(wasm_compiler) { wasm_compiler } else { vexe }
}

fn compile_to_wasm(v3_bin string, src string, name string) string {
	src_path := os.join_path(os.vtmp_dir(), '${name}.v')
	out_path := os.join_path(os.vtmp_dir(), '${name}.wasm')
	os.write_file(src_path, src) or { panic(err) }
	os.rm(out_path) or {}
	res :=
		os.exec([v3_bin, '-enable-globals', '-b', 'wasm', '-o', out_path, src_path])
	assert res.exit_code == 0, '${name}: ${res.output}'
	assert os.exists(out_path), 'missing wasm output for ${name}'
	return out_path
}

// assert_valid_wasm checks the module header and that it is non-trivial.
fn assert_valid_wasm(path string) {
	bytes := os.read_bytes(path) or { panic(err) }
	assert bytes.len > 8, 'wasm too small'
	assert bytes[0] == 0x00 && bytes[1] == 0x61 && bytes[2] == 0x73 && bytes[3] == 0x6d, 'bad wasm magic'
	assert bytes[4] == 0x01 && bytes[5] == 0x00 && bytes[6] == 0x00 && bytes[7] == 0x00, 'bad wasm version'
}

fn node_path() ?string {
	path := os.find_abs_path_of_executable('node') or { return none }
	return path
}

// last_line returns the last non-empty line, ignoring any stderr noise such as
// node's WASI ExperimentalWarning that os.execute folds into the output.
fn last_line(out string) string {
	mut result := ''
	for line in out.split_into_lines() {
		if line.trim_space().len > 0 {
			result = line.trim_space()
		}
	}
	return result
}

fn run_node(node string, runner string, wasm string) os.Result {
	return os.exec(['${node}', '--no-warnings', '${runner}', '${wasm}'])
}

// run_wasi_expect runs a WASI module and asserts its trailing output lines.
// Skips execution (compile/validate already happened) when node is absent.
fn run_wasi_expect(wasm string, expected []string) {
	node := node_path() or { return }
	runner := os.join_path(os.vtmp_dir(), 'wasm_run_wasi.mjs')
	os.write_file(runner, wasi_runner_js) or { panic(err) }
	res := run_node(node, runner, wasm)
	assert res.exit_code == 0, res.output
	lines := res.output.split_into_lines().map(it.trim_space()).filter(it.len > 0)
	assert lines.len >= expected.len, res.output
	for i, want in expected {
		got := lines[lines.len - expected.len + i]
		assert got == want, '${os.base(wasm)} line ${i}: got ${got}, want ${want} (full: ${res.output})'
	}
}

fn test_wasm_local_scopes_and_assignments() {
	wasm := compile_to_wasm(v3_binary(), "// A for-initializer and a loop-body declaration must not alter outer locals:
// after the loops the outer i and x remain 10 and 1.
fn block_scoping_preserves_outer_locals() {
	i := 10
	for inner_i := 0; inner_i < 1; inner_i++ {
	}
	println(i)
	x := 1
	for j := 0; j < 2; j++ {
		inner_x := j + 2
		println(inner_x)
	}
	println(x)
}

// A body-local value must not replace the counter used by the post `i++`; the
// outer loop counter must still advance. The count/break bound keeps the
// test terminating even if the fix regresses (it would loop otherwise).
fn for_post_uses_loop_var_with_body_local() {
	mut i := 0
	mut count := 0
	for ; i < 3; i++ {
		value := 10
		_ = value
		count++
		if count > 100 {
			break
		}
	}
	println(i)
	println(count)
}

// The inner `value := x + 1` initializer reads the outer x (5), giving 6,
// and the outer x is unchanged afterwards.
fn inner_initializer_reads_outer() {
	x := 5
	{
		value := x + 1
		println(value)
	}
	println(x)
}


fn parallel_multi_assign_swaps() {
	mut a := 1
	mut b := 2
	a, b = b, a
	println(a)
	println(b)
}

// Both initializers of a parallel declaration must be evaluated against the
// enclosing scope, so `a, b := x + 1, x + 2` reads the outer x for both,
// giving a == 6 and b == 7.
fn multi_decl_buffers_rhs() {
	x := 5
	{
		a, b := x + 1, x + 2
		println(a)
		println(b)
	}
}

fn classify(n int) int {
	return if n > 0 { 1 } else if n < 0 { -1 } else { 0 }
}

// `if` used as a value (decl rhs, return, else-if chain) selects the branch
// value rather than producing zero.
fn if_expression_value() {
	x := if 3 > 2 { 10 } else { 20 }
	println(x)
	println(classify(5))
	println(classify(-5))
	println(classify(0))
}

fn main() {
	println('block_scoping_preserves_outer_locals')
	block_scoping_preserves_outer_locals()
	println('for_post_uses_loop_var_with_body_local')
	for_post_uses_loop_var_with_body_local()
	println('inner_initializer_reads_outer')
	inner_initializer_reads_outer()
	println('parallel_multi_assign_swaps')
	parallel_multi_assign_swaps()
	println('multi_decl_buffers_rhs')
	multi_decl_buffers_rhs()
	println('if_expression_value')
	if_expression_value()
}
", 'wasm_local_scopes_and_assignments')
	assert_valid_wasm(wasm)
	run_wasi_expect(wasm, [
		'block_scoping_preserves_outer_locals',
		'10',
		'2',
		'3',
		'1',
		'for_post_uses_loop_var_with_body_local',
		'3',
		'3',
		'inner_initializer_reads_outer',
		'6',
		'5',
		'parallel_multi_assign_swaps',
		'2',
		'1',
		'multi_decl_buffers_rhs',
		'6',
		'7',
		'if_expression_value',
		'10',
		'1',
		'-1',
		'0',
	])
}

fn test_wasm_scalar_values_and_printing() {
	wasm := compile_to_wasm(v3_binary(), "fn narrow_integer_casts_and_arithmetic_wrap() {
	println(int(i8(i16(128))))
	println(int(u8(u16(256))))
	println(int(u16(u32(65536))))
	println(int(i16(i32(32768))))
	mut a := u8(250)
	a += u8(10)
	println(int(a))
	mut b := i8(127)
	b++
	println(int(b))
	println(int(u8(200) + u8(100)))
}

fn osc() u64 {
	return u64(64)
}

// V yields 0 when a runtime shift count is >= the operand width, whereas
// raw WASM masks the count modulo the width; in-range shifts are unchanged.
fn runtime_oversized_shift_is_zero() {
	shift := osc()
	bits := u64(9221120237041090561)
	println(bits >> shift)
	println(bits << shift)
	mut left := u64(1)
	left <<= shift
	println(left)
	println(u64(1) << u64(40))
}

// Print lowering uses dotted numeric str methods, including u64's unsigned range.
fn lowered_integer_print_formats() {
	for number in 1 .. 4 {
		println(number * number)
	}
	println(i8(-8))
	println(i16(-160))
	println(i32(-320))
	println(i64(-6400000000))
	println(int(-32))
	println(u8(255).str())
	println(u16(65535).str())
	println(u32(4294967295).str())
	println(u64(18446744073709551615))
	println(uint(4294967295).str())
}

fn unsigned_i64_and_bool() {
	a := u32(4000000000)
	println(a / u32(3))
	c := u64(10000000000)
	println(c * 2)
	d := u64(1)
	println(d << 40)
	println(a > u32(3))
}

// f32->int/i64 truncation casts and full-range unsigned u64 printing.
fn f32_cast_and_unsigned_u64_print() {
	f := f32(3.9)
	println(int(f))
	println(i64(f))
	g := f32(-2.7)
	println(int(g))
	println(u64(0) - u64(1))
}

// signed /= %= >>=, unsigned /=, float ++/--, full-range u64 decimal literal.
fn signed_compound_float_postfix_and_u64_literal() {
	mut x := -6
	x /= 2
	println(x)
	mut y := -7
	y %= 3
	println(y)
	mut z := -8
	z >>= 1
	println(z)
	mut u := u32(4000000000)
	u /= u32(2)
	println(u)
	mut f := 1.5
	f++
	println(int(f))
	mut g := f32(5.5)
	g--
	println(int(g))
	println(u64(18446744073709551615))
}

const answer = 42
const big = u64(9223372036854775808)

// Numeric/bool consts are inlined at the use site instead of warning + 0.
fn top_level_const_inlined() {
	println(answer)
	println(big)
	println(answer + 1)
}

fn main() {
	println('narrow_integer_casts_and_arithmetic_wrap')
	narrow_integer_casts_and_arithmetic_wrap()
	println('runtime_oversized_shift_is_zero')
	runtime_oversized_shift_is_zero()
	println('lowered_integer_print_formats')
	lowered_integer_print_formats()
	println('unsigned_i64_and_bool')
	unsigned_i64_and_bool()
	println('f32_cast_and_unsigned_u64_print')
	f32_cast_and_unsigned_u64_print()
	println('signed_compound_float_postfix_and_u64_literal')
	signed_compound_float_postfix_and_u64_literal()
	println('top_level_const_inlined')
	top_level_const_inlined()
}
", 'wasm_scalar_values_and_printing')
	assert_valid_wasm(wasm)
	run_wasi_expect(wasm, [
		'narrow_integer_casts_and_arithmetic_wrap',
		'-128',
		'0',
		'0',
		'-32768',
		'4',
		'-128',
		'44',
		'runtime_oversized_shift_is_zero',
		'0',
		'0',
		'0',
		'1099511627776',
		'lowered_integer_print_formats',
		'1',
		'4',
		'9',
		'-8',
		'-160',
		'-320',
		'-6400000000',
		'-32',
		'255',
		'65535',
		'4294967295',
		'18446744073709551615',
		'4294967295',
		'unsigned_i64_and_bool',
		'1333333333',
		'20000000000',
		'1099511627776',
		'true',
		'f32_cast_and_unsigned_u64_print',
		'3',
		'3',
		'-2',
		'18446744073709551615',
		'signed_compound_float_postfix_and_u64_literal',
		'-3',
		'-1',
		'-4',
		'2000000000',
		'2',
		'4',
		'18446744073709551615',
		'top_level_const_inlined',
		'42',
		'9223372036854775808',
		'43',
	])
}

fn test_wasm_hello_world() {
	v3_bin := v3_binary()
	wasm := compile_to_wasm(v3_bin, "fn main() {\n\tprintln('hello world')\n}\n", 'wasm_hello')
	assert_valid_wasm(wasm)

	node := node_path() or {
		eprintln('> node not found, skipping wasm execution check')
		return
	}
	runner := os.join_path(os.vtmp_dir(), 'wasm_run_wasi.mjs')
	os.write_file(runner, wasi_runner_js) or { panic(err) }
	res := run_node(node, runner, wasm)
	assert res.exit_code == 0, res.output
	assert last_line(res.output) == 'hello world', res.output
}

fn test_wasm_implicit_main() {
	wasm := compile_to_wasm(v3_binary(), "println('hello script')\n", 'wasm_implicit_main')
	assert_valid_wasm(wasm)
	run_wasi_expect(wasm, ['hello script'])
}

fn assert_wasm_source_before_and_after_optimization(name string, source string, checks string) {
	dir := os.join_path(os.vtmp_dir(), 'wasm_review_${name}_${os.getpid()}')
	os.mkdir_all(dir) or { panic(err) }
	defer { os.rmdir_all(dir) or {} }
	source_path := os.join_path(dir, 'main.v')
	runner := os.join_path(dir, 'check.mjs')
	os.write_file(source_path, source) or { panic(err) }
	os.write_file(runner, "import assert from 'node:assert/strict';
import { readFileSync } from 'node:fs';
const bytes = readFileSync(process.argv[2]);
assert.ok(WebAssembly.validate(bytes));
const { instance } = await WebAssembly.instantiate(bytes, {});
const e = instance.exports;
${checks}
") or { panic(err) }
	for production in [false, true] {
		output := os.join_path(dir, 'main_${production}.wasm')
		mut args := [v3_binary(), '-enable-globals', '-b', 'wasm', '-o', output, source_path]
		if production {
			args.insert(1, '-prod')
		}
		result := os.exec(args)
		assert result.exit_code == 0, result.output
		assert_valid_wasm(output)
		if node := node_path() {
			execution := run_node(node, runner, output)
			assert execution.exit_code == 0, '${name}, production=${production}: ${execution.output}'
		}
	}
}

fn test_wasm_constant_evaluation() {
	assert_wasm_source_before_and_after_optimization('constant_evaluation', '
// Float constant comparisons.
pub fn less() bool { return f64(1.25) < f64(1.5) }
pub fn less_equal() bool { return f64(1.25) <= f64(1.5) }
pub fn greater() bool { return f64(1.25) > f64(1.5) }
pub fn greater_equal() bool { return f64(1.25) >= f64(1.5) }
pub fn equal() bool { return f64(1.25) == f64(1.5) }
pub fn unequal() bool { return f64(1.25) != f64(1.5) }
pub fn f32_less() bool { return f32(-1.5) < f32(-1.25) }
pub fn f32_rounded_equal() bool { return f32(16777216) == f32(16777217) }

// Full-width integer constant folding.
pub fn check() bool { return 9223372036854775808 == 9223372036854775809 }
pub fn full_width_unequal() bool { return 9223372036854775808 != 9223372036854775809 }
pub fn hex_unequal() bool { return 0x8000000000000000 != 0x8000000000000001 }
pub fn unsigned_less() bool { return u64(9223372036854775808) < u64(9223372036854775809) }
pub fn unsigned_difference() u64 { return u64(9223372036854775809) - u64(9223372036854775808) }
pub fn unsigned_maximum() u64 { return 18446744073709551615 }
pub fn minimum_equal(n i64) bool { return n == -9223372036854775808 }

// Wide integer literal operands.
pub fn wide_equal(n i64) bool { return n == 4294967296 }
pub fn reversed_equal(n i64) bool { return 4294967296 == n }
pub fn add(n i64) i64 { return n + 4294967296 }
pub fn reversed_add(n i64) i64 { return 4294967296 + n }
pub fn float_add(n f64) f64 { return n + 4294967296 }
pub fn float_equal(n f64) bool { return n == 4294967296 }
pub fn narrow_equal(n int) bool { return n == 4294967296 }
pub fn narrow_less(n int) bool { return n < 4294967296 }
pub fn narrow_negative_equal(n int) bool { return n == -4294967296 }
pub fn narrow_unsigned_equal(n u8) bool { return n == 256 }
pub fn unsigned_wide_equal(n u64) bool { return n == 18446744073709551615 }
', '
assert.equal(e.less(), 1);
assert.equal(e.less_equal(), 1);
assert.equal(e.greater(), 0);
assert.equal(e.greater_equal(), 0);
assert.equal(e.equal(), 0);
assert.equal(e.unequal(), 1);
assert.equal(e.f32_less(), 1);
assert.equal(e.f32_rounded_equal(), 1);

assert.equal(e.check(), 0);
assert.equal(e.full_width_unequal(), 1);
assert.equal(e.hex_unequal(), 1);
assert.equal(e.unsigned_less(), 1);
assert.equal(e.unsigned_difference(), 1n);
assert.equal(e.unsigned_maximum(), -1n);
assert.equal(e.minimum_equal(-9223372036854775808n), 1);
assert.equal(e.minimum_equal(9223372036854775807n), 0);

assert.equal(e.wide_equal(4294967296n), 1);
assert.equal(e.wide_equal(0n), 0);
assert.equal(e.reversed_equal(4294967296n), 1);
assert.equal(e.reversed_equal(0n), 0);
assert.equal(e.add(1n), 4294967297n);
assert.equal(e.reversed_add(1n), 4294967297n);
assert.equal(e.float_add(1), 4294967297);
assert.equal(e.float_equal(4294967296), 1);
assert.equal(e.float_equal(0), 0);
assert.equal(e.narrow_equal(0), 0);
assert.equal(e.narrow_less(0), 1);
assert.equal(e.narrow_less(2147483647), 1);
assert.equal(e.narrow_negative_equal(0), 0);
assert.equal(e.narrow_unsigned_equal(0), 0);
assert.equal(e.narrow_unsigned_equal(255), 0);
assert.equal(e.unsigned_wide_equal(-1n), 1);
assert.equal(e.unsigned_wide_equal(0n), 0);
')
}

fn test_wasm_scalar_evaluation() {
	assert_wasm_source_before_and_after_optimization('scalar_evaluation', '
// Numeric assignment conversions.
pub fn check(n int) bool {
	mut x := f64(0)
	x = n
	return x < 1.5
}
pub fn promoted_float(n f32) bool {
	mut x := f64(0)
	x = n
	return x < 1.5
}
pub fn unsigned_to_float(n u32) f64 {
	mut x := f64(0)
	x = f64(n)
	return x + 0.5
}
pub fn branch_float(n int, flag bool) f64 {
	mut x := f64(0)
	if flag { x = n } else { x = 3 }
	return x + 0.5
}
pub fn parallel_floats(n int) f64 {
	mut x := f64(0)
	mut y := f64(0)
	x, y = n, n + 1
	return x + y + 0.5
}
pub fn unsigned_to_signed(n u32) bool {
	mut x := i64(0)
	x = i64(n)
	return x > 2147483647
}

// Numeric arguments in indirect calls.
fn scale(x f64) f64 { return x * 2.0 }
fn scale32(x f32) f32 { return x * f32(2) }
pub fn integer_argument() f64 {
	callback := scale
	return callback(3)
}
pub fn f32_argument() f64 {
	callback := scale
	return callback(f32(3.5))
}
pub fn integer_to_f32_argument() f32 {
	callback := scale32
	return callback(3)
}

// Unicode rune initializers and expressions.
__global letter = `é`
__global ideograph = `界`
__global emoji = `😀`
pub fn global_letter() rune { return letter }
pub fn global_ideograph() rune { return ideograph }
pub fn global_emoji() rune { return emoji }
pub fn direct_letter() rune { return `é` }
pub fn direct_ideograph() rune { return `界` }
pub fn direct_emoji() rune { return `😀` }
pub fn newline() rune { return `\\n` }

// Moduleless numeric constants.
const base = 10
const answer = base * 4 + 2
const big = u64(9223372036854775808)
__global result = answer - 1

fn answer_value() int { return answer }
fn big_value() u64 { return big }
fn result_value() int { return result }

// Signed zero under floating-point unary minus.
pub fn negative_zero64() f64 { return -0.0 }
pub fn negative_zero32() f32 { return -f32(0.0) }
pub fn negate64(value f64) f64 { return -value }
pub fn negate32(value f32) f32 { return -value }
', '
assert.equal(e.check(1), 1);
assert.equal(e.check(2), 0);
assert.equal(e.check(-1), 1);
assert.equal(e.promoted_float(1.25), 1);
assert.equal(e.promoted_float(1.75), 0);
assert.equal(e.unsigned_to_float(4000000000), 4000000000.5);
assert.equal(e.branch_float(1, 1), 1.5);
assert.equal(e.branch_float(1, 0), 3.5);
assert.equal(e.parallel_floats(3), 7.5);
assert.equal(e.unsigned_to_signed(4000000000), 1);
assert.equal(e.unsigned_to_signed(1), 0);

assert.equal(e.integer_argument(), 6);
assert.equal(e.f32_argument(), 7);
assert.equal(e.integer_to_f32_argument(), 6);

assert.equal(e.global_letter(), 233);
assert.equal(e.global_ideograph(), 30028);
assert.equal(e.global_emoji(), 128512);
assert.equal(e.direct_letter(), 233);
assert.equal(e.direct_ideograph(), 30028);
assert.equal(e.direct_emoji(), 128512);
assert.equal(e.newline(), 10);

assert.equal(e.answer_value(), 42);
assert.equal(BigInt.asUintN(64, e.big_value()), 9223372036854775808n);
assert.equal(e.result_value(), 41);

assert.ok(Object.is(e.negative_zero64(), -0));
assert.ok(Object.is(e.negative_zero32(), -0));
for (const negate of [e.negate64, e.negate32]) {
    assert.ok(Object.is(negate(0), -0));
    assert.ok(Object.is(negate(-0), 0));
    assert.equal(negate(17), -17);
    assert.equal(negate(-17), 17);
}
')
}

fn test_wasm_user_functions_keep_runtime_stub_names() {
	assert_wasm_source_before_and_after_optimization('user_runtime_names', '
fn current_rss_kb() i64 { return 123 }
fn macos_rss_kb() i64 { return 456 }
fn linux_rss_kb() i64 { return 789 }
pub fn sum() i64 { return current_rss_kb() + macos_rss_kb() + linux_rss_kb() }
', '
assert.equal(e.current_rss_kb(), 123n);
assert.equal(e.macos_rss_kb(), 456n);
assert.equal(e.linux_rss_kb(), 789n);
assert.equal(e.sum(), 1368n);
')
}

fn test_wasm_user_functions_keep_other_runtime_names() {
	assert_wasm_source_before_and_after_optimization('other_runtime_names', '
import hash
fn wyhash64(a u64, b u64) u64 { return a + b }
fn wyhash(a u64, b u64) u64 { return a * 10 + b }
fn array_new(n int) int { return n + 11 }
fn new_map(n int) int { return n + 13 }
fn all_after_last(n int) int { return n + 17 }
fn prealloc_malloc(n int) int { return n + 19 }
fn join_path(n int) int { return n + 23 }
fn name_list(n int) int { return n + 29 }
pub fn call_hash() u64 { return wyhash64(3, 4) }
pub fn runtime_control() u64 {
	return hash.wyhash64_c(3, 4)
}
', '
assert.equal(e.wyhash64(3n, 4n), 7n);
assert.equal(e.wyhash(3n, 4n), 34n);
assert.equal(e.array_new(5), 16);
assert.equal(e.new_map(5), 18);
assert.equal(e.all_after_last(5), 22);
assert.equal(e.prealloc_malloc(5), 24);
assert.equal(e.join_path(5), 28);
assert.equal(e.name_list(5), 34);
assert.equal(e.call_hash(), 7n);
assert.equal(e.runtime_control(), 725283166682890696n);
')
}

fn test_wasm_user_hash_runtime_name_in_main() {
	source := os.join_path(os.vtmp_dir(), 'wasm_user_hash_${os.getpid()}.v')
	output := os.join_path(os.vtmp_dir(), 'wasm_user_hash_${os.getpid()}.wasm')
	defer {
		os.rm(source) or {}
		os.rm(output) or {}
	}
	os.write_file(source, 'fn wyhash64(a u64, b u64) u64 { return a + b }
fn main() { println(wyhash64(3, 4)) }
') or { panic(err) }
	for production in [false, true] {
		mut args := [v3_binary(), '-b', 'wasm', '-o', output, source]
		if production {
			args.insert(1, '-prod')
		}
		result := os.exec(args)
		assert result.exit_code == 0, result.output
		assert_valid_wasm(output)
		run_wasi_expect(output, ['7'])
	}
}

fn test_wasm_implicit_main_imported_scalar_calls() {
	dir := os.join_path(os.vtmp_dir(), 'wasm_script_import_${os.getpid()}')
	os.mkdir_all(os.join_path(dir, 'foo')) or { panic(err) }
	defer { os.rmdir_all(dir) or {} }
	main_v := os.join_path(dir, 'main.v')
	os.write_file(main_v, 'import foo as helper
println(helper.answer())
callback := helper.answer
println(callback())
') or { panic(err) }
	os.write_file(os.join_path(dir, 'foo', 'foo.v'), 'module foo
fn value() int { return 21 }
pub fn answer() int { return value() * 2 }
') or { panic(err) }
	for production in [false, true] {
		output := os.join_path(dir, 'main_${production}.wasm')
		mut args := [v3_binary(), '-b', 'wasm', '-o', output, main_v]
		if production {
			args.insert(1, '-prod')
		}
		result := os.exec(args)
		assert result.exit_code == 0, result.output
		assert_valid_wasm(output)
		run_wasi_expect(output, ['42', '42'])
	}
}

fn test_wasm_imported_function_value() {
	dir := os.join_path(os.vtmp_dir(), 'wasm_imported_fn_value_${os.getpid()}')
	defer { os.rmdir_all(dir) or {} }
	os.mkdir_all(os.join_path(dir, 'foo')) or { panic(err) }
	main_v := os.join_path(dir, 'main.v')
	wasm := os.join_path(dir, 'main.wasm')
	os.write_file(main_v, 'module main
import foo as helper
fn main() {
	callback := helper.add
	println(callback(35))
}
') or { panic(err) }
	os.write_file(os.join_path(dir, 'foo', 'foo.v'), 'module foo
pub fn add(value int) int {
	return value + 7
}
') or { panic(err) }
	result := os.exec([v3_binary(), '-b', 'wasm', '-o', wasm, main_v])
	assert result.exit_code == 0, result.output
	assert_valid_wasm(wasm)
	run_wasi_expect(wasm, ['42'])
}

fn test_wasm_control_flow_and_int_print() {
	v3_bin := v3_binary()
	src := 'fn main() {\n\tmut sum := 0\n\tfor i := 0; i < 5; i++ {\n\t\tsum = sum + i\n\t}\n\tif sum > 5 {\n\t\tprintln(sum)\n\t} else {\n\t\tprintln(-1)\n\t}\n}\n'
	wasm := compile_to_wasm(v3_bin, src, 'wasm_flow')
	assert_valid_wasm(wasm)

	node := node_path() or { return }
	runner := os.join_path(os.vtmp_dir(), 'wasm_run_wasi.mjs')
	os.write_file(runner, wasi_runner_js) or { panic(err) }
	res := run_node(node, runner, wasm)
	assert res.exit_code == 0, res.output
	assert last_line(res.output) == '10', res.output
}

fn test_wasm_production_control_flow_and_recursion() {
	source := os.join_path(os.vtmp_dir(), 'wasm_ssa_prod_${os.getpid()}.v')
	output := os.join_path(os.vtmp_dir(), 'wasm_ssa_prod_${os.getpid()}.wasm')
	defer {
		os.rm(source) or {}
		os.rm(output) or {}
	}
	os.write_file(source, 'fn fib(n int) int {
	if n < 2 { return n }
	return fib(n - 1) + fib(n - 2)
}
fn main() {
	mut a := 1
	mut b := 2
	for i := 0; i < 3; i++ { a, b = b, a }
	println(a * 10 + b)
	println(fib(10))
}
') or { panic(err) }
	result := os.exec([v3_binary(), '-prod', '-b', 'wasm', '-o', output, source])
	assert result.exit_code == 0, result.output
	assert_valid_wasm(output)
	run_wasi_expect(output, ['21', '55'])
}

fn test_wasm_exported_functions() {
	v3_bin := v3_binary()
	src := 'fn add(a int, b int) int {\n\treturn a + b\n}\n\nfn fib(n int) int {\n\tif n < 2 {\n\t\treturn n\n\t}\n\treturn fib(n - 1) + fib(n - 2)\n}\n\nfn gcd(a int, b int) int {\n\tmut x := a\n\tmut y := b\n\tfor y != 0 {\n\t\tt := y\n\t\ty = x % y\n\t\tx = t\n\t}\n\treturn x\n}\n'
	wasm := compile_to_wasm(v3_bin, src, 'wasm_exports')
	assert_valid_wasm(wasm)

	node := node_path() or { return }
	runner := os.join_path(os.vtmp_dir(), 'wasm_call_exports.mjs')
	os.write_file(runner, exports_runner_js) or { panic(err) }
	res := run_node(node, runner, wasm)
	assert res.exit_code == 0, res.output
	// runner prints "add fib gcd" results space-separated
	assert last_line(res.output) == '7 55 12', res.output
}

fn test_wasm_add_example_in_javascript_and_wasmer() {
	dir := os.join_path(os.vtmp_dir(), 'wasm_embedding_${os.getpid()}')
	os.mkdir_all(dir) or { panic(err) }
	defer { os.rmdir_all(dir) or {} }
	example_dir := os.join_path(os.dir(os.dir(v3_dir)), 'examples', 'wasm')
	for production in [false, true] {
		output := os.join_path(dir, 'add.wasm')
		mut args := [v3_binary(), '-b', 'wasm', '-o', output]
		if production {
			args << '-prod'
		}
		args << os.join_path(example_dir, 'add.v')
		build := os.exec(args)
		assert build.exit_code == 0, build.output
		assert_valid_wasm(output)
		if node := node_path() {
			result := os.exec([node, os.join_path(example_dir, 'add.mjs'), output])
			assert result.exit_code == 0, result.output
			assert result.output.trim_space() == '42', result.output
		}
		if wasmer := os.find_abs_path_of_executable('wasmer') {
			result := os.exec([wasmer, 'run', '--disable-cache', output, '--invoke', 'add', '20',
				'22'])
			assert result.exit_code == 0, result.output
			assert result.output.trim_space() == '42', result.output
		} else {
			eprintln('> wasmer unavailable; skipping Wasmer embedding check')
		}
	}
}

fn test_wasm_host_imports_and_shared_allocator() {
	node := node_path() or { return }
	dir := os.join_path(os.vtmp_dir(), 'wasm_host_${os.getpid()}')
	os.mkdir_all(dir) or { panic(err) }
	defer { os.rmdir_all(dir) or {} }
	source := os.join_path(dir, 'host.wasm.v')
	output := os.join_path(dir, 'host.wasm')
	runner := os.join_path(dir, 'host.mjs')
	os.write_file(source, '
module main

@[wasm_import_namespace: "host"]
fn WASM.sum_bytes(ptr &u8, count int) int

@[wasm_import_namespace: "unused"]
fn WASM.unused() int

pub fn sum_bytes(ptr &u8, count int) int {
	return WASM.sum_bytes(ptr, count)
}

pub fn runtime_allocation(n int) &u8 {
	// Keep the buffer alive until the host discards the instance.
	return unsafe { malloc(n) }
}
') or { panic(err) }
	os.write_file(runner, "import assert from 'node:assert/strict';
import { readFileSync } from 'node:fs';
const module = await WebAssembly.compile(readFileSync(process.argv[2]));
assert.deepEqual(WebAssembly.Module.imports(module), [
  { module: 'host', name: 'sum_bytes', kind: 'function' }
]);
assert.throws(() => new WebAssembly.Instance(module, {}), TypeError);
assert.throws(() => new WebAssembly.Instance(module, { host: {} }), WebAssembly.LinkError);
let e;
const instance = await WebAssembly.instantiate(module, {
  host: {
    sum_bytes(ptr, count) {
      // Reentrant allocation may grow memory; obtain the view after it.
      e.__v_alloc(131072);
      return new Uint8Array(e.memory.buffer, ptr, count).reduce((a, b) => a + b, 0);
    }
  }
});
e = instance.exports;
const p = e.__v_alloc(3) >>> 0;
assert.equal(p % 16, 0);
assert.deepEqual([...new Uint8Array(e.memory.buffer, p, 3)], [0, 0, 0]);
new Uint8Array(e.memory.buffer, p, 3).set([20, 22, 7]);
const before = e.memory.buffer.byteLength;
const q = e.__v_alloc(131073) >>> 0;
assert.equal(q % 16, 0);
assert.ok(q >= p + 3);
assert.ok(e.memory.buffer.byteLength > before);
assert.ok(new Uint8Array(e.memory.buffer, q, 131073).every(x => x === 0));
const r = e.runtime_allocation(100) >>> 0;
assert.ok(r >= q + 131073);
assert.equal(r % 16, 0);
assert.ok(new Uint8Array(e.memory.buffer, r, 100).every(x => x === 0));
new Uint8Array(e.memory.buffer, r, 100).fill(9);
assert.equal(e.sum_bytes(p, 3), 49);
assert.deepEqual([...new Uint8Array(e.memory.buffer, p, 3)], [20, 22, 7]);
assert.ok(new Uint8Array(e.memory.buffer, r, 100).every(x => x === 9));
assert.throws(() => e.__v_alloc(-1), WebAssembly.RuntimeError);
") or { panic(err) }
	for production in [false, true] {
		mut args := [v3_binary(), '-b', 'wasm', '-o', output]
		if production {
			args << '-prod'
		}
		args << source
		build := os.exec(args)
		assert build.exit_code == 0, build.output
		result := os.exec([node, runner, output])
		assert result.exit_code == 0, result.output
	}
}

fn test_wasm_string_literal_escapes_not_double_decoded() {
	v3_bin := v3_binary()
	// The parser already unescapes literals, so `'a\\nb'` is the four bytes
	// a, backslash, n, b and must print verbatim (not as a newline). Compare
	// against the C backend to avoid escape miscounts.
	src := "fn main() {\n\tprintln('a\\\\nb')\n\tprintln('c\\nd')\n}\n"
	src_path := os.join_path(os.vtmp_dir(), 'wasm_esc.v')
	os.write_file(src_path, src) or { panic(err) }
	c_bin := os.join_path(os.vtmp_dir(), 'wasm_esc_c')
	cres :=
		os.exec([v3_bin, '-b', 'c', '-cc', 'clang', '-o', c_bin, src_path])
	assert cres.exit_code == 0, cres.output
	cout := os.exec([c_bin])
	assert cout.exit_code == 0, cout.output

	wasm := compile_to_wasm(v3_bin, src, 'wasm_esc')
	assert_valid_wasm(wasm)
	node := node_path() or { return }
	runner := os.join_path(os.vtmp_dir(), 'wasm_run_wasi.mjs')
	os.write_file(runner, wasi_runner_js) or { panic(err) }
	wres := run_node(node, runner, wasm)
	assert wres.exit_code == 0, wres.output
	wlines := wres.output.split_into_lines().map(it.trim_space()).filter(it.len > 0)
	clines := cout.output.split_into_lines().map(it.trim_space()).filter(it.len > 0)
	assert wlines.len >= clines.len, wres.output
	for i, want in clines {
		got := wlines[wlines.len - clines.len + i]
		assert got == want, 'line ${i}: wasm ${got} != c ${want}'
	}
}

fn test_wasm_imported_module_numeric_call() {
	v3_bin := v3_binary()
	dir := os.join_path(os.vtmp_dir(), 'wasm_modtest')
	os.rmdir_all(dir) or {}
	os.mkdir_all(os.join_path(dir, 'moda')) or { panic(err) }
	os.write_file(os.join_path(dir, 'main.v'),
		'import moda\n\nfn main() {\n\tprintln(moda.answer())\n\tprintln(moda.add(3, 4))\n}\n') or {
		panic(err)
	}
	// answer() calls a bare helper() in the same module, which must resolve to
	// moda.helper (not main-module helper).
	os.write_file(os.join_path(dir, 'moda', 'moda.v'),
		'module moda\n\nfn helper() int {\n\treturn 21\n}\n\npub fn answer() int {\n\treturn helper() * 2\n}\n\npub fn add(a int, b int) int {\n\treturn a + b\n}\n') or {
		panic(err)
	}
	out_wasm := os.join_path(dir, 'main.wasm')
	main_v := os.join_path(dir, 'main.v')
	res :=
		os.exec([v3_bin, '-b', 'wasm', '-o', '${out_wasm}', '${main_v}'])
	assert res.exit_code == 0, res.output
	assert_valid_wasm(out_wasm)
	// No fallback warnings: the imported module calls must resolve.
	assert !res.output.contains('unsupported call'), res.output

	node := node_path() or { return }
	runner := os.join_path(os.vtmp_dir(), 'wasm_run_wasi.mjs')
	os.write_file(runner, wasi_runner_js) or { panic(err) }
	rres := run_node(node, runner, out_wasm)
	assert rres.exit_code == 0, rres.output
	lines := rres.output.split_into_lines().map(it.trim_space()).filter(it.len > 0)
	expected := ['42', '7']
	assert lines.len >= expected.len, rres.output
	for i, want in expected {
		got := lines[lines.len - expected.len + i]
		assert got == want, 'line ${i}: got ${got}, want ${want} (full: ${rres.output})'
	}
}

fn test_wasm_module_scoped_globals() {
	v3_bin := v3_binary()
	// main and an imported module both declare `__global counter`. They must map
	// to two distinct wasm globals keyed by module, so bumping the module's copy
	// leaves main's untouched (main=100, foo=3).
	dir := os.join_path(os.vtmp_dir(), 'wasm_modglobals')
	os.rmdir_all(dir) or {}
	os.mkdir_all(os.join_path(dir, 'foo')) or { panic(err) }
	os.write_file(os.join_path(dir, 'main.v'),
		'module main\n\nimport foo\n\n__global counter int\n\nfn main() {\n\tcounter = 100\n\tfoo.bump()\n\tfoo.bump()\n\tfoo.bump()\n\tprintln(counter)\n\tprintln(foo.get())\n}\n') or {
		panic(err)
	}
	os.write_file(os.join_path(dir, 'foo', 'foo.v'),
		'module foo\n\n__global counter int\n\npub fn bump() {\n\tcounter++\n}\n\npub fn get() int {\n\treturn counter\n}\n') or {
		panic(err)
	}
	out_wasm := os.join_path(dir, 'main.wasm')
	main_v := os.join_path(dir, 'main.v')
	res :=
		os.exec([v3_bin, '-enable-globals', '-b', 'wasm', '-o', '${out_wasm}', '${main_v}'])
	assert res.exit_code == 0, res.output
	assert_valid_wasm(out_wasm)

	node := node_path() or { return }
	runner := os.join_path(os.vtmp_dir(), 'wasm_run_wasi.mjs')
	os.write_file(runner, wasi_runner_js) or { panic(err) }
	rres := run_node(node, runner, out_wasm)
	assert rres.exit_code == 0, rres.output
	lines := rres.output.split_into_lines().map(it.trim_space()).filter(it.len > 0)
	expected := ['100', '3']
	assert lines.len >= expected.len, rres.output
	for i, want in expected {
		got := lines[lines.len - expected.len + i]
		assert got == want, 'line ${i}: got ${got}, want ${want} (full: ${rres.output})'
	}
}

fn test_wasm_imported_module_alias_call() {
	v3_bin := v3_binary()
	dir := os.join_path(os.vtmp_dir(), 'wasm_modalias')
	os.rmdir_all(dir) or {}
	os.mkdir_all(os.join_path(dir, 'moda')) or { panic(err) }
	os.write_file(os.join_path(dir, 'main.v'),
		'import moda as m\n\nfn main() {\n\tprintln(m.answer())\n\tprintln(m.add(3, 4))\n}\n') or {
		panic(err)
	}
	os.write_file(os.join_path(dir, 'moda', 'moda.v'),
		'module moda\n\npub fn answer() int {\n\treturn 42\n}\n\npub fn add(a int, b int) int {\n\treturn a + b\n}\n') or {
		panic(err)
	}
	out_wasm := os.join_path(dir, 'main.wasm')
	main_v := os.join_path(dir, 'main.v')
	res :=
		os.exec([v3_bin, '-b', 'wasm', '-o', '${out_wasm}', '${main_v}'])
	assert res.exit_code == 0, res.output
	assert_valid_wasm(out_wasm)
	assert !res.output.contains('unsupported call'), res.output
	run_wasi_expect(out_wasm, ['42', '7'])
}

fn test_wasm_global_updates() {
	wasm := compile_to_wasm(v3_binary(), "__global a int
__global b int
__global p = u8(250)
__global q = u8(10)

// Evaluate both values on the right before writing either global,
// including globals narrower than 32 bits.
fn parallel_multi_assign_swaps_globals() {
	a = 1
	b = 2
	a, b = b, a
	println(a)
	println(b)
	p, q = q, p
	println(int(p))
	println(int(q))
}

__global counter int
__global x = 10
__global narrow_value u8

fn inc() {
	counter = counter + 1
}

// Check reads and writes across functions, initializers, compound
// assignments, and narrowing for numeric globals.
fn numeric_globals() {
	counter = 3
	inc()
	inc()
	println(counter)
	x += 5
	println(x)
	narrow_value = u8(250)
	narrow_value += u8(10)
	println(int(narrow_value))
}

__global postfix_counter int
__global postfix_byte = u8(250)

fn bump() {
	postfix_counter++
}

// `counter++`/`counter--` on a numeric global must read-modify-write the wasm
// global (incl. across functions) and narrow-wrap for sub-32-bit globals.
fn global_postfix() {
	postfix_counter = 3
	postfix_counter++
	bump()
	postfix_counter--
	println(postfix_counter)
	postfix_byte++
	println(int(postfix_byte))
}

fn main() {
	println('parallel_multi_assign_swaps_globals')
	parallel_multi_assign_swaps_globals()
	println('numeric_globals')
	numeric_globals()
	println('global_postfix')
	global_postfix()
}
", 'wasm_global_updates')
	assert_valid_wasm(wasm)
	run_wasi_expect(wasm, [
		'parallel_multi_assign_swaps_globals',
		'2',
		'1',
		'10',
		'250',
		'numeric_globals',
		'5',
		'15',
		'4',
		'global_postfix',
		'4',
		'251',
	])
}

fn test_wasm_import_aliases_are_file_scoped() {
	v3_bin := v3_binary()
	// Two modules each `import ... as m` for different real modules; the alias
	// must resolve per file, not via a single global map.
	dir := os.join_path(os.vtmp_dir(), 'wasm_aliascol')
	os.rmdir_all(dir) or {}
	for sub in ['mc', 'md', 'usea', 'useb'] {
		os.mkdir_all(os.join_path(dir, sub)) or { panic(err) }
	}
	os.write_file(os.join_path(dir, 'main.v'),
		'import usea\nimport useb\n\nfn main() {\n\tprintln(usea.go())\n\tprintln(useb.go())\n}\n') or {
		panic(err)
	}
	os.write_file(os.join_path(dir, 'mc', 'mc.v'),
		'module mc\n\npub fn val() int {\n\treturn 100\n}\n') or { panic(err) }
	os.write_file(os.join_path(dir, 'md', 'md.v'),
		'module md\n\npub fn val() int {\n\treturn 200\n}\n') or { panic(err) }
	os.write_file(os.join_path(dir, 'usea', 'usea.v'),
		'module usea\n\nimport mc as m\n\npub fn go() int {\n\treturn m.val()\n}\n') or {
		panic(err)
	}
	os.write_file(os.join_path(dir, 'useb', 'useb.v'),
		'module useb\n\nimport md as m\n\npub fn go() int {\n\treturn m.val()\n}\n') or {
		panic(err)
	}
	out_wasm := os.join_path(dir, 'main.wasm')
	main_v := os.join_path(dir, 'main.v')
	res :=
		os.exec([v3_bin, '-b', 'wasm', '-o', '${out_wasm}', '${main_v}'])
	assert res.exit_code == 0, res.output
	assert_valid_wasm(out_wasm)
	assert !res.output.contains('unsupported call'), res.output
	run_wasi_expect(out_wasm, ['100', '200'])
}

fn test_wasm_numeric_operations() {
	wasm := compile_to_wasm(v3_binary(), "type Byte = u8
type MyInt = int

fn val() Byte {
	return Byte(u16(300))
}

fn add(a MyInt, b MyInt) MyInt {
	return a + b
}

// Scalar aliases must classify as their base type: the alias-typed function
// is emitted, and Byte(u16(300)) wraps to u8 (44) rather than keeping 300.
fn numeric_type_aliases() {
	println(int(val()))
	println(int(add(3, 4)))
	mut x := Byte(250)
	x += Byte(10)
	println(int(x))
}

// V's float->int casts saturate: u64(-1.0) -> 0, 2^63 keeps the high bit,
// 2^64 saturates to max. Signedness comes from the target type.
fn float_to_unsigned_cast_saturates() {
	println(u64(-1.0))
	println(u64(9223372036854775808.0))
	println(u64(18446744073709551616.0))
	println(int(f32(3.9)))
	println(int(f32(-2.7)))
}

// `1 + u64(x)` promotes to u64, so it must print unsigned, not as a signed
// int. Also a u32 sum with the high bit set must zero-extend.
fn mixed_numeric_println_promotes() {
	println(1 + u64(9223372036854775808))
	println(u64(9223372036854775808) + 1)
	a := u32(4000000000)
	println(1 + a)
}

// An unsigned operand makes / and % unsigned even when the lhs is an
// untyped/signed literal; signed div/rem still works for signed operands.
fn unsigned_division_promotes() {
	println(4000000000 / u32(2))
	println(4000000001 % u32(3))
	println(-10 / 3)
	println(-10 % 3)
}

// A non-negative literal beyond i64 max must keep its magnitude/sign when
// cast to a float (the integer path would wrap it negative first).
fn float_cast_of_large_literal() {
	println(int(f64(9223372036854775808) > 0.0))
	println(int(f64(18446744073709551615) > 0.0))
	println(int(f64(9223372036854775808) == 9223372036854775808.0))
}

// Mixed signed/unsigned comparisons compare by mathematical value, so a
// negative signed operand is smaller than any unsigned operand.
fn mixed_signed_unsigned_comparison() {
	println(int(i64(-89) <= u64(567)))
	println(int(int(-1) != u32(0xffffffff)))
	println(int(u64(0xfffffffffffffffe) == i64(-2)))
	println(int(u32(8543) > int(-7523)))
	println(int(i16(-27) < u32(65463356)))
}

// Negating a literal outside i32 range must keep i64 width (and wrap on
// overflow like V's cast), not truncate to 32 bits.
fn unary_minus_large_literal() {
	println(i64(-9223372036854775808))
	println(i64(-9223372036854775809))
	println(-9223372036854775807)
	println(i64(-5))
}

fn main() {
	println('numeric_type_aliases')
	numeric_type_aliases()
	println('float_to_unsigned_cast_saturates')
	float_to_unsigned_cast_saturates()
	println('mixed_numeric_println_promotes')
	mixed_numeric_println_promotes()
	println('unsigned_division_promotes')
	unsigned_division_promotes()
	println('float_cast_of_large_literal')
	float_cast_of_large_literal()
	println('mixed_signed_unsigned_comparison')
	mixed_signed_unsigned_comparison()
	println('unary_minus_large_literal')
	unary_minus_large_literal()
}
", 'wasm_numeric_operations')
	assert_valid_wasm(wasm)
	run_wasi_expect(wasm, [
		'numeric_type_aliases',
		'44',
		'7',
		'4',
		'float_to_unsigned_cast_saturates',
		'0',
		'9223372036854775808',
		'18446744073709551615',
		'3',
		'-2',
		'mixed_numeric_println_promotes',
		'9223372036854775809',
		'9223372036854775809',
		'4000000001',
		'unsigned_division_promotes',
		'2000000000',
		'2',
		'-3',
		'-1',
		'float_cast_of_large_literal',
		'1',
		'1',
		'1',
		'mixed_signed_unsigned_comparison',
		'1',
		'1',
		'0',
		'1',
		'1',
		'unary_minus_large_literal',
		'-9223372036854775808',
		'9223372036854775807',
		'-9223372036854775807',
		'-5',
	])
}

fn test_wasm_bitwise_operations() {
	wasm := compile_to_wasm(v3_binary(), "fn w32() u32 {
	return u32(40)
}

fn w64() u64 {
	return u64(40)
}

// The shift result keeps the lhs width even with a wider count: a u32 shift
// by 40 is over-width (>= 32) -> 0, while a u64 shift by 40 is valid.
fn shift_keeps_lhs_width() {
	a := u32(1)
	println(a << w64())
	b := u64(1)
	println(b << w32())
	mut e := u32(1)
	e <<= w64()
	println(e)
}

// `>>>` on a signed narrow operand works on the 8/16-bit pattern, so the
// sign-extension bits must be masked off first.
fn unsigned_right_shift_masks_narrow() {
	println(int(i8(-5) >>> 1))
	println(int(i16(-5) >>> 1))
	println(int(u8(250) >>> 1))
}

fn allset() u8 {
	return ~u8(0)
}

// ~ keeps the operand width: ~u8(0) is 255, ~u16(0) is 65535, and a u8
// return carries the narrowed value.
fn bit_not_narrows_result() {
	println(~u8(0))
	println(~u16(0))
	println(allset())
}

// `>>>=` on a signed narrow local must shift the 8/16-bit pattern, not the
// sign-extended i32.
fn unsigned_shift_assign_masks_narrow() {
	mut x := i8(-5)
	x >>>= 1
	println(int(x))
	mut y := i16(-5)
	y >>>= 1
	println(int(y))
}

fn rc(n int) int {
	return n
}

// V promotes narrow types to int for shifts, so i8(-1) >> 8 stays -1 (the
// computation width is 32), while full-width over-width shifts zero out.
fn shift_over_width_semantics() {
	println(int(i8(-1) >> rc(8)))
	println(u32(1) << rc(32))
	a := u64(1)
	println(a << rc(64))
	println(int(i8(-5) >>> rc(1)))
}

fn main() {
	println('shift_keeps_lhs_width')
	shift_keeps_lhs_width()
	println('unsigned_right_shift_masks_narrow')
	unsigned_right_shift_masks_narrow()
	println('bit_not_narrows_result')
	bit_not_narrows_result()
	println('unsigned_shift_assign_masks_narrow')
	unsigned_shift_assign_masks_narrow()
	println('shift_over_width_semantics')
	shift_over_width_semantics()
}
", 'wasm_bitwise_operations')
	assert_valid_wasm(wasm)
	run_wasi_expect(wasm, [
		'shift_keeps_lhs_width',
		'0',
		'1099511627776',
		'0',
		'unsigned_right_shift_masks_narrow',
		'125',
		'32765',
		'125',
		'bit_not_narrows_result',
		'255',
		'65535',
		'255',
		'unsigned_shift_assign_masks_narrow',
		'125',
		'32765',
		'shift_over_width_semantics',
		'-1',
		'0',
		'0',
		'125',
	])
}

fn test_wasm_output_path_is_exact() {
	v3_bin := v3_binary()
	src_path := os.join_path(os.vtmp_dir(), 'wasm_exactpath.v')
	os.write_file(src_path, "fn main() {\n\tprintln('hi')\n}\n") or { panic(err) }
	// -o with a path that does not end in .wasm must write that exact file,
	// not <path>.wasm.
	out_path := os.join_path(os.vtmp_dir(), 'wasm_exact_out')
	os.rm(out_path) or {}
	os.rm(out_path + '.wasm') or {}
	res :=
		os.exec([v3_bin, '-b', 'wasm', '-o', out_path, src_path])
	assert res.exit_code == 0, res.output
	assert os.exists(out_path), 'expected exact output ${out_path}'
	assert !os.exists(out_path + '.wasm'), 'unexpected ${out_path}.wasm'
	assert_valid_wasm(out_path)
}

fn test_wasm_nested_module_import_call() {
	v3_bin := v3_binary()
	// `import foo.bar` selects `bar`; calls must resolve to the `module bar`
	// declaration name, not the full import path.
	dir := os.join_path(os.vtmp_dir(), 'wasm_nestmod')
	os.rmdir_all(dir) or {}
	os.mkdir_all(os.join_path(dir, 'foo', 'bar')) or { panic(err) }
	os.write_file(os.join_path(dir, 'main.v'),
		'import foo.bar\n\nfn main() {\n\tprintln(bar.answer())\n\tprintln(bar.add(3, 4))\n}\n') or {
		panic(err)
	}
	os.write_file(os.join_path(dir, 'foo', 'bar', 'bar.v'),
		'module bar\n\npub fn answer() int {\n\treturn 42\n}\n\npub fn add(a int, b int) int {\n\treturn a + b\n}\n') or {
		panic(err)
	}
	out_wasm := os.join_path(dir, 'main.wasm')
	main_v := os.join_path(dir, 'main.v')
	res :=
		os.exec([v3_bin, '-b', 'wasm', '-o', '${out_wasm}', '${main_v}'])
	assert res.exit_code == 0, res.output
	assert_valid_wasm(out_wasm)
	assert !res.output.contains('unsupported call'), res.output
	run_wasi_expect(out_wasm, ['42', '7'])
}

fn test_wasm_nested_imported_modules() {
	v3_bin := v3_binary()
	// Nested modules imported under different aliases must resolve to their own
	// functions. (Two nested modules that share the same leaf name collapse to a
	// single identity in the shared import-resolution layer — the C backend emits
	// only one such function too — so distinct leaves are used here.)
	dir := os.join_path(os.vtmp_dir(), 'wasm_nestedmods')
	os.rmdir_all(dir) or {}
	os.mkdir_all(os.join_path(dir, 'foo', 'alpha')) or { panic(err) }
	os.mkdir_all(os.join_path(dir, 'bar', 'beta')) or { panic(err) }
	os.write_file(os.join_path(dir, 'main.v'),
		'import foo.alpha as fu\nimport bar.beta as bu\n\nfn main() {\n\tprintln(fu.val())\n\tprintln(bu.val())\n}\n') or {
		panic(err)
	}
	os.write_file(os.join_path(dir, 'foo', 'alpha', 'alpha.v'),
		'module alpha\n\npub fn val() int {\n\treturn 111\n}\n') or { panic(err) }
	os.write_file(os.join_path(dir, 'bar', 'beta', 'beta.v'),
		'module beta\n\npub fn val() int {\n\treturn 222\n}\n') or { panic(err) }
	out_wasm := os.join_path(dir, 'main.wasm')
	main_v := os.join_path(dir, 'main.v')
	res :=
		os.exec([v3_bin, '-b', 'wasm', '-o', '${out_wasm}', '${main_v}'])
	assert res.exit_code == 0, res.output
	assert_valid_wasm(out_wasm)
	assert !res.output.contains('unsupported call'), res.output
	run_wasi_expect(out_wasm, ['111', '222'])
}

fn test_wasm_main_dir_matching_import_name() {
	v3_bin := v3_binary()
	// The main file lives in a directory whose name matches an imported module;
	// `fn main` must still be treated as main-module (export main/_start).
	dir := os.join_path(os.vtmp_dir(), 'wasm_dircollide', 'moda')
	os.rmdir_all(os.dir(dir)) or {}
	os.mkdir_all(os.join_path(dir, 'moda')) or { panic(err) }
	os.write_file(os.join_path(dir, 'main.v'),
		'import moda\n\nfn main() {\n\tprintln(moda.answer())\n}\n') or { panic(err) }
	os.write_file(os.join_path(dir, 'moda', 'moda.v'),
		'module moda\n\npub fn answer() int {\n\treturn 42\n}\n') or { panic(err) }
	out_wasm := os.join_path(dir, 'main.wasm')
	main_v := os.join_path(dir, 'main.v')
	res :=
		os.exec([v3_bin, '-b', 'wasm', '-o', '${out_wasm}', '${main_v}'])
	assert res.exit_code == 0, res.output
	assert_valid_wasm(out_wasm)
	run_wasi_expect(out_wasm, ['42'])
}

fn test_wasm_user_function_named_memory() {
	v3_bin := v3_binary()
	// A user `fn memory()` must not collide with the exported linear memory;
	// the module stays valid (the conflicting export is skipped).
	src := 'fn memory() int {\n\treturn 7\n}\n\nfn main() {\n\tprintln(memory())\n}\n'
	wasm := compile_to_wasm(v3_bin, src, 'wasm_memname')
	assert_valid_wasm(wasm)
	run_wasi_expect(wasm, ['7'])
}

fn test_wasm_init_runs_before_main() {
	v3_bin := v3_binary()
	// fn init() is an entry point: it (and imported-module inits) run before main.
	src := "fn init() {\n\tprintln('init')\n}\n\nfn main() {\n\tprintln('main')\n}\n"
	wasm := compile_to_wasm(v3_bin, src, 'wasm_init')
	assert_valid_wasm(wasm)
	run_wasi_expect(wasm, ['init', 'main'])
}

fn test_wasm_imported_module_init() {
	v3_bin := v3_binary()
	dir := os.join_path(os.vtmp_dir(), 'wasm_initmod')
	os.rmdir_all(dir) or {}
	os.mkdir_all(os.join_path(dir, 'moda')) or { panic(err) }
	os.write_file(os.join_path(dir, 'main.v'),
		"import moda\n\nfn main() {\n\tprintln('main')\n\tprintln(moda.answer())\n}\n") or {
		panic(err)
	}
	os.write_file(os.join_path(dir, 'moda', 'moda.v'),
		"module moda\n\nfn init() {\n\tprintln('moda init')\n}\n\npub fn answer() int {\n\treturn 42\n}\n") or {
		panic(err)
	}
	out_wasm := os.join_path(dir, 'main.wasm')
	res := os.exec([v3_bin, '-b', 'wasm', '-o', '${out_wasm}', os.join_path(dir, 'main.v')])
	assert res.exit_code == 0, res.output
	assert_valid_wasm(out_wasm)
	run_wasi_expect(out_wasm, ['moda init', 'main', '42'])
}

fn test_wasm_char_literals() {
	v3_bin := v3_binary()
	src := 'fn code() u8 {\n\treturn `A`\n}\n\nfn main() {\n\tprintln(int(code()))\n\tprintln(int(`A`))\n\tprintln(int(`0`))\n\tprintln(int(`\\n`))\n}\n'
	wasm := compile_to_wasm(v3_bin, src, 'wasm_char')
	assert_valid_wasm(wasm)
	run_wasi_expect(wasm, ['65', '65', '48', '10'])
}

fn test_wasm_init_only_imported_module() {
	v3_bin := v3_binary()
	// An imported module with only an init() (no called function) must still
	// run its init before main.
	dir := os.join_path(os.vtmp_dir(), 'wasm_initonly')
	os.rmdir_all(dir) or {}
	os.mkdir_all(os.join_path(dir, 'moda')) or { panic(err) }
	os.write_file(os.join_path(dir, 'main.v'), "import moda\n\nfn main() {\n\tprintln('main')\n}\n") or {
		panic(err)
	}
	os.write_file(os.join_path(dir, 'moda', 'moda.v'),
		"module moda\n\nfn init() {\n\tprintln('moda init')\n}\n") or { panic(err) }
	out_wasm := os.join_path(dir, 'main.wasm')
	res := os.exec([v3_bin, '-b', 'wasm', '-o', '${out_wasm}', os.join_path(dir, 'main.v')])
	assert res.exit_code == 0, res.output
	assert_valid_wasm(out_wasm)
	run_wasi_expect(out_wasm, ['moda init', 'main'])
}

fn test_wasm_init_dependency_order() {
	v3_bin := v3_binary()
	// main -> a -> b: inits run dependency-first (b, then a), then main.
	dir := os.join_path(os.vtmp_dir(), 'wasm_initdep')
	os.rmdir_all(dir) or {}
	os.mkdir_all(os.join_path(dir, 'a')) or { panic(err) }
	os.mkdir_all(os.join_path(dir, 'b')) or { panic(err) }
	os.write_file(os.join_path(dir, 'main.v'),
		"import a\n\nfn main() {\n\tprintln('main')\n\tprintln(a.av())\n}\n") or { panic(err) }
	os.write_file(os.join_path(dir, 'a', 'a.v'),
		"module a\n\nimport b\n\nfn init() {\n\tprintln('a init')\n}\n\npub fn av() int {\n\treturn b.bv()\n}\n") or {
		panic(err)
	}
	os.write_file(os.join_path(dir, 'b', 'b.v'),
		"module b\n\nfn init() {\n\tprintln('b init')\n}\n\npub fn bv() int {\n\treturn 5\n}\n") or {
		panic(err)
	}
	out_wasm := os.join_path(dir, 'main.wasm')
	res := os.exec([v3_bin, '-b', 'wasm', '-o', '${out_wasm}', os.join_path(dir, 'main.v')])
	assert res.exit_code == 0, res.output
	assert_valid_wasm(out_wasm)
	run_wasi_expect(out_wasm, ['b init', 'a init', 'main', '5'])
}

fn test_wasm_imported_module_const() {
	v3_bin := v3_binary()
	// A numeric const accessed through an imported-module selector (mod.name)
	// must be inlined, not emitted as 0.
	dir := os.join_path(os.vtmp_dir(), 'wasm_constmod')
	os.rmdir_all(dir) or {}
	os.mkdir_all(os.join_path(dir, 'moda')) or { panic(err) }
	os.write_file(os.join_path(dir, 'main.v'),
		'import moda\n\nfn main() {\n\tprintln(moda.answer)\n\tprintln(moda.answer + 1)\n}\n') or {
		panic(err)
	}
	os.write_file(os.join_path(dir, 'moda', 'moda.v'), 'module moda\n\npub const answer = 42\n') or {
		panic(err)
	}
	out_wasm := os.join_path(dir, 'main.wasm')
	res := os.exec([v3_bin, '-b', 'wasm', '-o', '${out_wasm}', os.join_path(dir, 'main.v')])
	assert res.exit_code == 0, res.output
	assert_valid_wasm(out_wasm)
	assert !res.output.contains('unsupported'), res.output
	run_wasi_expect(out_wasm, ['42', '43'])
}

fn test_wasm_labeled_break_continue() {
	v3_bin := v3_binary()
	// `break label` exits the labeled outer loop (count 1, not 3); `continue
	// label` advances the outer loop (count 3, not 9); plain inner break works.
	src := 'fn main() {\n\tmut cb := 0\n\touter: for i := 0; i < 3; i++ {\n\t\tfor j := 0; j < 3; j++ {\n\t\t\tcb++\n\t\t\tif j == 0 {\n\t\t\t\tbreak outer\n\t\t\t}\n\t\t}\n\t}\n\tprintln(cb)\n\tmut cc := 0\n\to2: for i := 0; i < 3; i++ {\n\t\tfor j := 0; j < 3; j++ {\n\t\t\tcc++\n\t\t\tcontinue o2\n\t\t}\n\t}\n\tprintln(cc)\n\tmut cn := 0\n\tfor i := 0; i < 3; i++ {\n\t\tfor j := 0; j < 3; j++ {\n\t\t\tcn++\n\t\t\tbreak\n\t\t}\n\t}\n\tprintln(cn)\n}\n'
	wasm := compile_to_wasm(v3_bin, src, 'wasm_labeled')
	assert_valid_wasm(wasm)
	assert !res_contains_unsupported(v3_bin, src), 'labeled loop emitted a warning'
	run_wasi_expect(wasm, ['1', '3', '3'])
}

fn res_contains_unsupported(v3_bin string, src string) bool {
	src_path := os.join_path(os.vtmp_dir(), 'wasm_labeled_warn.v')
	out := os.join_path(os.vtmp_dir(), 'wasm_labeled_warn.wasm')
	os.write_file(src_path, src) or { panic(err) }
	res :=
		os.exec([v3_bin, '-b', 'wasm', '-o', '${out}', src_path])
	return res.output.contains('unsupported statement: label_stmt')
}

fn test_wasm_global_initializers() {
	wasm := compile_to_wasm(v3_binary(), "__global narrow_byte = u8(u16(300))
__global s = i8(i16(128))
__global w = u16(u32(70000))

// A narrowing cast initializer on an inferred narrow global must be
// wrapped to the global's width at compile time, so the first read already
// sees the V value (u8(u16(300))=44, i8(i16(128))=-128, u16(u32(70000))=4464).
fn global_narrow_cast_initializer() {
	println(int(narrow_byte))
	println(int(s))
	println(int(w))
}

const base = 10

__global a = 1 + 2
__global c = base * 4 + 1
__global e = 1 << 4
__global f = (7 - 2) * 3
__global neg = -5 + 2
__global big = i64(1) << 40

// A const-expression initializer (binary ops, const-identifier references,
// shifts) is folded into the wasm global's constant init, not zeroed.
fn global_const_expr_initializers() {
	println(a)
	println(c)
	println(e)
	println(f)
	println(neg)
	println(big)
}

__global b = int(u8(u16(300)))
__global g = int(i8(i16(128)))
__global h = u8(u16(300)) + u8(100)
__global fl = 1.5 + 2.0

// A nested cast initializer keeps each cast's width: the inner cast narrows
// before the wider outer cast, so `int(u8(u16(300)))` is 44, not 300, and a folded
// float initializer rounds through an int cast (1.5 + 2.0 -> 3).
fn global_nested_cast_initializer() {
	println(b)
	println(g)
	println(int(h))
	println(int(fl))
}

__global x = int(u8(250) + u8(10))
__global y = int(u8(200) + u8(100))

// A folded infix initializer must wrap to its own resolved type before a
// wider outer cast observes it: `u8(250)+u8(10)` narrows to 4 (not 260)
// before the surrounding `int`, matching normal codegen.
fn global_infix_narrows_before_outer_cast() {
	println(x)
	println(y)
}

__global large_float = f64(9223372036854775808)

// A float cast of a large non-negative integer literal must stay positive,
// like gen_cast: f64(9223372036854775808) (2^63) must not flip sign through
// parse_int_literal's wrapped i64.
fn global_float_cast_of_large_literal() {
	println(large_float > 0.0)
}

__global half = u64(18446744073709551615) / u64(2)
__global rem = u64(18446744073709551615) % u64(10)
__global cmp = u64(18446744073709551615) > u64(1)

// Unsigned constant initializers must fold with unsigned div/rem/compare:
// folded operands are i64 bit patterns (u64 max -> -1), so signed division
// would mis-fold `u64(max) / u64(2)` to 0 instead of 9223372036854775807.
fn global_unsigned_const_fold() {
	println(half)
	println(rem)
	println(cmp)
}

fn main() {
	println('global_narrow_cast_initializer')
	global_narrow_cast_initializer()
	println('global_const_expr_initializers')
	global_const_expr_initializers()
	println('global_nested_cast_initializer')
	global_nested_cast_initializer()
	println('global_infix_narrows_before_outer_cast')
	global_infix_narrows_before_outer_cast()
	println('global_float_cast_of_large_literal')
	global_float_cast_of_large_literal()
	println('global_unsigned_const_fold')
	global_unsigned_const_fold()
}
", 'wasm_global_initializers')
	assert_valid_wasm(wasm)
	run_wasi_expect(wasm, [
		'global_narrow_cast_initializer',
		'44',
		'-128',
		'4464',
		'global_const_expr_initializers',
		'3',
		'41',
		'16',
		'15',
		'-3',
		'1099511627776',
		'global_nested_cast_initializer',
		'44',
		'-128',
		'144',
		'3',
		'global_infix_narrows_before_outer_cast',
		'4',
		'44',
		'global_float_cast_of_large_literal',
		'true',
		'global_unsigned_const_fold',
		'9223372036854775807',
		'5',
		'true',
	])
}

const wasi_runner_js = "import { WASI } from 'node:wasi';
import { readFile } from 'node:fs/promises';
const wasi = new WASI({ version: 'preview1', args: [], env: {} });
const bytes = await readFile(process.argv[2]);
const mod = await WebAssembly.compile(bytes);
const inst = await WebAssembly.instantiate(mod, wasi.getImportObject());
wasi.start(inst);
"

const exports_runner_js = "import { readFileSync } from 'node:fs';
const { instance: i } = await WebAssembly.instantiate(readFileSync(process.argv[2]));
const e = i.exports;
process.stdout.write(`\${e.add(3, 4)} \${e.fib(10)} \${e.gcd(48, 36)}`);
"
