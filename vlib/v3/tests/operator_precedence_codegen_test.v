import os

// Locks in V's documented operator precedence (see the language reference,
// Appendix II) for the v3 parser. `+ - | ^` all share the `sum` level and
// `* / % << >> >>> &` all share the `product` level, so shifts and `&` bind
// tighter than `+`/`-`/`|`/`^`, and operators inside one level fold left.
// A finer-grained binding-power table (e.g. splitting `|` below `^` below `+`,
// or `<<` below `*`) silently reparses real programs, so every boundary pair is
// pinned here.

const vexe = @VEXE
const tests_dir = os.dir(@FILE)
const v3_dir = os.dir(tests_dir)
const vlib_dir = os.dir(v3_dir)
const v3_src = os.join_path(v3_dir, 'v3.v')

fn tmp_precedence_path(name string) string {
	return os.join_path(os.temp_dir(), 'v3_prec_${name}_${os.getpid()}')
}

fn build_v3_precedence() string {
	v3_bin := tmp_precedence_path('compiler')
	build :=
		os.execute('${os.quoted_path(vexe)} -path "${vlib_dir}|@vlib|@vmodules" -o ${os.quoted_path(v3_bin)} ${os.quoted_path(v3_src)}')
	assert build.exit_code == 0, build.output
	return v3_bin
}

fn eval_int(v3_bin string, name string, expr string) string {
	src := 'fn main() {\n\tprintln(${expr})\n}\n'
	src_path := '${tmp_precedence_path(name)}.v'
	bin_path := tmp_precedence_path('${name}_bin')
	os.write_file(src_path, src) or { panic(err) }
	compile :=
		os.execute('${os.quoted_path(v3_bin)} ${os.quoted_path(src_path)} -b c -o ${os.quoted_path(bin_path)}')
	assert compile.exit_code == 0, '${expr}\n${compile.output}'
	assert !compile.output.contains('C compilation failed'), '${expr}\n${compile.output}'
	run := os.execute(os.quoted_path(bin_path))
	assert run.exit_code == 0, '${expr}\n${run.output}'
	return run.output.trim_space()
}

fn test_operator_precedence_boundaries() {
	v3_bin := build_v3_precedence()

	// product binds tighter than sum
	assert eval_int(v3_bin, 'mul_vs_add', '2 + 3 * 4') == '14'
	assert eval_int(v3_bin, 'div_vs_sub', '20 - 12 / 4') == '17'

	// shifts live at product, so they bind tighter than +/-
	assert eval_int(v3_bin, 'add_then_shift', '1 + 2 << 3') == '17'
	assert eval_int(v3_bin, 'shift_then_add', '8 << 1 + 1') == '17'
	assert eval_int(v3_bin, 'shift_then_sub', '2 << 3 - 1') == '15'

	// & lives at product, |/^ live at sum
	assert eval_int(v3_bin, 'and_vs_add', '10 & 3 + 1') == '3'
	assert eval_int(v3_bin, 'or_vs_and', '1 | 2 & 3') == '3'
	assert eval_int(v3_bin, 'xor_vs_and', '5 & 3 ^ 1') == '0'

	// |, ^, +, - all share the sum level and fold left
	assert eval_int(v3_bin, 'or_vs_xor', '1 | 2 ^ 3') == '0'
	assert eval_int(v3_bin, 'xor_then_or', '1 ^ 2 | 4') == '7'
	assert eval_int(v3_bin, 'xor_vs_add', '1 ^ 2 + 3') == '6'

	// left associativity within a level
	assert eval_int(v3_bin, 'sub_left_assoc', '10 - 3 - 2') == '5'
	assert eval_int(v3_bin, 'mod_left_assoc', '2 * 3 % 4') == '2'
	assert eval_int(v3_bin, 'shift_left_assoc', '32 >> 1 >> 2') == '4'

	// explicit parentheses override
	assert eval_int(v3_bin, 'paren_add_shift', '(1 + 2) << 3') == '24'

	// bitwise binds tighter than comparison
	assert eval_int(v3_bin, 'or_vs_eq', 'int(3 | 4 == 7)') == '1'
	assert eval_int(v3_bin, 'and_vs_eq', 'int(5 & 3 == 1)') == '1'

	// comparison binds tighter than &&, which binds tighter than ||
	assert eval_int(v3_bin, 'cmp_vs_and', 'int(1 < 2 && 3 < 4)') == '1'
	assert eval_int(v3_bin, 'and_vs_or', 'int(true || false && false)') == '1'
}
