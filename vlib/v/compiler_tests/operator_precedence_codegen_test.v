import os
import v.tempname

// Locks in V's documented operator precedence (see the language reference,
// Appendix II) for the v3 parser. `+ - | ^` all share the `sum` level and
// `* / % << >> >>> &` all share the `product` level, so shifts and `&` bind
// tighter than `+`/`-`/`|`/`^`, and operators inside one level fold left.
// A finer-grained binding-power table (e.g. splitting `|` below `^` below `+`,
// or `<<` below `*`) silently reparses real programs, so every boundary pair is
// pinned here.

struct PrecedenceCase {
	expression string
	expected   string
}

fn test_operator_precedence_boundaries() {
	cases := [
		// product binds tighter than sum
		PrecedenceCase{'2 + 3 * 4', '14'},
		PrecedenceCase{'20 - 12 / 4', '17'},

		// power binds tighter than product
		PrecedenceCase{'2 * 3 ** 2', '18'},

		// shifts live at product, so they bind tighter than +/-
		PrecedenceCase{'1 + 2 << 3', '17'},
		PrecedenceCase{'8 << 1 + 1', '17'},
		PrecedenceCase{'2 << 3 - 1', '15'},

		// & lives at product, |/^ live at sum
		PrecedenceCase{'10 & 3 + 1', '3'},
		PrecedenceCase{'1 | 2 & 3', '3'},
		PrecedenceCase{'5 & 3 ^ 1', '0'},

		// |, ^, +, - all share the sum level and fold left
		PrecedenceCase{'1 | 2 ^ 3', '0'},
		PrecedenceCase{'1 ^ 2 | 4', '7'},
		PrecedenceCase{'1 ^ 2 + 3', '6'},

		// left associativity within a level
		PrecedenceCase{'10 - 3 - 2', '5'},
		PrecedenceCase{'2 * 3 % 4', '2'},
		PrecedenceCase{'32 >> 1 >> 2', '4'},

		// explicit parentheses override
		PrecedenceCase{'(1 + 2) << 3', '24'},

		// bitwise binds tighter than comparison
		PrecedenceCase{'int(3 | 4 == 7)', '1'},
		PrecedenceCase{'int(5 & 3 == 1)', '1'},

		// comparison binds tighter than &&, which binds tighter than ||
		PrecedenceCase{'int(1 < 2 && 3 < 4)', '1'},
		PrecedenceCase{'int(true || false && false)', '1'},
	]

	dir := os.join_path(os.vtmp_dir(), 'v3_prec_${tempname.unique_token()}')
	os.mkdir(dir)!
	defer {
		os.rmdir_all(dir) or {}
	}

	mut lines := ['fn main() {']
	for item in cases {
		lines << '\tprintln(${item.expression})'
	}
	lines << '}'
	src := os.join_path(dir, 'main.v')
	os.write_file(src, lines.join('\n') + '\n')!

	bin := os.join_path(dir, 'app')
	compile := os.exec([@VEXE, src, '-b', 'c', '-o', bin])
	assert compile.exit_code == 0, compile.output
	assert !compile.output.contains('C compilation failed'), compile.output
	run := os.exec([bin])
	assert run.exit_code == 0, run.output

	output := run.output.trim_space().split_into_lines()
	assert output.len == cases.len, run.output
	for i, item in cases {
		assert output[i] == item.expected, item.expression
	}
}
