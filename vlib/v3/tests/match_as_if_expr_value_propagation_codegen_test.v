// Regression test for https://github.com/vlang/v/issues/28000
// A `match` whose arms use `!`/`?` propagation, used as the value of an
// `if`-expression, must assign the unwrapped result to the if-expression's
// result temp (it used to emit an empty expression `_t = ;`).
import os

const vexe = @VEXE
const tests_dir = os.dir(@FILE)
const v3_dir = os.dir(tests_dir)
const vlib_dir = os.dir(v3_dir)
const v3_src = os.join_path(v3_dir, 'v3.v')

fn test_match_as_if_expr_value_with_propagation() {
	v3_bin := os.join_path(os.temp_dir(), 'v3_match_as_if_expr_value_propagation_test')
	build :=
		os.execute('${vexe} -gc none -path "${vlib_dir}|@vlib|@vmodules" -o ${v3_bin} ${v3_src}')
	assert build.exit_code == 0, build.output

	src := os.join_path(os.temp_dir(), 'v3_match_as_if_expr_value_propagation_input.v')
	os.write_file(src, 'module main

struct First {}
struct Second {}

type Node = First | Second

fn lower_first(_ First) !int {
	return 1
}

fn lower_second(_ Second) !int {
	return 2
}

fn select_value(node ?Node) !int {
	result := if value := node {
		match value {
			First { lower_first(value)! }
			Second { lower_second(value)! }
		}
	} else {
		0
	}
	return result
}

fn direct_match(node Node) !int {
	return match node {
		First { lower_first(node)! }
		Second { lower_second(node)! }
	}
}

fn main() {
	println(select_value(First{})!)
	println(select_value(Second{})!)
	println(direct_match(Second{})!)
}
') or {
		panic(err)
	}

	bin := os.join_path(os.temp_dir(), 'v3_match_as_if_expr_value_propagation_out')
	compile := os.execute('${v3_bin} ${src} -b c -o ${bin}')
	assert compile.exit_code == 0, compile.output
	assert !compile.output.contains('C compilation failed'), compile.output

	run := os.execute(bin)
	assert run.exit_code == 0, run.output
	assert run.output.trim_space() == '1\n2\n2'
}
