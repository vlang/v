import os

// Three things a match branch and a rune literal have to be read correctly for.
//
// A rune names its code point in hex, in 16- or 32-bit unicode, or in octal. The
// duplicate-case check read only the letter after the backslash, so a line separator
// and a form feed written as unicode escapes both came out as the letter `u`, and a
// match over them was rejected as handling the same case twice.
//
// A match returned as several values can hand the whole tuple over in one branch --
// a call that returns it -- and list the parts in another. Only the listing form was
// recognised, so the mixture was reported as returning the type of the last element.

const match_tail_vexe = @VEXE
const match_tail_tests_dir = os.dir(@FILE)
const match_tail_v3_dir = os.dir(match_tail_tests_dir)
const match_tail_vlib_dir = os.dir(match_tail_v3_dir)
const match_tail_v3_src = os.join_path(match_tail_v3_dir, 'v.v')

fn match_tail_build_v3() string {
	v3_bin := os.join_path(os.temp_dir(), 'v3_match_tail_compiler_${os.getpid()}')
	os.rm(v3_bin) or {}
	build := os.execute('${match_tail_vexe} -gc none -path "${match_tail_vlib_dir}|@vlib|@vmodules" -o ${v3_bin} ${match_tail_v3_src}')
	assert build.exit_code == 0, build.output
	return v3_bin
}

fn match_tail_build_and_run(v3_bin string, root string, source string) os.Result {
	os.rmdir_all(root) or {}
	os.mkdir_all(root) or { panic(err) }
	main_v := os.join_path(root, 'main.v')
	os.write_file(main_v, source) or { panic(err) }
	exe := os.join_path(root, 'prog')
	compile := os.execute('${v3_bin} -nocache ${main_v} -b c -o ${exe}')
	if compile.exit_code != 0 {
		return compile
	}
	return os.execute(exe)
}

fn test_runes_spelled_as_escapes_are_distinct_match_cases() {
	v3_bin := match_tail_build_v3()
	root := os.join_path(os.vtmp_dir(), 'v3_rune_escape_cases_${os.getpid()}')
	defer {
		os.rmdir_all(root) or {}
	}
	res := match_tail_build_and_run(v3_bin, root, r'const soft_break = `\u2028`
const page_break = `\u000c`
const column_break = `\u000b`
const wide = `\U0001F600`
const octal = `\101`

fn classify(ch rune) string {
	return match ch {
		`\t` { "tab" }
		`\n`, soft_break { "br" }
		page_break { "page" }
		column_break { "column" }
		wide { "wide" }
		octal { "octal" }
		else { "other" }
	}
}

fn main() {
	println(classify(`\t`))
	println(classify(soft_break))
	println(classify(page_break))
	println(classify(column_break))
	println(classify(wide))
	println(classify(octal))
	println(int(soft_break))
	println(int(wide))
	println(int(octal))
	println(int(`\e`))
}
')
	assert res.exit_code == 0, res.output
	assert res.output.trim_space().split('\n').map(it.trim_space()) == ['tab', 'br', 'page', 'column',
		'wide', 'octal', '8232', '128512', '65', '27'], res.output
}

fn test_a_match_returning_a_tuple_may_mix_a_call_with_a_listed_pair() {
	v3_bin := match_tail_build_v3()
	root := os.join_path(os.vtmp_dir(), 'v3_tuple_match_tail_${os.getpid()}')
	defer {
		os.rmdir_all(root) or {}
	}
	res := match_tail_build_and_run(v3_bin, root, 'fn stepped(a int, b f64) (int, f64) {
	return a + 1, b + 1.0
}

fn pick(kind int, a int, b f64) (int, f64) {
	return match kind {
		1 { stepped(a, b) }
		else { a, b }
	}
}

fn main() {
	x, y := pick(1, 3, 4.0)
	println(x)
	println(y)
	p, q := pick(9, 3, 4.0)
	println(p)
	println(q)
}
')
	assert res.exit_code == 0, res.output
	assert res.output.trim_space().split('\n').map(it.trim_space()) == ['4', '5.0', '3', '4.0'], res.output
}
