import os
import v.tempname

fn for_in_review_fixture_dir() string {
	root := os.join_path(os.vtmp_dir(), 'v3_for_in_review_${tempname.unique_token()}')
	os.mkdir(root) or { panic(err) }
	return root
}

fn for_in_review_run_good(name string, src string) string {
	root := for_in_review_fixture_dir()
	defer { os.rmdir_all(root) or {} }
	out := os.join_path(root, 'v3_${name}')
	src_path := out + '.v'
	os.write_file(src_path, src) or { panic(err) }
	compile := os.exec([@VEXE, src_path, '-b', 'c', '-o', '${out}'])
	assert compile.exit_code == 0, '${name}: compile failed\n${compile.output}'
	assert !compile.output.contains('C compilation failed'), '${name}: C compilation failed\n${compile.output}'
	run := os.exec([out])
	assert run.exit_code == 0, '${name}: run failed\n${run.output}'
	return run.output.trim_space()
}

fn test_loop_review_rejects_invalid_conditions_and_iterables() {
	root := for_in_review_fixture_dir()
	defer { os.rmdir_all(root) or {} }
	out := os.join_path(root, 'v3_loop_review_rejections')
	src_path := out + '.v'
	source := 'fn check_pointer_if() {
	x := 1
	p := &x
	if p {
		println("bad")
	}
}

fn check_pointer_for() {
	x := 1
	p := &x
	for p {
		break
	}
}

fn maybe_values() ?map[string]int {
	return none
}

fn check_optional_map() {
	for key, value in maybe_values() {
		println(key + int_str(value))
	}
}

fn main() {
	check_pointer_if()
	check_pointer_for()
	check_optional_map()
}
'
	os.write_file(src_path, source) or { panic(err) }
	result := os.exec([@VEXE, src_path, '-b', 'c', '-nocolor', '-o', out])
	assert result.exit_code != 0, result.output
	assert !result.output.contains('C compilation failed'), result.output
	expected := {
		'\tif p {':                             'non-bool type `&int` used as if condition'
		'\tfor p {':                            'if condition must be `bool`, not `&int`'
		'\tfor key, value in maybe_values() {': 'for in: cannot index `?map[string]int`'
	}
	lines := source.split_into_lines()
	diagnostics := result.output.split_into_lines().filter(it.contains(': error: '))
	for statement, message in expected {
		line := lines.index(statement) + 1
		assert line > 0, statement
		location := 'v3_loop_review_rejections.v:${line}:'
		assert diagnostics.any(it.contains(location) && it.contains(message)), 'expected ${message} at ${location}\n${result.output}'
	}
}

fn test_loop_review_runtime_cases() {
	out := for_in_review_run_good('loop_review', 'fn check_c_style_pointer_condition() {
	mut x := 1
	mut p := &x
	mut count := 0
	// Clearing p in the post expression makes the next condition false.
	for ; p; p = unsafe { nil } {
		count++
	}
	println(count)
}

fn maybe_values(ok bool) ?[]int {
	if !ok {
		return none
	}
	return [1, 2, 3]
}

fn check_optional_array_payload() {
	mut total := 0
	for value in maybe_values(false) {
		total += value
	}
	println(int_str(total))
	for value in maybe_values(true) {
		total += value
	}
	println(int_str(total))
}

fn entries(lang string) map[string]string {
	return {\'message\': lang.repeat(64)}
}

fn check_temporary_map_values() {
	mut translations := map[string]map[string]string{}
	for lang in [\'en\', \'fr\', \'de\', \'es\'] {
		for key, value in entries(lang) {
			translations[lang][key] = value
		}
	}
	println(translations[\'en\'][\'message\'] == \'en\'.repeat(64))
}

struct Counter {
mut:
	n int
}

fn (mut c Counter) next() ?int {
	if c.n >= 3 {
		return none
	}
	c.n++
	return c.n
}

fn check_custom_iterator_pointer() {
	mut c := Counter{}
	for x in &c {
		println(x)
	}
	println("count: \${c.n}")
}

fn main() {
	check_c_style_pointer_condition()
	check_optional_array_payload()
	check_temporary_map_values()
	check_custom_iterator_pointer()
}
')
	assert out.split_into_lines() == ['1', '0', '6', 'true', '1', '2', '3', 'count: 3'], out
}
