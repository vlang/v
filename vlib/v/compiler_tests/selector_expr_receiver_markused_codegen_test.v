import os
import v.tempname

fn test_selector_receiver_or_expr_roots_string_methods() {
	root := os.join_path(os.vtmp_dir(), 'v3_selector_expr_receiver_markused_input_${tempname.unique_token()}')
	os.mkdir(root)!
	defer {
		os.rmdir_all(root) or {}
	}
	src := os.join_path(root, 'main.v')
	os.write_file(src, "fn maybe_input() ?string {
	return none
}

fn main() {
	input := maybe_input() or {
		' exit '
	}.trim_space()
	if input.to_upper() == 'EXIT' {
		println('ok')
	}
}
") or {
		panic(err)
	}

	bin := os.join_path(root, 'app')
	compile := os.exec([@VEXE, src, '-b', 'c', '-o', bin])
	assert compile.exit_code == 0, compile.output
	assert !compile.output.contains('C compilation failed'), compile.output

	generated := os.read_file(bin + '.c') or { panic(err) }
	assert generated.contains('string__trim_space'), generated
	assert generated.contains('string__to_upper'), generated
	assert generated.contains('rune__to_upper'), generated

	run := os.exec([bin])
	assert run.exit_code == 0, run.output
	assert run.output.trim_space() == 'ok'
}

fn test_top_level_selector_receiver_or_expr_roots_string_methods() {
	root := os.join_path(os.vtmp_dir(), 'v3_top_level_selector_expr_receiver_markused_${tempname.unique_token()}')
	os.mkdir(root)!
	defer {
		os.rmdir_all(root) or {}
	}
	src := os.join_path(root, 'main.v')
	os.write_file(src, "fn maybe_input() ?string {
	return none
}

for i := 0; i < 1; i++ {
	input := maybe_input() or {
		' exit '
	}.trim_space()
	if input.to_upper() == 'EXIT' {
		println('ok')
	}
}
") or {
		panic(err)
	}

	bin := os.join_path(root, 'app')
	compile := os.exec([@VEXE, src, '-b', 'c', '-o', bin])
	assert compile.exit_code == 0, compile.output
	assert !compile.output.contains('C compilation failed'), compile.output

	generated := os.read_file(bin + '.c') or { panic(err) }
	assert generated.contains('string__trim_space'), generated
	assert generated.contains('string__to_upper'), generated

	run := os.exec([bin])
	assert run.exit_code == 0, run.output
	assert run.output.trim_space() == 'ok'
}

fn test_result_selector_receiver_or_expr_roots_string_methods() {
	root := os.join_path(os.vtmp_dir(), 'v3_result_selector_expr_receiver_markused_${tempname.unique_token()}')
	os.mkdir(root)!
	defer {
		os.rmdir_all(root) or {}
	}
	src := os.join_path(root, 'main.v')
	os.write_file(src, "fn get_value() !string {
	return error('x')
}

fn main() {
	input := get_value() or {
		' exit '
	}.trim_space().to_upper()
	if input == 'EXIT' {
		println('ok')
	}
}
") or {
		panic(err)
	}

	bin := os.join_path(root, 'app')
	compile := os.exec([@VEXE, src, '-b', 'c', '-o', bin])
	assert compile.exit_code == 0, compile.output
	assert !compile.output.contains('C compilation failed'), compile.output

	generated := os.read_file(bin + '.c') or { panic(err) }
	assert generated.contains('string__trim_space'), generated
	assert generated.contains('string__to_upper'), generated

	run := os.exec([bin])
	assert run.exit_code == 0, run.output
	assert run.output.trim_space() == 'ok'
}
