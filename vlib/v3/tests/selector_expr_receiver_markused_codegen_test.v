import os

const vexe = @VEXE
const tests_dir = os.dir(@FILE)
const v3_dir = os.dir(tests_dir)
const vlib_dir = os.dir(v3_dir)
const v3_src = os.join_path(v3_dir, 'v3.v')

fn build_v3() string {
	v3_bin := os.join_path(os.temp_dir(), 'v3_selector_expr_receiver_markused_${os.getpid()}')
	build :=
		os.execute('${vexe} -gc none -path "${vlib_dir}|@vlib|@vmodules" -o ${v3_bin} ${v3_src}')
	assert build.exit_code == 0, build.output
	return v3_bin
}

fn test_selector_receiver_or_expr_roots_string_methods() {
	v3_bin := build_v3()

	src := os.join_path(os.temp_dir(), 'v3_selector_expr_receiver_markused_input_${os.getpid()}.v')
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

	bin := os.join_path(os.temp_dir(), 'v3_selector_expr_receiver_markused_input_${os.getpid()}')
	compile := os.execute('${v3_bin} ${src} -b c -o ${bin}')
	assert compile.exit_code == 0, compile.output
	assert !compile.output.contains('C compilation failed'), compile.output

	generated := os.read_file(bin + '.c') or { panic(err) }
	assert generated.contains('string__trim_space'), generated
	assert generated.contains('string__to_upper'), generated
	assert generated.contains('rune__to_upper'), generated

	run := os.execute(bin)
	assert run.exit_code == 0, run.output
	assert run.output.trim_space() == 'ok'
}

fn test_top_level_selector_receiver_or_expr_roots_string_methods() {
	v3_bin := build_v3()

	src := os.join_path(os.temp_dir(),
		'v3_top_level_selector_expr_receiver_markused_${os.getpid()}.v')
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

	bin := os.join_path(os.temp_dir(),
		'v3_top_level_selector_expr_receiver_markused_${os.getpid()}')
	compile := os.execute('${v3_bin} ${src} -b c -o ${bin}')
	assert compile.exit_code == 0, compile.output
	assert !compile.output.contains('C compilation failed'), compile.output

	generated := os.read_file(bin + '.c') or { panic(err) }
	assert generated.contains('string__trim_space'), generated
	assert generated.contains('string__to_upper'), generated

	run := os.execute(bin)
	assert run.exit_code == 0, run.output
	assert run.output.trim_space() == 'ok'
}

fn test_result_selector_receiver_or_expr_roots_string_methods() {
	v3_bin := build_v3()

	src := os.join_path(os.temp_dir(), 'v3_result_selector_expr_receiver_markused_${os.getpid()}.v')
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

	bin := os.join_path(os.temp_dir(), 'v3_result_selector_expr_receiver_markused_${os.getpid()}')
	compile := os.execute('${v3_bin} ${src} -b c -o ${bin}')
	assert compile.exit_code == 0, compile.output
	assert !compile.output.contains('C compilation failed'), compile.output

	generated := os.read_file(bin + '.c') or { panic(err) }
	assert generated.contains('string__trim_space'), generated
	assert generated.contains('string__to_upper'), generated

	run := os.execute(bin)
	assert run.exit_code == 0, run.output
	assert run.output.trim_space() == 'ok'
}
