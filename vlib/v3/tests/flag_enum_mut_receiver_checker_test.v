import os

const flag_enum_mut_receiver_vexe = @VEXE
const flag_enum_mut_receiver_tests_dir = os.dir(@FILE)
const flag_enum_mut_receiver_v3_dir = os.dir(flag_enum_mut_receiver_tests_dir)
const flag_enum_mut_receiver_vlib_dir = os.dir(flag_enum_mut_receiver_v3_dir)
const flag_enum_mut_receiver_v3_src = os.join_path(flag_enum_mut_receiver_v3_dir, 'v3.v')

fn flag_enum_mut_receiver_build_v3() string {
	v3_bin := os.join_path(os.temp_dir(), 'v3_flag_enum_mut_receiver_test_${os.getpid()}')
	os.rm(v3_bin) or {}
	build :=
		os.execute('${flag_enum_mut_receiver_vexe} -gc none -path "${flag_enum_mut_receiver_vlib_dir}|@vlib|@vmodules" -o ${v3_bin} ${flag_enum_mut_receiver_v3_src}')
	assert build.exit_code == 0, build.output
	return v3_bin
}

fn flag_enum_mut_receiver_run_bad(v3_bin string, name string, source string, expected string) {
	src := os.join_path(os.temp_dir(), 'v3_${name}_${os.getpid()}.v')
	os.write_file(src, source) or { panic(err) }
	bin := os.join_path(os.temp_dir(), 'v3_${name}_${os.getpid()}')
	compile := os.execute('${v3_bin} ${src} -b c -o ${bin}')
	assert compile.exit_code != 0, compile.output
	assert compile.output.contains(expected), compile.output
	assert !compile.output.contains('C compilation failed'), compile.output
}

fn flag_enum_mut_receiver_run_good(v3_bin string, name string, source string) string {
	src := os.join_path(os.temp_dir(), 'v3_${name}_${os.getpid()}.v')
	os.write_file(src, source) or { panic(err) }
	bin := os.join_path(os.temp_dir(), 'v3_${name}_${os.getpid()}')
	compile := os.execute('${v3_bin} ${src} -b c -o ${bin}')
	assert compile.exit_code == 0, compile.output
	assert !compile.output.contains('C compilation failed'), compile.output
	run := os.execute(bin)
	assert run.exit_code == 0, run.output
	return run.output.trim_space()
}

fn test_flag_enum_set_clear_require_mutable_receiver() {
	v3_bin := flag_enum_mut_receiver_build_v3()
	expected := 'flag enum method `set` requires a mutable receiver'
	expected_clear := 'flag enum method `clear` requires a mutable receiver'
	flag_enum_mut_receiver_run_bad(v3_bin, 'bad_flag_enum_immutable_set', '@[flag]
enum Mode {
	a
	b
}

fn main() {
	flags := Mode.a
	flags.set(.b)
}
',
		expected)
	flag_enum_mut_receiver_run_bad(v3_bin, 'bad_flag_enum_rvalue_set', '@[flag]
enum Mode {
	a
	b
}

fn make_flags() Mode {
	return Mode.a
}

fn main() {
	make_flags().set(.b)
}
',
		expected)
	flag_enum_mut_receiver_run_bad(v3_bin, 'bad_flag_enum_immutable_clear', '@[flag]
enum Mode {
	a
	b
}

fn main() {
	flags := Mode.a | Mode.b
	flags.clear(.b)
}
',
		expected_clear)
	flag_enum_mut_receiver_run_bad(v3_bin, 'bad_flag_enum_immutable_field_set', '@[flag]
enum Mode {
	a
	b
}

struct Box {
	mode Mode
}

fn main() {
	box := Box{
		mode: Mode.a
	}
	box.mode.set(.b)
}
',
		expected)
	out := flag_enum_mut_receiver_run_good(v3_bin, 'good_flag_enum_mut_set_clear', '@[flag]
enum Mode {
	a
	b
}

struct Box {
	mode Mode
}

fn main() {
	mut flags := Mode.a
	flags.set(.b)
	assert flags.has(.b)
	flags.clear(.a)
	assert !flags.has(.a)
	mut box := Box{
		mode: Mode.a
	}
	box.mode.set(.b)
	assert box.mode.has(.b)
	box.mode.clear(.a)
	assert !box.mode.has(.a)
	println("ok")
}
')
	assert out == 'ok'
}
