import os
import v.tempname

fn flag_enum_mut_receiver_run_bad(dir string, name string, source string, expected string) {
	bin := os.join_path(dir, 'v3_${name}')
	src := bin + '.v'
	os.write_file(src, source) or { panic(err) }
	compile := os.exec([@VEXE, src, '-b', 'c', '-o', bin])
	assert compile.exit_code != 0, compile.output
	assert compile.output.contains(expected), compile.output
	assert !compile.output.contains('C compilation failed'), compile.output
}

fn flag_enum_mut_receiver_run_good(dir string, name string, source string) string {
	bin := os.join_path(dir, 'v3_${name}')
	src := bin + '.v'
	os.write_file(src, source) or { panic(err) }
	compile := os.exec([@VEXE, src, '-b', 'c', '-o', bin])
	assert compile.exit_code == 0, compile.output
	assert !compile.output.contains('C compilation failed'), compile.output
	run := os.exec([bin])
	assert run.exit_code == 0, run.output
	return run.output.trim_space()
}

fn test_flag_enum_set_clear_require_mutable_receiver() {
	dir := os.join_path(os.vtmp_dir(), 'v3_flag_enum_mut_receiver_${tempname.unique_token()}')
	os.mkdir(dir)!
	defer {
		os.rmdir_all(dir) or {}
	}
	expected := 'flag enum method `set` requires a mutable receiver'
	expected_clear := 'flag enum method `clear` requires a mutable receiver'
	flag_enum_mut_receiver_run_bad(dir, 'bad_flag_enum_immutable_set', '@[flag]
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
	flag_enum_mut_receiver_run_bad(dir, 'bad_flag_enum_rvalue_set', '@[flag]
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
	flag_enum_mut_receiver_run_bad(dir, 'bad_flag_enum_immutable_clear', '@[flag]
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
	flag_enum_mut_receiver_run_bad(dir, 'bad_flag_enum_immutable_field_set', '@[flag]
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
	out := flag_enum_mut_receiver_run_good(dir, 'good_flag_enum_mut_set_clear', '@[flag]
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
