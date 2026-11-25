import os
import v.tempname

struct FlagEnumReceiverRejection {
	name     string
	source   string
	expected string
}

fn flag_enum_mut_receiver_run_bad(dir string, cases []FlagEnumReceiverRejection) {
	root := os.join_path(dir, 'rejected')
	mut imports := []string{cap: cases.len}
	for i, case in cases {
		module_name := 'fixture_${i}'
		module_dir := os.join_path(root, module_name)
		os.mkdir_all(module_dir) or { panic(err) }
		imports << 'import ${module_name} as _'
		os.write_file(os.join_path(module_dir, 'v3_${case.name}.v'), 'module ${module_name}\n\n' +
			case.source) or { panic(err) }
	}
	os.write_file(os.join_path(root, 'main.v'), 'module main\n\n${imports.join('\n')}\n\nfn main() {}\n') or {
		panic(err)
	}
	compile := os.exec([@VEXE, '-b', 'c', '-nocolor', '-o', os.join_path(root, 'program'), root])
	assert compile.exit_code != 0, compile.output
	assert !compile.output.contains('C compilation failed'), compile.output
	mut errors := map[string][]string{}
	for line in compile.output.split_into_lines() {
		location, message := line.split_once(': error: ') or { continue }
		// Drop the line and column from the right to preserve any colon in the path.
		file := location.all_before_last(':').all_before_last(':')
		errors[os.file_name(file)] << message
	}
	for case in cases {
		assert errors['v3_${case.name}.v'].any(it.contains(case.expected)), '${case.name}: expected `${case.expected}` in\n${compile.output}'
	}
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
	flag_enum_mut_receiver_run_bad(dir, [
		FlagEnumReceiverRejection{
			name:     'bad_flag_enum_immutable_set'
			source:   '@[flag]
enum Mode {
	a
	b
}

fn main() {
	flags := Mode.a
	flags.set(.b)
}
'
			expected: expected
		},
		FlagEnumReceiverRejection{
			name:     'bad_flag_enum_rvalue_set'
			source:   '@[flag]
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
'
			expected: expected
		},
		FlagEnumReceiverRejection{
			name:     'bad_flag_enum_immutable_clear'
			source:   '@[flag]
enum Mode {
	a
	b
}

fn main() {
	flags := Mode.a | Mode.b
	flags.clear(.b)
}
'
			expected: expected_clear
		},
		FlagEnumReceiverRejection{
			name:     'bad_flag_enum_immutable_field_set'
			source:   '@[flag]
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
'
			expected: expected
		},
	])
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
