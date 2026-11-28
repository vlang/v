import os

fn test_ownership_arc_optional_guard_and_borrowed_array_fields() {
	root := os.join_path(os.vtmp_dir(), 'ownership_arc_guard_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	source := os.join_path(root, 'main.v')
	os.write_file(source, 'module main

import sync.arc

struct Config {
	replacement arc.Arc[?[]u8]
}

struct Printer[W] {
	config Config
	writer W
}

struct Sink[^a, W] {
	printer &^a Printer[W]
}

fn (sink &Sink[^a, W]) replacement_len[^a]() int {
	if replacement := (*sink.printer.config.replacement.get()) {
		return replacement.len
	}
	return 0
}

fn main() {
	printer := Printer[int]{
		config: Config{replacement: arc.new(?[]u8([u8(4), 2]))}
	}
	sink := Sink{printer: &printer}
	assert sink.replacement_len() == 2
	empty_printer := Printer[int]{
		config: Config{replacement: arc.new(?[]u8(none))}
	}
	empty_sink := Sink{printer: &empty_printer}
	assert empty_sink.replacement_len() == 0
	println("ok")
	check_borrowed_array_fields()
}
')!
	os.write_file(os.join_path(root, 'borrowed_array.v'), r'
struct Worker[T] {
	value T
}

struct Factory[T] {
	workers &[]Worker[T]
}

fn (factory &Factory[T]) get(index int) T {
	return factory.workers[index].value
}

fn check_borrowed_array_fields() {
	workers := [Worker[int]{value: 7}, Worker[int]{value: 11}]
	factory := Factory[int]{workers: &workers}
	assert factory.get(0) == 7
	assert factory.get(1) == 11
	assert workers[0].value == 7
	println("borrowed array fields")
}
')!
	for mode in [['-no-parallel'], []string{}] {
		out := os.exec([@VEXE, '-nocache', '-ownership', '-d', 'ownership', '-cc', 'clang', ...mode,
			'run', root])
		assert out.exit_code == 0, '${mode}: ${out.output}'
		assert out.output.split_into_lines() == ['ok', 'borrowed array fields'], out.output
	}
}

fn test_ownership_if_guard_rejects_dereferenced_nonoptional_value() {
	root := os.join_path(os.vtmp_dir(), 'ownership_arc_guard_invalid_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	source := os.join_path(root, 'main.v')
	os.write_file(source, 'fn main() {
	value := 7
	ptr := &value
	if item := (*ptr) {
		println(item)
	}
}
')!
	out := os.exec([@VEXE, '-nocache', '-ownership', '-d', 'ownership', '-check', source])
	assert out.exit_code != 0, out.output
	assert out.output.contains('expression should either return an Option or a Result'), out.output
}
