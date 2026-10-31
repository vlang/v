import os

fn test_ownership_if_guard_dereferences_arc_optional_payload() {
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
}
')!
	for mode in ['-no-parallel', ''] {
		out := os.execute('${os.quoted_path(@VEXE)} -new-compiler -nocache -ownership -d ownership -cc clang ${mode} run ${os.quoted_path(source)}')
		assert out.exit_code == 0, '${mode}: ${out.output}'
		assert out.output.trim_space() == 'ok', out.output
	}
	os.write_file(source, 'fn main() {
	value := 7
	ptr := &value
	if item := (*ptr) {
		println(item)
	}
}
')!
	out := os.execute('${os.quoted_path(@VEXE)} -new-compiler -nocache -ownership -d ownership -check ${os.quoted_path(source)}')
	assert out.exit_code != 0, out.output
	assert out.output.contains('expression should either return an Option or a Result'), out.output
}
