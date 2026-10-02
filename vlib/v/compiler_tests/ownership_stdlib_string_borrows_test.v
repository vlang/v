import os

fn test_ownership_standard_string_readers_borrow_owned_arguments() {
	root := os.join_path(os.vtmp_dir(), 'ownership_stdlib_string_borrows_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	source := os.join_path(root, 'main.v')
	os.write_file(source, 'import os
import strconv
import strings

struct Command {
	program string
}

fn main() {
	command := &Command{program: "/tmp/example".to_owned()}
	program := command.program
	assert os.is_abs_path(program)
	assert os.is_abs_path(program)
	_ = os.exists(program)
	assert os.join_path(program, "child") == "/tmp/example/child"
	assert program == "/tmp/example"
	value := "42".to_owned()
	assert strconv.parse_uint(value, 10, 64)! == 42
	assert strconv.parse_int(value, 10, 64)! == 42
	assert value == "42"
	mut builder := strings.new_builder(8)
	builder.write_string(value)
	builder.write_string(value)
	assert builder.str() == "4242"
	assert value == "42"
	replacement := "description".to_owned()
	assert "TOKEN".replace("TOKEN", replacement) == "description"
	assert "TOKEN".replace("TOKEN", replacement) == "description"
	assert replacement == "description"
	println("ok")
}
')!
	for mode in ['-no-parallel', ''] {
		out := os.execute('${os.quoted_path(@VEXE)} -new-compiler -nocache -ownership -d ownership -cc clang ${mode} run ${os.quoted_path(source)}')
		assert out.exit_code == 0, '${mode}: ${out.output}'
		assert out.output.trim_space() == 'ok', out.output
	}
}

fn test_ownership_user_string_consumers_keep_move_semantics() {
	root := os.join_path(os.vtmp_dir(), 'ownership_user_string_consumers_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	source := os.join_path(root, 'main.v')
	os.write_file(source, 'fn exists(value string) bool { return value.len > 0 }
fn main() {
	value := "owned".to_owned()
	assert exists(value)
	println(value)
}
')!
	out := os.execute('${os.quoted_path(@VEXE)} -new-compiler -nocache -ownership -d ownership -check ${os.quoted_path(source)}')
	assert out.exit_code != 0, out.output
	assert out.output.contains('use of moved value: `value`'), out.output
}

fn test_ownership_file_operations_borrow_their_path_arguments() {
	root := os.join_path(os.vtmp_dir(), 'ownership_file_string_borrows_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }

	source := os.join_path(root, 'main.v')
	os.write_file(source, 'import os
fn main() {
	path := @FILE.to_owned()
	resolved := os.real_path(path)
	assert resolved.len > 0
	assert os.file_ext(path) == ".v"
	contents := os.read_file(path)!
	assert contents.len > 0
	assert path == @FILE
	resource := os.resource_abs_path("not-present")
	assert resource.len > 0
	folder := os.join_path(os.temp_dir(), "ownership-file-borrows-" + os.getpid().str()).to_owned()
	os.mkdir(folder) or { panic(folder + ": " + err.msg()) }
	os.mkdir_all(folder) or { panic(folder + ": " + err.msg()) }
	assert os.is_dir(folder)
	_ = os.ls(folder)!
	assert folder.len > 0
	missing := os.join_path(folder, "missing").to_owned()
	os.read_bytes(missing) or { assert missing.len > 0 }
	assert missing.len > 0
	normalized := os.norm_path("base/../child".to_owned())
	assert normalized == "child"
	os.mkdir_all(os.join_path(folder, "parent", "child"))!
	assert os.is_dir(os.join_path(folder, "parent", "child"))
	os.walk(folder, fn (child string) { assert child.len > 0 })
	assert folder.len > 0
	os.rmdir_all(folder)!
	println("ok")
}
')!
	for mode in ['-no-parallel', ''] {
		out := os.execute('${os.quoted_path(@VEXE)} -new-compiler -nocache -ownership -d ownership -cc clang ${mode} run ${os.quoted_path(source)}')
		assert out.exit_code == 0, '${mode}: ${out.output}'
		assert out.output.trim_space() == 'ok', out.output
	}
}

fn test_ownership_returned_string_fallback_moves_owned_argument() {
	root := os.join_path(os.vtmp_dir(), 'ownership_string_fallback_move_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	source := os.join_path(root, 'main.v')
	os.write_file(source, 'fn main() {
	fallback := "fallback".to_owned()
	result := "abc".substr_or(0, 4, fallback)
	println(result)
	println(fallback)
}
')!
	for mode in ['-no-parallel', ''] {
		out := os.execute('${os.quoted_path(@VEXE)} -new-compiler -nocache -ownership -d ownership ${mode} -check ${os.quoted_path(source)}')
		assert out.exit_code != 0, out.output
		assert out.output.contains('use of moved value: `fallback`'), out.output
	}
}

fn test_ownership_returned_string_fallback_copies_nonowning_arguments() {
	root := os.join_path(os.vtmp_dir(), 'ownership_string_fallback_copy_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	source := os.join_path(root, 'main.v')
	os.write_file(source, 'fn main() {
	fallback := "fallback".to_owned()
	result := "abc".substr_or(0, 4, fallback.clone())
	assert result == fallback
	assert voidptr(result.str) != voidptr(fallback.str)
	assert "abc".substr_or(0, 2, fallback.clone()) == "ab"
	view := fallback.substr_unsafe(1, 4)
	copied := "abc".substr_or(0, 4, view)
	assert copied == "all"
	assert voidptr(copied.str) != voidptr(view.str)
	assert view == "all"
	literal := "abc".substr_or(0, 4, "literal")
	assert literal == "literal"
	println("ok")
}
')!
	for mode in ['-no-parallel', ''] {
		out := os.execute('${os.quoted_path(@VEXE)} -new-compiler -nocache -ownership -d ownership -cc clang ${mode} run ${os.quoted_path(source)}')
		assert out.exit_code == 0, '${mode}: ${out.output}'
		assert out.output.trim_space() == 'ok', out.output
	}
}
