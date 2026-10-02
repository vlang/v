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
	folder := os.join_path(os.temp_dir(), "ownership-file-borrows-" + os.getpid().str()).to_owned()
	os.mkdir(folder) or { panic(folder + ": " + err.msg()) }
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
