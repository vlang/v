import os

fn test_ownership_stdlib_string_calls_preserve_borrowed_arguments() {
	root := os.join_path(os.vtmp_dir(), 'ownership_stdlib_string_borrows_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	source := os.join_path(root, 'main.v')
	os.write_file(source, 'import encoding.utf8
import os
import strconv
import strings

struct Command {
	program string
}

fn check_standard_string_readers() ! {
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
	assert utf8.validate_str(value)
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
	println("readers")
}

fn check_file_operations() ! {
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
	println("paths")
}

fn check_nonowning_string_fallbacks() {
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
	println("fallbacks")
}

fn main() {
	check_standard_string_readers()!
	check_file_operations()!
	check_nonowning_string_fallbacks()
}
')!
	for mode in [['-no-parallel'], []string{}] {
		out := os.exec([@VEXE, '-nocache', '-ownership', '-d', 'ownership', '-cc', 'clang', ...mode,
			'run', source])
		assert out.exit_code == 0, '${mode}: ${out.output}'
		assert out.output.trim_space().split_into_lines() == ['readers', 'paths', 'fallbacks'], out.output
	}
}

fn test_ownership_string_arguments_keep_move_semantics() {
	root := os.join_path(os.vtmp_dir(), 'ownership_string_argument_moves_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	source := os.join_path(root, 'main.v')
	consumer := 'fn exists(value string) bool { return value.len > 0 }
'
	user_body := '
	value := "owned".to_owned()
	assert exists(value)
	println(value)
'
	fallback_body := '
	fallback := "fallback".to_owned()
	result := "abc".substr_or(0, 4, fallback)
	println(result)
	println(fallback)
'
	// Custom flags can limit the number of diagnostics.
	batch := os.getenv('VFLAGS').trim_space() in ['', '-gc none']
	if batch {
		os.write_file(source, consumer + 'fn user_consumer() {${user_body}}
fn returned_fallback() {${fallback_body}}
fn main() {
	user_consumer()
	returned_fallback()
}
')!
		check_ownership_string_argument_moves(source, [], ['value', 'fallback'])
	} else {
		os.write_file(source, consumer + 'fn main() {${user_body}}\n')!
		check_ownership_string_argument_moves(source, [], ['value'])
	}
	os.write_file(source, 'fn main() {${fallback_body}}\n')!
	check_ownership_string_argument_moves(source, ['-no-parallel'], ['fallback'])
	if !batch {
		check_ownership_string_argument_moves(source, [], ['fallback'])
	}
}

fn check_ownership_string_argument_moves(source string, mode []string, names []string) {
	out := os.exec([@VEXE, '-nocache', '-ownership', '-d', 'ownership', ...mode, '-check', source])
	assert out.exit_code != 0, '${mode}: ${out.output}'
	for name in names {
		assert out.output.contains('use of moved value: `${name}`'), '${mode}: ${name}: ${out.output}'
	}
}

fn test_ownership_cached_utf8_validation_borrows_owned_strings() {
	cc := 'cc'
	if os.exec([cc, '--version']).exit_code != 0 {
		return
	}
	root := os.join_path(os.vtmp_dir(), 'ownership_cached_utf8_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	previous_cache := os.getenv_opt('V3CACHE')
	os.setenv('V3CACHE', os.join_path(root, 'cache'), true)
	defer {
		if value := previous_cache {
			os.setenv('V3CACHE', value, true)
		} else {
			os.unsetenv('V3CACHE')
		}
	}
	// Distinct programs share module objects and each still needs checking.
	for name in ['cold', 'warm'] {
		source := os.join_path(root, '${name}.v')
		executable := os.join_path(root, name + if os.user_os() == 'windows' { '.exe' } else { '' })
		os.write_file(source, 'import encoding.utf8

fn main() {
	value := "${name}".to_owned()
	assert utf8.validate_str(value)
	println(value)
}
')!
		build := os.exec([@VEXE, '-ownership', '-gc', 'none', '-cc', cc, '-v', '-o', executable,
			source])
		assert build.exit_code == 0, build.output
		if name == 'warm' {
			assert build.output.split_into_lines().any(it.trim_space().starts_with('.vh files:')
				&& it.replace('\\', '/').contains('/utf8_')), build.output
		}
		run := os.exec([executable])
		assert run.exit_code == 0, run.output
		assert run.output.trim_space() == name, run.output
	}
}
