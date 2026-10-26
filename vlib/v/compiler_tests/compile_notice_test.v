import os

fn test_compile_notices_survive_generic_instantiation_and_production_builds() ! {
	root := os.join_path(os.vtmp_dir(), 'compile_notice_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	input := os.join_path(root, 'main.v')
	os.write_file(input, 'module main

$compile_notice("module notice")

fn unused[T](value T) T {
	$compile_notice("unused generic")
	return value
}

fn selected[T](value T) T {
	$compile_notice("generic always")
	$if T is int {
		$compile_notice("generic int")
	} $else $if T is string {
		$compile_notice("generic string")
	} $else {
		$compile_notice("unused generic branch")
	}
	return value
}

fn main() {
	$compile_notice("plain notice")
	$if notice_inactive ? {
		$compile_notice("unused define")
	}
	assert selected[int](1) == 1
	assert selected[int](2) == 2
	assert selected[string]("text") == "text"
	println("notice program ran")
}
')!
	for production in [false, true] {
		mut args := [@VEXE, '-b', 'c', '-cc', 'clang', '-nocache']
		if production {
			args << '-prod'
		}
		args << ['run', input]
		result := os.exec(args)
		assert result.exit_code == 0, result.output
		assert result.output.contains('notice program ran'), result.output
		for message in ['module notice', 'plain notice', 'generic always', 'generic int',
			'generic string'] {
			assert result.output.count('notice: ${message}') == 1, result.output
		}
		for message in ['unused generic', 'unused generic branch', 'unused define'] {
			assert !result.output.contains('notice: ${message}'), result.output
		}
	}
	formatted := os.exec([@VEXE, 'fmt', '-w', input])
	assert formatted.exit_code == 0, formatted.output
	assert os.read_file(input)!.contains('$compile_notice(')
}

fn test_compile_notice_rejects_nonliteral_messages() ! {
	root := os.join_path(os.vtmp_dir(), 'compile_notice_invalid_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	input := os.join_path(root, 'main.v')
	for message in ['42', 'message', r'"value ${message}"'] {
		os.write_file(input, 'module main\nfn main() {\n message := "runtime"\n $compile_notice(${message})\n}\n')!
		result := os.exec([@VEXE, '-b', 'c', '-check', input])
		assert result.exit_code != 0, result.output
		assert result.output.contains('`$compile_notice` expects a string literal'), result.output
	}
}

fn test_compile_notices_in_imported_modules_without_function_calls() ! {
	root := os.join_path(os.vtmp_dir(), 'compile_notice_import_${os.getpid()}')
	os.mkdir_all(os.join_path(root, 'helper'))!
	defer { os.rmdir_all(root) or {} }
	os.write_file(os.join_path(root, 'v.mod'), 'Module { name: "notice_import" }')!
	os.write_file(os.join_path(root, 'helper', 'helper.v'), 'module helper
$compile_notice("imported module notice")
pub const value = 1
pub fn unused() {
	$compile_notice("imported function notice")
}
')!
	input := os.join_path(root, 'main.v')
	os.write_file(input, 'module main\nimport helper\nfn main() { assert helper.value == 1 }\n')!
	for production in [false, true] {
		mut args := [@VEXE, '-b', 'c', '-cc', 'clang', '-nocache']
		if production {
			args << '-prod'
		}
		args << ['run', input]
		result := os.exec(args)
		assert result.exit_code == 0, result.output
		assert result.output.count('notice: imported module notice') == 1, result.output
	}
}
