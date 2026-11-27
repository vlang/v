import os

fn test_compile_notices_survive_generic_instantiation_and_production_builds() ! {
	root := os.join_path(os.vtmp_dir(), 'compile_notice_${os.getpid()}')
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
	os.write_file(input, 'module main

import helper

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
	// Use the imported module without calling any of its functions.
	assert helper.value == 1
	println("notice program ran")
}
')!
	for production in [false, true] {
		mut args := [@VEXE, '-b', 'c', '-nocache']
		$if !windows {
			args << ['-cc', 'clang']
		}
		if production {
			$if windows {
				// VFLAGS may select TCC, which does not support production builds.
				args << ['-cc', 'gcc']
			}
			args << '-prod'
		}
		args << ['run', input]
		result := os.exec(args)
		assert result.exit_code == 0, result.output
		assert result.output.contains('notice program ran'), result.output
		for message in ['module notice', 'plain notice', 'generic always', 'generic int',
			'generic string', 'imported module notice'] {
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
	messages := ['42', 'message', r'"value ${message}"']
	header := 'module main\nfn main() {\n message := "runtime"\n'
	calls := messages.map(' $compile_notice(${it})').join('\n')
	os.write_file(input, header + calls + '\n}\n')!
	result := os.exec([@VEXE, '-b', 'c', '-nocolor', '-check', input])
	assert result.exit_code != 0, result.output
	diagnostics := result.output.split_into_lines()
	first_call_line := header.count('\n') + 1
	for i, message in messages {
		location := '${os.file_name(input)}:${first_call_line + i}:'
		assert diagnostics.any(it.contains(location)
			&& it.ends_with('error: `$compile_notice` expects a string literal')), '${message}: ${result.output}'
	}
}
