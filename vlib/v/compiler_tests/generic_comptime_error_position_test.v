import os

fn test_generic_reflection_compile_error_has_position_without_unknown_function() {
	root := os.join_path(os.vtmp_dir(), 'generic_compile_error_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	for member in ['methods', 'fields'] {
		source := os.join_path(root, '${member}.v')
		declaration := if member == 'methods' {
			'struct App {}\nfn (a &App) handler(x int) int { return x }'
		} else {
			'struct App { name string; age int }'
		}
		condition := if member == 'methods' { 'item.args.len != 2' } else { 'item.typ is int' }
		os.write_file(source, 'module main\n${declaration}\nfn check[T]() {\n\t\$for item in T.${member} {\n\t\t\$if ${condition} {\n\t\t\t\$compile_error("invalid reflected member")\n\t\t}\n\t}\n}\nfn main() { check[App]() }')!
		result := os.exec([@VEXE, '-new-compiler', '-no-retry-compilation', '-nocache', '-o',
			os.join_path(root, 'output.c'), source])
		assert result.exit_code != 0
		expected_line := if member == 'methods' { 7 } else { 6 }
		assert result.output.contains('${member}.v:${expected_line}:4: error:'), result.output
		assert result.output.contains('invalid reflected member'), result.output
		assert !result.output.contains('unknown function'), result.output
		assert result.output.count('error: compile-time error: invalid reflected member') == 1, result.output
	}
}

fn test_compile_error_rejects_non_literal_message_explicitly() {
	root := os.join_path(os.vtmp_dir(), 'compile_error_message_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	source := os.join_path(root, 'message.v')
	os.write_file(source, 'fn main() { \$compile_error("name " + "handler") }')!
	result := os.exec([@VEXE, '-new-compiler', '-no-retry-compilation', '-nocache', '-o',
		os.join_path(root, 'output.c'), source])
	assert result.exit_code != 0
	assert result.output.contains('`\$compile_error` expects a string literal'), result.output
	assert !result.output.contains('unknown function'), result.output
}
