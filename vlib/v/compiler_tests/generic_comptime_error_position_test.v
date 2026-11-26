import os

fn test_generic_reflection_compile_error_has_position_without_unknown_function() {
	root := os.join_path(os.vtmp_dir(), 'generic_compile_error_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	expected_lines := {
		'methods': 7
		'fields':  6
	}
	for member in expected_lines.keys() {
		module_dir := os.join_path(root, member)
		os.mkdir(module_dir)!
		source := os.join_path(module_dir, '${member}.v')
		declaration := if member == 'methods' {
			'struct App {}\nfn (a &App) handler(x int) int { return x }'
		} else {
			'struct App { name string; age int }'
		}
		condition := if member == 'methods' { 'item.args.len != 2' } else { 'item.typ is int' }
		os.write_file(source, 'module ${member}\n${declaration}\nfn check[T]() {\n\t\$for item in T.${member} {\n\t\t\$if ${condition} {\n\t\t\t\$compile_error("invalid reflected member")\n\t\t}\n\t}\n}\npub fn exercise() { check[App]() }')!
	}
	main := os.join_path(root, 'main.v')
	os.write_file(main, 'module main

import methods
import fields

fn main() {
	methods.exercise()
	fields.exercise()
}
')!
	result := os.exec([@VEXE, '-no-retry-compilation', '-nocache', '-o',
		os.join_path(root, 'output.c'), main])
	assert result.exit_code != 0, result.output
	assert !result.output.contains('unknown function'), result.output
	message := 'error: compile-time error: invalid reflected member'
	for member, line in expected_lines {
		assert result.output.count('${member}.v:${line}:4: ${message}') == 1, result.output
	}
	assert result.output.count(message) == expected_lines.len, result.output
}

fn test_compile_error_rejects_non_literal_message_explicitly() {
	root := os.join_path(os.vtmp_dir(), 'compile_error_message_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	source := os.join_path(root, 'message.v')
	os.write_file(source, 'fn main() { \$compile_error("name " + "handler") }')!
	result := os.exec([@VEXE, '-no-retry-compilation', '-nocache', '-o',
		os.join_path(root, 'output.c'), source])
	assert result.exit_code != 0
	assert result.output.contains('`\$compile_error` expects a string literal'), result.output
	assert !result.output.contains('unknown function'), result.output
}
