// vtest build: false

module fastc

import os
import v.cmdexec
import v.pref

fn test_fastc_byte_constant_uses_its_symbol() {
	prefs := pref.new_preferences()
	c_source := generate('module main
const byte = 8

fn main() {
	println(byte + 1)
}
', 'byte_constant.v', prefs) or { panic(err) }
	test_dir := os.join_path(os.vtmp_dir(), 'fastc_byte_constant_${os.getpid()}')
	os.mkdir_all(test_dir) or { panic(err) }
	defer {
		os.rmdir_all(test_dir) or {}
	}
	c_file := os.join_path(test_dir, 'program.c')
	bin_file := os.join_path(test_dir, 'program')
	os.write_file(c_file, c_source) or { panic(err) }
	tcc := os.join_path(prefs.vroot, 'thirdparty', 'tcc', 'tcc.exe')
	compiled := cmdexec.run(tcc, ['-std=gnu11', '-o', bin_file, c_file])
	assert compiled.exit_code == 0, compiled.output
	executed := cmdexec.run(bin_file, [])
	assert executed.exit_code == 0, executed.output
	assert executed.output == '9\n'
}

fn test_fastc_byte_without_a_declaration_is_unresolved() {
	prefs := pref.new_preferences()
	mut message := ''
	_ := generate('module main\nfn main() { println(byte) }\n', 'undeclared_byte.v', prefs) or {
		message = err.msg()
		''
	}
	assert message.contains('unresolved name `byte`'), message
}

fn test_fastc_arm64_byte_function_is_not_a_cast() {
	$if arm64 ? {
		test_dir := os.join_path(os.vtmp_dir(), 'fastc_arm64_byte_function_${os.getpid()}')
		os.mkdir_all(test_dir) or { panic(err) }
		defer {
			os.rmdir_all(test_dir) or {}
		}
		source_path := os.join_path(test_dir, 'main.v')
		output_path := os.join_path(test_dir, 'program')
		os.write_file(source_path, 'fn byte(value int) int { return value + 1 }\nfn main() { println(byte(8)) }\n') or {
			panic(err)
		}
		mut prefs := pref.new_preferences()
		prefs.backend = 'fastc'
		prefs.user_defines = ['arm64']
		generate_arm64_files([source_path], prefs, output_path) or { panic(err) }
		executed := os.execute(os.quoted_path(output_path))
		assert executed.exit_code == 0, executed.output
		assert executed.output == '9\n'
	}
}

fn test_fastc_arm64_sizeof_byte_constant_uses_its_type() {
	$if arm64 ? {
		test_dir := os.join_path(os.vtmp_dir(), 'fastc_arm64_byte_sizeof_${os.getpid()}')
		os.mkdir_all(test_dir) or { panic(err) }
		defer {
			os.rmdir_all(test_dir) or {}
		}
		source_path := os.join_path(test_dir, 'main.v')
		output_path := os.join_path(test_dir, 'program')
		os.write_file(source_path, 'const byte = 8\nfn main() { println(sizeof(byte)) }\n') or {
			panic(err)
		}
		mut prefs := pref.new_preferences()
		prefs.backend = 'fastc'
		prefs.user_defines = ['arm64']
		generate_arm64_files([source_path], prefs, output_path) or { panic(err) }
		executed := os.execute(os.quoted_path(output_path))
		assert executed.exit_code == 0, executed.output
		assert executed.output == '8\n'
	}
}
