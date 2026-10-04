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

fn test_fastc_byte_function_does_not_collide_with_c_typedef() {
	prefs := pref.new_preferences()
	c_source := generate('module main

fn main() {
	println(byte(8))
}

fn byte(value int) int {
	return value + 1
}
', 'byte_function.v', prefs) or { panic(err) }
	assert c_source.contains('typedef unsigned char byte;')
	test_dir := os.join_path(os.vtmp_dir(), 'fastc_byte_function_${os.getpid()}')
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
		executed := os.exec([output_path])
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
		executed := os.exec([output_path])
		assert executed.exit_code == 0, executed.output
		assert executed.output == '8\n'
	}
}

fn test_fastc_byte_globals_do_not_collide_with_c_typedef() {
	for fixture, source in {
		'scalar':  'module main
__global byte int
fn update() { byte = 8; byte += 2; byte++; byte-- }
fn read() int { return byte }
fn main() {
 println(byte)
 update()
 println(read())
 byte = 12
 println(byte)
 println(read())
}
'
		'pointer': 'module main
__global byte &int
fn main() { value := 9; byte = &value; println(*byte) }

'
	} {
		mut prefs := pref.new_preferences()
		prefs.enable_globals = true
		c_source := generate(source, 'byte_global_${fixture}.v', prefs) or { panic(err) }
		assert c_source.contains('typedef unsigned char byte;')
		test_dir := os.join_path(os.vtmp_dir(), 'fastc_byte_global_${fixture}_${os.getpid()}')
		os.mkdir_all(test_dir) or { panic(err) }
		defer { os.rmdir_all(test_dir) or {} }
		c_file := os.join_path(test_dir, 'program.c')
		bin_file := os.join_path(test_dir, 'program')
		os.write_file(c_file, c_source) or { panic(err) }
		tcc := os.join_path(prefs.vroot, 'thirdparty', 'tcc', 'tcc.exe')
		compiled := cmdexec.run(tcc, ['-std=gnu11', '-o', bin_file, c_file])
		assert compiled.exit_code == 0, compiled.output
		executed := cmdexec.run(bin_file, [])
		assert executed.exit_code == 0, executed.output
		assert executed.output == if fixture == 'scalar' { '0\n10\n12\n12\n' } else { '9\n' }
	}
}

fn test_fastc_byte_global_mapping_keeps_other_symbol_names() {
	assert fastc_c_global_name('byte') == 'main__byte'
	assert fastc_c_global_name('main.byte') == 'main__byte'
	assert fastc_c_global_name('worker.byte') == 'worker__byte'
	assert fastc_c_constant_name('main', 'byte') == 'main__byte'
	assert fastc_c_function_name('main', 'byte') == '__vf_function_byte'
}
