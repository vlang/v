module fastc

import os
import v3.cmdexec
import v3.pref

fn test_generate_and_compile_without_flat_ast() {
	source := 'module main

fn main() {
	mut total := 0
	label := "total="
	for i in 0 .. 3 {
		total += twice(i)
	}
	if true {
		print(label)
		println(total)
	} else {
		println(0)
	}
}

fn twice(value int) int {
	return value * 2
}
'
	prefs := pref.new_preferences()
	c_source := generate(source, 'fastc_test.v', prefs) or { panic(err) }
	assert c_source.contains('__typeof__((0)) total = (0);')
	assert c_source.contains('string label = ("total=");')
	assert c_source.contains('__v_fastc_range_start_0 = (0);')
	assert c_source.contains('__v_fastc_range_end_1 = (3);')
	assert c_source.contains('int twice(int value);')
	assert c_source.contains('setvbuf(stdout, NULL, _IONBF, 0);')
	assert !c_source.contains('v3.flat')

	root := os.join_path(os.vtmp_dir(), 'v3_fastc_${os.getpid()}')
	os.rmdir_all(root) or {}
	os.mkdir_all(root) or { panic(err) }
	defer {
		os.rmdir_all(root) or {}
	}
	c_file := os.join_path(root, 'program.c')
	bin_file := os.join_path(root, 'program')
	os.write_file(c_file, c_source) or { panic(err) }
	tcc := os.join_path(prefs.vroot, 'thirdparty', 'tcc', 'tcc.exe')
	compile_result := cmdexec.run(tcc, ['-std=gnu11', '-o', bin_file, c_file])
	assert compile_result.exit_code == 0, compile_result.output
	run_result := cmdexec.run(bin_file, [])
	assert run_result.exit_code == 0, run_result.output
	assert run_result.output.trim_space() == 'total=6'
}

fn test_ordinary_string_interpolation_has_runtime_support() {
	prefs := pref.new_preferences()
	c_source := generate(r"module main

fn greeting(name string) string {
	return 'hello ${name}!'
}

fn main() {
	println(greeting('FastC'))
}
",
		'ordinary_string_interpolation.v', prefs) or { panic(err) }
	assert c_source.contains('static string builtin__string_plus_many'), c_source
	assert c_source.contains('builtin__string_plus_many(3, (string[]){_S("hello "), name, _S("!")})'), c_source

	root := os.join_path(os.vtmp_dir(), 'v3_fastc_interpolation_${os.getpid()}')
	os.rmdir_all(root) or {}
	os.mkdir_all(root) or { panic(err) }
	defer {
		os.rmdir_all(root) or {}
	}
	c_file := os.join_path(root, 'program.c')
	bin_file := os.join_path(root, 'program')
	os.write_file(c_file, c_source) or { panic(err) }
	tcc := os.join_path(prefs.vroot, 'thirdparty', 'tcc', 'tcc.exe')
	compile_result := cmdexec.run(tcc, ['-std=gnu11', '-o', bin_file, c_file])
	assert compile_result.exit_code == 0, compile_result.output
	run_result := cmdexec.run(bin_file, [])
	assert run_result.exit_code == 0, run_result.output
	assert run_result.output == 'hello FastC!\n'
}

fn test_ordinary_primitive_interpolation_has_runtime_support() {
	prefs := pref.new_preferences()
	c_source := generate(r"module main

fn main() {
	value := 7
	negative := -2
	large := u64(42)
	enabled := true
	println('value=${value}; negative=${negative}; large=${large}; enabled=${enabled}')
	hex_value := 15
	println('${hex_value:x}|${hex_value:04x}|${hex_value:X}|${hex_value:04d}|${hex_value:b}|${hex_value:o}')
}
",
		'ordinary_primitive_interpolation.v', prefs) or { panic(err) }
	assert c_source.contains('v_fastc_signed_str((long long)(value))'), c_source
	assert c_source.contains('v_fastc_signed_str((long long)(negative))'), c_source
	assert c_source.contains('v_fastc_unsigned_str((unsigned long long)(large))'), c_source
	assert c_source.contains('v_fastc_bool_str(enabled)'), c_source
	assert c_source.contains('v_fastc_signed_format((long long)(hex_value), "x")'), c_source
	assert c_source.contains('v_fastc_signed_format((long long)(hex_value), "04x")'), c_source

	root := os.join_path(os.vtmp_dir(), 'v3_fastc_primitive_interpolation_${os.getpid()}')
	os.rmdir_all(root) or {}
	os.mkdir_all(root) or { panic(err) }
	defer {
		os.rmdir_all(root) or {}
	}
	c_file := os.join_path(root, 'program.c')
	bin_file := os.join_path(root, 'program')
	os.write_file(c_file, c_source) or { panic(err) }
	tcc := os.join_path(prefs.vroot, 'thirdparty', 'tcc', 'tcc.exe')
	compile_result := cmdexec.run(tcc, ['-std=gnu11', '-o', bin_file, c_file])
	assert compile_result.exit_code == 0, compile_result.output
	run_result := cmdexec.run(bin_file, [])
	assert run_result.exit_code == 0, run_result.output
	assert run_result.output == 'value=7; negative=-2; large=42; enabled=true\nf|000f|F|0015|1111|17\n'
}

fn test_zero_value_strings_print_as_empty_strings() {
	mut prefs := pref.new_preferences()
	prefs.enable_globals = true
	c_source := generate('module main

__global name string

fn main() {
	print(name)
	println(name)
	println("done")
}
',
		'zero_value_string.v', prefs) or { panic(err) }
	assert c_source.contains('fputs(value ? value : "", stdout)'), c_source
	assert c_source.contains('puts(value ? value : "")'), c_source

	root := os.join_path(os.vtmp_dir(), 'v3_fastc_zero_string_${os.getpid()}')
	os.rmdir_all(root) or {}
	os.mkdir_all(root) or { panic(err) }
	defer {
		os.rmdir_all(root) or {}
	}
	c_file := os.join_path(root, 'program.c')
	bin_file := os.join_path(root, 'program')
	os.write_file(c_file, c_source) or { panic(err) }
	tcc := os.join_path(prefs.vroot, 'thirdparty', 'tcc', 'tcc.exe')
	compile_result := cmdexec.run(tcc, ['-std=gnu11', '-o', bin_file, c_file])
	assert compile_result.exit_code == 0, compile_result.output
	run_result := cmdexec.run(bin_file, [])
	assert run_result.exit_code == 0, run_result.output
	assert run_result.output == '\ndone\n'
}

fn test_top_level_statements_emit_main_directly() {
	prefs := pref.new_preferences()
	c_source := generate("println('Hello, World!')\n", 'hello_world.v', prefs) or { panic(err) }
	assert c_source.contains('int main(void) {')
	assert c_source.contains('println("Hello, World!");')
	assert c_source.contains('setvbuf(stdout, NULL, _IONBF, 0);')
}

fn test_unsupported_import_is_rejected() {
	prefs := pref.new_preferences()
	mut failed := false
	_ := generate('module main\nimport os\nfn main() {}\n', 'imports.v', prefs) or {
		failed = true
		''
	}
	assert failed
}

fn test_c_build_directives_are_rejected_instead_of_discarded() {
	mut prefs := pref.new_preferences()
	for building_v in [false, true] {
		prefs.building_v = building_v
		for directive in ['#flag -D FEATURE=1', '#pkgconfig sqlite3'] {
			mut message := ''
			_ := generate('module main\n${directive}\nfn main() {}\n', 'c_build_directive.v', prefs) or {
				message = err.msg()
				''
			}
			assert message.contains('C build directive `${directive}`'), message
		}
	}

	root := os.join_path(os.vtmp_dir(), 'v3_fastc_imported_c_flag_${os.getpid()}')
	os.rmdir_all(root) or {}
	os.mkdir_all(os.join_path(root, 'dependency')) or { panic(err) }
	defer {
		os.rmdir_all(root) or {}
	}
	main_file := os.join_path(root, 'main.v')
	os.write_file(main_file, 'module main\nimport dependency\nfn main() { dependency.run() }\n') or {
		panic(err)
	}
	os.write_file(os.join_path(root, 'dependency', 'dependency.v'),
		'module dependency\n#flag -D FEATURE=1\npub fn run() {}\n') or { panic(err) }
	prefs.building_v = false
	prefs.module_search_paths = [root]
	mut message := ''
	_ := generate_files([main_file], prefs) or {
		message = err.msg()
		''
	}
	assert message.contains('C build directive `#flag -D FEATURE=1`'), message
}

fn test_colliding_import_aliases_are_rejected() {
	prefs := pref.new_preferences()
	for source in [
		'module main\nimport alpha as dep\nimport beta as dep\n',
		'module main\nimport (\nalpha as dep\nbeta as dep\n)\n',
	] {
		mut message := ''
		_ := fastc_scan_source_header(source, 'colliding_import_alias.v', prefs) or {
			message = err.msg()
			FastcSourceHeader{}
		}
		assert message.contains('cannot reuse import alias `dep`'), message
		assert message.contains('`alpha`') && message.contains('`beta`'), message
	}
}

fn test_generate_files_resolves_modules_without_an_ast() {
	root := os.join_path(os.vtmp_dir(), 'v3_fastc_modules_${os.getpid()}')
	os.rmdir_all(root) or {}
	os.mkdir_all(os.join_path(root, 'mathutil')) or { panic(err) }
	defer {
		os.rmdir_all(root) or {}
	}
	main_file := os.join_path(root, 'main.v')
	module_file := os.join_path(root, 'mathutil', 'mathutil.v')
	os.write_file(main_file,
		'module main\nimport mathutil\nfn main() { println(mathutil.twice(21)) }\n') or {
		panic(err)
	}
	os.write_file(module_file,
		'module mathutil\npub fn twice(value int) int { return value * 2 }\n') or { panic(err) }
	mut prefs := pref.new_preferences()
	prefs.module_search_paths = [root]
	c_source := generate_files([main_file], prefs) or { panic(err) }
	assert c_source.contains('int mathutil__twice(int value);')
	assert c_source.contains('println(mathutil__twice(21));'), c_source

	c_file := os.join_path(root, 'program.c')
	bin_file := os.join_path(root, 'program')
	os.write_file(c_file, c_source) or { panic(err) }
	tcc := os.join_path(prefs.vroot, 'thirdparty', 'tcc', 'tcc.exe')
	compile_result := cmdexec.run(tcc, ['-std=gnu11', '-o', bin_file, c_file])
	assert compile_result.exit_code == 0, compile_result.output
	run_result := cmdexec.run(bin_file, [])
	assert run_result.exit_code == 0, run_result.output
	assert run_result.output.trim_space() == '42'
}

fn test_header_discovers_imports_only_from_selected_comptime_branches() {
	root := os.join_path(os.vtmp_dir(), 'v3_fastc_comptime_imports_${os.getpid()}')
	os.rmdir_all(root) or {}
	os.mkdir_all(os.join_path(root, 'alpha')) or { panic(err) }
	os.mkdir_all(os.join_path(root, 'beta')) or { panic(err) }
	defer {
		os.rmdir_all(root) or {}
	}
	main_file := os.join_path(root, 'main.v')
	os.write_file(main_file, 'module main

\$if linux {
	import alpha as dep
} \$else {
	import beta as dep
}

fn main() {
	dep.ping()
}
') or {
		panic(err)
	}
	os.write_file(os.join_path(root, 'alpha', 'alpha.v'), "module alpha

fn init() {
	println('alpha init')
}

fn cleanup() {
	println('alpha cleanup')
}

pub fn ping() {}
") or {
		panic(err)
	}
	os.write_file(os.join_path(root, 'beta', 'beta.v'), "module beta

fn init() {
	println('beta init')
}

fn cleanup() {
	println('beta cleanup')
}

pub fn ping() {}
") or {
		panic(err)
	}
	mut prefs := pref.new_preferences()
	prefs.target = pref.target_from('linux', pref.host_arch()) or { panic(err) }
	prefs.module_search_paths = [root]
	header := fastc_scan_source_header(os.read_file(main_file) or { panic(err) }, main_file, prefs) or {
		panic(err)
	}
	assert header.import_order == ['alpha']
	assert header.imports['dep'] == 'alpha'
	assert 'beta' !in header.imports.values()
	sources := fastc_resolve_source_files([main_file], prefs) or { panic(err) }
	mut resolved_modules := []string{}
	for source_file in sources {
		if source_file.header.module_name !in resolved_modules {
			resolved_modules << source_file.header.module_name
		}
	}
	assert resolved_modules == ['main', 'alpha']
	prefs.building_v = true
	c_source := generate_source_files(sources, prefs) or { panic(err) }
	assert c_source.contains('\talpha__init();'), c_source
	assert c_source.contains('\talpha__cleanup();'), c_source
	assert !c_source.contains('beta__init'), c_source
	assert !c_source.contains('beta__cleanup'), c_source
}

fn test_generate_files_rejects_mismatched_imported_module_declarations() {
	root := os.join_path(os.vtmp_dir(), 'v3_fastc_module_mismatch_${os.getpid()}')
	os.rmdir_all(root) or {}
	os.mkdir_all(os.join_path(root, 'foo')) or { panic(err) }
	defer {
		os.rmdir_all(root) or {}
	}
	main_file := os.join_path(root, 'main.v')
	os.write_file(main_file, 'module main\nimport foo\nfn main() { println(foo.answer()) }\n') or {
		panic(err)
	}
	os.write_file(os.join_path(root, 'foo', 'foo.v'),
		'module bar\npub fn answer() int { return 42 }\n') or { panic(err) }
	mut prefs := pref.new_preferences()
	prefs.module_search_paths = [root]
	mut message := ''
	_ := generate_files([main_file], prefs) or {
		message = err.msg()
		''
	}
	assert message.contains('declares module `bar` instead of `foo`'), message
}

fn test_generate_files_preserves_all_blank_imports() {
	root := os.join_path(os.vtmp_dir(), 'v3_fastc_blank_imports_${os.getpid()}')
	os.rmdir_all(root) or {}
	os.mkdir_all(os.join_path(root, 'alpha')) or { panic(err) }
	os.mkdir_all(os.join_path(root, 'beta')) or { panic(err) }
	defer {
		os.rmdir_all(root) or {}
	}
	main_file := os.join_path(root, 'main.v')
	os.write_file(main_file, "module main

import alpha as _
import beta as _

fn main() {
	println('main')
}
") or {
		panic(err)
	}
	os.write_file(os.join_path(root, 'alpha', 'alpha.v'), "module alpha

fn init() {
	println('alpha init')
}
") or {
		panic(err)
	}
	os.write_file(os.join_path(root, 'beta', 'beta.v'), "module beta

fn init() {
	println('beta init')
}
") or {
		panic(err)
	}
	mut prefs := pref.new_preferences()
	prefs.module_search_paths = [root]
	header := fastc_scan_source_header(os.read_file(main_file) or { panic(err) }, main_file, prefs) or {
		panic(err)
	}
	assert header.blank_imports == ['alpha', 'beta']
	assert header.import_order == ['alpha', 'beta']
	assert '_' !in header.imports
	c_source := generate_files([main_file], prefs) or { panic(err) }
	assert c_source.contains('\talpha__init();'), c_source
	assert c_source.contains('\tbeta__init();'), c_source

	c_file := os.join_path(root, 'program.c')
	bin_file := os.join_path(root, 'program')
	os.write_file(c_file, c_source) or { panic(err) }
	tcc := os.join_path(prefs.vroot, 'thirdparty', 'tcc', 'tcc.exe')
	compile_result := cmdexec.run(tcc, ['-std=gnu11', '-o', bin_file, c_file])
	assert compile_result.exit_code == 0, compile_result.output
	run_result := cmdexec.run(bin_file, [])
	assert run_result.exit_code == 0, run_result.output
	assert run_result.output.trim_space() == 'alpha init\nbeta init\nmain'
}

fn test_generate_files_rejects_private_imported_functions() {
	root := os.join_path(os.vtmp_dir(), 'v3_fastc_private_import_${os.getpid()}')
	os.rmdir_all(root) or {}
	os.mkdir_all(os.join_path(root, 'secrets')) or { panic(err) }
	defer {
		os.rmdir_all(root) or {}
	}
	main_file := os.join_path(root, 'main.v')
	module_file := os.join_path(root, 'secrets', 'secrets.v')
	os.write_file(main_file,
		'module main\nimport secrets\nfn main() { println(secrets.secret()) }\n') or { panic(err) }
	os.write_file(module_file, 'module secrets\nfn secret() int { return 42 }\n') or { panic(err) }
	mut prefs := pref.new_preferences()
	prefs.module_search_paths = [root]
	mut message := ''
	_ := generate_files([main_file], prefs) or {
		message = err.msg()
		''
	}
	assert message.contains('private function `secret` from imported module `secrets`'), message

	os.write_file(module_file, 'module secrets\npub fn secret() int { return 42 }\n') or {
		panic(err)
	}
	c_source := generate_files([main_file], prefs) or { panic(err) }
	assert c_source.contains('println(secrets__secret());'), c_source
}

fn test_generate_files_rejects_private_imported_constants() {
	root := os.join_path(os.vtmp_dir(), 'v3_fastc_private_constant_${os.getpid()}')
	os.rmdir_all(root) or {}
	os.mkdir_all(os.join_path(root, 'secrets')) or { panic(err) }
	defer {
		os.rmdir_all(root) or {}
	}
	main_file := os.join_path(root, 'main.v')
	module_file := os.join_path(root, 'secrets', 'secrets.v')
	os.write_file(main_file, 'module main\nimport secrets\nfn main() { println(secrets.secret) }\n') or {
		panic(err)
	}
	os.write_file(module_file, 'module secrets\nconst secret = 42\n') or { panic(err) }
	mut prefs := pref.new_preferences()
	prefs.module_search_paths = [root]
	mut message := ''
	_ := generate_files([main_file], prefs) or {
		message = err.msg()
		''
	}
	assert message.contains('private constant `secret` from imported module `secrets`'), message

	os.write_file(module_file, 'module secrets\npub const secret = 42\n') or { panic(err) }
	c_source := generate_files([main_file], prefs) or { panic(err) }
	assert c_source.contains('println(secrets__secret);'), c_source
}

fn test_duplicate_constant_declarations_are_rejected() {
	prefs := pref.new_preferences()
	for source in [
		'module main\nconst answer = 1\nconst answer = 2\nfn main() {}\n',
		'module main\nconst (\nanswer = 1\nanswer = 2\n)\nfn main() {}\n',
	] {
		mut message := ''
		_ := generate(source, 'duplicate_constant.v', prefs) or {
			message = err.msg()
			''
		}
		assert message.contains('duplicate constant `answer`'), message
	}
}

fn test_constant_declarations_require_an_assignment_after_the_name() {
	prefs := pref.new_preferences()
	mut message := ''
	_ := generate('module main\nconst answer nonsense = 42\nfn main() {}\n',
		'invalid_constant_assignment.v', prefs) or {
		message = err.msg()
		''
	}
	assert message.contains('constant `answer` requires `=` or `:=` after its name'), message

	for assignment in ['=', ':='] {
		c_source := generate('module main\nconst answer ${assignment} 42\nfn main() { println(answer) }\n',
			'valid_constant_assignment.v', prefs) or { panic(err) }
		assert c_source.contains('main__answer'), c_source
	}
}

fn test_duplicate_type_declarations_are_rejected() {
	prefs := pref.new_preferences()
	for source in [
		'module main\ntype UserId = int\ntype UserId = int\nfn main() {}\n',
		'module main\nstruct Item {}\ntype Item = int\nfn main() {}\n',
	] {
		mut message := ''
		_ := generate(source, 'duplicate_type.v', prefs) or {
			message = err.msg()
			''
		}
		assert message.contains('duplicate type declaration'), message
	}
}

fn test_global_declarations_require_enable_globals_or_module_attribute() {
	prefs := pref.new_preferences()
	mut message := ''
	_ := generate('module main\n__global answer = 42\nfn main() {}\n', 'plain_global.v', prefs) or {
		message = err.msg()
		''
	}
	assert message.contains('use `v -enable-globals ...` to enable globals'), message

	attributed_source := generate('@[has_globals]\nmodule main\n__global answer = 42\nfn main() {}\n',
		'attributed_global.v', prefs) or { panic(err) }
	assert attributed_source.contains('static int answer;'), attributed_source

	mut enabled_prefs := pref.new_preferences()
	enabled_prefs.enable_globals = true
	enabled_source := generate('module main\n__global answer = 42\nfn main() {}\n',
		'enabled_global.v', enabled_prefs) or { panic(err) }
	assert enabled_source.contains('static int answer;'), enabled_source
}

fn test_duplicate_global_declarations_are_rejected() {
	mut prefs := pref.new_preferences()
	prefs.enable_globals = true
	mut message := ''
	_ := generate('module main\n__global answer = 1\n__global answer = 2\nfn main() {}\n',
		'duplicate_global.v', prefs) or {
		message = err.msg()
		''
	}
	assert message.contains('duplicate global `answer`'), message
}

fn test_generate_files_rejects_private_imported_globals() {
	root := os.join_path(os.vtmp_dir(), 'v3_fastc_private_global_${os.getpid()}')
	os.rmdir_all(root) or {}
	os.mkdir_all(os.join_path(root, 'secrets')) or { panic(err) }
	defer {
		os.rmdir_all(root) or {}
	}
	main_file := os.join_path(root, 'main.v')
	module_file := os.join_path(root, 'secrets', 'secrets.v')
	os.write_file(main_file, 'module main\nimport secrets\nfn main() { println(secrets.secret) }\n') or {
		panic(err)
	}
	os.write_file(module_file, 'module secrets\n__global secret = 42\n') or { panic(err) }
	mut prefs := pref.new_preferences()
	prefs.enable_globals = true
	prefs.module_search_paths = [root]
	mut message := ''
	_ := generate_files([main_file], prefs) or {
		message = err.msg()
		''
	}
	assert message.contains('private global `secret` from imported module `secrets`'), message
	os.write_file(main_file,
		'module main\nimport secrets\nconst copied = secrets.secret\nfn main() { println(copied) }\n') or {
		panic(err)
	}
	message = ''
	_ := generate_files([main_file], prefs) or {
		message = err.msg()
		''
	}
	assert message.contains('private global `secret` from imported module `secrets`'), message

	os.write_file(module_file, 'module secrets\npub __global secret = 42\n') or { panic(err) }
	os.write_file(main_file, 'module main\nimport secrets\nfn main() { println(secrets.secret) }\n') or {
		panic(err)
	}
	c_source := generate_files([main_file], prefs) or { panic(err) }
	assert c_source.contains('println(secrets__secret);'), c_source
}

fn test_generate_files_rejects_private_imported_types() {
	root := os.join_path(os.vtmp_dir(), 'v3_fastc_private_types_${os.getpid()}')
	os.rmdir_all(root) or {}
	os.mkdir_all(os.join_path(root, 'secrets')) or { panic(err) }
	defer {
		os.rmdir_all(root) or {}
	}
	main_file := os.join_path(root, 'main.v')
	module_file := os.join_path(root, 'secrets', 'secrets.v')
	os.write_file(module_file, 'module secrets

struct SecretStruct {}
enum SecretEnum { value }
interface SecretInterface {}
union SecretUnion { value int }
type SecretAlias = int
') or {
		panic(err)
	}
	mut prefs := pref.new_preferences()
	prefs.module_search_paths = [root]
	for type_name in ['SecretStruct', 'SecretEnum', 'SecretInterface', 'SecretUnion', 'SecretAlias'] {
		os.write_file(main_file,
			'module main\nimport secrets\nfn consume(value secrets.${type_name}) {}\nfn main() {}\n') or {
			panic(err)
		}
		mut message := ''
		_ := generate_files([main_file], prefs) or {
			message = err.msg()
			''
		}
		assert message.contains('private type `${type_name}` from imported module `secrets`'), message
	}

	os.write_file(module_file, 'module secrets\npub struct SecretStruct {}\n') or { panic(err) }
	os.write_file(main_file,
		'module main\nimport secrets\nfn consume(value secrets.SecretStruct) {}\nfn main() {}\n') or {
		panic(err)
	}
	c_source := generate_files([main_file], prefs) or { panic(err) }
	assert c_source.contains('void consume(secrets__SecretStruct value);'), c_source
}

fn test_generate_files_restricts_unqualified_imported_type_lookup() {
	root := os.join_path(os.vtmp_dir(), 'v3_fastc_type_scope_${os.getpid()}')
	os.rmdir_all(root) or {}
	os.mkdir_all(os.join_path(root, 'widgets')) or { panic(err) }
	defer {
		os.rmdir_all(root) or {}
	}
	main_file := os.join_path(root, 'main.v')
	module_file := os.join_path(root, 'widgets', 'widgets.v')
	os.write_file(module_file, 'module widgets\npub struct Widget {}\n') or { panic(err) }
	mut prefs := pref.new_preferences()
	prefs.module_search_paths = [root]

	os.write_file(main_file,
		'module main\nimport widgets\nfn consume(value Widget) {}\nfn main() {}\n') or {
		panic(err)
	}
	mut message := ''
	_ := generate_files([main_file], prefs) or {
		message = err.msg()
		''
	}
	assert message.contains('undeclared type `Widget`'), message

	os.write_file(main_file,
		'module main\nimport widgets { Widget }\nfn consume(value Widget) {}\nfn main() {}\n') or {
		panic(err)
	}
	c_source := generate_files([main_file], prefs) or { panic(err) }
	assert c_source.contains('void consume(widgets__Widget value);'), c_source

	os.write_file(module_file, 'module widgets\nstruct Widget {}\n') or { panic(err) }
	message = ''
	_ := generate_files([main_file], prefs) or {
		message = err.msg()
		''
	}
	assert message.contains('private type `Widget` from imported module `widgets`'), message
}

fn test_selfhost_struct_field_defaults_are_preserved() {
	mut prefs := pref.new_preferences()
	prefs.building_v = true
	c_source := generate('module main

fn default_retries() int {
	return 3
}

struct Config {
	retries int = default_retries()
}

fn main() {
	config := Config{}
	println(config.retries)
}
',
		'struct_field_default.v', prefs) or { panic(err) }
	assert c_source.contains('int default_retries(void)'), c_source
	assert c_source.contains('__v_fastc_struct_default.retries=(default_retries());'), c_source
}

fn test_required_struct_fields_must_be_initialized() {
	mut prefs := pref.new_preferences()
	prefs.building_v = true
	mut message := ''
	_ := generate('module main

struct Config {
	name string @[required]
}

fn main() {
	Config{}
}
',
		'missing_required_struct_field.v', prefs) or {
		message = err.msg()
		''
	}
	assert message.contains('field `Config.name` must be initialized'), message

	generate('module main

struct Config {
	name string @[required]
}

fn main() {
	config := Config{name: "set"}
	println(config.name)
}
',
		'initialized_required_struct_field.v', prefs) or { panic(err) }
}

fn test_generate_files_rejects_private_imported_struct_fields() {
	root := os.join_path(os.vtmp_dir(), 'v3_fastc_private_fields_${os.getpid()}')
	os.rmdir_all(root) or {}
	os.mkdir_all(os.join_path(root, 'records')) or { panic(err) }
	defer {
		os.rmdir_all(root) or {}
	}
	main_file := os.join_path(root, 'main.v')
	module_file := os.join_path(root, 'records', 'records.v')
	module_source := 'module records

pub struct Settings {
	secret int
pub:
	visible int
}

pub fn make() Settings {
	return Settings{secret: 1, visible: 2}
}
'
	mut prefs := pref.new_preferences()
	prefs.building_v = true
	prefs.module_search_paths = [root]
	for source in [
		'module main\nimport records\nfn main() { value := records.make(); println(value.secret) }\n',
		'module main\nimport records\nfn main() { value := records.Settings{secret: 1}; println(value.visible) }\n',
	] {
		mut message := ''
		_ := generate_source_files([
			FastcSourceFile{
				path:   main_file
				source: source
				header: fastc_scan_source_header(source, main_file, prefs) or { panic(err) }
			},
			FastcSourceFile{
				path:   module_file
				source: module_source
				header: fastc_scan_source_header(module_source, module_file, prefs) or {
					panic(err)
				}
			},
		], prefs) or {
			message = err.msg()
			''
		}
		assert message.contains('private field `Settings.secret` from imported module `records`'), message
	}

	valid_source := 'module main\nimport records\nfn main() { value := records.Settings{visible: 2}; println(value.visible) }\n'
	c_source := generate_source_files([
		FastcSourceFile{
			path:   main_file
			source: valid_source
			header: fastc_scan_source_header(valid_source, main_file, prefs) or { panic(err) }
		},
		FastcSourceFile{
			path:   module_file
			source: module_source
			header: fastc_scan_source_header(module_source, module_file, prefs) or { panic(err) }
		},
	], prefs) or { panic(err) }
	assert c_source.contains('.visible=(2)'), c_source
}

fn test_imported_public_field_mutability_is_preserved() {
	root := os.join_path(os.vtmp_dir(), 'v3_fastc_field_mutability_${os.getpid()}')
	os.rmdir_all(root) or {}
	os.mkdir_all(os.join_path(root, 'settings')) or { panic(err) }
	defer {
		os.rmdir_all(root) or {}
	}
	main_file := os.join_path(root, 'main.v')
	module_file := os.join_path(root, 'settings', 'settings.v')
	module_source := 'module settings

pub struct Config {
pub:
	read_only int
pub mut:
	writable int
}
'
	mut prefs := pref.new_preferences()
	prefs.building_v = true
	invalid_source := 'module main

import settings

fn main() {
	mut config := settings.Config{}
	config.read_only = 2
}
'
	mut message := ''
	_ := generate_source_files([
		FastcSourceFile{
			path:   main_file
			source: invalid_source
			header: fastc_scan_source_header(invalid_source, main_file, prefs) or { panic(err) }
		},
		FastcSourceFile{
			path:   module_file
			source: module_source
			header: fastc_scan_source_header(module_source, module_file, prefs) or { panic(err) }
		},
	], prefs) or {
		message = err.msg()
		''
	}
	assert message.contains('mutation of immutable field `Config.read_only`'), message

	valid_source := 'module main

import settings

fn main() {
	mut config := settings.Config{}
	config.writable = 2
}
'
	c_source := generate_source_files([
		FastcSourceFile{
			path:   main_file
			source: valid_source
			header: fastc_scan_source_header(valid_source, main_file, prefs) or { panic(err) }
		},
		FastcSourceFile{
			path:   module_file
			source: module_source
			header: fastc_scan_source_header(module_source, module_file, prefs) or { panic(err) }
		},
	], prefs) or { panic(err) }
	assert c_source.contains('config.writable=2;'), c_source
}

fn test_struct_literal_fields_are_validated() {
	mut prefs := pref.new_preferences()
	prefs.building_v = true
	for source, expected in {
		'module main\nstruct Config { enabled bool }\nfn main() { config := Config{enabled: 2}; println(config.enabled) }\n':                            'for struct field `Config.enabled` expecting `bool`'
		'module main\nstruct Config { value int }\nfn main() { config := Config{value: 1, value: 2}; println(config.value) }\n':                         'duplicate field `Config.value` in struct literal'
		'module main\nstruct Config { values [2]int }\nfn main() { config := Config{values: [true, false]!}; println(config.values) }\n':                'element 1 has type `bool` instead of `int`'
		'module main\nstruct Config { values [2]int }\nfn main() { config := Config{values: [1]!}; println(config.values) }\n':                          'expects 2 elements, got 1'
		'module main\nconst size = 2\nstruct Config { values [size]int }\nfn main() { config := Config{values: [1, 2, 3]!}; println(config.values) }\n': 'expects 2 elements, got 3'
	} {
		mut message := ''
		_ := generate(source, 'invalid_struct_literal.v', prefs) or {
			message = err.msg()
			''
		}
		assert message.contains(expected), message
	}

	c_source := generate('module main

struct Config {
	enabled bool
	value int
}

fn main() {
	enabled := true
	config := Config{enabled: enabled, value: 2}
	println(config.value)
}
',
		'valid_struct_literal.v', prefs) or { panic(err) }
	assert c_source.contains('.enabled=(enabled)'), c_source
	assert c_source.contains('.value=(2)'), c_source
}

fn test_embedded_struct_fields_use_storage_paths() {
	mut prefs := pref.new_preferences()
	prefs.building_v = true
	c_source := generate('module main

struct Child {
	mut:
	count int
}

struct Leaf {
	mut:
	number int
}

struct Inner {
	Leaf

	mut:
	value int
	child Child
}

struct Outer {
	Inner
}

fn main() {
	mut outer := Outer{value: 3, number: 7}
	outer.child.count = 5
	println(outer.value)
	println(outer.child.count)
	println(outer.number)
	outer.value = 4
	outer.child.count = 6
	outer.number = 8
	println(outer.value)
	println(outer.child.count)
	println(outer.number)
}
',
		'embedded_struct_fields.v', prefs) or { panic(err) }
	assert c_source.contains('.__embedded_0.value=(3)'), c_source
	assert c_source.contains('.__embedded_0.__embedded_0.number=(7)'), c_source
	assert c_source.contains('outer.__embedded_0.value'), c_source
	assert c_source.contains('outer.__embedded_0.child.count'), c_source
	assert c_source.contains('outer.__embedded_0.__embedded_0.number'), c_source
}

fn test_interface_dispatch_validates_every_method_parameter() {
	mut prefs := pref.new_preferences()
	prefs.building_v = true
	for declaration in [
		'fn (wrong Wrong) work(value string) int { return 0 }',
		'fn (wrong Wrong) work(value int) int { return value }',
	] {
		interface_parameter := if declaration.contains('value string') {
			'value int'
		} else {
			'mut value int'
		}
		mut message := ''
		_ := generate('module main

interface Worker {
	work(${interface_parameter}) int
}

struct Wrong {}

${declaration}

fn main() {
	worker := Worker(Wrong{})
	println(worker.work(1))
}
',
			'invalid_interface_implementation.v', prefs) or {
			message = err.msg()
			''
		}
		assert message.contains('incompatible signature for interface `Worker` method `work`'), message
	}

	c_source := generate('module main

interface Worker {
	work(value int) int
	update(mut value int)
}

struct Good {}

fn (good Good) work(value int) int {
	return value
}

fn (good Good) update(mut value int) {
	value = 2
}

fn main() {
	worker := Worker(Good{})
	println(worker.work(1))
	mut value := 1
	worker.update(mut value)
}
',
		'valid_interface_implementation.v', prefs) or { panic(err) }
	assert c_source.contains('case __v_typeid_Good:'), c_source
	assert c_source.contains('void builtin__Worker_update(Worker value, int* arg1)'), c_source
}

fn test_disabled_function_attributes_emit_empty_stubs() {
	mut prefs := pref.new_preferences()
	prefs.user_defines = []
	c_source := generate('module main

@[if fastc_missing_define ?]
fn traced(value int) {
	println("must not run")
}

fn side_effect() int {
	println(99)
	return 1
}

fn main() {
	traced(side_effect())
	println(0)
}
',
		'disabled_function_attribute.v', prefs) or { panic(err) }
	assert c_source.contains('void traced(int value) {\n}')
	assert !c_source.contains('must not run')
	assert !c_source.contains('traced(side_effect())')
	assert c_source.contains('((void)0);')

	root := os.join_path(os.vtmp_dir(), 'v3_fastc_disabled_call_${os.getpid()}')
	os.rmdir_all(root) or {}
	os.mkdir_all(root) or { panic(err) }
	defer {
		os.rmdir_all(root) or {}
	}
	c_file := os.join_path(root, 'program.c')
	bin_file := os.join_path(root, 'program')
	os.write_file(c_file, c_source) or { panic(err) }
	tcc := os.join_path(prefs.vroot, 'thirdparty', 'tcc', 'tcc.exe')
	compile_result := cmdexec.run(tcc, ['-std=gnu11', '-o', bin_file, c_file])
	assert compile_result.exit_code == 0, compile_result.output
	run_result := cmdexec.run(bin_file, [])
	assert run_result.exit_code == 0, run_result.output
	assert run_result.output == '0\n'

	module_dir := os.join_path(root, 'tracing')
	os.mkdir_all(module_dir) or { panic(err) }
	main_file := os.join_path(root, 'main.v')
	module_file := os.join_path(module_dir, 'tracing.v')
	os.write_file(main_file, 'module main

import tracing

fn side_effect() int {
	println(99)
	return 1
}

fn main() {
	tracing.trace(side_effect())
	println(0)
}
') or {
		panic(err)
	}
	os.write_file(module_file, 'module tracing

@[if fastc_missing_define ?]
pub fn trace(value int) {}
') or {
		panic(err)
	}
	prefs.module_search_paths = [root]
	imported_source := generate_files([main_file], prefs) or { panic(err) }
	assert !imported_source.contains('tracing__trace(side_effect())')
	os.write_file(c_file, imported_source) or { panic(err) }
	imported_compile_result := cmdexec.run(tcc, ['-std=gnu11', '-o', bin_file, c_file])
	assert imported_compile_result.exit_code == 0, imported_compile_result.output
	imported_run_result := cmdexec.run(bin_file, [])
	assert imported_run_result.exit_code == 0, imported_run_result.output
	assert imported_run_result.output == '0\n'

	mut selfhost_prefs := pref.new_preferences()
	selfhost_prefs.building_v = true
	method_source := generate('module main

struct Tracer {}

@[if fastc_missing_define ?]
fn (tracer Tracer) trace(value int) {}

fn side_effect() int {
	return 1
}

fn run(tracer Tracer) {
	tracer.trace(side_effect())
}

fn main() {}
',
		'disabled_method_attribute.v', selfhost_prefs) or { panic(err) }
	assert !method_source.contains('Tracer_trace(tracer,side_effect())')
	assert method_source.contains('((void)0);')
}

fn test_compound_function_attributes_evaluate_the_complete_condition() {
	mut prefs := pref.new_preferences()
	prefs.target = pref.target_from('linux', pref.host_arch()) or { panic(err) }
	c_source := generate('module main

@[if linux && windows]
fn impossible() {
	println("disabled compound condition")
}

@[if linux || windows]
fn supported() {
	println("enabled compound condition")
}

fn main() {
	impossible()
	supported()
}
',
		'compound_function_attribute.v', prefs) or { panic(err) }
	assert c_source.contains('void impossible(void) {\n}')
	assert !c_source.contains('disabled compound condition')
	assert c_source.contains('enabled compound condition')
}

fn test_disabled_type_attributes_skip_collection_and_emission() {
	prefs := pref.new_preferences()
	c_source := generate('module main

@[if fastc_missing_define ?]
struct DisabledStruct {
	bad MissingDisabledType
}

@[if fastc_missing_define ?]
union DisabledUnion {
	bad MissingDisabledType
}

@[if fastc_missing_define ?]
enum DisabledEnum {
	value
}

@[if fastc_missing_define ?]
interface DisabledInterface {
	bad(value MissingDisabledType)
}

@[if fastc_missing_define ?]
type DisabledAlias = MissingDisabledType

fn main() {
	println(7)
}
',
		'disabled_type_attribute.v', prefs) or { panic(err) }
	for disabled_name in ['DisabledStruct', 'DisabledUnion', 'DisabledEnum', 'DisabledInterface',
		'DisabledAlias', 'MissingDisabledType'] {
		assert !c_source.contains(disabled_name), c_source
	}
}

fn test_selected_top_level_comptime_function_signatures_are_collected() {
	mut prefs := pref.new_preferences()
	prefs.building_v = true
	prefs.target = pref.target_from('linux', pref.host_arch()) or { panic(err) }
	c_source := generate('module main

$if windows {
	fn platform() string {
		return "wrong"
	}
} $else $if linux {
	fn platform() int {
		return 42
	}
} $else {
	fn platform() bool {
		return false
	}
}

fn main() {
	println(platform())
}
',
		'top_level_comptime_function.v', prefs) or { panic(err) }
	assert c_source.contains('int platform(void)'), c_source
	assert c_source.contains('println(platform());'), c_source
	assert !c_source.contains('return "wrong";'), c_source
}

fn test_selected_top_level_comptime_types_are_collected_and_emitted() {
	mut prefs := pref.new_preferences()
	prefs.building_v = true
	prefs.target = pref.target_from('linux', pref.host_arch()) or { panic(err) }
	c_source := generate('module main

$if windows {
	struct Choice {
		wrong bool
	}
} $else $if linux {
	struct Choice {
		value int
	}

	enum Mode {
		selected
	}

	type ChoiceId = int

	union Payload {
		number int
	}

	interface Named {
		name() string
	}
}

fn main() {
	choice := Choice{
		value: 42
	}
	println(choice.value)
}
',
		'top_level_comptime_types.v', prefs) or { panic(err) }
	assert c_source.contains('struct Choice {\n\tint value;'), c_source
	assert !c_source.contains('bool wrong;'), c_source
	assert c_source.contains('#define Mode__selected ((Mode)0)'), c_source
	assert c_source.contains('typedef int ChoiceId;'), c_source
	assert c_source.contains('union Payload {\n\tint number;'), c_source
	assert c_source.contains('struct Named { void *_object; u32 _typ; void *_methods; };'), c_source
	assert c_source.contains('Named_name(Named value) {'), c_source
	assert c_source.contains('__typeof__(((Choice){.value=(42)})) choice'), c_source
}

fn test_selected_top_level_comptime_constants_are_collected_and_emitted() {
	mut prefs := pref.new_preferences()
	prefs.building_v = true
	prefs.target = pref.target_from('linux', pref.host_arch()) or { panic(err) }
	c_source := generate('module main

$if windows {
	const answer = "wrong"
} $else $if linux {
	const answer = 42
} $else {
	const answer = false
}

fn main() {
	println(answer)
}
',
		'top_level_comptime_constant.v', prefs) or { panic(err) }
	assert c_source.contains('#define main__answer (42)'), c_source
	assert c_source.contains('println(main__answer);'), c_source
	assert !c_source.contains('wrong'), c_source
}

fn test_initialized_global_value_is_emitted() {
	mut prefs := pref.new_preferences()
	prefs.enable_globals = true
	c_source := generate('module main

__global answer = 42

fn main() {
	println(answer)
}
',
		'initialized_global.v', prefs) or { panic(err) }
	assert c_source.contains('static int answer;'), c_source
	assert c_source.contains('\tanswer = 42;'), c_source
	assert c_source.contains('v_fastc_init_globals();'), c_source
}

fn test_script_main_initializes_globals_before_statements() {
	mut prefs := pref.new_preferences()
	prefs.enable_globals = true
	c_source := generate('module main

__global answer = 42

fn init() {
	answer = 43
}

println(answer)
',
		'initialized_script_global.v', prefs) or { panic(err) }
	main_source := c_source.all_after('int main(void) {')
	startup_source := c_source.all_after('static void v_fastc_init_globals(void) {')
	initializer := startup_source.index('answer = 42;') or { -1 }
	module_initializer := startup_source.index('\n\tinit();') or { -1 }
	startup_call := main_source.index('v_fastc_init_globals();') or { -1 }
	statement := main_source.index('println(answer);') or { -1 }
	assert initializer >= 0, c_source
	assert module_initializer > initializer, c_source
	assert startup_call >= 0, c_source
	assert statement > startup_call, c_source
}

fn test_runtime_constants_are_materialized_exactly_once() {
	root := os.join_path(os.vtmp_dir(), 'v3_fastc_runtime_constants_${os.getpid()}')
	os.rmdir_all(root) or {}
	os.mkdir_all(root) or { panic(err) }
	defer {
		os.rmdir_all(root) or {}
	}
	mut prefs := pref.new_preferences()
	prefs.enable_globals = true
	c_source := generate('module main

__global calls int

const value = next()
const unused = next()

fn next() int {
	calls++
	return calls
}

fn main() {
	println(value)
	println(value)
	println(calls)
}
',
		'runtime_constants.v', prefs) or { panic(err) }
	assert c_source.contains('static int main__value;'), c_source
	assert c_source.contains('static int main__unused;'), c_source
	assert !c_source.contains('#define main__value'), c_source
	assert c_source.count('main__value = next();') == 1, c_source
	assert c_source.count('main__unused = next();') == 1, c_source

	c_file := os.join_path(root, 'program.c')
	bin_file := os.join_path(root, 'program')
	os.write_file(c_file, c_source) or { panic(err) }
	tcc := os.join_path(prefs.vroot, 'thirdparty', 'tcc', 'tcc.exe')
	compile_result := cmdexec.run(tcc, ['-std=gnu11', '-o', bin_file, c_file])
	assert compile_result.exit_code == 0, compile_result.output
	run_result := cmdexec.run(bin_file, [])
	assert run_result.exit_code == 0, run_result.output
	assert run_result.output.trim_space() == '1\n1\n2'
}

fn test_runtime_constant_initializers_follow_module_dependencies() {
	root := os.join_path(os.vtmp_dir(), 'v3_fastc_constant_order_${os.getpid()}')
	os.rmdir_all(root) or {}
	os.mkdir_all(os.join_path(root, 'dep')) or { panic(err) }
	defer {
		os.rmdir_all(root) or {}
	}
	main_file := os.join_path(root, 'main.v')
	dep_file := os.join_path(root, 'dep', 'dep.v')
	os.write_file(main_file, 'module main

import dep

const copied = dep.original

fn main() {
	println(copied)
}
') or {
		panic(err)
	}
	os.write_file(dep_file, 'module dep

pub const original = next()

pub fn next() int {
	return 42
}
') or {
		panic(err)
	}
	mut prefs := pref.new_preferences()
	prefs.module_search_paths = [root]
	c_source := generate_files([main_file], prefs) or { panic(err) }
	dependency_initializer := c_source.index('dep__original = dep__next();') or { -1 }
	importer_initializer := c_source.index('copied = dep__original;') or { -1 }
	assert dependency_initializer >= 0, c_source
	assert importer_initializer > dependency_initializer, c_source
}

fn test_imported_global_initializers_run_before_importer_globals() {
	root := os.join_path(os.vtmp_dir(), 'v3_fastc_global_order_${os.getpid()}')
	os.rmdir_all(root) or {}
	os.mkdir_all(os.join_path(root, 'dep')) or { panic(err) }
	defer {
		os.rmdir_all(root) or {}
	}
	main_file := os.join_path(root, 'main.v')
	dep_file := os.join_path(root, 'dep', 'dep.v')
	os.write_file(main_file, 'module main

import dep

__global copied = dep.current()

fn main() {
	println(copied)
}
') or {
		panic(err)
	}
	os.write_file(dep_file, 'module dep

__global current_value = 42

pub fn current() int {
	return current_value
}
') or {
		panic(err)
	}
	mut prefs := pref.new_preferences()
	prefs.enable_globals = true
	prefs.module_search_paths = [root]
	c_source := generate_files([main_file], prefs) or { panic(err) }
	dependency_initializer := c_source.index('dep__current_value = 42;') or { -1 }
	importer_initializer := c_source.index('copied = dep__current();') or { -1 }
	assert dependency_initializer >= 0, c_source
	assert importer_initializer > dependency_initializer, c_source

	c_file := os.join_path(root, 'program.c')
	bin_file := os.join_path(root, 'program')
	os.write_file(c_file, c_source) or { panic(err) }
	tcc := os.join_path(prefs.vroot, 'thirdparty', 'tcc', 'tcc.exe')
	compile_result := cmdexec.run(tcc, ['-std=gnu11', '-o', bin_file, c_file])
	assert compile_result.exit_code == 0, compile_result.output
	run_result := cmdexec.run(bin_file, [])
	assert run_result.exit_code == 0, run_result.output
	assert run_result.output.trim_space() == '42'
}

fn test_module_initializers_run_in_dependency_order_before_main() {
	root := os.join_path(os.vtmp_dir(), 'v3_fastc_module_init_${os.getpid()}')
	os.rmdir_all(root) or {}
	os.mkdir_all(os.join_path(root, 'dep')) or { panic(err) }
	defer {
		os.rmdir_all(root) or {}
	}
	main_file := os.join_path(root, 'main.v')
	dep_file := os.join_path(root, 'dep', 'dep.v')
	os.write_file(main_file, 'module main

import dep

__global observed = 0

const copied = dep.value()

fn init() {
	observed = copied + 1
}

fn main() {
	println(observed)
}
') or {
		panic(err)
	}
	os.write_file(dep_file, 'module dep

__global state = 1

fn init() {
	state = 41
}

pub fn value() int {
	return state
}
') or {
		panic(err)
	}
	mut prefs := pref.new_preferences()
	prefs.enable_globals = true
	prefs.module_search_paths = [root]
	c_source := generate_files([main_file], prefs) or { panic(err) }
	main_source := c_source.all_after('int main(void) {')
	startup_source := c_source.all_after('static void v_fastc_init_globals(void) {')
	dependency_initializer := startup_source.index('dep__state = 1;') or { -1 }
	dependency_init := startup_source.index('\tdep__init();') or { -1 }
	importer_initializer := startup_source.index('main__copied = dep__value();') or { -1 }
	entry_global_initializer := startup_source.index('observed = 0;') or { -1 }
	entry_init := startup_source.index('\n\tinit();') or { -1 }
	startup_call := main_source.index('v_fastc_init_globals();') or { -1 }
	main_statement := main_source.index('println(observed);') or { -1 }
	assert dependency_initializer >= 0, c_source
	assert dependency_init > dependency_initializer, c_source
	assert importer_initializer > dependency_init, c_source
	assert entry_global_initializer > importer_initializer, c_source
	assert entry_init > entry_global_initializer, c_source
	assert startup_call >= 0, c_source
	assert main_statement > startup_call, c_source

	c_file := os.join_path(root, 'program.c')
	bin_file := os.join_path(root, 'program')
	os.write_file(c_file, c_source) or { panic(err) }
	tcc := os.join_path(prefs.vroot, 'thirdparty', 'tcc', 'tcc.exe')
	compile_result := cmdexec.run(tcc, ['-std=gnu11', '-o', bin_file, c_file])
	assert compile_result.exit_code == 0, compile_result.output
	run_result := cmdexec.run(bin_file, [])
	assert run_result.exit_code == 0, run_result.output
	assert run_result.output.trim_space() == '42'
}

fn test_module_cleanup_hooks_run_in_reverse_order_on_main_return() {
	root := os.join_path(os.vtmp_dir(), 'v3_fastc_module_cleanup_${os.getpid()}')
	os.rmdir_all(root) or {}
	os.mkdir_all(os.join_path(root, 'dep')) or { panic(err) }
	defer {
		os.rmdir_all(root) or {}
	}
	main_file := os.join_path(root, 'main.v')
	dep_file := os.join_path(root, 'dep', 'dep.v')
	os.write_file(main_file, "module main

import dep

fn init() {
	println('main init')
}

fn cleanup() {
	println('main cleanup')
}

fn main() {
	dep.ping()
	defer {
		println('main defer')
	}
	println('main')
	if true {
		return
	}
	println('unreachable')
}
") or {
		panic(err)
	}
	os.write_file(dep_file, "module dep

fn init() {
	println('dep init')
}

fn cleanup() {
	println('dep cleanup')
}

pub fn ping() {}
") or {
		panic(err)
	}
	mut prefs := pref.new_preferences()
	prefs.module_search_paths = [root]
	c_source := generate_files([main_file], prefs) or { panic(err) }
	cleanup_source := c_source.all_after('static void v_fastc_cleanup_modules(void) {')
	main_cleanup := cleanup_source.index('\n\tcleanup();') or { -1 }
	dependency_cleanup := cleanup_source.index('\n\tdep__cleanup();') or { -1 }
	assert main_cleanup >= 0, c_source
	assert dependency_cleanup > main_cleanup, c_source
	main_source := c_source.all_after('int main(void) {')
	early_cleanup := main_source.index('v_fastc_cleanup_modules();') or { -1 }
	early_return := main_source.index('return 0;') or { -1 }
	assert early_cleanup >= 0, c_source
	assert early_return > early_cleanup, c_source

	c_file := os.join_path(root, 'program.c')
	bin_file := os.join_path(root, 'program')
	os.write_file(c_file, c_source) or { panic(err) }
	tcc := os.join_path(prefs.vroot, 'thirdparty', 'tcc', 'tcc.exe')
	compile_result := cmdexec.run(tcc, ['-std=gnu11', '-o', bin_file, c_file])
	assert compile_result.exit_code == 0, compile_result.output
	run_result := cmdexec.run(bin_file, [])
	assert run_result.exit_code == 0, run_result.output
	assert run_result.output.trim_space() == 'dep init\nmain init\nmain\nmain defer\nmain cleanup\ndep cleanup'
}

fn test_module_lifecycle_preserves_source_import_order() {
	root := os.join_path(os.vtmp_dir(), 'v3_fastc_import_order_${os.getpid()}')
	os.rmdir_all(root) or {}
	os.mkdir_all(os.join_path(root, 'zed')) or { panic(err) }
	os.mkdir_all(os.join_path(root, 'alpha')) or { panic(err) }
	defer {
		os.rmdir_all(root) or {}
	}
	main_file := os.join_path(root, 'main.v')
	os.write_file(main_file, "module main

import zed
import alpha

fn main() {
	zed.ping()
	alpha.ping()
	println('main')
}
") or {
		panic(err)
	}
	os.write_file(os.join_path(root, 'zed', 'zed.v'), "module zed

fn init() {
	println('zed init')
}

fn cleanup() {
	println('zed cleanup')
}

pub fn ping() {}
") or {
		panic(err)
	}
	os.write_file(os.join_path(root, 'alpha', 'alpha.v'), "module alpha

fn init() {
	println('alpha init')
}

fn cleanup() {
	println('alpha cleanup')
}

pub fn ping() {}
") or {
		panic(err)
	}
	mut prefs := pref.new_preferences()
	prefs.module_search_paths = [root]
	header := fastc_scan_source_header(os.read_file(main_file) or { panic(err) }, main_file, prefs) or {
		panic(err)
	}
	assert header.import_order == ['zed', 'alpha']
	c_source := generate_files([main_file], prefs) or { panic(err) }
	startup_source := c_source.all_after('static void v_fastc_init_globals(void) {')
	zed_init := startup_source.index('\tzed__init();') or { -1 }
	alpha_init := startup_source.index('\talpha__init();') or { -1 }
	assert zed_init >= 0, c_source
	assert alpha_init > zed_init, c_source
	cleanup_source := c_source.all_after('static void v_fastc_cleanup_modules(void) {')
	alpha_cleanup := cleanup_source.index('\talpha__cleanup();') or { -1 }
	zed_cleanup := cleanup_source.index('\tzed__cleanup();') or { -1 }
	assert alpha_cleanup >= 0, c_source
	assert zed_cleanup > alpha_cleanup, c_source

	c_file := os.join_path(root, 'program.c')
	bin_file := os.join_path(root, 'program')
	os.write_file(c_file, c_source) or { panic(err) }
	tcc := os.join_path(prefs.vroot, 'thirdparty', 'tcc', 'tcc.exe')
	compile_result := cmdexec.run(tcc, ['-std=gnu11', '-o', bin_file, c_file])
	assert compile_result.exit_code == 0, compile_result.output
	run_result := cmdexec.run(bin_file, [])
	assert run_result.exit_code == 0, run_result.output
	assert run_result.output.trim_space() == 'zed init\nalpha init\nmain\nalpha cleanup\nzed cleanup'
}

fn test_module_initializer_signatures_are_validated() {
	prefs := pref.new_preferences()
	for source in [
		'module main\nfn init(value int) {}\nfn main() {}\n',
		'module main\nfn init() int { return 1 }\nfn main() {}\n',
	] {
		mut message := ''
		_ := generate(source, 'invalid_module_init.v', prefs) or {
			message = err.msg()
			''
		}
		assert message.contains('module `init` with parameters or a return value'), message
	}
}

fn test_module_cleanup_signatures_are_validated() {
	prefs := pref.new_preferences()
	for source in [
		'module main\nfn cleanup(value int) {}\nfn main() {}\n',
		'module main\nfn cleanup() int { return 1 }\nfn main() {}\n',
	] {
		mut message := ''
		_ := generate(source, 'invalid_module_cleanup.v', prefs) or {
			message = err.msg()
			''
		}
		assert message.contains('module `cleanup` with parameters or a return value'), message
	}
}

fn test_negative_enum_discriminants_are_preserved() {
	prefs := pref.new_preferences()
	c_source := generate('module main

enum Foo {
	a = 1
	d = -10
	e
}

fn main() {}
',
		'negative_enum_discriminant.v', prefs) or { panic(err) }
	assert c_source.contains('#define Foo__d ((Foo)-10)'), c_source
	assert c_source.contains('#define Foo__e ((Foo)-9)'), c_source
}

fn test_duplicate_enum_fields_are_rejected() {
	prefs := pref.new_preferences()
	mut message := ''
	_ := generate('module main

enum Item {
	value
	value
}

fn main() {}
',
		'duplicate_enum_field.v', prefs) or {
		message = err.msg()
		''
	}
	assert message.contains('duplicate enum field `Item.value`'), message
}

fn test_flag_enum_custom_values_are_rejected() {
	prefs := pref.new_preferences()
	mut message := ''
	_ := generate('module main

@[flag]
enum Permissions {
	read = 4
}

fn main() {}
',
		'flag_enum_custom_value.v', prefs) or {
		message = err.msg()
		''
	}
	assert message.contains('custom value for flag enum field `Permissions.read`'), message
}

fn test_flag_mutating_methods_require_mutable_receivers() {
	mut prefs := pref.new_preferences()
	prefs.building_v = true
	mut ordinary_message := ''
	_ := generate('module main

enum Color {
	red
	green
}

fn main() {
	mut color := Color.red
	color.set(.green)
}
',
		'ordinary_enum_flag_method.v', prefs) or {
		ordinary_message = err.msg()
		''
	}
	assert ordinary_message.contains('unresolved method call'), ordinary_message

	for source, receiver_name in {
		'module main\n@[flag]\nenum Permissions { read write }\nfn main() { flags := Permissions.read; flags.set(.write) }\n':                                                   'flags'
		'module main\n@[flag]\nenum Permissions { read write }\nstruct Holder { permissions Permissions }\nfn main() { holder := Holder{}; holder.permissions.clear(.read) }\n': 'holder'
	} {
		mut message := ''
		_ := generate(source, 'immutable_flag_receiver.v', prefs) or {
			message = err.msg()
			''
		}
		assert message.contains('receiver `${receiver_name}` is immutable'), message
	}
	module_source := 'module settings

@[flag]
pub enum Permissions {
	read
	write
}

pub struct Config {
pub:
	permissions Permissions
}
'
	main_source := 'module main

import settings

fn main() {
	mut config := settings.Config{}
	config.permissions.set(.write)
}
'
	mut field_message := ''
	_ := generate_source_files([
		FastcSourceFile{
			path:   'immutable_flag_field.v'
			source: main_source
			header: fastc_scan_source_header(main_source, 'immutable_flag_field.v', prefs) or {
				panic(err)
			}
		},
		FastcSourceFile{
			path:   'settings.v'
			source: module_source
			header: fastc_scan_source_header(module_source, 'settings.v', prefs) or { panic(err) }
		},
	], prefs) or {
		field_message = err.msg()
		''
	}
	assert field_message.contains('receiver field `Config.permissions` is not `pub mut`'), field_message

	c_source := generate('module main

@[flag]
enum Permissions {
	read
	write
}

fn main() {
	mut flags := Permissions.read
	flags.set(.write)
	flags.clear(.read)
}
',
		'mutable_flag_receiver.v', prefs) or { panic(err) }
	assert c_source.contains('flags) |= (Permissions__write)'), c_source
	assert c_source.contains('flags) &= ~(Permissions__read)'), c_source
}

fn test_flag_methods_require_matching_enum_arguments() {
	mut prefs := pref.new_preferences()
	prefs.building_v = true
	for call in ['flags.set(1)', 'flags.has(Other.write)', 'flags.clear(Other.write)'] {
		mut message := ''
		_ := generate('module main

@[flag]
enum Permissions {
	read
	write
}

@[flag]
enum Other {
	read
	write
}

fn main() {
	mut flags := Permissions.read
	${call}
}
',
			'flag_enum_argument.v', prefs) or {
			message = err.msg()
			''
		}
		assert message.contains('does not match receiver type `Permissions`'), '${call}: ${message}'
	}
}

fn test_large_flag_enums_use_unsigned_64_bit_values() {
	prefs := pref.new_preferences()
	c_source := generate('module main

@[flag]
enum PawnsBoard as u64 {
	a8 b8 c8 d8 e8 f8 g8 h8
	a7 b7 c7 d7 e7 f7 g7 h7
	a6 b6 c6 d6 e6 f6 g6 h6
	a5 b5 c5 d5 e5 f5 g5 h5
	a4 b4 c4 d4 e4 f4 g4 h4
	a3 b3 c3 d3 e3 f3 g3 h3
	a2 b2 c2 d2 e2 f2 g2 h2
	a1 b1 c1 d1 e1 f1 g1 h1
}

fn main() {
	println(u64(PawnsBoard.h1))
}
',
		'flag_enum_64.v', prefs) or { panic(err) }
	assert c_source.contains('typedef u64 PawnsBoard;'), c_source
	assert c_source.contains('#define PawnsBoard__h1 ((PawnsBoard)(((u64)1) << (63)))'), c_source

	root := os.join_path(os.vtmp_dir(), 'v3_fastc_flag_enum_64_${os.getpid()}')
	os.rmdir_all(root) or {}
	os.mkdir_all(root) or { panic(err) }
	defer {
		os.rmdir_all(root) or {}
	}
	c_file := os.join_path(root, 'program.c')
	bin_file := os.join_path(root, 'program')
	os.write_file(c_file, c_source) or { panic(err) }
	tcc := os.join_path(prefs.vroot, 'thirdparty', 'tcc', 'tcc.exe')
	compile_result := cmdexec.run(tcc, ['-std=gnu11', '-o', bin_file, c_file])
	assert compile_result.exit_code == 0, compile_result.output
	run_result := cmdexec.run(bin_file, [])
	assert run_result.exit_code == 0, run_result.output
	assert run_result.output == '9223372036854775808\n'
}

fn test_mutable_receiver_methods_auto_address_mutable_values() {
	mut prefs := pref.new_preferences()
	prefs.building_v = true
	declarations := 'module main

struct Holder {
	value int
}

fn (mut holder Holder) reset() {}
'
	mut message := ''
	_ := generate(declarations + '
fn main() {
	holder := Holder{}
	holder.reset()
}
',
		'immutable_method_receiver.v', prefs) or {
		message = err.msg()
		''
	}
	assert message.contains('mutating method `reset` receiver `holder` is immutable'), message

	c_source := generate(declarations + '
fn main() {
	mut holder := Holder{}
	holder.reset()
}
',
		'mutable_method_receiver.v', prefs) or { panic(err) }
	assert c_source.contains('void Holder_reset(Holder* holder)'), c_source
	assert c_source.contains('Holder_reset(&(holder))'), c_source
}

fn test_symbolic_enum_discriminants_and_printing_are_preserved() {
	prefs := pref.new_preferences()
	c_source := generate('module main

const base = 10

enum Color {
	red = base
	green
	blue = base + 1 << 1
}

fn main() {
	println(int(Color.red))
	println(int(Color.blue))
	print(Color.green)
	println(Color.red)
}
',
		'symbolic_enum_discriminant.v', prefs) or { panic(err) }
	assert c_source.contains('#define Color__red ((Color)main__base)'), c_source
	assert c_source.contains('#define Color__green ((Color)(main__base + 1))'), c_source
	assert c_source.contains('v_fastc_print_enum_Color(Color__green, false);'), c_source
	assert c_source.contains('v_fastc_print_enum_Color(Color__red, true);'), c_source

	root := os.join_path(os.vtmp_dir(), 'v3_fastc_symbolic_enum_${os.getpid()}')
	os.rmdir_all(root) or {}
	os.mkdir_all(root) or { panic(err) }
	defer {
		os.rmdir_all(root) or {}
	}
	c_file := os.join_path(root, 'program.c')
	bin_file := os.join_path(root, 'program')
	os.write_file(c_file, c_source) or { panic(err) }
	tcc := os.join_path(prefs.vroot, 'thirdparty', 'tcc', 'tcc.exe')
	compile_result := cmdexec.run(tcc, ['-std=gnu11', '-o', bin_file, c_file])
	assert compile_result.exit_code == 0, compile_result.output
	run_result := cmdexec.run(bin_file, [])
	assert run_result.exit_code == 0, run_result.output
	assert run_result.output == '10\n12\ngreenred\n'

	ordinary_interpolation_source := generate(r"module main

enum Color {
	red
	green
}

fn main() {
	println('${Color.red}')
	println('${Color.green:d}')
}
",
		'ordinary_enum_interpolation.v', prefs) or { panic(err) }
	assert ordinary_interpolation_source.contains('static string v_fastc_enum_str_Color(Color value)'), ordinary_interpolation_source
	assert ordinary_interpolation_source.contains('v_fastc_enum_str_Color(Color__red)'), ordinary_interpolation_source
	assert ordinary_interpolation_source.contains('v_fastc_signed_str((long long)((int)(Color__green)))'), ordinary_interpolation_source
	os.write_file(c_file, ordinary_interpolation_source) or { panic(err) }
	interpolation_compile_result := cmdexec.run(tcc, ['-std=gnu11', '-o', bin_file, c_file])
	assert interpolation_compile_result.exit_code == 0, interpolation_compile_result.output
	interpolation_run_result := cmdexec.run(bin_file, [])
	assert interpolation_run_result.exit_code == 0, interpolation_run_result.output
	assert interpolation_run_result.output == 'red\n1\n'

	mut selfhost_prefs := pref.new_preferences()
	selfhost_prefs.building_v = true
	interpolation_source := generate(r"module main

enum Color {
	red
	green
}

@[flag]
enum Permissions {
	read
	write
}

fn color_label(color Color) string {
	return '${color}'
}

fn color_number(color Color) string {
	return '${color:d}'
}

fn permissions_label(permissions Permissions) string {
	return '${permissions}'
}

fn main() {
	println(color_label(Color.green))
	println(color_number(Color.green))
	println(permissions_label(Permissions.read))
}
",
		'enum_interpolation.v', selfhost_prefs) or { panic(err) }
	assert interpolation_source.contains('static string v_fastc_enum_str_Color(Color value)'), interpolation_source
	assert interpolation_source.contains('if (value == Color__green) return _S("green");'), interpolation_source
	assert interpolation_source.contains('return _S("unknown enum value");'), interpolation_source
	assert interpolation_source.contains('v_fastc_enum_str_Color(color)'), interpolation_source
	assert interpolation_source.contains('builtin__int_str((int)(color))'), interpolation_source
	assert interpolation_source.contains('static string v_fastc_enum_str_Permissions(Permissions value)'), interpolation_source
	assert interpolation_source.contains('_S("Permissions{")'), interpolation_source
	assert interpolation_source.contains('v_fastc_enum_str_Permissions(permissions)'), interpolation_source
}

fn test_enum_alias_member_access_uses_underlying_enum_symbols() {
	prefs := pref.new_preferences()
	c_source := generate('module main

enum MyEnum {
	something
	another
}

type MyEnumAlias = MyEnum

fn main() {
	x := MyEnum.something
	a := MyEnumAlias.something
	println(x == a)
	println(MyEnumAlias.another)
	println(MyEnumAlias(MyEnum.another))
	println(int(MyEnumAlias.another))
}
',
		'enum_alias_member.v', prefs) or { panic(err) }
	assert c_source.contains('typedef MyEnum MyEnumAlias;'), c_source
	assert c_source.contains('MyEnum__something'), c_source
	assert c_source.contains('MyEnum__another'), c_source
	assert !c_source.contains('MyEnumAlias__something'), c_source
	assert !c_source.contains('MyEnumAlias__another'), c_source
	assert c_source.count('v_fastc_print_enum_MyEnum') == 3, c_source

	root := os.join_path(os.vtmp_dir(), 'v3_fastc_enum_alias_${os.getpid()}')
	os.rmdir_all(root) or {}
	os.mkdir_all(root) or { panic(err) }
	defer {
		os.rmdir_all(root) or {}
	}
	c_file := os.join_path(root, 'program.c')
	bin_file := os.join_path(root, 'program')
	os.write_file(c_file, c_source) or { panic(err) }
	tcc := os.join_path(prefs.vroot, 'thirdparty', 'tcc', 'tcc.exe')
	compile_result := cmdexec.run(tcc, ['-std=gnu11', '-o', bin_file, c_file])
	assert compile_result.exit_code == 0, compile_result.output
	run_result := cmdexec.run(bin_file, [])
	assert run_result.exit_code == 0, run_result.output
	assert run_result.output == 'true\nanother\nanother\n1\n'
}

fn test_unresolved_enum_discriminants_are_rejected() {
	prefs := pref.new_preferences()
	mut message := ''
	_ := generate('module main

enum Color {
	red = missing
}

fn main() {}
',
		'unresolved_enum_discriminant.v', prefs) or {
		message = err.msg()
		''
	}
	assert message.contains('unresolved enum discriminant name `missing`'), message
}

fn test_select_statements_are_rejected() {
	prefs := pref.new_preferences()
	mut message := ''
	_ := generate('module main

fn main() {
	select {
		value := <-messages { println(value) }
		else { println(0) }
	}
}
',
		'select_statement.v', prefs) or {
		message = err.msg()
		''
	}
	assert message.contains('fastc parser does not support select statements'), message
}

fn test_unresolved_names_are_rejected_before_c_emission() {
	prefs := pref.new_preferences()
	for source in [
		"module main\nfn main() { puts('hello') }\n",
		'module main\nfn main() { printf("hello") }\n',
		'module main\nfn main() { value := stdout; println(value) }\n',
	] {
		mut message := ''
		_ := generate(source, 'unresolved_name.v', prefs) or {
			message = err.msg()
			''
		}
		assert message.contains('fastc parser does not support unresolved name'), message
	}
}

fn test_declared_names_are_available_without_an_ast() {
	prefs := pref.new_preferences()
	c_source := generate('module main

fn main() {
	println(later(2))
}

fn later(value int) int {
	return value + 1
}
',
		'declared_names.v', prefs) or { panic(err) }
	assert c_source.contains('println(later(2));')
}

fn test_narrow_integer_cast_expressions_are_rejected() {
	prefs := pref.new_preferences()
	mut message := ''
	_ := generate('module main

fn main() {
	println(u8(255) + u8(1))
}
',
		'narrow_cast_expression.v', prefs) or {
		message = err.msg()
		''
	}
	assert message.contains('narrow integer cast expressions'), message
}

fn test_undeclared_function_signature_types_are_rejected() {
	prefs := pref.new_preferences()
	for source, undeclared_type in {
		'module main\nfn show(x size_t) { println(1) }\nfn main() { show(1) }\n':        'size_t'
		'module main\nfn value() size_t { return 1 }\nfn main() { println(value()) }\n': 'size_t'
		'module main\nfn consume(x Foo) {}\nfn main() {}\n':                             'Foo'
		'module main\nfn value() ID { return unsafe { nil } }\nfn main() {}\n':          'ID'
	} {
		mut message := ''
		_ := generate(source, 'undeclared_signature_type.v', prefs) or {
			message = err.msg()
			''
		}
		assert message.contains('undeclared type `${undeclared_type}`'), message
	}
}

fn test_declared_function_call_argument_types_are_validated() {
	prefs := pref.new_preferences()
	mut message := ''
	_ := generate('module main

fn show(x bool) {
	println(x)
}

fn main() {
	show(2)
}
',
		'invalid_call_argument.v', prefs) or {
		message = err.msg()
		''
	}
	assert message.contains('argument 1 of type `integer literal`'), message
	assert message.contains('function `show` expecting `bool`'), message

	c_source := generate('module main

fn increment(x int) int {
	return x + 1
}

fn show(x bool) {
	println(x)
}

fn main() {
	value := 2
	flag := true
	println(increment(value))
	show(flag)
}
',
		'valid_call_arguments.v', prefs) or { panic(err) }
	assert c_source.contains('println(increment(value));')
	assert c_source.contains('show(flag);')
}

fn test_variadic_call_argument_types_are_validated() {
	mut prefs := pref.new_preferences()
	prefs.building_v = true
	mut message := ''
	_ := generate('module main

fn consume(values ...int) {}

fn main() {
	consume(true)
}
',
		'invalid_variadic_argument.v', prefs) or {
		message = err.msg()
		''
	}
	assert message.contains('argument 1 of type `bool`'), message
	assert message.contains('function `consume` expecting `int`'), message

	c_source := generate('module main

fn consume(values ...int) {}

fn main() {
	consume(1, 2)
}
',
		'valid_variadic_arguments.v', prefs) or { panic(err) }
	assert c_source.contains('sizeof(int), (int[]){1,2}'), c_source
}

fn test_scanner_diagnostics_are_rejected() {
	prefs := pref.new_preferences()
	source := "module main\nfn main() { println('" + r'\_' + "') }\n"
	mut message := ''
	_ := generate(source, 'scanner_diagnostic.v', prefs) or {
		message = err.msg()
		''
	}
	assert message.contains('fastc scanner error'), message
	assert message.contains('`_` unknown escape sequence'), message
}

fn test_conditions_must_be_boolean() {
	prefs := pref.new_preferences()
	for source in [
		'module main\nfn main() { if 2 { println(1) } }\n',
		'module main\nfn main() { value := 2; for value { break } }\n',
		'module main\nfn main() { for 2 { break } }\n',
		'module main\nfn main() { for i := 0; 2; i++ { break } }\n',
	] {
		mut message := ''
		_ := generate(source, 'non_boolean_condition.v', prefs) or {
			message = err.msg()
			''
		}
		assert message.contains('condition of type'), message
		assert message.contains('instead of `bool`'), message
	}

	c_source := generate('module main

fn ready() bool {
	return true
}

fn main() {
	flag := true
	if flag {
		println(1)
	}
	for ready() {
		break
	}
	for i := 0; ready(); i++ {
		break
	}
}
',
		'boolean_conditions.v', prefs) or { panic(err) }
	assert c_source.contains('if (flag) {')
	assert c_source.contains('while (ready()) {')
	assert c_source.contains('; ready(); i++) {')
}

fn test_comparison_and_logical_operands_are_validated() {
	prefs := pref.new_preferences()
	for source, expected in {
		'module main\nfn main() { println(1 == true) }\n':          'comparison `==` operands of incompatible types'
		'module main\nfn main() { println(true < false) }\n':       'comparison `<` operands of incompatible types'
		'module main\nfn main() { if true && 1 { println(1) } }\n': 'logical `&&` operands of types'
		'module main\nfn main() { if !1 { println(1) } }\n':        'logical `!` operand of type'
	} {
		mut message := ''
		_ := generate(source, 'invalid_boolean_operands.v', prefs) or {
			message = err.msg()
			''
		}
		assert message.contains(expected), message
	}

	c_source := generate("module main

type Label = string

fn same(left string, right string) bool {
	return left == right
}

fn same_label(first Label, second Label) bool {
	return first == second
}

fn main() {
	left := 1
	right := 2
	ok := left < right && true
	println(ok)
	println(left < right)
	println(same('same', 'same'))
	println('alpha' < 'beta')
	println('beta' > 'alpha')
	println('alpha' <= 'alpha')
	println('beta' >= 'beta')
	println('alpha' != 'beta')
	println(same_label(Label('same'), Label('same')))
	println(Label('alpha') < Label('beta'))
}
",
		'valid_boolean_operands.v', prefs) or { panic(err) }
	assert c_source.contains('left<right'), c_source
	assert c_source.contains('&&'), c_source
	assert c_source.contains('v_fastc_println_bool'), c_source
	assert c_source.contains('static bool builtin__string_eq'), c_source
	assert c_source.contains('builtin__string_eq(left,right)'), c_source
	assert c_source.contains('builtin__string_eq(first,second)'), c_source
	assert c_source.contains('builtin__string_lt("alpha","beta")'), c_source

	root := os.join_path(os.vtmp_dir(), 'v3_fastc_boolean_operands_${os.getpid()}')
	os.rmdir_all(root) or {}
	os.mkdir_all(root) or { panic(err) }
	defer {
		os.rmdir_all(root) or {}
	}
	c_file := os.join_path(root, 'program.c')
	bin_file := os.join_path(root, 'program')
	os.write_file(c_file, c_source) or { panic(err) }
	tcc := os.join_path(prefs.vroot, 'thirdparty', 'tcc', 'tcc.exe')
	compile_result := cmdexec.run(tcc, ['-std=gnu11', '-o', bin_file, c_file])
	assert compile_result.exit_code == 0, compile_result.output
	run_result := cmdexec.run(bin_file, [])
	assert run_result.exit_code == 0, run_result.output
	assert run_result.output.trim_space() == 'true\ntrue\ntrue\ntrue\ntrue\ntrue\ntrue\ntrue\ntrue\ntrue'
}

fn test_mixed_integer_comparisons_preserve_signed_semantics() {
	prefs := pref.new_preferences()
	c_source := generate('module main

fn main() {
	println(i64(-1) < u64(1))
	println(u64(1) > i64(-1))
	println(i64(-27) < u32(65463356))
	println(u32(8543) > int(-7523))
	println(i64(-89) <= u64(567))
	println(int(-1) != u32(0) - u32(1))
	println(i64(-1) < u64(1) && u64(2) >= i64(2))
	println(!((u64(1) < i64(-1))))
}
',
		'mixed_integer_comparisons.v', prefs) or { panic(err) }
	assert c_source.contains('v_fastc_us_gt('), c_source
	assert c_source.contains('v_fastc_us_ne('), c_source

	root := os.join_path(os.vtmp_dir(), 'v3_fastc_mixed_integer_comparisons_${os.getpid()}')
	os.rmdir_all(root) or {}
	os.mkdir_all(root) or { panic(err) }
	defer {
		os.rmdir_all(root) or {}
	}
	c_file := os.join_path(root, 'program.c')
	bin_file := os.join_path(root, 'program')
	os.write_file(c_file, c_source) or { panic(err) }
	tcc := os.join_path(prefs.vroot, 'thirdparty', 'tcc', 'tcc.exe')
	compile_result := cmdexec.run(tcc, ['-std=gnu11', '-o', bin_file, c_file])
	assert compile_result.exit_code == 0, compile_result.output
	run_result := cmdexec.run(bin_file, [])
	assert run_result.exit_code == 0, run_result.output
	assert run_result.output.trim_space() == 'true\ntrue\ntrue\ntrue\ntrue\ntrue\ntrue\ntrue'
}

fn test_match_branch_values_must_match_the_subject_type() {
	prefs := pref.new_preferences()
	for source in [
		'module main\nfn main() { x := 1; match x { true { println(1) } else {} } }\n',
		'module main\nfn main() { x := true; match x { 1 { println(1) } else {} } }\n',
	] {
		mut message := ''
		_ := generate(source, 'invalid_match_branch_type.v', prefs) or {
			message = err.msg()
			''
		}
		assert message.contains('match branch value of type'), message
		assert message.contains('subject of type'), message
	}

	c_source := generate('module main

fn main() {
	x := 1
	match x {
		0, 1 { println(1) }
		else {}
	}
}
',
		'valid_match_branch_types.v', prefs) or { panic(err) }
	assert c_source.contains('if (((__v_fastc_match_'), c_source
	assert c_source.contains('== (0)) || '), c_source
	assert c_source.contains('== (1))'), c_source
}

fn test_primitive_cast_operands_and_unsafe_context_are_validated() {
	prefs := pref.new_preferences()
	for source in [
		'module main\nfn main() { println(bool(2)) }\n',
		"module main\nfn main() { println(int('2')) }\n",
		'module main\nfn main() { println(string(2)) }\n',
	] {
		mut message := ''
		_ := generate(source, 'invalid_primitive_cast.v', prefs) or {
			message = err.msg()
			''
		}
		assert message.contains('cast'), message
	}

	c_source := generate('module main

fn main() {
	println(bool(true))
	println(int(true))
	unsafe {
		println(bool(2))
	}
	println(unsafe { bool(0) })
}
',
		'valid_primitive_casts.v', prefs) or { panic(err) }
	assert c_source.contains('println(((bool)(2)));'), c_source
	assert c_source.contains('println(((bool)(0)));'), c_source
}

fn test_declared_cast_operands_are_validated() {
	prefs := pref.new_preferences()
	for source, expected in {
		'module main\ntype MyType = string\nfn main() { println(MyType(5)) }\n':       'alias to `string`'
		"module main\nenum Color { red blue }\nfn main() { println(Color('red')) }\n": 'to enum `Color`'
		'module main\nenum Color { red blue }\nfn main() { println(Color(1)) }\n':     'outside an `unsafe` block'
	} {
		mut message := ''
		_ := generate(source, 'invalid_declared_cast.v', prefs) or {
			message = err.msg()
			''
		}
		assert message.contains(expected), message
	}

	c_source := generate("module main

type Label = string
type Count = int

enum Color {
	red
	blue
}

fn main() {
	label := Label('ok')
	count := Count(2)
	color := unsafe { Color(1) }
	println(label == Label('ok'))
	println(int(count))
	println(color)
}
",
		'valid_declared_casts.v', prefs) or { panic(err) }
	assert c_source.contains('((Label)("ok"))'), c_source
	assert c_source.contains('((Count)(2))'), c_source
	assert c_source.contains('((Color)(1))'), c_source
}

fn test_defer_is_emitted_when_its_lexical_scope_exits() {
	prefs := pref.new_preferences()
	c_source := generate('module main

fn main() {
	if true {
		defer { println(1) }
		println(2)
	}
	println(3)
}
',
		'scoped_defer.v', prefs) or { panic(err) }
	print_two := c_source.index('println(2);') or { panic(c_source) }
	deferred_one := c_source.index_after('println(1);', print_two) or { panic(c_source) }
	print_three := c_source.index_after('println(3);', deferred_one) or { panic(c_source) }
	assert print_two < deferred_one
	assert deferred_one < print_three
}

fn test_return_expression_is_evaluated_before_deferred_blocks() {
	prefs := pref.new_preferences()
	c_source := generate('module main

fn value() int {
	mut x := 1
	defer { x = 2 }
	return x
}

fn main() {
	println(value())
}
',
		'return_before_defer.v', prefs) or { panic(err) }
	evaluation := c_source.index('__typeof__((x)) __v_fastc_return_') or { panic(c_source) }
	deferred_assignment := c_source.index_after('x=2;', evaluation) or { panic(c_source) }
	returned_temporary := c_source.index_after('return __v_fastc_return_', deferred_assignment) or {
		panic(c_source)
	}
	assert evaluation < deferred_assignment
	assert deferred_assignment < returned_temporary
}

fn test_control_flow_is_rejected_inside_deferred_blocks() {
	prefs := pref.new_preferences()
	for source, expected in {
		'module main\nfn value() int { defer { return 2 } return 1 }\n':     '`return` not allowed inside a `defer` block'
		'module main\nfn main() { for { defer { break } break } }\n':        '`break` is not allowed in defer statements'
		'module main\nfn main() { for { defer { continue } break } }\n':     '`continue` is not allowed in defer statements'
		'module main\nfn main() { defer { goto done } done: println(1) }\n': 'goto is not allowed in defer statements'
		'module main\nfn main() { defer { defer { println(1) } } }\n':       '`defer` blocks cannot be nested'
	} {
		mut message := ''
		_ := generate(source, 'invalid_defer_control_flow.v', prefs) or {
			message = err.msg()
			''
		}
		assert message.contains(expected), message
	}
}

fn test_mutable_function_parameters_require_mutable_arguments() {
	prefs := pref.new_preferences()
	mut pointer_message := ''
	_ := generate('module main

fn change(mut x int) {
	x = 2
}

fn main() {
	x := 1
	change(&x)
}
',
		'immutable_pointer_argument.v', prefs) or {
		pointer_message = err.msg()
		''
	}
	assert pointer_message.contains('requires a mutable argument written with `mut`'), pointer_message

	mut immutable_message := ''
	_ := generate('module main

fn change(mut x int) {
	x = 2
}

fn main() {
	x := 1
	change(mut x)
}
',
		'immutable_mut_argument.v', prefs) or {
		immutable_message = err.msg()
		''
	}
	assert immutable_message.contains('mutable argument `x` to function `change` is immutable'), immutable_message

	c_source := generate('module main

fn change(mut x int) {
	x = 2
}

fn main() {
	mut x := 1
	change(mut x)
	println(x)
}
',
		'mutable_argument.v', prefs) or { panic(err) }
	assert c_source.contains('void change(int* x)'), c_source
	assert c_source.contains('change(&x);'), c_source
}

fn test_mutable_arguments_require_mutable_imported_fields() {
	root := os.join_path(os.vtmp_dir(), 'v3_fastc_mut_argument_field_${os.getpid()}')
	os.rmdir_all(root) or {}
	os.mkdir_all(os.join_path(root, 'settings')) or { panic(err) }
	defer {
		os.rmdir_all(root) or {}
	}
	main_file := os.join_path(root, 'main.v')
	module_file := os.join_path(root, 'settings', 'settings.v')
	module_source := 'module settings

pub struct Config {
pub:
	read_only int
pub mut:
	writable int
}
'
	mut prefs := pref.new_preferences()
	prefs.module_search_paths = [root]
	os.write_file(module_file, module_source) or { panic(err) }
	os.write_file(main_file, 'module main

import settings

fn change(mut value int) {
	value = 2
}

fn mutate(mut config settings.Config) {
	change(mut config.read_only)
}

fn main() {}
') or {
		panic(err)
	}
	mut message := ''
	_ := generate_files([main_file], prefs) or {
		message = err.msg()
		''
	}
	assert message.contains('mutable argument field `Config.read_only`'), message
	assert message.contains('is not `pub mut` in imported module `settings`'), message

	os.write_file(main_file, 'module main

import settings

fn change(mut value int) {
	value = 2
}

fn mutate(mut config settings.Config) {
	change(mut config.writable)
}

fn main() {}
') or {
		panic(err)
	}
	c_source := generate_files([main_file], prefs) or { panic(err) }
	assert c_source.contains('change(&config.writable);'), c_source
}

fn test_match_expression_requires_else() {
	prefs := pref.new_preferences()
	mut message := ''
	_ := generate('module main

fn main() {
	x := 2
	y := match x { 1 { 7 } }
	println(y)
}
',
		'non_exhaustive_match_expression.v', prefs) or {
		message = err.msg()
		''
	}
	assert message.contains('non-exhaustive match expression without `else`'), message

	c_source := generate('module main

fn main() {
	x := 2
	y := match x { 1 { 7 } else { 9 } }
	println(y)
}
',
		'exhaustive_match_expression.v', prefs) or { panic(err) }
	assert c_source.contains('? (7) : (9)')
}

fn test_match_statement_without_else_does_not_terminate_function() {
	prefs := pref.new_preferences()
	mut message := ''
	_ := generate('module main

fn value(x int) int {
	match x {
		1 { return 7 }
	}
}

fn main() {
	println(value(1))
}
',
		'non_exhaustive_match_statement.v', prefs) or {
		message = err.msg()
		''
	}
	assert message.contains('non-void function `value` that can fall through'), message

	c_source := generate('module main

fn value(x int) int {
	match x {
		1 { return 7 }
		else { return 9 }
	}
}

fn main() {
	println(value(1))
}
',
		'exhaustive_match_statement.v', prefs) or { panic(err) }
	assert c_source.contains('else {\n\t\treturn 9;'), c_source
}

fn test_c_reserved_identifiers_are_escaped_consistently() {
	prefs := pref.new_preferences()
	c_source := generate('module main

struct Holder {
	auto int
}

fn calculate(holder Holder, register int) int {
	restrict := register
	return holder.auto + restrict
}

fn auto() int {
	return 42
}

fn main() {
	result := auto()
	auto := result
	println(auto)
}
',
		'reserved_identifiers.v', prefs) or { panic(err) }
	assert c_source.contains('int v_auto;'), c_source
	assert c_source.contains('int calculate(Holder holder, int v_register)'), c_source
	assert c_source.contains('__typeof__((v_register)) v_restrict = (v_register);'), c_source
	assert c_source.contains('return holder.v_auto+v_restrict;'), c_source
	assert c_source.contains('int v_auto(void)'), c_source
	assert c_source.contains(' v_auto = (result);'), c_source
}

fn test_return_expression_type_is_validated() {
	prefs := pref.new_preferences()
	for source in [
		'module main\nfn value() bool { return 2 }\nfn main() { println(value()) }\n',
		'module main\nfn value() int { return true }\nfn main() { println(value()) }\n',
	] {
		mut message := ''
		_ := generate(source, 'invalid_return_type.v', prefs) or {
			message = err.msg()
			''
		}
		assert message.contains('return expression of type'), message
		assert message.contains('function returning'), message
	}

	c_source := generate('module main

fn enabled() bool {
	return true
}

fn value() int {
	return 2
}

fn main() {
	println(enabled())
	println(value())
}
',
		'valid_return_types.v', prefs) or { panic(err) }
	assert c_source.contains('return ((bool)true);')
	assert c_source.contains('return 2;')
}

fn test_assignment_value_type_is_validated() {
	prefs := pref.new_preferences()
	for source in [
		'module main\nfn main() { mut enabled := false; enabled = 2; println(enabled) }\n',
		'module main\nfn main() { mut count := 1; count = true; println(count) }\n',
		'module main\nfn main() { mut enabled := false; enabled += 1; println(enabled) }\n',
	] {
		mut message := ''
		_ := generate(source, 'invalid_assignment_type.v', prefs) or {
			message = err.msg()
			''
		}
		assert message.contains('assignment of type'), message
		assert message.contains('of type'), message
	}

	c_source := generate('module main

fn ready() bool {
	return true
}

fn main() {
	mut enabled := false
	enabled = ready()
	mut count := 1
	count = 2
	count += 3
	println(enabled)
	println(count)
}
',
		'valid_assignment_types.v', prefs) or { panic(err) }
	assert c_source.contains('enabled=ready();')
	assert c_source.contains('count=2;')
	assert c_source.contains('count+=3;')
}

fn test_aggregate_lvalue_mutability_is_validated() {
	mut prefs := pref.new_preferences()
	prefs.building_v = true
	mut message := ''
	_ := generate('module main

struct Holder {
mut:
	value int
}

fn main() {
	holder := Holder{}
	holder.value = 2
}
',
		'immutable_aggregate_root.v', prefs) or {
		message = err.msg()
		''
	}
	assert message.contains('mutation of immutable or unknown name `holder`'), message

	c_source := generate('module main

struct Holder {
mut:
	value int
}

fn main() {
	mut holder := Holder{}
	holder.value = 2
}
',
		'mutable_aggregate_root.v', prefs) or { panic(err) }
	assert c_source.contains('holder.value=2;'), c_source
}

fn test_c_style_loop_initializer_type_is_validated() {
	prefs := pref.new_preferences()
	mut message := ''
	_ := generate('module main

fn main() {
	mut enabled := false
	for enabled = 2; enabled; enabled = false {
		println(1)
	}
}
',
		'invalid_loop_initializer.v', prefs) or {
		message = err.msg()
		''
	}
	assert message.contains('assignment of type `integer literal` to `enabled` of type `bool`'), message

	c_source := generate('module main

fn main() {
	mut enabled := false
	for enabled = true; enabled; enabled = false {}
}
',
		'valid_loop_initializer.v', prefs) or { panic(err) }
	assert c_source.contains('for (enabled = (((bool)true)); enabled; enabled=((bool)false)) {'), c_source

	mut selfhost_prefs := pref.new_preferences()
	selfhost_prefs.building_v = true
	empty_initializer_source := generate('module main

fn main() {
	mut i := 0
	for ; i < 2; i++ {}
}
',
		'empty_loop_initializer.v', selfhost_prefs) or { panic(err) }
	assert empty_initializer_source.contains('for (; i<2; i++) {'), empty_initializer_source
}

fn test_negative_integer_literals_are_rejected_for_unsigned_targets() {
	prefs := pref.new_preferences()
	for source in [
		'module main\nfn take(x u32) { println(x) }\nfn main() { take(-1) }\n',
		'module main\nfn main() { mut value := u32(0); value = -1; println(value) }\n',
		'module main\nfn value() u32 { return -1 }\nfn main() { println(value()) }\n',
	] {
		mut message := ''
		_ := generate(source, 'negative_unsigned_literal.v', prefs) or {
			message = err.msg()
			''
		}
		assert message.contains('negative integer literal'), message
		assert message.contains('u32'), message
	}

	c_source := generate('module main

fn take(x u32) {
	println(x)
}

fn value() u32 {
	return 1
}

fn take_signed(x int) {
	println(x)
}

fn signed_value() int {
	return -1
}

fn main() {
	mut number := u32(0)
	number = 1
	take(1)
	println(value())
	mut signed := 0
	signed = -1
	take_signed(-1)
	println(signed_value())
}
',
		'positive_unsigned_literals.v', prefs) or { panic(err) }
	assert c_source.contains('number=1;')
	assert c_source.contains('take(1);')
	assert c_source.contains('return 1;')
	assert c_source.contains('signed=-1;')
	assert c_source.contains('take_signed(-1);')
	assert c_source.contains('return -1;')
}

fn test_main_must_not_return_a_value() {
	prefs := pref.new_preferences()
	mut message := ''
	_ := generate('module main\nfn main() int { return 7 }\n', 'value_returning_main.v', prefs) or {
		message = err.msg()
		''
	}
	assert message.contains('main function returning `int`'), message
}

fn test_main_must_not_have_parameters() {
	prefs := pref.new_preferences()
	for source in [
		'module main\nfn main(code int) {}\n',
		'module main\nfn main(code int) int { return code }\n',
	] {
		mut message := ''
		_ := generate(source, 'parameterized_main.v', prefs) or {
			message = err.msg()
			''
		}
		assert message.contains('main function with parameters'), message
	}
}

fn test_mutable_iteration_requires_a_mutable_collection() {
	mut prefs := pref.new_preferences()
	prefs.building_v = true
	mut message := ''
	_ := generate('module main

fn change(mut value int) {
	value = 3
}

fn main() {
	items := [1, 2]
	for mut item in items {
		change(mut item)
	}
}
',
		'immutable_mutable_iteration.v', prefs) or {
		message = err.msg()
		''
	}
	assert message.contains('mutable iteration over immutable collection `items`'), message

	c_source := generate('module main

fn change(mut value int) {
	value = 3
}

fn main() {
	mut items := [1, 2]
	for mut item in items {
		println(item)
		item = 3
		change(mut item)
	}
}
',
		'mutable_iteration.v', prefs) or { panic(err) }
	assert c_source.contains('int *item = &(((int *)'), c_source
	assert c_source.contains('println((*(item)));'), c_source
	assert c_source.contains('(*item)=3;'), c_source
	assert c_source.contains('change(item);'), c_source
}

fn test_map_pointer_iteration_passes_the_map_pointer_directly() {
	mut prefs := pref.new_preferences()
	prefs.building_v = true
	c_source := generate('module main

fn iterate(source map[string]int) {
	for key, value in source {
		println(key)
		println(value)
	}
	pointer := &source
	for key, value in pointer {
		println(key)
		println(value)
	}
}

fn main() {
	iterate(map[string]int{})
}
',
		'map_pointer_iteration.v', prefs) or { panic(err) }
	assert c_source.count('builtin__map_keys((map *)&__v_fastc_map_collection_') == 1, c_source
	assert c_source.count('builtin__map_values((map *)&__v_fastc_map_collection_') == 1, c_source
	assert c_source.count('builtin__map_keys((map *)__v_fastc_map_collection_') == 1, c_source
	assert c_source.count('builtin__map_values((map *)__v_fastc_map_collection_') == 1, c_source
}

fn test_selfhost_array_and_string_indexing_uses_bounds_checked_helpers() {
	mut prefs := pref.new_preferences()
	prefs.building_v = true
	c_source := generate('module main

fn checked(mut values []int, text string, nested [][]int, index int) int {
	values[index] = values[index]
	println(text[index])
	return nested[index][index]
}

fn main() {
	mut values := []int{}
	checked(mut values, "x", [][]int{}, 0)
}
',
		'checked_indexing.v', prefs) or { panic(err) }
	assert c_source.count('builtin__array_get(*(values), index)') == 2, c_source
	assert c_source.contains('builtin__string_at(text, index)'), c_source
	assert c_source.contains('builtin__array_get(nested, index)'), c_source
	assert c_source.contains('builtin__array_get((*(Array_int *)builtin__array_get(nested, index)), index)'), c_source

	assert !c_source.contains('values.data)[index]'), c_source
	assert !c_source.contains('text.str[index]'), c_source
}

fn test_selfhost_array_slices_use_the_runtime_helper() {
	mut prefs := pref.new_preferences()
	prefs.building_v = true
	c_source := generate('module main

fn middle(values []int, start int, end int) []int {
	return values[start..end]
}

fn main() {
	values := []int{}
	result := middle(values, 0, 0)
	println(result.len)
}
',
		'array_slice.v', prefs) or { panic(err) }
	assert c_source.contains('return builtin__array_slice(values, start, end);'), c_source
	assert !c_source.contains('__v_slice.flags |= ArrayFlags__is_slice'), c_source
}

fn test_range_bounds_must_be_integers() {
	prefs := pref.new_preferences()
	for source in [
		'module main\nfn main() { for i in 0.0 .. 2.0 { println(i) } }\n',
		'module main\nfn main() { for i in 0 .. 2.0 { println(i) } }\n',
		'module main\nfn main() { for i in false .. true { println(i) } }\n',
	] {
		mut message := ''
		_ := generate(source, 'invalid_range_bounds.v', prefs) or {
			message = err.msg()
			''
		}
		assert message.contains('range bounds of types'), message
		assert message.contains('must both be integers'), message
	}
}

fn test_range_bound_integer_types_must_be_compatible() {
	prefs := pref.new_preferences()
	for source in [
		'module main\nfn main() { for i in u64(0) .. -1 { println(i) } }\n',
		'module main\nfn main() { for i in i64(0) .. u64(3) { println(i) } }\n',
	] {
		mut message := ''
		_ := generate(source, 'incompatible_range_bounds.v', prefs) or {
			message = err.msg()
			''
		}
		assert message.contains('range bounds of types'), message
		assert message.contains('must have compatible integer types'), message
	}

	generate('module main\nfn main() { for i in u64(0) .. 3 { println(i) } }\n',
		'compatible_range_bound_literal.v', prefs) or { panic(err) }
}

fn test_literal_range_must_not_be_empty() {
	prefs := pref.new_preferences()
	for source in [
		'module main\nfn main() { for i in 4 .. 2 { println(i) } }\n',
		'module main\nfn main() { for i in 2 .. 2 { println(i) } }\n',
		'module main\nfn main() { for i in 4 .. -2 { println(i) } }\n',
	] {
		mut message := ''
		_ := generate(source, 'empty_literal_range.v', prefs) or {
			message = err.msg()
			''
		}
		assert message.contains('empty range:'), message
		assert message.contains('will never execute'), message
	}

	c_source := generate('module main\nfn main() { for i in 2 .. 4 { println(i) } }\n',
		'valid_literal_range.v', prefs) or { panic(err) }
	assert c_source.contains('for (__typeof__((__v_fastc_range_start_0)) i = (__v_fastc_range_start_0); i < (__v_fastc_range_end_1); i++) {'), c_source
}

fn test_arithmetic_operands_must_be_numeric() {
	prefs := pref.new_preferences()
	for source in [
		'module main\nfn main() { println(true + false) }\n',
		'module main\nfn main() { value := true * false; println(value) }\n',
		'module main\nfn main() { mut value := true; value += false; println(value) }\n',
		"module main\nfn main() { mut value := 'abc'; value++; println(value) }\n",
		"module main\nfn main() { mut value := 'abc'; value--; println(value) }\n",
		'module main\nfn main() { mut value := 1; mut pointer := &value; pointer++ }\n',
	] {
		mut message := ''
		_ := generate(source, 'non_numeric_arithmetic.v', prefs) or {
			message = err.msg()
			''
		}
		assert message.contains('arithmetic'), message
		assert message.contains('non-numeric') || message.contains('operands of types'), message
	}

	c_source := generate('module main

fn main() {
	mut value := 1
	mut pointer := &value
	value++
	unsafe {
		pointer--
	}
}
',
		'numeric_and_pointer_mutations.v', prefs) or { panic(err) }
	assert c_source.contains('value++;'), c_source
	assert c_source.contains('pointer--;'), c_source
}

fn test_nil_requires_an_unsafe_block() {
	prefs := pref.new_preferences()
	mut message := ''
	_ := generate('module main\nfn show(p &int) { println(*p) }\nfn main() { show(nil) }\n',
		'nil_outside_unsafe.v', prefs) or {
		message = err.msg()
		''
	}
	assert message.contains('`nil` outside an `unsafe` block'), message

	c_source := generate('module main\nfn accept(p &int) {}\nfn main() { unsafe { accept(nil) } }\n',
		'nil_inside_unsafe.v', prefs) or { panic(err) }
	assert c_source.contains('accept(NULL);')
}

fn test_bitwise_negation_requires_an_integer() {
	prefs := pref.new_preferences()
	mut message := ''
	_ := generate('module main\nfn main() { println(~true) }\n', 'bool_bit_not.v', prefs) or {
		message = err.msg()
		''
	}
	assert message.contains('bitwise negation of non-integer type `bool`'), message

	c_source := generate('module main\nfn main() { println(~1) }\n', 'integer_bit_not.v', prefs) or {
		panic(err)
	}
	assert c_source.contains('println(~1);')
}

fn test_value_only_expression_statements_are_rejected() {
	prefs := pref.new_preferences()
	for source in [
		'module main\nfn main() { 1 }\n',
		'module main\nfn main() { true }\n',
		'module main\nfn main() { value := 1; value }\n',
		'module main\nfn main() { int(1) }\n',
	] {
		mut message := ''
		_ := generate(source, 'value_expression_statement.v', prefs) or {
			message = err.msg()
			''
		}
		assert message.contains('value-only expression statement'), message
	}

	c_source := generate('module main\nfn touch() {}\nfn main() { mut count := 0; touch(); count++ }\n',
		'valid_expression_statements.v', prefs) or { panic(err) }
	assert c_source.contains('touch();')
	assert c_source.contains('count++;')
}

fn test_nested_mutations_are_rejected() {
	prefs := pref.new_preferences()
	for source in [
		'module main\nfn main() { x := 1; println(x++) }\n',
		'module main\nfn main() { mut x := 1; println(x = 2) }\n',
		'module main\nfn main() { mut x := 1; y := x++; println(y) }\n',
	] {
		mut message := ''
		_ := generate(source, 'nested_mutation.v', prefs) or {
			message = err.msg()
			''
		}
		assert message.contains('mutation'), message
		assert message.contains('inside an expression'), message
	}

	c_source := generate('module main\nfn main() { mut x := 1; x++; x += 2; println(x) }\n',
		'mutation_statements.v', prefs) or { panic(err) }
	assert c_source.contains('x++;')
	assert c_source.contains('x+=2;')
}

fn test_bare_return_from_main_emits_zero() {
	prefs := pref.new_preferences()
	c_source := generate('module main

fn stop() {
	return
}

fn main() {
	if true {
		return
	}
}
',
		'bare_return.v', prefs) or { panic(err) }
	assert c_source.contains('void stop(void) {\n\treturn;\n}')
	assert c_source.contains('if (((bool)true)) {\n\t\treturn 0;\n\t}')
}

fn test_non_void_functions_must_return_on_every_path() {
	prefs := pref.new_preferences()
	for source in [
		'module main\nfn value() int {}\nfn main() { println(value()) }\n',
		'module main\nfn value() int { if true { return 1 } }\nfn main() { println(value()) }\n',
		'module main\nfn value() int { return }\nfn main() { println(value()) }\n',
		'module main\nfn value() int { for { break } }\nfn main() {}\n',
		'module main\nfn value(flag bool) int { for { if flag { break } } }\nfn main() {}\n',
	] {
		mut message := ''
		_ := generate(source, 'non_void_fallthrough.v', prefs) or {
			message = err.msg()
			''
		}
		assert message.contains('fastc parser does not support'), message
	}
	c_source := generate('module main

fn value(flag bool) int {
	if flag {
		return 1
	} else {
		return 2
	}
}

fn main() {
	println(value(true))
}
',
		'non_void_returns.v', prefs) or { panic(err) }
	assert c_source.contains('return 1;')
	assert c_source.contains('return 2;')
	infinite_source := generate('module main

fn wait_forever() int {
	for {}
}

fn nested_wait() int {
	for {
		for {}
		break
	}
}

fn main() {}
',
		'infinite_loop_returns.v', prefs) or { panic(err) }
	assert infinite_source.count('for (;;) {') == 3, infinite_source
}

fn test_integer_range_caches_bounds() {
	prefs := pref.new_preferences()
	c_source := generate('module main

fn start() int {
	return 0
}

fn limit() int {
	return 3
}

fn main() {
	for i in start() .. limit() {
		println(i)
	}
}
',
		'range_bounds.v', prefs) or { panic(err) }
	assert c_source.contains('__v_fastc_range_start_0 = (start());')
	assert c_source.contains('__v_fastc_range_end_1 = (limit());')
	assert c_source.contains('i < (__v_fastc_range_end_1)')
	assert !c_source.contains('i < (limit())')
}

fn test_decimal_literals_preserve_v_values() {
	prefs := pref.new_preferences()
	c_source := generate('module main

fn main() {
	println(0_123)
}
', 'literal_values.v', prefs) or {
		panic(err)
	}
	assert c_source.contains('println(123);')
}

fn test_v_octal_literals_are_translated_to_gnu_c() {
	assert fastc_c_number('0o17')! == '017'
	assert fastc_c_number('0O7_1')! == '071'
	mut oversized_message := ''
	_ := fastc_c_number('0o20000000000') or {
		oversized_message = err.msg()
		''
	}
	assert oversized_message.contains('high-bit nondecimal literals')
	prefs := pref.new_preferences()
	c_source := generate('module main

fn main() {
	println(0o17)
}
', 'octal_literal.v', prefs) or {
		panic(err)
	}
	assert c_source.contains('println(017);')
}

fn test_hex_string_escape_has_fixed_width_in_c() {
	prefs := pref.new_preferences()
	c_source := generate("module main\nfn main() { println('\\x61ardvark') }\n", 'hex_escape.v',
		prefs) or { panic(err) }
	assert c_source.contains(r'println("\141ardvark");')
}

fn test_partial_octal_string_escapes_are_reencoded() {
	assert fastc_c_string(r"'\1'")! == r'"\\1"'
	assert fastc_c_string(r"'\12'")! == r'"\\12"'
	assert fastc_c_string(r"'\123'")! == r'"\123"'
}

fn test_string_line_continuations_match_v_unescaping() {
	prefs := pref.new_preferences()
	source := r"module main

fn main() {
	println('left\
	   right')
}
"
	c_source := generate(source, 'continued_string.v', prefs) or { panic(err) }
	assert c_source.contains(r'println("leftright");')
	crlf_literal := "'left\\" + '\r\n' + "\t  right'"
	assert fastc_c_string(crlf_literal)! == '"leftright"'
	assert fastc_c_string(r"'left\nright'")! == r'"left\nright"'
}

fn test_runtime_sensitive_constructs_are_rejected() {
	prefs := pref.new_preferences()
	for source in ['module main

fn main() {
	println("a\\0b")
}
',
		"module main\nfn main() { println('\\400tail') }\n"] {
		mut nul_failed := false
		_ := generate(source, 'nul_string.v', prefs) or {
			nul_failed = true
			''
		}
		assert nul_failed
	}
	assert fastc_string_contains_nul(r'\400tail', false)
	assert !fastc_string_contains_nul(r'\401tail', false)
	non_nul_octal_c := generate("module main\nfn main() { println('\\401tail') }\n",
		'non_nul_octal_string.v', prefs) or { panic(err) }
	assert non_nul_octal_c.contains(r'println("\401tail");')

	mut assert_failed := false
	_ := generate('module main

fn main() {
	assert false
}
', 'assert.v', prefs) or {
		assert_failed = true
		''
	}
	assert assert_failed
}

fn test_type_sensitive_expressions_are_rejected() {
	prefs := pref.new_preferences()
	for source in [
		'module main\nfn show(a u8, b u8) { println(a + b) }\nfn main() { show(255, 1) }\n',
		'module main\nfn show(x int, n int) { println(x << n) }\nfn main() { show(1, 32) }\n',
		'module main\nfn shift(n int) { mut x := 1; x <<= n; println(x) }\nfn main() { shift(32) }\n',
		'module main\nfn shift(n int) { mut x := 1; x >>= n; println(x) }\nfn main() { shift(32) }\n',
		'module main\nfn shift(n int) { mut x := 1; x >>>= n; println(x) }\nfn main() { shift(32) }\n',
		'module main\nfn divide(a int, b int) int { return a / b }\nfn main() { println(divide(1, 0)) }\n',
		'module main\nfn modulo(a int, b int) int { return a % b }\nfn main() { println(modulo(1, 0)) }\n',
		'module main\nfn divide(b int) { mut x := 1; x /= b; println(x) }\nfn main() { divide(0) }\n',
		'module main\nfn modulo(b int) { mut x := 1; x %= b; println(x) }\nfn main() { modulo(0) }\n',
		'module main\nfn main() { println(sizeof(string)) }\n',
		"module main\nfn main() { s := 'abc'; println(s[0]) }\n",
		"module main\nfn main() { println(c'a') }\n",
		'module main\nfn main() { println(`A`) }\n',
		'module main\nfn show(r rune) { println(r) }\nfn main() { show(65) }\n',
		'module main\nfn main() { println(rune(65)) }\n',
		'module main\nfn show(p charptr) { println(p) }\nfn main() { unsafe { show(nil) } }\n',
		'module main\nfn main() { p := charptr(0); println(p) }\n',
		'module main\nfn main() { println(1 ^ 2 + 3) }\n',
		'module main\nfn main() { println(10 & 3 + 1) }\n',
		'module main\nfn main() { println(1 | 2 ^ 3) }\n',
		'module main\nfn main() { println(1 & 2 * 3) }\n',
		'module main\nfn main() { mut x := -2_147_483_648; x--; println(x) }\n',
		'module main\nfn main() { for i := -2_147_483_648; true; i-- { println(i); break } }\n',
		'module main\nfn main() { mut x := -2_147_483_648 - 1; println(x) }\n',
		'module main\nfn main() { x := 2_147_483_649 | 0; println(x) }\n',
		'module main\nfn main() { x := 0xffff_ffff | 0; println(x) }\n',
		'module main\nfn main() { x := 0b11111111111111111111111111111111 | 0; println(x) }\n',
		'module main\nfn main() { mut a := 1; mut b := 2; a, b = b, a; println(a); println(b) }\n',
	] {
		mut failed := false
		_ := generate(source, 'typed_expression.v', prefs) or {
			failed = true
			''
		}
		assert failed
	}

	bool_c := generate('module main\nfn main() { println(true) }\n', 'bool_literal.v', prefs) or {
		panic(err)
	}
	assert bool_c.contains('println(((bool)true));')
	low_hex_c := generate('module main\nfn main() { x := 0x7fff_ffff | 0; println(x) }\n',
		'low_hex_literal.v', prefs) or { panic(err) }
	assert low_hex_c.contains('__typeof__((0x7fffffff|0)) x = (0x7fffffff|0);')
	low_binary_c := generate('module main\nfn main() { x := 0b01111111111111111111111111111111 | 0; println(x) }\n',
		'low_binary_literal.v', prefs) or { panic(err) }
	assert low_binary_c.contains('__typeof__((0b01111111111111111111111111111111|0))')
	max_int_c := generate('module main\nfn main() { x := 2_147_483_647 - 1; println(x) }\n',
		'max_int_expression.v', prefs) or { panic(err) }
	assert max_int_c.contains('__typeof__((2147483647-1)) x = (2147483647-1);')
	call_c := generate('module main\nfn sum(a int, b int) int { return a + b }\nfn main() { println(sum(1, 2)) }\n',
		'call_comma.v', prefs) or { panic(err) }
	assert call_c.contains('println(sum(1,2));')
}
