module main

import os
import rand

fn test_generated_calls_compile_for_void_multiline_and_complex_signatures() ! {
	root := os.join_path(os.vtmp_dir(), 'vcover-writer-${rand.ulid()}')
	os.mkdir_all(root)!
	defer {
		os.rmdir_all(root) or {}
	}
	declarations := [
		'fn plain() {}',
		'fn answer(a int) int { return a }',
		'fn multiline(\n a int,\n) bool{ return a == 0 }',
		'fn result() !int { return error("not implemented") }',
		'fn optional() ?int { return none }',
		'fn pair(a int) (int, string) { return a, "" }',
		'fn generic[T]() T { return T{} }',
		'fn takes_map(a map[string]int) int { return a.len }',
		'fn unknown(a []Unknown) {}',
	]
	source := 'module main\n' + declarations.join('\n') + '\nstruct Unknown {}\n'
	file := os.join_path(root, 'source.v')
	os.write_file(file, source)!
	test_path := test_file_for(file)
	existing := 'module main\n\n// existing whitespace survives  \n\n'
	os.write_file(test_path, existing)!
	mut functions := []FunctionCoverage{}
	mut line := 2
	for declaration in declarations {
		functions << FunctionCoverage{
			file: file
			name: declaration[3..].all_before('(').all_before('[')
			line: line
		}
		line += declaration.split_into_lines().len
	}
	functions << functions[0]
	ctx := Context{ working_folder: root }
	assert ctx.write_tests_for_file(file, functions)! == 9
	generated := os.read_file(test_path)!
	assert generated.starts_with(existing)
	assert generated.count('fn test_plain()') == 1
	assert generated.contains('\n\tplain()\n')
	assert !generated.contains('_ = plain()')
	assert generated.contains('_ = multiline(0)')
	assert !generated.contains('_ = result()')
	assert !generated.contains('_ = optional()')
	assert !generated.contains('_ = pair(')
	assert !generated.contains('_ = generic(')
	assert !generated.contains('unknown([]Unknown{})')
	assert ctx.write_tests_for_file(file, functions)! == 0
	assert os.read_file(test_path)! == generated
	compiled := os.exec([@VEXE, '-nocache', '-silent', 'test', test_path])
	assert compiled.exit_code == 0, compiled.output
}

fn test_writer_uses_import_aliases_and_preserves_backend_suffixes() {
	assert test_file_for('source.c.v') == 'source_test.c.v'
	assert test_file_for('source.js.v') == 'source_test.js.v'
	assert call_name_for('module nested\n', 'answer', 'import parent.nested as alias\n') == 'alias.answer'
	assert source_module_name('fn answer() int { return 42 }') == 'main'
}

fn test_writer_rejects_sibling_prefixes_and_symlink_destinations() ! {
	root := os.join_path(os.vtmp_dir(), 'vcover-boundary-${rand.ulid()}')
	os.mkdir_all(root)!
	defer {
		os.rmdir_all(root) or {}
	}
	folder := os.join_path(root, 'project')
	outside := os.join_path(root, 'project-other')
	os.mkdir_all(folder)!
	os.mkdir_all(outside)!
	ctx := Context{ working_folder: folder }
	assert ctx.within_working_folder(os.join_path(folder, 'source.v'))
	assert !ctx.within_working_folder(os.join_path(outside, 'source.v'))
	assert !ctx.within_working_folder(os.join_path(folder, '..', 'project-other', 'source.v'))
	$if !windows {
		file := os.join_path(folder, 'source.v')
		os.write_file(file, 'module main\nfn answer() int { return 42 }\n')!
		outside_test := os.join_path(outside, 'existing.v')
		os.write_file(outside_test, 'original contents')!
		os.symlink(outside_test, test_file_for(file))!
		mut refused := false
		ctx.write_tests_for_file(file, [FunctionCoverage{ name: 'answer', file: file, line: 2 }]) or {
			refused = true
		}
		assert refused, 'the writer followed a test-file symlink outside its working directory'
		assert os.read_file(outside_test)! == 'original contents'
		os.rm(test_file_for(file))!
		missing_target := os.join_path(outside, 'missing.v')
		os.symlink(missing_target, test_file_for(file))!
		refused = false
		ctx.write_tests_for_file(file, [FunctionCoverage{ name: 'answer', file: file, line: 2 }]) or {
			refused = true
		}
		assert refused, 'the writer followed a dangling test-file symlink'
		assert !os.exists(missing_target)
	}
}
