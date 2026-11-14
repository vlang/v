module driver

import os
import v.cmdexec

fn test_unused_compiler_entry_and_worker_helpers_leave_generated_c() ! {
	root := os.join_path(os.vtmp_dir(), 'unused_compiler_import_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	source := os.join_path(root, 'main.v')
	output := os.join_path(root, 'main.c')
	binary := os.join_path(root, 'main' + $if windows { '.exe' } $else { '' })
	environment := {
		'VFLAGS':                 ''
		'V_MACOS_V3_NO_FALLBACK': '1'
	}
	os.write_file(source, 'import v.driver\nfn main() { println(driver.project_backend_flag) }\n')!
	compiled := cmdexec.run_in_merged_with_env(@VEXE, ['-new-compiler', '-no-retry-compilation',
		'-nocache', '-o', output, source], @VEXEROOT, environment)
	assert compiled.exit_code == 0, compiled.output
	generated := os.read_file(output)!
	// The program uses a compiler constant, so it must not carry the dispatcher
	// or backend worker bodies. Real compiler entry points call them normally.
	for declaration in ['void driver__run(', 'void c__gen_expr_lvalue(',
		'void c__FlatGen_gen_fn_items_scoped_batches(',
		'void markused__CallCollector_collect_bodies_scoped_batches(',
		'void parser__Parser_precollect_parallel_comptime_consts(',
		'void types__TypeChecker_check_scoped_batches('] {
		assert !generated.contains(declaration), declaration
	}
	built := cmdexec.run_in_merged_with_env(@VEXE, ['-new-compiler', '-no-retry-compilation', '-nocache',
		'-o', binary, source], @VEXEROOT, environment)
	assert built.exit_code == 0, built.output
	executed := cmdexec.run(binary, [])
	assert executed.exit_code == 0, executed.output
	assert executed.output.trim_space() == project_backend_flag, executed.output
}
