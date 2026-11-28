module driver

import os
import v.cmdexec
import v.gen.c.naming

fn test_unused_compiler_entry_and_worker_helpers_leave_generated_c() ! {
	root := os.join_path(os.vtmp_dir(), 'unused_compiler_import_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	source := os.join_path(root, 'main.v')
	// One build gives both the binary and, through -keepc, the C it was compiled from.
	program := 'unused_compiler_import_${os.getpid()}'
	binary := os.join_path(root, program + $if windows { '.exe' } $else { '' })
	environment := {
		'VFLAGS': ''
	}
	os.write_file(source, 'import v.driver\nfn main() { println(driver.project_backend_flag) }\n')!
	tmp := os.vtmp_dir()
	existing := os.ls(tmp)!.filter(it.starts_with('${program}.') && it.ends_with('.tmp.c'))
	built := cmdexec.run_in_merged_with_env(@VEXE, ['-no-retry-compilation', '-nocache', '-keepc',
		'-o', binary, source], @VEXEROOT, environment)
	kept := os.ls(tmp)!.filter(it.starts_with('${program}.') && it.ends_with('.tmp.c')
		&& it !in existing)
	defer {
		for file in kept {
			os.rm(os.join_path(tmp, file)) or {}
		}
	}
	assert built.exit_code == 0, built.output
	assert kept.len == 1, 'expected one kept C file for ${binary}, got ${kept}'
	generated := os.read_file(os.join_path(tmp, kept[0]))!
	assert generated.contains('project_backend_flag')
	for name in ['driver.run', 'c.gen_expr_lvalue', 'c.FlatGen.gen_fn_items_scoped_batches',
		'markused.CallCollector.collect_bodies_scoped_batches',
		'parser.Parser.precollect_parallel_comptime_consts', 'types.TypeChecker.check_scoped_batches'] {
		declaration := 'void ${naming.c_name(name)}('
		assert !generated.contains(declaration), declaration
	}
	executed := cmdexec.run(binary, [])
	assert executed.exit_code == 0, executed.output
	assert executed.output.trim_space() == project_backend_flag, executed.output
}
