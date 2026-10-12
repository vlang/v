module c

import os
import v.cmdexec
import v.flat
import v.types

fn test_partial_c_closure_view_keeps_the_v_runtime_layout() {
	mut ast := flat.FlatAst.new()
	mut tc := types.TypeChecker.new(&ast)
	tc.structs['closure.Closure'] = [types.StructField{
		name: 'closure_ptr'
		typ:  types.Pointer{ base_type: types.Type(types.void_) }
	}, types.StructField{
		name: 'closure_cap'
		typ:  types.Type(types.int_)
	}]
	tc.structs['C.builtin__closure__Closure'] = [types.StructField{
		name: 'closure_cap'
		typ:  types.Type(types.int_)
	}]
	mut g := FlatGen.new()
	g.a = &ast
	g.tc = &tc
	g.struct_decls()
	code := g.sb.str()
	assert code.count('struct closure__Closure {') == 1
	assert code.contains('void* closure_ptr;'), code
	assert code.contains('closure_cap;'), code
}

fn test_cross_snapshot_runs_after_relocation_without_native_helper_objects() ! {
	$if windows || freestanding || vinix {
		return
	}
	cc := os.find_abs_path_of_executable('clang') or { return }
	root := os.join_path(os.vtmp_dir(), 'portable_cross_runtime_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	source := os.join_path(root, 'main.v')
	payload := os.join_path(root, 'payload.txt')
	c_source := os.join_path(root, 'snapshot.c')
	os.write_file(payload, 'portable payload')!
	os.write_file(source, r'const payload = $embed_file("payload.txt")
fn read_payload() string { return payload.to_string() }
fn main() {
 mut workers := []thread string{}
 for _ in 0 .. 4 { workers << spawn read_payload() }
 for value in workers.wait() { assert value == "portable payload" }
 println("portable")
}
')!
	generated := cmdexec.run(@VEXE, ['-gc', 'none', '-prealloc', '-nocache', '-os', 'cross', '-o',
		c_source, source])
	assert generated.exit_code == 0, generated.output
	moved := os.join_path(root, 'relocated')
	os.mkdir_all(moved)!
	relocated := os.join_path(moved, 'snapshot.c')
	os.mv(c_source, relocated)!
	os.rm(source)!
	os.rm(payload)!
	executable := os.join_path(moved, 'program')
	built := cmdexec.run(cc, ['-std=gnu11', '-w', '-o', executable, relocated, '-lm', '-lpthread'])
	assert built.exit_code == 0, built.output
	ran := cmdexec.run(executable, [])
	assert ran.exit_code == 0, ran.output
	assert ran.output.trim_space() == 'portable', ran.output
}

fn test_object_output_links_from_a_relocated_directory_without_native_helpers() ! {
	$if windows || freestanding || vinix {
		return
	}
	cc := os.find_abs_path_of_executable('clang') or { return }
	root := os.join_path(os.vtmp_dir(), 'portable_object_runtime_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	source := os.join_path(root, 'library.v')
	object := os.join_path(root, 'library.o')
	os.write_file(source, "module portable\n@[export: 'portable_add']\npub fn add(left i32, right i32) i32 { return left + right }\n")!
	generated := cmdexec.run(@VEXE, ['-gc', 'none', '-nocache', '-cc', cc, '-is_o', '-o', object,
		source])
	assert generated.exit_code == 0, generated.output
	moved := os.join_path(root, 'relocated')
	os.mkdir_all(moved)!
	relocated := os.join_path(moved, 'library.o')
	os.mv(object, relocated)!
	os.rm(source)!
	main_c := os.join_path(moved, 'main.c')
	os.write_file(main_c, 'extern int portable_add(int, int);\nint main(void) { return portable_add(10, 20) == 30 ? 0 : 1; }\n')!
	executable := os.join_path(moved, 'program')
	built := cmdexec.run(cc, ['-o', executable, relocated, main_c, '-lm', '-lpthread'])
	assert built.exit_code == 0, built.output
	ran := cmdexec.run(executable, [])
	assert ran.exit_code == 0, ran.output
}

fn test_directory_test_runner_declares_hidden_string_comparisons() ! {
	$if windows || freestanding || vinix {
		return
	}
	cc := os.find_abs_path_of_executable('clang') or { return }
	root := os.join_path(os.vtmp_dir(), 'portable_directory_test_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	os.write_file(os.join_path(root, 'sample_test.v'), 'fn test_sample() { assert 6 * 7 == 42 }\n')!
	ran := cmdexec.run(@VEXE, ['-cc', cc, '-nocache', '-stats', 'test', root])
	assert ran.exit_code == 0, ran.output
	assert ran.output.contains('1 passed'), ran.output
}
