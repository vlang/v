module c

import os
import v.cmdexec
import v.flat
import v.types

fn test_detached_spawn_nested_error_cleanup_compiles_as_c11() ! {
	cc := os.find_abs_path_of_executable('clang') or { return }
	dir := os.join_path(os.vtmp_dir(), 'detached_error_cleanup_${os.getpid()}')
	os.mkdir_all(dir)!
	defer { os.rmdir_all(dir) or {} }
	source := os.join_path(dir, 'main.v')
	os.write_file(source, 'struct NestedError { cause IError }
fn (e NestedError) msg() string { return e.cause.msg() }
fn (e NestedError) code() int { return 1 }
interface Container { cause IError }
type Choice = NestedError | int
fn fail() ! { return NestedError{cause: error("nested")} }
fn boxed() Container { return NestedError{cause: error("boxed")} }
fn choice() Choice { return NestedError{cause: error("choice")} }
fn main() {
	spawn fail()
	spawn boxed()
	spawn choice()
}
')!
	// Modern Clang accepts declarations after case labels as a C23 extension;
	// older macOS CI compilers reject them. Exercise uncached, cold and warm builds.
	for mode in ['uncached', 'cold', 'warm'] {
		mut args := ['-cc', cc, '-cstrict', '-show-c-output', '-cflags',
			'-std=gnu11 -Wc23-extensions -Wno-unknown-warning-option', '-o',
			os.join_path(dir, 'program')]
		if mode == 'uncached' {
			args << '-nocache'
		}
		args << source
		result := cmdexec.run_in_merged_with_env(@VEXE, args, dir, {
			'V3CACHE': os.join_path(dir, 'cache')
			'VJOBS':   '1'
		})
		assert result.exit_code == 0, '${mode}: ${result.output}'
		assert !result.output.contains('label followed by a declaration'), '${mode}: ${result.output}'
	}
}

fn test_detached_spawn_cleanup_reaches_deep_thread_results() {
	mut ast := flat.FlatAst.new()
	mut tc := types.TypeChecker.new(&ast)
	mut g := FlatGen.new()
	g.a = &ast
	g.tc = &tc
	mut name := 'int'
	for _ in 0 .. 18 {
		name = 'thread ${name}'
	}
	cleanup := g.detached_spawn_result_cleanup(tc.parse_type(flat.source_type_id(name)), 'result', 0)
	assert cleanup.count('__v_thread_join') == 18, cleanup
}

fn test_detached_spawn_cleanup_uses_sum_type_and_variant_drops() {
	for sum_name in ['Box[[2]string]', 'model.Box[[2]string]'] {
		mut ast := flat.FlatAst.new()
		mut tc := types.TypeChecker.new(&ast)
		tc.sum_types.set(sum_name, ['[2]string', 'int'])
		tc.cur_module = 'builtin'
		tc.cur_file = 'builtin.v'
		mut g := FlatGen.new()
		g.a = &ast
		g.tc = &tc
		g.detached_spawn_drop = true
		sum_id := flat.source_type_id(sum_name)
		cleanup := g.detached_spawn_aggregate_cleanup(tc.parse_canonical_type(sum_id), 'result')
		generated := g.spawn_wrapper_defs.join('\n')
		helper_name := g.ownership_recursive_drop_helper_name(sum_id)
		sum_c_type := g.cname(sum_name)
		assert sum_c_type != g.struct_cname(sum_name)
		assert generated.count('(${sum_c_type}* _value)') == 2, generated
		assert generated.contains('switch ((*_value).typ)'), generated
		assert generated.contains('string__free('), generated
		// The helper expands its own sum once; the call site uses that helper.
		assert !generated.contains('${helper_name}(&(*_value))'), generated
		assert cleanup.contains('(void*)&(result)'), cleanup
		assert cleanup.contains('${helper_name}(&_drop_sum'), cleanup
		assert cleanup.contains('memcpy(_drop_sum_storage'), cleanup
		assert tc.cur_module == 'builtin'
		assert tc.cur_file == 'builtin.v'
	}
}
