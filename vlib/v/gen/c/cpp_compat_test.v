module c

import os
import v.cmdexec
import v.flat
import v.types

fn cpp_test_gen() &FlatGen {
	mut ast := &flat.FlatAst{}
	mut tc := types.TypeChecker.new(ast)
	tc.structs[flat.source_type_id('Pair')] = [
		types.StructField{ name: 'first', typ: types.Type(types.int_) },
		types.StructField{ name: 'second', typ: types.Type(types.int_) },
	]
	mut g := FlatGen.new()
	g.a = ast
	g.tc = &tc
	g.ccompiler = 'cplusplus'
	return &g
}

fn test_cpp_initializer_keeps_source_evaluation_order() {
	mut g := cpp_test_gen()
	g.write('(Pair){.second = next(2), .first = next(1)}')
	g.cpp_order_struct_initializer(0, 'Pair', flat.source_type_id('Pair'))
	source := g.sb.str()
	second_evaluation := source.index('next(2)') or { panic(source) }
	first_evaluation := source.index('next(1)') or { panic(source) }
	first_field := source.index('.first =') or { panic(source) }
	second_field := source.index('.second =') or { panic(source) }
	assert second_evaluation < first_evaluation
	assert first_field < second_field
	assert source.count('next(2)') == 1
	assert source.count('next(1)') == 1
	assert !source.contains('auto ')

	mut c := cpp_test_gen()
	c.ccompiler = 'gcc'
	original := '(Pair){.second = next(2), .first = next(1)}'
	c.write(original)
	c.cpp_order_struct_initializer(0, 'Pair', flat.source_type_id('Pair'))
	assert c.sb.str() == original
}

fn test_cpp_reordered_fields_keep_temporaries_alive_through_the_consumer() {
	$if !linux {
		return
	}
	cpp := os.find_abs_path_of_executable('g++') or { return }
	root := os.join_path(os.vtmp_dir(), 'cgen_cpp_lifetime_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	mut helpers := cpp_test_gen()
	helpers.cpp_runtime_helpers()
	mut direct := cpp_test_gen()
	direct.write('(Pair){.second = &(Watched){&alive}, .first = next(2)}')
	direct.cpp_order_struct_initializer(0, 'Pair', flat.source_type_id('Pair'))
	mut nested := cpp_test_gen()
	nested.write('(Pair){.second = (Holder){.ptr = &(Watched){&alive}}, .first = next(3)}')
	nested.cpp_order_struct_initializer(0, 'Pair', flat.source_type_id('Pair'))
	copy_source := direct.cpp_copy_source('(int[2]){next(4), next(5)}')
	program := '#include <cassert>
${helpers.sb.str()}
static int effects;
static int next(int value) { effects = effects * 10 + value; return value; }
struct Watched { int* alive; ~Watched() { --*alive; } };
namespace direct_case {
struct Pair { int first; const Watched* second; };
static int consume(Pair pair) { assert(*pair.second->alive == 1); return pair.first; }
static void run() {
    int alive = 1;
    assert(consume(${direct.sb.str()}) == 2);
    assert(alive == 0);
}
}
namespace nested_case {
struct Holder { const Watched* ptr; };
struct Pair { int first; Holder second; };
static int consume(Pair pair) { assert(*pair.second.ptr->alive == 1); return pair.first; }
static void run() {
    int alive = 1;
    assert(consume(${nested.sb.str()}) == 3);
    assert(alive == 0);
}
}
static int consume_array(const int* values) { return values[0] + values[1]; }
int main() {
    direct_case::run();
    nested_case::run();
    assert(consume_array(${copy_source}) == 9);
    assert(effects == 2345);
}
'
	source := os.join_path(root, 'lifetime.cpp')
	executable := os.join_path(root, 'lifetime')
	os.write_file(source, program)!
	compiled := cmdexec.run(cpp, ['-std=c++11', '-fpermissive', '-w', source, '-o', executable])
	assert compiled.exit_code == 0, compiled.output
	run := cmdexec.run(executable, [])
	assert run.exit_code == 0, run.output
}

fn test_cpp_runtime_copy_order_names_and_thread_local_storage() {
	$if !linux {
		return
	}
	cpp := os.find_abs_path_of_executable('g++') or { return }
	root := os.join_path(os.vtmp_dir(), 'cgen_cpp_compat_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	source := os.join_path(root, 'main.v')
	os.cp(os.join_path(@VMODROOT, 'vlib/v/gen/c/testdata/cpp_compat_runtime.vv'), source)!
	for flags in [
		['-no-std', '-cflags', '-std=c++11'],
		['-no-std', '-cflags', '-std=c++20'],
		['-prealloc'],
	] {
		mut args := ['-new-compiler', '-no-retry-compilation', '-gc', 'none', '-cc', cpp]
		args << flags
		args << ['run', source]
		result := cmdexec.run(@VEXE, args)
		assert result.exit_code == 0, '${args}: ${result.output}'
	}
	if cc := os.find_abs_path_of_executable('gcc') {
		result := cmdexec.run(@VEXE, ['-new-compiler', '-no-retry-compilation', '-gc', 'none',
			'-cc', cc, 'run', source])
		assert result.exit_code == 0, result.output
	}
}

fn test_cpp_keyword_escaping_preserves_explicit_c_export_name() {
	root := os.join_path(os.vtmp_dir(), 'cgen_cpp_export_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	source := os.join_path(root, 'main.v')
	c_path := os.join_path(root, 'main.c')
	os.write_file(source, "@[export: 'operator']
fn exported_value() int { return 41 }
fn main() { assert exported_value() == 41 }
")!
	result := cmdexec.run(@VEXE, ['-new-compiler', '-no-retry-compilation', '-gc', 'none', '-cc',
		'gcc', '-o', c_path, source])
	assert result.exit_code == 0, result.output
	generated := os.read_file(c_path)!
	assert generated.contains(' operator(void) {')
	assert !generated.contains('__v3_internal_symbol_cpp_operator(void) {')
}
