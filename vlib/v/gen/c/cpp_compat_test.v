module c

import os
import v.cmdexec
import v.flat
import v.types

fn cpp_test_compiler() ?string {
	return os.find_abs_path_of_executable(os.getenv_opt('CXX') or { 'g++' }) or { return none }
}

fn cpp_test_standards() []string {
	if standard := os.getenv_opt('V_CPP_TEST_STANDARD') {
		return [standard]
	}
	return ['c++11', 'c++20']
}

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
	cpp := cpp_test_compiler() or { return }
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
	cpp := cpp_test_compiler() or { return }
	root := os.join_path(os.vtmp_dir(), 'cgen_cpp_compat_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	source := os.join_path(root, 'main.v')
	os.cp(os.join_path(@VMODROOT, 'vlib/v/gen/c/testdata/cpp_compat_runtime.vv'), source)!
	mut modes := [][]string{}
	for standard in cpp_test_standards() {
		modes << ['-no-std', '-cflags', '-std=${standard}']
	}
	modes << ['-prealloc']
	for flags in modes {
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

fn test_cpp_nested_sum_initializers_preserve_values() {
	$if !linux {
		return
	}
	cpp := cpp_test_compiler() or { return }
	root := os.join_path(os.vtmp_dir(), 'cgen_cpp_nested_sum_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	source := os.join_path(root, 'main.v')
	os.cp(os.join_path(@VMODROOT, 'vlib/v/gen/c/testdata/cpp_nested_sum_runtime.vv'), source)!
	for standard in cpp_test_standards() {
		result := cmdexec.run(@VEXE, ['-new-compiler', '-no-retry-compilation', '-gc', 'none',
			'-cc', cpp, '-no-std', '-cflags', '-std=${standard}', 'run', source])
		assert result.exit_code == 0, result.output
	}
	if cc := os.find_abs_path_of_executable('gcc') {
		result := cmdexec.run(@VEXE, ['-new-compiler', '-no-retry-compilation', '-gc', 'none',
			'-cc', cc, 'run', source])
		assert result.exit_code == 0, result.output
	}
}

fn test_cpp_interface_metadata_keeps_layout_lifetime_and_constant_initialization() {
	$if !linux {
		return
	}
	cpp := cpp_test_compiler() or { return }
	root := os.join_path(os.vtmp_dir(), 'cgen_cpp_interface_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	owner := flat.source_type_id('IError')
	mut declarations := cpp_test_gen()
	declarations.interfaces[owner] = []string{}
	declarations.cpp_runtime_helpers()
	declarations.emit_interface_struct(owner)
	mut runtime := cpp_test_gen()
	runtime.interfaces[owner] = []string{}
	runtime.write('(IError){._typ = next(3), ._object = &(Watched){&alive, next(4)}, ._object_is_boxed = next(1)}')
	runtime.cpp_order_struct_initializer(0, 'IError', owner)
	mut constant := cpp_test_gen()
	constant.interfaces[owner] = []string{}
	constant.static_c_initializer = true
	constant.write('(IError){._typ = 7, ._object = &global_object, ._object_is_boxed = false}')
	constant.cpp_order_struct_initializer(0, 'IError', owner)
	static_error := constant.cpp_ierror_initializer('&global_object', 9, false)
	boxed_error := runtime.cpp_ierror_initializer('&global_object', 0x7fffffff, true)
	program := '#include <cassert>
#include <cstddef>
typedef unsigned int u32;
${declarations.sb.str()}
static int global_object;
constexpr IError permanent = ${constant.sb.str()};
constexpr IError permanent_error = ${static_error};
static_assert(permanent._object == &global_object, "constant object address");
static_assert(permanent_error._object == &global_object, "constant error address");
static_assert(sizeof(IError) == 2 * sizeof(void*), "interface size");
static_assert(alignof(IError) == alignof(void*), "interface alignment");
static_assert(offsetof(IError, _interface_meta) == sizeof(void*), "metadata offset");
// constexpr above enforces constant initialization. GCC 10 cannot read these
// anonymous bitfields in a constant expression, so observe them before main.
static bool observed_static_metadata;
struct ObserveStaticMetadata {
    ObserveStaticMetadata() {
        assert(permanent._typ == 7 && !permanent._object_is_boxed);
        assert(permanent_error._typ == 9 && !permanent_error._object_is_boxed);
        observed_static_metadata = true;
    }
};
static ObserveStaticMetadata observer;
static int effects;
static int next(int value) { effects = effects * 10 + value; return value; }
struct Watched { int* alive; int value; ~Watched() { --*alive; } };
static void consume(IError value) {
    const Watched* object = (const Watched*)value._object;
    assert(*object->alive == 1 && object->value == 4);
    assert(value._typ == 3 && value._object_is_boxed);
}
int main() {
    assert(observed_static_metadata);
    int alive = 1;
    consume(${runtime.sb.str()});
    assert(alive == 0 && effects == 341);
    IError boxed = ${boxed_error};
    assert(boxed._object == &global_object);
    assert(boxed._typ == 0x7fffffff && boxed._object_is_boxed);
}
'
	source := os.join_path(root, 'metadata.cpp')
	executable := os.join_path(root, 'metadata')
	os.write_file(source, program)!
	for standard in cpp_test_standards() {
		compiled := cmdexec.run(cpp, ['-std=${standard}', '-fpermissive', '-w', source, '-o',
			executable])
		assert compiled.exit_code == 0, compiled.output
		run := cmdexec.run(executable, [])
		assert run.exit_code == 0, run.output
	}
}
