module fastc

import os
import v.cmdexec
import v.flat
import v.pref

fn test_fastc_mutable_interface_dispatches_value_and_pointer_receivers() {
	$if !macos && !linux {
		return
	}
	root := os.join_path(os.vtmp_dir(), 'fastc_interface_value_receiver_${os.getpid()}')
	os.mkdir_all(root) or { panic(err) }
	defer {
		os.rmdir_all(root) or {}
	}
	os.write_file(os.join_path(root, 'main.v'), r'module main
interface Factory {
mut:
	instantiate(offset int) &Factory
	record(mut total Total) int
}
struct Total {
mut:
	value int
}
struct ValueFactory {
	seed int
}
struct PointerFactory {
mut:
	seed int
}
fn (factory ValueFactory) instantiate(offset int) &Factory {
	instance := &ValueFactory{seed: factory.seed + offset}
	return instance
}
fn (factory ValueFactory) record(mut total Total) int {
	total.value += factory.seed
	return factory.seed
}
fn (mut factory PointerFactory) instantiate(offset int) &Factory {
	factory.seed += offset
	instance := &PointerFactory{seed: factory.seed}
	return instance
}
fn (mut factory PointerFactory) record(mut total Total) int {
	factory.seed++
	total.value += factory.seed
	return factory.seed
}
fn main() {
	value := &ValueFactory{seed: 35}
	mut factory := &Factory(value)
	mut factories := map[string]&Factory{}
	factories["value"] = factory
	mut selected := unsafe { factories["value"] }
	mut instance := selected.instantiate(7)
	assert instance != unsafe { nil }, "value receiver dispatch returned nil"
	mut total := Total{}
	assert instance.record(mut total) == 42
	assert total.value == 42 && value.seed == 35
	mut pointer := &PointerFactory{seed: 10}
	mut mutable_factory := &Factory(pointer)
	mut other := mutable_factory.instantiate(4)
	assert pointer.seed == 14
	assert other.record(mut total) == 15
	assert total.value == 57 && pointer.seed == 14
	println("interface receiver ABI works")
}
') or { panic(err) }
	ordinary := os.join_path(root, 'ordinary')
	ordinary_build := cmdexec.run_in_merged_with_env(@VEXE, ['-b', 'c', '-nocache', '-gc', 'none',
		'-o', ordinary, root], @VEXEROOT, {
		'VEXE':                   @VEXE
		'VFLAGS':                 ''
		'VOSARGS':                ''
		'V_MACOS_V3_NO_FALLBACK': '1'
	})
	assert ordinary_build.exit_code == 0, ordinary_build.output
	ordinary_run := cmdexec.run(ordinary, [])
	assert ordinary_run.exit_code == 0, ordinary_run.output
	assert ordinary_run.output == 'interface receiver ABI works\n', ordinary_run.output
	for target_headers in [false, true] {
		mut prefs := pref.new_preferences()
		prefs.vroot = @VEXEROOT
		prefs.building_v = true
		prefs.backend = 'fastc'
		prefs.ccompiler = 'clang'
		prefs.user_defines = ['gc_none', 'no_backtrace', 'v3_backend']
		if target_headers {
			prefs.target_libc_headers = true
			prefs.target = pref.target_from('vinix', pref.host_arch()) or { panic(err) }
			prefs.exclude = [os.join_path(@VEXEROOT, 'vlib/builtin/backtraces_nix.c.v')]
		}
		generated := generate_files([root], prefs) or { panic(err) }
		assert generated.contains('ValueFactory_instantiate(*(ValueFactory *)value._object,arg1)'), generated
		assert generated.contains('PointerFactory_instantiate((PointerFactory *)value._object,arg1)'), generated
		c_path := os.join_path(root, 'main_${target_headers}.c')
		program := os.join_path(root, 'fastc_${target_headers}')
		os.write_file(c_path, generated) or { panic(err) }
		build := cmdexec.run('cc', ['-std=gnu11', '-O2', '-fno-strict-aliasing',
			'-Werror=incompatible-pointer-types', '-o', program, c_path])
		assert build.exit_code == 0, build.output
		run := cmdexec.run(program, [])
		assert run.exit_code == 0, run.output
		assert run.output == ordinary_run.output, run.output
	}
}

fn test_fastc_interface_dispatch_keeps_invalid_mutability_contracts_rejected() {
	$if !macos && !linux {
		return
	}
	root := os.join_path(os.vtmp_dir(), 'fastc_interface_invalid_mutability_${os.getpid()}')
	os.mkdir_all(root) or { panic(err) }
	defer {
		os.rmdir_all(root) or {}
	}
	for i, source in [
		r'module main
interface Reader { read() int }
struct Counter { mut: value int }
fn (mut counter Counter) read() int { counter.value++; return counter.value }
fn main() { _ := Reader(Counter{}) }
',
		r'module main
struct Counter { mut: value int }
interface Reader { read(mut total Counter) int }
struct Source {}
fn (source Source) read(total Counter) int { return total.value }
fn main() { _ := Reader(Source{}) }
',
	] {
		path := os.join_path(root, 'invalid_${i}.v')
		os.write_file(path, source) or { panic(err) }
		ordinary := cmdexec.run_in_merged_with_env(@VEXE, ['-b', 'c', '-nocache', '-gc', 'none',
			'-o', os.join_path(root, 'invalid_${i}'), path], @VEXEROOT, {
			'VEXE':                   @VEXE
			'VFLAGS':                 ''
			'VOSARGS':                ''
			'V_MACOS_V3_NO_FALLBACK': '1'
		})
		assert ordinary.exit_code != 0, ordinary.output
	}
	read_only := FastcFunctionSignature{
		parameter_types:      flat.TypeTextList.from_names(['Reader'])
		parameter_mutability: [false]
		return_type:          flat.source_type_id('int')
	}
	mutating := FastcFunctionSignature{
		parameter_types:      flat.TypeTextList.from_names(['Counter*'])
		parameter_mutability: [true]
		return_type:          flat.source_type_id('int')
	}
	assert !fastc_interface_method_signatures_match(read_only, mutating)
	mutable_argument := FastcFunctionSignature{
		parameter_types:      flat.TypeTextList.from_names(['Reader', 'Counter*'])
		parameter_mutability: [true, true]
		return_type:          flat.source_type_id('int')
	}
	read_only_argument := FastcFunctionSignature{
		parameter_types:      flat.TypeTextList.from_names(['Source', 'Counter*'])
		parameter_mutability: [false, false]
		return_type:          flat.source_type_id('int')
	}
	assert !fastc_interface_method_signatures_match(mutable_argument, read_only_argument)
}

fn test_fastc_mutable_interface_accepts_minimal_value_receiver_implementation() {
	mut prefs := pref.new_preferences()
	prefs.building_v = true
	source := generate('module main
interface Worker {
mut:
	work()
}
struct Good {}
fn (good Good) work() {}
fn main() {
	mut worker := Worker(Good{})
	worker.work()
}
', 'mutable_interface_read_only_implementation.v', prefs) or { panic(err) }
	assert source.contains('case __v_typeid_Good: Good_work(*(Good *)value._object); return;'), source
}
