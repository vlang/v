module fastc

import os
import v.cmdexec

fn fastc_review_fixture(name string) string {
	root := os.join_path(os.vtmp_dir(), 'fastc_review_${name}_${os.getpid()}')
	os.mkdir_all(root) or { panic(err) }
	os.write_file(os.join_path(root, 'v.mod'), "Module { name: 'fastc_review' }") or {
		panic(err)
	}
	return root
}

fn fastc_review_run(root string, flags []string) string {
	program := os.join_path(root, 'program')
	build := cmdexec.run(@VEXE, ['-b', 'fastc', '-enable-globals', ...flags, '-o', program, root])
	assert build.exit_code == 0, build.output
	run := cmdexec.run(program, [])
	assert run.exit_code == 0, run.output
	return run.output.trim_space()
}

fn test_fastc_local_capturing_closures_release_runtime_slots() {
	$if !macos && !linux {
		return
	}
	root := fastc_review_fixture('closures')
	defer {
		os.rmdir_all(root) or {}
	}
	os.write_file(os.join_path(root, 'main.v'), r'module main
import builtin.closure as _

type Callback = fn () int

fn local_return(value int) int {
	callback := fn [value] () int { return value }
	defer { assert callback() == value }
	return callback()
}

fn escaping(value int) Callback {
	callback := fn [value] () int { return value }
	alias := callback
	return alias
}

struct Holder {
	callback Callback
}

fn stored(value int) Holder {
	callback := fn [value] () int { return value }
	return Holder{callback: callback}
}

fn main() {
	baseline := g_closure.live.len
	for i in 0 .. 2048 {
		callback := fn [i] () int { return i }
		other := fn [i] () int { return i + 1 }
		assert callback() == i
		assert other() == i + 1
		assert g_closure.live.len == baseline + 2
		defer { assert callback() == i }
	}
	assert g_closure.live.len == baseline
	for i in 0 .. 16 {
		callback := fn [i] () int { return i }
		assert callback() == i
		if i < 8 { continue }
		break
	}
	assert g_closure.live.len == baseline
	assert local_return(73) == 73
	assert g_closure.live.len == baseline
	{
		value := 42
		callback := fn [value] () int { return value }
		assert "${callback()}" == "42"
	}
	assert g_closure.live.len == baseline
	{
		value := 42
		_ := fn [value] () int { return value }
	}
	assert g_closure.live.len == baseline
	callback := escaping(42)
	holder := stored(73)
	assert callback() == 42
	assert holder.callback() == 73
	assert g_closure.live.len == baseline + 2
	println("released")
}
') or { panic(err) }
	assert fastc_review_run(root, ['-nocache']) == 'released'
}

fn test_fastc_blank_import_retains_native_registration() {
	$if !macos && !linux {
		return
	}
	root := fastc_review_fixture('registration')
	defer {
		os.rmdir_all(root) or {}
	}
	os.mkdir_all(os.join_path(root, 'registration')) or { panic(err) }
	os.write_file(os.join_path(root, 'registration', 'registration.v'), r'module registration
#flag @VMODROOT/registration.c
fn C.unused_registration_function() int
pub struct Marker {}
') or { panic(err) }
	os.write_file(os.join_path(root, 'registration.c'), 'extern int registered;
__attribute__((constructor)) static void register_native(void) { registered = 73; }
int unused_registration_function(void) { return 0; }
') or { panic(err) }
	os.write_file(os.join_path(root, 'reader.c'), 'int registered;
int read_registration(void) { return registered; }
') or { panic(err) }
	os.write_file(os.join_path(root, 'reader.h'), 'int read_registration(void);\n') or {
		panic(err)
	}
	main_path := os.join_path(root, 'main.v')
	os.write_file(main_path, r'module main
import registration as _
#flag @VMODROOT/reader.c
#include "@VMODROOT/reader.h"
fn C.read_registration() int
fn main() { println(C.read_registration()) }
') or { panic(err) }
	assert fastc_review_run(root, ['-nocache']) == '73'
	// A type-only import cannot prove a binary artifact has no side effects.
	tcc := os.join_path(@VEXEROOT, 'thirdparty', 'tcc', 'tcc.exe')
	object := os.join_path(root, 'registration.o')
	compile := cmdexec.run(tcc, ['-c', '-o', object, os.join_path(root, 'registration.c')])
	assert compile.exit_code == 0, compile.output
	os.write_file(os.join_path(root, 'registration', 'registration.v'), r'module registration
#flag @VMODROOT/registration.o
fn C.unused_registration_function() int
pub struct Marker {}
') or { panic(err) }
	os.write_file(main_path, r'module main
import registration
#flag @VMODROOT/reader.c
#include "@VMODROOT/reader.h"
fn C.read_registration() int
fn main() {
	_ := registration.Marker{}
	println(C.read_registration())
}
') or { panic(err) }
	assert fastc_review_run(root, ['-nocache']) == '73'
}

fn test_fastc_cached_links_notice_replaced_native_artifacts() {
	$if !macos && !linux {
		return
	}
	root := fastc_review_fixture('native_cache')
	defer {
		os.rmdir_all(root) or {}
	}
	tcc := os.join_path(@VEXEROOT, 'thirdparty', 'tcc', 'tcc.exe')
	source := os.join_path(root, 'native.c')
	object := os.join_path(root, 'native.o')
	archive := os.join_path(root, 'native.a')
	os.write_file(os.join_path(root, 'native.h'), 'int native_answer(void);\n') or {
		panic(err)
	}
	for artifact in ['native.o', 'native.a'] {
		os.write_file(os.join_path(root, 'main.v'), 'module main
#flag @VMODROOT/${artifact}
#include "@VMODROOT/native.h"
fn C.native_answer() int
fn main() { println(C.native_answer()) }
') or { panic(err) }
		for answer in [41, 73] {
			os.write_file(source, 'int native_answer(void) { return ${answer}; }\n') or {
				panic(err)
			}
			compile := cmdexec.run(tcc, ['-c', '-o', object, source])
			assert compile.exit_code == 0, compile.output
			if artifact.ends_with('.a') {
				pack := cmdexec.run(tcc, ['-ar', 'rcs', archive, object])
				assert pack.exit_code == 0, pack.output
			}
			// Keep V source and native path unchanged, with caching enabled.
			assert fastc_review_run(root, []) == answer.str()
		}
	}
}
