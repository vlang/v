module fastc

import os
import v.cmdexec
import v.pref

fn test_fastc_native_flags_select_existing_paths() {
	root := os.join_path(os.vtmp_dir(), 'fastc native flags ${os.getpid()}')
	os.mkdir_all(root) or { panic(err) }
	defer {
		os.rmdir_all(root) or {}
	}
	missing := os.join_path(root, 'missing.so')
	first := os.join_path(root, 'first.so')
	second := os.join_path(root, 'second.so')
	os.write_file(first, '') or { panic(err) }
	os.write_file(second, '') or { panic(err) }
	for macro_name in [r'$first_existing', r'$when_first_existing'] {
		args := fastc_c_flag_args("${macro_name}('${missing}', '${first}', '${second}')", root,
			os.join_path(root, 'main.v')) or { panic(err) }
		assert args == [first], args.str()
		prefixed := fastc_c_flag_args("-I${macro_name}('${missing}', '@VEXEROOT')", root,
			os.join_path(root, 'main.v')) or { panic(err) }
		assert prefixed == ['-I${root}'], prefixed.str()
	}
	args := fastc_c_flag_args("-I\$when_first_existing('${missing}') -DIGNORED=1", root,
		os.join_path(root, 'main.v')) or { panic(err) }
	assert args.len == 0
	if _ := fastc_c_flag_args("\$first_existing('${missing}')", root,
		os.join_path(root, 'main.v')) {
		assert false, 'a required missing path was accepted'
	} else {
		assert err.msg().contains('none of the paths'), err.msg()
	}

	mut prefs := pref.new_preferences()
	prefs.vroot = @VEXEROOT
	source_file := os.join_path(root, 'main.v')
	os.write_file(source_file, "#flag \$when_first_existing('${missing}', '${first}')\nfn main() {}") or {
		panic(err)
	}
	generated := generate_files_with_source_paths([source_file], prefs) or { panic(err) }
	assert first in generated.c_flags, generated.c_flags.str()
	os.write_file(source_file, "#flag \$when_first_existing('${missing}')\nfn main() {}") or {
		panic(err)
	}
	skipped := generate_files_with_source_paths([source_file], prefs) or { panic(err) }
	assert skipped.c_flags.len == 0, skipped.c_flags.str()
}

fn test_fastc_linux_atomic_library_directives() {
	root := os.join_path(os.vtmp_dir(), 'fastc_linux_atomic_flags_${os.getpid()}')
	os.mkdir_all(root) or { panic(err) }
	defer {
		os.rmdir_all(root) or {}
	}
	os.write_file(os.join_path(root, 'main.v'), 'import sync.stdatomic as _\nfn main() {}') or {
		panic(err)
	}
	for arch in ['amd64', 'arm64'] {
		mut prefs := pref.new_preferences()
		prefs.vroot = @VEXEROOT
		prefs.building_v = true
		prefs.ccompiler = 'tinyc'
		prefs.target = pref.target_from('linux', arch) or { panic(err) }
		prefs.user_defines << 'v3_backend'
		generation := generate_files_with_source_paths([root], prefs) or { panic(err) }
		assert generation.source_paths.any(it.ends_with('sync/stdatomic/1.declarations.c.v'))
		assert !generation.c_flags.any(it.contains(r'$when_first_existing'))
		assert generation.c_source().contains('int main(')
	}
}

fn test_fastc_linux_gc_atomic_support_without_sync_import() {
	root := os.join_path(os.vtmp_dir(), 'fastc_linux_gc_atomic_${os.getpid()}')
	os.mkdir_all(root) or { panic(err) }
	defer {
		os.rmdir_all(root) or {}
	}
	os.write_file(os.join_path(root, 'main.v'), 'fn main() { println(42) }') or { panic(err) }
	for libc in ['musl', 'glibc'] {
		for arch in ['amd64', 'arm64'] {
			mut prefs := pref.new_preferences()
			prefs.vroot = @VEXEROOT
			prefs.backend = 'fastc'
			prefs.building_v = true
			prefs.ccompiler = 'tinyc'
			prefs.target = pref.target_from('linux', arch) or { panic(err) }
			prefs.user_defines = ['v3_backend', 'gcboehm', libc]
			// Exercise both initial resolution and reuse of its source memo.
			for _ in 0 .. 2 {
				generation := generate_files_with_source_paths([root], prefs) or { panic(err) }
				assert generation.source_paths.any(it.ends_with('builtin_d_gcboehm.c.v'))
				has_stdatomic := generation.source_paths.any(it.ends_with('sync/stdatomic/1.declarations.c.v'))
				assert has_stdatomic == !(libc == 'musl' && arch == 'amd64'), '${libc}/${arch}'
				assert !generation.c_flags.any(it.ends_with('atomic.S')), generation.c_flags.str()
				assert generation.c_source().contains('int main(')
			}
		}
	}
}

fn test_fastc_prealloc_arena_is_separate_for_worker_threads() {
	$if !macos && !linux {
		return
	}
	root := os.join_path(os.vtmp_dir(), 'fastc_native_tls_${os.getpid()}')
	os.mkdir_all(root) or { panic(err) }
	defer {
		os.rmdir_all(root) or {}
	}
	os.write_file(os.join_path(root, 'main.v'), r'module main
fn arena_root() usize {
	values := []int{len: 64}
	assert values.len == 64
	return usize(g_memory_block)
}

fn main() {
	main_root := arena_root()
	first := spawn arena_root()
	second := spawn arena_root()
	assert main_root != 0
	first_root := first.wait()
	second_root := second.wait()
	assert first_root != 0 && first_root != main_root
	assert second_root != 0 && second_root != main_root
	println("separate arenas")
}
') or { panic(err) }
	program := os.join_path(root, 'program')
	build := cmdexec.run(@VEXE, ['-b', 'fastc', '-enable-globals', '-prealloc', '-o', program,
		root])
	assert build.exit_code == 0, build.output
	run := cmdexec.run(program, [])
	assert run.exit_code == 0, run.output
	assert run.output.trim_space() == 'separate arenas', run.output
}

fn test_fastc_struct_array_initializers_evaluate_each_field_once() {
	$if !macos && !linux {
		return
	}
	root := os.join_path(os.vtmp_dir(), 'fastc_native_struct_array_${os.getpid()}')
	os.mkdir_all(root) or { panic(err) }
	defer {
		os.rmdir_all(root) or {}
	}
	os.write_file(os.join_path(root, 'main.v'), r'module main
struct Range {
	lo u16
	hi u16
	stride u16
}
struct Table {
	ranges []Range
}
const literals = Table{ranges: [Range{1, 2, 3}, Range{4, 5, 6}]}
__global calls int
fn next() u16 {
	calls += 1
	return u16(calls)
}
fn make_range() Range {
	return Range{next(), next(), next()}
}
fn make_table() Table {
	return Table{ranges: [make_range(), make_range()]}
}
fn main() {
	table := Table{ranges: [Range{next(), next(), next()}, Range{next(), next(), next()}]}
	assert calls == 6
	assert "ranges ${table.ranges.len}" == "ranges 2"
	for i in 0 .. 2 {
		assert table.ranges[i].lo == literals.ranges[i].lo
		assert table.ranges[i].hi == literals.ranges[i].hi
		assert table.ranges[i].stride == literals.ranges[i].stride
	}
	returned := make_table()
	assert calls == 12
	assert returned.ranges[0].lo == 7
	assert returned.ranges[1].stride == 12
	println("initialized")
}
') or { panic(err) }
	program := os.join_path(root, 'program')
	build := cmdexec.run(@VEXE, ['-b', 'fastc', '-enable-globals', '-o', program, root])
	assert build.exit_code == 0, build.output
	run := cmdexec.run(program, [])
	assert run.exit_code == 0, run.output
	assert run.output.trim_space() == 'initialized', run.output
}

fn test_fastc_linux_backtrace_accepts_erased_pointer_arguments() {
	$if !linux || !glibc {
		return
	}
	root := os.join_path(os.vtmp_dir(), 'fastc_native_backtrace_${os.getpid()}')
	os.mkdir_all(root) or { panic(err) }
	defer {
		os.rmdir_all(root) or {}
	}
	os.write_file(os.join_path(root, 'main.c.v'), r'module main
fn main() {
	mut frames := [8]voidptr{}
	count := C.backtrace(&frames[0], 8)
	assert count > 0
	symbols := C.backtrace_symbols(voidptr(&frames[0]), count)
	assert symbols != unsafe { nil }
	C.free(symbols)
	println("backtrace")
}
') or { panic(err) }
	program := os.join_path(root, 'program')
	build := cmdexec.run(@VEXE, ['-b', 'fastc', '-enable-globals', '-o', program, root])
	assert build.exit_code == 0, build.output
	run := cmdexec.run(program, [])
	assert run.exit_code == 0, run.output
	assert run.output.trim_space() == 'backtrace', run.output
}

fn test_fastc_native_helpers_link_before_static_archives() {
	$if !macos && !linux {
		return
	}
	root := os.join_path(os.vtmp_dir(), 'fastc_native_archive_${os.getpid()}')
	os.mkdir_all(root) or { panic(err) }
	defer {
		os.rmdir_all(root) or {}
	}
	provider := os.join_path(root, 'provider.c')
	object := os.join_path(root, 'provider.o')
	archive := os.join_path(root, 'provider.a')
	os.write_file(provider, 'int library_answer(void) { return 42; }\n') or { panic(err) }
	// Match FastC's object format; the macOS system compiler emits Mach-O.
	tcc := os.join_path(@VEXEROOT, 'thirdparty', 'tcc', 'tcc.exe')
	compiled := cmdexec.run(tcc, [
		'-B${os.join_path(@VEXEROOT, 'thirdparty', 'tcc', 'lib')}',
		'-c',
		provider,
		'-o',
		object,
	])
	assert compiled.exit_code == 0, compiled.output
	archived := cmdexec.run(tcc, ['-ar', 'rcs', archive, object])
	assert archived.exit_code == 0, archived.output
	os.write_file(os.join_path(root, 'helper.c'),
		'int library_answer(void);\nint native_answer(void) { return library_answer(); }\n') or {
		panic(err)
	}
	os.write_file(os.join_path(root, 'main.c.v'), r'module main
#flag @DIR/provider.a
#flag @DIR/helper.c
@[c_extern]
fn C.native_answer() i32
fn main() {
	assert C.native_answer() == 42
	println("archive")
}
') or { panic(err) }
	for jobs in ['1', '2'] {
		program := os.join_path(root, 'program_${jobs}')
		build := cmdexec.run_in_merged_with_env(@VEXE, ['-new-compiler', '-b', 'fastc',
			'-enable-globals', '-o', program, root], '', {
			'VJOBS': jobs
		})
		assert build.exit_code == 0, '${jobs} jobs: ${build.output}'
		run := cmdexec.run(program, [])
		assert run.exit_code == 0, run.output
		assert run.output.trim_space() == 'archive', run.output
	}
}

fn test_fastc_prepared_link_recognizes_native_c_sources() {
	$if macos && !tinyc && !fastc_selfhost ?&& !v3_backend ? {
		root := os.join_path(os.vtmp_dir(), 'fastc_link_source_${os.getpid()}')
		os.mkdir_all(root) or { panic(err) }
		defer { os.rmdir_all(root) or {} }
		source := os.join_path(root, 'helper.c')
		os.write_file(source, 'int native_answer(void) { return 42; }\n') or { panic(err) }
		assert fastc_libtcc_is_link_input(source)
		assert !fastc_libtcc_is_link_input(os.join_path(root, 'missing.c'))
		assert !fastc_libtcc_is_link_input(root)
		tcc_lib := os.join_path(@VEXEROOT, 'thirdparty', 'tcc', 'lib')
		sdk := cmdexec.macos_sdk_root()
		assert sdk != ''
		mut link := fastc_prepare_link(os.join_path(@VEXEROOT, 'thirdparty', 'tcc', 'tcc.exe'),
			tcc_lib, ['-std=gnu11', '-B${tcc_lib}', '-Werror=implicit-function-declaration'],
			['-L${tcc_lib}', '-L${os.join_path(sdk, 'usr', 'lib')}'])
		defer { fastc_discard_link(mut link) }
		assert fastc_prepared_link_accepts_inputs(&link)
	}
}

fn test_fastc_existing_path_macros_preserve_quoted_commas() {
	root := os.join_path(os.vtmp_dir(), 'fastc_quoted_paths_${os.getpid()}')
	os.mkdir_all(root) or { panic(err) }
	defer { os.rmdir_all(root) or {} }
	path := os.join_path(root, 'native, helper.a')
	os.write_file(path, '') or { panic(err) }
	selected, enabled := fastc_expand_existing_path_macros('PRE\$first_existing("@DIR/missing", "@DIR/native, helper.a")POST', root, os.join_path(root, 'main.v')) or { panic(err) }
	assert enabled
	assert selected == 'PRE' + os.quoted_path(path) + 'POST'
	optional, optional_enabled := fastc_expand_existing_path_macros(r"-L$when_first_existing('@DIR/native, helper.a', '@DIR/missing')", root, os.join_path(root, 'main.v')) or { panic(err) }
	assert optional_enabled
	assert optional == '-L' + os.quoted_path(path)
}

fn test_fastc_linux_prealloc_without_spawn_requires_pthread_linking() {
	root := os.join_path(os.vtmp_dir(), 'fastc_linux_tls_link_${os.getpid()}')
	os.mkdir_all(root) or { panic(err) }
	defer { os.rmdir_all(root) or {} }
	os.write_file(os.join_path(root, 'main.v'), 'fn main() { println(42) }') or { panic(err) }
	mut prefs := pref.new_preferences()
	prefs.vroot = @VEXEROOT
	prefs.backend = 'fastc'
	prefs.building_v = true
	prefs.ccompiler = 'tinyc'
	prefs.target = pref.target_from('linux', 'amd64') or { panic(err) }
	prefs.user_defines = ['v3_backend', 'gc_none', 'prealloc']
	generation := generate_files_with_source_paths([root], prefs) or { panic(err) }
	assert generation.c_source().contains('pthread_key_create')
	assert generation.uses_threads
}

fn test_fastc_backtrace_symbols_accept_erased_frames() {
	$if !macos && !(linux && glibc) {
		return
	}
	root := os.join_path(os.vtmp_dir(), 'fastc_backtrace_headers_${os.getpid()}')
	os.mkdir_all(root) or { panic(err) }
	defer { os.rmdir_all(root) or {} }
	os.write_file(os.join_path(root, 'main.c.v'), r'module main
fn main() {
	mut frames := [8]voidptr{}
	count := C.backtrace(&frames[0], 8)
	assert count > 0
	symbols := C.backtrace_symbols(voidptr(&frames[0]), count)
	assert symbols != unsafe { nil }
	C.free(symbols)
	println("backtrace preserved")
}
') or { panic(err) }
	program := os.join_path(root, 'program')
	build := cmdexec.run(@VEXE, ['-b', 'fastc', '-enable-globals', '-gc', 'none', '-no-prealloc',
		'-nocache', '-o', program, root])
	assert build.exit_code == 0, build.output
	run := cmdexec.run(program, [])
	assert run.exit_code == 0, run.output
	assert run.output.trim_space() == 'backtrace preserved'
}

fn test_fastc_glibc_gettid_uses_the_system_call() {
	$if !(linux && glibc) {
		return
	}
	root := os.join_path(os.vtmp_dir(), 'fastc_glibc_gettid_${os.getpid()}')
	os.mkdir_all(root) or { panic(err) }
	defer { os.rmdir_all(root) or {} }
	os.write_file(os.join_path(root, 'main.v'), 'module main
fn main() {
	assert v_gettid() == v_getpid()
	println("main thread id")
}
') or { panic(err) }
	program := os.join_path(root, 'program')
	build := cmdexec.run(@VEXE, ['-b', 'fastc', '-gc', 'none', '-nocache', '-o', program, root])
	assert build.exit_code == 0, build.output
	run := cmdexec.run(program, [])
	assert run.exit_code == 0, run.output
	assert run.output.trim_space() == 'main thread id'
	// glibc exports `gettid` only from 2.30 on, so the program must not import it.
	binary := os.read_file(program) or { panic(err) }
	assert !binary.contains('\x00gettid\x00')
}

// The gettid helper declares `syscall` itself: a strict feature mode such as
// `-D_POSIX_C_SOURCE=200809L` hides glibc's declaration in <unistd.h>.
fn test_fastc_glibc_gettid_helper_compiles_in_a_strict_feature_mode() {
	$if !(linux && glibc) {
		return
	}
	root := os.join_path(os.vtmp_dir(), 'fastc_strict_gettid_${os.getpid()}')
	os.mkdir_all(root) or { panic(err) }
	defer { os.rmdir_all(root) or {} }
	source := os.join_path(root, 'strict.c')
	os.write_file(source, c_selfhost_preamble_includes + c_selfhost_post_directives +
		'int main(void) { return gettid() == getpid() ? 0 : 1; }\n') or { panic(err) }
	program := os.join_path(root, 'strict')
	tcc_base := os.join_path(@VEXEROOT, 'thirdparty', 'tcc', 'lib', 'tcc')
	compiled := cmdexec.run(os.join_path(@VEXEROOT, 'thirdparty', 'tcc', 'tcc.exe'), [
		'-B${tcc_base}',
		'-D_POSIX_C_SOURCE=200809L',
		'-Werror=implicit-function-declaration',
		'-o',
		program,
		source,
	])
	assert compiled.exit_code == 0, compiled.output
	run := cmdexec.run(program, [])
	assert run.exit_code == 0, run.output
}

fn test_fastc_glibc_gettid_declares_syscall_with_strict_feature_flags() {
	$if !macos && !linux {
		return
	}
	root := os.join_path(os.vtmp_dir(), 'fastc_gettid_declaration_${os.getpid()}')
	os.mkdir_all(root) or { panic(err) }
	defer {
		os.rmdir_all(root) or {}
	}
	// A strict system header can hide syscall; check the actual preamble without its prototype.
	source := '#include <stddef.h>\ntypedef int pid_t;\n#ifndef __linux__\n#define __linux__ 1\n#endif\n#ifndef __GLIBC__\n#define __GLIBC__ 2\n#endif\n#define SYS_gettid 186\n' + c_selfhost_post_directives.all_before('#ifndef memory_order_relaxed')
	path := os.join_path(root, 'gettid.c')
	os.write_file(path, source) or { panic(err) }
	check := cmdexec.run('cc', ['-std=c11', '-D_POSIX_C_SOURCE=200809L',
		'-Werror=implicit-function-declaration', '-fsyntax-only', path])
	assert check.exit_code == 0, check.output
}
