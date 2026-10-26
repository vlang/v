module c

import os
import v.cmdexec
import v.flat
import v.pref
import v.types

fn test_core_header_ownership_preserves_application_and_thirdparty_headers() {
	mut a := flat.FlatAst{}
	mut g := FlatGen.new()
	g.a = &a
	g.compiler_vroot = '/vroot'
	g.set_target(pref.host_target())
	assert g.core_native_header_is_redeclared('/vroot/vlib/builtin/builtin_windows.c.v', '"/vroot/vlib/builtin/core_windows.h"')
	assert g.core_native_header_is_redeclared('/vroot/vlib/time/time_windows.c.v', '"/vroot/vlib/time/ticks_windows_api.h"')
	assert !g.core_native_header_is_redeclared('/vroot/vlib/builtin/builtin_windows.c.v', '"/project/vlib/builtin/core_windows.h"')
	assert !g.core_native_header_is_redeclared('/vroot/vlib/time/time_windows.c.v', '"/project/vlib/time/ticks_windows_api.h"')
	assert !g.core_native_header_is_redeclared('/project/builtin/builtin_windows.c.v', '"/vroot/vlib/builtin/core_windows.h"')
	assert !g.core_native_header_is_redeclared('/project/time/time_windows.c.v', '"/vroot/vlib/time/ticks_windows_api.h"')
	assert g.core_native_header_is_redeclared('/vroot/vlib/time/timerfd.c.v', '<sys/timerfd.h>')
	assert g.core_native_header_is_redeclared('/vroot/vlib/os/mmap_nix.c.v', '<sys/mman.h>')
	assert g.core_native_header_is_redeclared('/vroot/vlib/v/driver/replace_file_windows.c.v', '<windows.h>')
	assert !g.core_native_header_is_redeclared('/project/v/driver/replace_file_windows.c.v', '<windows.h>')
	assert g.core_native_header_is_redeclared('/vroot/vlib/v/workers/thread_nix.c.v', '<sys/prctl.h>')
	assert g.core_native_header_is_redeclared('/vroot/vlib/v/workers/thread_nix.c.v', '"/vroot/vlib/v/pthread_helper.h"')
	assert !g.core_native_header_is_redeclared('/vroot/vlib/v/workers/thread_nix.c.v', '"/project/vlib/v/pthread_helper.h"')
	assert !g.core_native_header_is_redeclared('/project/v/workers/thread_nix.c.v', '"/vroot/vlib/v/pthread_helper.h"')
	assert g.core_native_header_is_redeclared('/vroot/vlib/v/cmdexec/cmdexec_pipe.c.v', '"/vroot/vlib/v/cmdexec/cmdexec_poll.h"')
	assert !g.core_native_header_is_redeclared('/project/v/cmdexec/cmdexec_pipe.c.v', '"/vroot/vlib/v/cmdexec/cmdexec_poll.h"')
	assert !g.core_native_header_is_redeclared('/vroot/vlib/v/cmdexec/cmdexec_pipe.c.v', '"/project/vlib/v/cmdexec/cmdexec_poll.h"')
	assert !g.core_native_header_is_redeclared('/vroot/vlib/v/cmdexec/cmdexec_pipe.c.v', '<poll.h>')
	a.cached_header_sources['/cache/cmdexec.vh'] = '/vroot/vlib/v/cmdexec/cmdexec_pipe.c.v'
	assert g.core_native_header_is_redeclared('/cache/cmdexec.vh', '"/vroot/vlib/v/cmdexec/cmdexec_poll.h"')
	assert g.core_native_header_is_redeclared('/vroot/vlib/v/gen/fastc/unit_compile_nix.c.v', '<signal.h>')
	assert !g.core_native_header_is_redeclared('/project/v/gen/fastc/unit_compile_nix.c.v', '<signal.h>')
	assert !g.core_native_header_is_redeclared('/project/time/timerfd.c.v', '<sys/timerfd.h>')
	assert !g.core_native_header_is_redeclared('/vroot/vlib/time/timerfd.c.v', '<custom.h>')
	assert !g.core_native_header_is_redeclared('/vroot/vlib/compress/brotli/brotli.c.v', '<stdio.h>')
	assert !g.core_native_header_is_redeclared('/vroot/vlib/builtin/custom_test.c.v', '<stdio.h>')
	a.cached_header_sources['/cache/worker.vh'] = '/vroot/vlib/v/workers/thread_nix.c.v'
	assert g.core_native_header_is_redeclared('/cache/worker.vh', '<sys/prctl.h>')
	assert g.c_source_file_is_in_vlib('/cache/worker.vh')
	g.output_cross_c = true
	assert !g.core_native_header_is_redeclared('/cache/worker.vh', '<sys/prctl.h>')
	assert !g.core_native_header_is_redeclared('/cache/cmdexec.vh', '"/vroot/vlib/v/cmdexec/cmdexec_poll.h"')
	g.output_cross_c = false
	g.target_libc_headers = true
	assert !g.core_native_header_is_redeclared('/cache/worker.vh', '<sys/prctl.h>')
	g.target_libc_headers = false
	for sdk_mode in ['object', 'no_builtin'] {
		g.object_file_mode = sdk_mode == 'object'
		g.no_builtin = sdk_mode == 'no_builtin'
		assert !g.core_native_header_is_redeclared('/cache/cmdexec.vh', '"/vroot/vlib/v/cmdexec/cmdexec_poll.h"')
	}
	assert !g.core_native_header_is_redeclared('/cache/cmdexec.vh', '"/vroot/vlib/v/cmdexec/cmdexec_poll.h"')
}

fn test_core_headers_are_restored_after_an_unrelated_explicit_sdk_header() {
	mut a := flat.FlatAst{}
	mut g := FlatGen.new()
	g.a = &a
	g.compiler_vroot = '/vroot'
	source := '/vroot/vlib/v/workers/thread_nix.c.v'
	g.core_header_directives << DeferredCoreHeader{
		source_file: source
		directive:   CDirective{ module: 'workers', text: '#include <sys/prctl.h>' }
	}
	g.restore_core_header_directives_for_system_libc()
	assert g.c_directives.len == 0
	assert !g.files_with_c_includes[source]
	// The user header can be collected after the core file.
	g.add_c_directive('main', '#include <stdio.h>', false)
	g.restore_core_header_directives_for_system_libc()
	assert g.c_directives.len == 2
	assert g.c_directives[1].text == '#include <sys/prctl.h>'
	assert g.files_with_c_includes[source]
	assert g.core_header_directives.len == 0
	g.files_linking_c_sources[source] = true
	assert g.should_emit_c_extern_decl_from_file('v3_linux_name_worker', source, 'workers')
}

fn test_unverified_libc_abis_keep_native_sdk_declarations() {
	mut a := flat.FlatAst{}
	mut g := FlatGen.new()
	g.a = &a
	for target in ['netbsd', 'solaris', 'haiku', 'qnx', 'serenity', 'vinix', 'wasm32_emscripten'] {
		g.set_target(pref.target_from(target, if target == 'wasm32_emscripten' {
			'wasm32'
		} else {
			'amd64'
		}) or { panic(err) })
		assert g.c_directives_use_system_libc(), target
		assert !g.core_native_header_is_redeclared('/vroot/vlib/v/cmdexec/cmdexec_pipe.c.v', '"/vroot/vlib/v/cmdexec/cmdexec_poll.h"'), target
	}
}

fn test_compiler_bootstrap_keeps_core_declarations_in_v() {
	mut ast := flat.FlatAst.new()
	mut tc := types.TypeChecker.new(&ast)
	mut prefs := pref.new_preferences()
	prefs.target = pref.host_target()
	prefs.output_cross_c = true
	prefs.user_defines = ['v3_headerless_bootstrap']
	assert !uses_manual_stdlib_c_headers(&ast, &tc, prefs, [], false)
	mut g := FlatGen.new()
	g.a = &ast
	g.compiler_vroot = '/vroot'
	g.set_output_cross_c(true)
	g.set_compile_defines(prefs.user_defines)
	assert !g.c_directives_use_system_libc()
	assert g.core_native_header_is_redeclared('/vroot/vlib/v/workers/thread_nix.c.v', '"/vroot/vlib/v/pthread_helper.h"')
	assert g.core_native_header_is_redeclared('/vroot/vlib/sync/sync_freebsd.c.v', '<pthread_np.h>')
	g.core_header_directives << DeferredCoreHeader{
		source_file: '/vroot/vlib/sync/sync_freebsd.c.v'
		directive:   CDirective{ module: 'sync', text: '#include <pthread_np.h>' }
	}
	g.restore_core_header_directives_for_system_libc()
	assert g.c_directives.len == 0
	// Other portable consumers still require the complete compatibility ABI.
	g.set_compile_defines([])
	assert g.c_directives_use_system_libc()
	assert !g.core_native_header_is_redeclared('/vroot/vlib/sync/sync_freebsd.c.v', '<pthread_np.h>')
	for object_file_mode in [false, true] {
		prefs.no_builtin = !object_file_mode
		assert uses_manual_stdlib_c_headers(&ast, &tc, prefs, [], object_file_mode)
	}
}

fn test_freebsd_bootstrap_constants_match_native_sdk() {
	$if !freebsd || freestanding {
		return
	}
	mut g := FlatGen.new()
	g.headerless_freebsd_constants()
	constants := ['O_NOFOLLOW', 'O_DIRECTORY', 'AT_FDCWD', 'AT_REMOVEDIR', '_SC_NPROCESSORS_ONLN',
		'_SC_PHYS_PAGES']
	mut code := '#include <fcntl.h>\n#include <unistd.h>\n'
	for name in constants {
		code += 'enum { SDK_${name} = ${name} };\n#undef ${name}\n'
	}
	for line in g.sb.str().split_into_lines() {
		if line.starts_with('#define ') {
			name := line.all_after('#define ').all_before(' ')
			if name in constants {
				code += '${line}\n'
			}
		}
	}
	for name in constants {
		code += '_Static_assert(${name} == SDK_${name}, "${name} value");\n'
	}
	dir := os.join_path(os.vtmp_dir(), 'freebsd_bootstrap_sdk_${os.getpid()}')
	os.mkdir_all(dir)!
	defer { os.rmdir_all(dir) or {} }
	source := os.join_path(dir, 'sdk.c')
	os.write_file(source, code)!
	result := cmdexec.run('cc', ['-std=c11', '-Werror', '-fsyntax-only', source])
	assert result.exit_code == 0, result.output
}

fn test_headerless_cmdexec_poll_uses_bionic_nfds_type() ! {
	cc := os.find_abs_path_of_executable('clang') or { return }
	root := os.join_path(os.vtmp_dir(), 'cmdexec_bionic_poll_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	source := os.join_path(root, 'poll.c')
	os.write_file(source, '#include <stddef.h>\n' + headerless_cmdexec_poll +
		'\n_Static_assert(__builtin_types_compatible_p(v_cmdexec_nfds_t, unsigned int), "Bionic nfds_t");\n' +
		'_Static_assert(__builtin_types_compatible_p(__typeof__(&v_cmdexec_poll_sys), int (*)(struct v_cmdexec_pollfd*, unsigned int, int)), "Bionic poll prototype");\n')!
	// Validate the emitted declaration without claiming execution on an Android device.
	checked := cmdexec.run(cc, ['-std=gnu11', '-D__ANDROID__', '-U__APPLE__', '-Werror', '-fsyntax-only',
		source])
	assert checked.exit_code == 0, checked.output
}

fn test_vectored_output_header_is_redeclared_only_for_the_shipped_binding() {
	mut a := flat.FlatAst{}
	mut g := FlatGen.new()
	g.a = &a
	g.compiler_vroot = '/vroot'
	g.set_target(pref.target_from('linux', 'amd64')!)
	source := '/vroot/vlib/v/gen/c/output_nix.c.v'
	assert g.core_native_header_is_redeclared(source, '<sys/uio.h>')
	assert !g.core_native_header_is_redeclared(source, '<custom.h>')
	assert !g.core_native_header_is_redeclared('/project/v/gen/c/output_nix.c.v', '<sys/uio.h>')
	a.cached_header_sources['/cache/cgen.vh'] = source
	assert g.core_native_header_is_redeclared('/cache/cgen.vh', '<sys/uio.h>')
	// General portable output still uses SDK headers; compiler bootstrap output does not.
	g.output_cross_c = true
	assert !g.core_native_header_is_redeclared(source, '<sys/uio.h>')
	g.set_compile_defines(['v3_headerless_bootstrap'])
	assert g.core_native_header_is_redeclared(source, '<sys/uio.h>')
	g.output_cross_c = false
	g.set_compile_defines([])
	g.target_libc_headers = true
	assert !g.core_native_header_is_redeclared(source, '<sys/uio.h>')
	g.target_libc_headers = false
	g.object_file_mode = true
	assert !g.core_native_header_is_redeclared(source, '<sys/uio.h>')
	g.object_file_mode = false
	g.no_builtin = true
	assert !g.core_native_header_is_redeclared(source, '<sys/uio.h>')
}

fn test_vectored_output_header_preserves_native_context_and_sdk_ownership() {
	mut a := flat.FlatAst.new()
	mut tc := types.TypeChecker.new(&a)
	mut g := FlatGen.new()
	g.a = &a
	g.tc = &tc
	g.compiler_vroot = '/vroot'
	g.set_target(pref.target_from('linux', 'amd64')!)
	source := '/vroot/vlib/v/gen/c/output_nix.c.v'
	g.register_struct_decl_info('C.iovec', 'C.iovec', 'c', source, flat.Node{})
	include_node := flat.Node{
		kind:  .directive
		value: 'include'
		typ:   flat.source_type_id('<sys/uio.h>')
	}
	assert g.collect_c_directive('c', include_node, source, false)
	assert !g.c_directives_use_system_libc()
	assert g.c_directives.len == 0
	assert g.core_header_directives.len == 1
	assert !g.skip_builtin_struct('C.iovec')
	assert g.should_emit_c_extern_decl_from_file('writev', source, 'c')
	context := g.native_source_contexts['c'] or { panic('missing native C context') }
	assert context.any(it.text == '#include <sys/uio.h>')
	// A later application header selects the SDK and restores its iovec/writev ownership.
	g.add_c_directive('main', '#include <stdio.h>', false)
	g.restore_core_header_directives_for_system_libc()
	assert g.c_directives_use_system_libc()
	assert g.core_header_directives.len == 0
	assert g.c_directives.len == 2
	assert g.c_directives[1].text == '#include <sys/uio.h>'
	assert g.files_with_c_includes[source]
	assert g.skip_builtin_struct('C.iovec')
	assert !g.should_emit_c_extern_decl_from_file('writev', source, 'c')
	// The same filename outside this compiler's vlib always keeps the user's include.
	mut user := FlatGen.new()
	user.a = &a
	user.tc = &tc
	user.compiler_vroot = '/vroot'
	user.set_target(pref.target_from('linux', 'amd64')!)
	assert user.collect_c_directive('main', include_node, '/project/v/gen/c/output_nix.c.v', false)
	assert user.core_header_directives.len == 0
	assert user.c_directives.len == 1
	assert user.c_directives[0].text == '#include <sys/uio.h>'
	assert user.c_directives_use_system_libc()
}
