module c

import v.flat
import v.pref

fn test_core_header_ownership_preserves_application_and_thirdparty_headers() {
	mut a := flat.FlatAst{}
	mut g := FlatGen.new()
	g.a = &a
	g.compiler_vroot = '/vroot'
	assert g.core_native_header_is_redeclared('/vroot/vlib/time/timerfd.c.v', '<sys/timerfd.h>')
	assert g.core_native_header_is_redeclared('/vroot/vlib/v/workers/thread_nix.c.v', '<sys/prctl.h>')
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
	g.output_cross_c = false
	g.target_libc_headers = true
	assert !g.core_native_header_is_redeclared('/cache/worker.vh', '<sys/prctl.h>')
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
	}
}
