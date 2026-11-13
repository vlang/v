module driver

import os
import v.cmdexec

fn test_tcc_cached_consumer_keeps_sokol_features_and_omits_native_implementation() {
	for define in ['SOKOL_TRACE_HOOKS', 'SOKOL_TRACE_HOOKS=1', 'SOKOL_NO_DEPRECATED', 'SOKOL_DLL',
		'SOKOL_GFX_API_DECL=extern', 'SOKOL_ASSERT(c)=assert(c)', 'SOKOL_GLCORE_FEATURE=1'] {
		for flags in [['-D${define}'], ['-D', define]] {
			assert tcc_cached_main_flags(flags) == flags, flags.str()
		}
	}
	for define in ['SOKOL_IMPL', 'SOKOL_GFX_IMPL=1', 'SOKOL_APP_IMPL', 'SOKOL_GL_IMPL', 'SOKOL_GLCORE',
		'SOKOL_GLCORE33', 'SOKOL_GLES2', 'SOKOL_GLES3', 'SOKOL_D3D11', 'SOKOL_METAL', 'SOKOL_WGPU',
		'SOKOL_VULKAN', 'SOKOL_DUMMY_BACKEND', 'SOKOL_NOAPI'] {
		for flags in [['-D${define}'], ['-D', define]] {
			assert tcc_cached_main_flags(flags).len == 0, flags.str()
		}
	}
	assert tcc_cached_main_flags(['-USOKOL_TRACE_HOOKS', '-U', 'SOKOL_METAL']) == [
		'-USOKOL_TRACE_HOOKS',
		'-U',
		'SOKOL_METAL',
	]
}

fn test_tcc_cached_consumer_can_install_the_real_sokol_trace_helper() ! {
	cc := os.find_abs_path_of_executable($if windows { 'gcc' } $else { 'cc' }) or { return }
	root := os.join_path(os.vtmp_dir(), 'tcc_sokol_trace_flags_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	implementation := os.join_path(root, 'implementation.c')
	consumer := os.join_path(root, 'consumer.c')
	object := os.join_path(root, 'implementation.o')
	executable := os.join_path(root, 'consumer' + $if windows { '.exe' } $else { '' })
	os.write_file(implementation, '#include "sokol_gfx.h"\n')!
	os.write_file(consumer, '#if defined(SOKOL_IMPL) || defined(SOKOL_DUMMY_BACKEND)\n#error native implementation flags escaped to the consumer\n#endif\n#include "sokol_gfx.h"\n#include "trace_helpers.h"\nint main(void) {\n    sg_desc desc = {0};\n    sg_setup(&desc);\n    if (!v_multiwindow_trace_install()) return 11;\n    sg_commit();\n    if (!strstr(v_multiwindow_trace_snapshot(), "commit")) return 12;\n    v_multiwindow_trace_uninstall();\n    sg_shutdown();\n    return 0;\n}\n')!
	includes := ['-I', os.join_path(@VEXEROOT, 'thirdparty', 'sokol'), '-I',
		os.join_path(@VEXEROOT, 'vlib', 'gg', 'testdata', 'multiwindow_sokol_trace')]
	for defines in [['-DSOKOL_IMPL', '-DSOKOL_DUMMY_BACKEND', '-DSOKOL_TRACE_HOOKS'],
		['-D', 'SOKOL_IMPL', '-D', 'SOKOL_DUMMY_BACKEND', '-D', 'SOKOL_TRACE_HOOKS']] {
		mut flags := includes.clone()
		flags << defines
		mut native_args := ['-std=c11', '-Werror']
		native_args << flags
		native_args << ['-c', implementation, '-o', object]
		native := cmdexec.run(cc, native_args)
		assert native.exit_code == 0, native.output
		mut consumer_args := ['-std=c11', '-Werror']
		consumer_args << tcc_cached_main_flags(flags)
		consumer_args << [consumer, object, '-o', executable]
		compiled := cmdexec.run(cc, consumer_args)
		assert compiled.exit_code == 0, compiled.output
		run := cmdexec.run(executable, [])
		assert run.exit_code == 0, 'trace helper exited ${run.exit_code}: ${run.output}'
	}
}
