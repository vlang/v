module c

import os
import v.cmdexec

fn test_macos_native_sdk_declarations_survive_cold_and_warm_consumer_cache() ! {
	$if !macos {
		return
	}
	root := os.join_path(os.vtmp_dir(), 'native_sdk_consumer_cache_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	child_env := {
		'VFLAGS':                 ''
		'VEXE':                   @VEXE
		'VTEST_ONLY_FN':          ''
		'VTMP':                   os.join_path(root, 'vtmp')
		'V3CACHE':                os.join_path(root, 'cache')
		'VTOOLS_CACHE_DIR':       os.join_path(root, 'tools')
		'VJOBS':                  '1'
		'V_MACOS_V3_NO_FALLBACK': '1'
	}
	vlib := os.join_path(@VEXEROOT, 'vlib')
	consumer := os.join_path(vlib, 'gg', 'testdata', 'multiwindow_dynamic_texture_consumer_probe.v')
	output := os.join_path(root, 'consumer')
	args := ['-cc', 'clang', '-no-retry-compilation', '-d', 'gg_multiwindow', '-path',
		'${vlib}|@vlib|@vmodules', '-o', output, consumer]
	probe := os.join_path(root, 'probe.c.v')
	os.write_file(probe, 'import sokol.gfx\nfn main() {\n assert !gfx.is_valid()\n println("SDK declarations OK")\n}\n')!
	for mode in ['cold', 'warm', 'uncached'] {
		if mode == 'warm' {
			// The consumer's shader header requires a monolithic build. A supported
			// SDK program publishes gfx's interfaces before the warm consumer reads them.
			seeded := cmdexec.run_in_merged_with_env(@VEXE, ['-cc', 'clang', '-no-retry-compilation',
				'-d', 'gg_multiwindow', '-path', '${vlib}|@vlib|@vmodules', 'run', probe], @VEXEROOT, child_env)
			assert seeded.exit_code == 0, seeded.output
			assert seeded.output.trim_space().ends_with('SDK declarations OK'), seeded.output
			headers := os.walk_ext(child_env['V3CACHE'], '.vh')
			assert headers.any(os.file_name(it).starts_with('gfx_')), seeded.output
		}
		flags := if mode == 'uncached' { ['-nocache'] } else { []string{} }
		compiled := cmdexec.run_in_merged_with_env(@VEXE, [...flags, ...args], @VEXEROOT,
			child_env)
		assert compiled.exit_code == 0, '${mode}: ${compiled.output}'
		assert os.is_executable(output), mode
	}
	// Exercise the linked SDK without opening a window or initializing a renderer.
	// The real consumer above also imports this unrelated shader fixture header.
	os.write_file(probe, 'import sokol.gfx\n#include <stddef.h>\nfn main() {\n assert !gfx.is_valid()\n println("SDK declarations OK")\n}\n')!
	for _ in 0 .. 2 {
		run := cmdexec.run_in_merged_with_env(@VEXE, ['-cc', 'clang', '-no-retry-compilation',
			'-path', '${vlib}|@vlib|@vmodules', 'run', probe], @VEXEROOT, child_env)
		assert run.exit_code == 0, run.output
		assert run.output.trim_space().ends_with('SDK declarations OK'), run.output
	}
}
