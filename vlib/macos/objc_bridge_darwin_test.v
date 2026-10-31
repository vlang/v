module macos

import os
import v.cmdexec

fn test_tcc_bridge_accepts_sdk_with_unguarded_block_declaration() ! {
	tcc_root := os.join_path(@VEXEROOT, 'thirdparty', 'tcc')
	tcc := os.join_path(tcc_root, 'tcc.exe')
	sdk := cmdexec.macos_sdk_root()
	if !os.is_file(tcc) || sdk == '' {
		eprintln('skipping: bundled tcc and a macOS SDK are required')
		return
	}
	dir := os.join_path(os.vtmp_dir(), 'objc_bridge_blocks_${os.getpid()}')
	os.mkdir_all(os.join_path(dir, 'objc'))!
	defer { os.rmdir_all(dir) or {} }
	// Older SDKs expose this Blocks declaration even to plain C compilers.
	os.write_file(os.join_path(dir, 'objc', 'runtime.h'), '#ifndef V_TEST_OBJC_RUNTIME_H\n#define V_TEST_OBJC_RUNTIME_H\n#include_next <objc/runtime.h>\nvoid objc_enumerateClasses(const void *image, const char *namePrefix, Protocol *conformingTo, Class subclassing, void (^block)(Class aClass, BOOL *stop));\n#endif\n')!
	source := os.join_path(dir, 'bridge.c')
	os.write_file(source, '#include "${@VEXEROOT}/vlib/macos/objc_bridge.h"\n#include <objc/runtime.h>\n#ifdef objc_enumerateClasses\n#error bridge leaked objc_enumerateClasses macro\n#endif\n')!
	build := cmdexec.run(tcc, ['-B${tcc_root}', '-I${tcc_root}/include', '-I${dir}',
		'-I${sdk}/usr/include', '-c', source, '-o', os.join_path(dir, 'bridge.o')])
	assert build.exit_code == 0, build.output
}
