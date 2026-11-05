module gfx

import os

fn test_windows_glcore_image_transfer_compiles() {
	compiler := os.find_abs_path_of_executable('x86_64-w64-mingw32-gcc') or {
		$if windows {
			os.find_abs_path_of_executable('gcc') or {
				eprintln('skipping Windows GLCORE compilation: no GCC available')
				return
			}
		} $else {
			eprintln('skipping Windows GLCORE compilation: no MinGW cross compiler available')
			return
		}
	}
	root := os.join_path(os.vtmp_dir(), 'sokol_gl_transfer_${os.getpid()}')
	os.mkdir_all(root)!
	defer {
		os.rmdir_all(root) or {}
	}
	probe := os.join_path(root, 'transfer.c')
	os.write_file(probe, '#include <stdio.h>\n#define SOKOL_IMPL\n#define SOKOL_GLCORE\n#include "sokol_gfx.h"\n')!
	result := os.exec([compiler, '-fsyntax-only', '-I', os.join_path(@VEXEROOT, 'thirdparty',
		'sokol'), probe])
	assert result.exit_code == 0, result.output
}
