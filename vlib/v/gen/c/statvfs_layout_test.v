module c

import os
import v.cmdexec

fn test_headerless_statvfs_matches_native_sdk() {
	$if !macos && !linux {
		return
	}
	mut g := FlatGen.new()
	g.headerless_statvfs_struct()
	fields := ['f_bsize', 'f_frsize', 'f_blocks', 'f_bfree', 'f_bavail', 'f_files', 'f_ffree',
		'f_favail', 'f_fsid', 'f_flag', 'f_namemax']
	mut code := '#define statvfs SDK_statvfs\n#include <sys/statvfs.h>\n#include <stddef.h>\n#undef statvfs\ntypedef unsigned int u32;\n' + g.sb.str()
	code += '_Static_assert(sizeof(struct statvfs) == sizeof(struct SDK_statvfs), "statvfs size");\n'
	code += '_Static_assert(_Alignof(struct statvfs) == _Alignof(struct SDK_statvfs), "statvfs alignment");\n'
	for field in fields {
		code += '_Static_assert(offsetof(struct statvfs, ${field}) == offsetof(struct SDK_statvfs, ${field}), "${field} offset");\n'
		code += '_Static_assert(sizeof(((struct statvfs*)0)->${field}) == sizeof(((struct SDK_statvfs*)0)->${field}), "${field} width");\n'
	}
	dir := os.join_path(os.vtmp_dir(), 'statvfs_sdk_${os.getpid()}')
	os.mkdir_all(dir)!
	defer { os.rmdir_all(dir) or {} }
	source := os.join_path(dir, 'sdk.c')
	os.write_file(source, code)!
	result := cmdexec.run('cc', ['-std=c11', '-Werror', '-fsyntax-only', source])
	assert result.exit_code == 0, result.output
}
