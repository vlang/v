module c

import os
import v.cmdexec

// OpenBSD keeps `__st_birthtim` after `st_gen`, at the end of `struct stat`.
// With it before `st_size`, `st_size` read `st_blksize | st_flags << 32`
// (16384 for most files), which truncated every source file bigger than
// 16 KiB that the parser read through `fstat`.
fn test_headerless_openbsd_stat_keeps_birthtime_last() {
	mut g := FlatGen.new()
	g.headerless_openbsd_stat_struct()
	c_code := g.sb.str()
	assert c_code.contains('long st_ctimensec; i64 st_size; i64 st_blocks;'), c_code
	assert c_code.trim_space().ends_with('u32 st_gen; i64 __st_birthtime; long __st_birthtimensec; };'), c_code
}

fn test_headerless_openbsd_stat_matches_lp64_offsets() {
	$if !macos && !linux {
		return
	}
	mut g := FlatGen.new()
	g.headerless_openbsd_stat_struct()
	mut code := '#include <stddef.h>\ntypedef unsigned int u32;\ntypedef int i32;\ntypedef unsigned long long u64;\ntypedef long long i64;\n' +
		g.sb.str()
	// Offsets of the OpenBSD amd64/arm64 `struct stat` from <sys/stat.h>.
	code += '#if __SIZEOF_POINTER__ == 8\n'
	for field, offset in {
		'st_mode':        0
		'st_dev':         4
		'st_ino':         8
		'st_nlink':       16
		'st_uid':         20
		'st_gid':         24
		'st_rdev':        28
		'st_atime':       32
		'st_mtime':       48
		'st_ctime':       64
		'st_size':        80
		'st_blocks':      88
		'st_blksize':     96
		'st_flags':       100
		'st_gen':         104
		'__st_birthtime': 112
	} {
		code += '_Static_assert(offsetof(struct stat, ${field}) == ${offset}, "${field} offset");\n'
	}
	code += '_Static_assert(sizeof(struct stat) == 128, "stat size");\n#endif\n'
	dir := os.join_path(os.vtmp_dir(), 'openbsd_stat_${os.getpid()}')
	os.mkdir_all(dir)!
	defer { os.rmdir_all(dir) or {} }
	source := os.join_path(dir, 'stat.c')
	os.write_file(source, code)!
	result := cmdexec.run('cc', ['-std=c11', '-Werror', '-fsyntax-only', source])
	assert result.exit_code == 0, result.output
}
