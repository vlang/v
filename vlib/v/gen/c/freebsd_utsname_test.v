module c

import os
import v.cmdexec

fn test_headerless_freebsd_uname_uses_current_field_width() ! {
	cc := os.find_abs_path_of_executable('cc') or { return }
	dir := os.join_path(os.vtmp_dir(), 'freebsd_utsname_' + os.getpid().str())
	os.mkdir_all(dir)!
	defer { os.rmdir_all(dir) or {} }
	mut g := FlatGen.new()
	g.headerless_utsname_struct()
	source := os.join_path(dir, 'probe.c')
	os.write_file(source, '#ifndef __FreeBSD__\n#define __FreeBSD__ 1\n#endif\n' + g.sb.str() + "
int uname(struct utsname*);
static int calls, field_width, status;
static void* buffer;
int __xuname(int width, void* name) {
	char* fields = name;
	calls++;
	field_width = width;
	buffer = name;
	for (int i = 0; i < 5; i++) fields[i * width] = (char)('A' + i);
	return status;
}
int main(void) {
	struct utsname name = {0};
	if (uname(&name) != 0) return 1;
	if (calls != 1 || field_width != 256 || buffer != &name) return 2;
	if (name.sysname[0] != 'A' || name.nodename[0] != 'B' ||
		name.release[0] != 'C' || name.version[0] != 'D' ||
		name.machine[0] != 'E') return 3;
	status = -1;
	int (*query)(struct utsname*) = uname;
	if (query(&name) != -1 || calls != 2) return 4;
	return 0;
}
")!
	// Model the exported legacy libc symbol separately from the header's wrapper.
	// Without the wrapper, this symbol fills fields at offsets for the 32-byte ABI.
	legacy := os.join_path(dir, 'legacy.c')
	os.write_file(legacy, '
struct utsname;
int __xuname(int, void*);
int uname(struct utsname* name) { return __xuname(32, name); }
')!
	executable := os.join_path(dir, 'probe' + $if windows { '.exe' } $else { '' })
	// Keep the mock symbols separate from the host C library on every platform.
	built := cmdexec.run(cc, ['-std=c99', '-Werror', '-Duname=v_test_uname', '-D__xuname=v_test_xuname',
		source, legacy, '-o', executable])
	assert built.exit_code == 0, built.output
	ran := cmdexec.run(executable, [])
	assert ran.exit_code == 0, 'exit ${ran.exit_code}: ${ran.output}'
}

fn test_headerless_freebsd_uname_wrapper_is_left_out_of_freestanding_builds() {
	mut g := FlatGen.new()
	g.compile_defines << 'freestanding'
	g.headerless_utsname_struct()
	out := g.sb.str()
	assert out.contains('struct utsname')
	assert !out.contains('__xuname'), out
}
