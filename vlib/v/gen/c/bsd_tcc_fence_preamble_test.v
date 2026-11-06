module c

import os
import v.cmdexec

fn test_bsd_tcc_fence_with_and_without_bundled_stdatomic_alias() ! {
	$if !amd64 {
		return
	}
	tcc := os.join_path(@VEXEROOT, 'thirdparty', 'tcc', 'tcc.exe')
	if !os.is_file(tcc) {
		return
	}
	dir := os.join_path(os.vtmp_dir(), 'bsd_tcc_fence_' + os.getpid().str())
	os.mkdir_all(dir)!
	defer { os.rmdir_all(dir) or {} }
	mut g := FlatGen.new()
	g.atomic_thread_fence_compat_decls()
	code := g.sb.str()
	for platform in ['__FreeBSD__', '__OpenBSD__'] {
		for has_header in [false, true] {
			// TinyCC's bundled stdatomic.h declares the public symbol using an enum
			// and maps the builtin spelling to it. Do not redeclare it as int or
			// create a circular alias by defining the public spelling back to it.
			prefix := if has_header {
				'typedef enum { relaxed = 0, sequential = 5 } memory_order;
extern void atomic_thread_fence(memory_order order);
#define __atomic_thread_fence(order) atomic_thread_fence(order)
'
			} else {
				''
			}
			source := os.join_path(dir, platform + '_' + has_header.str() + '.c')
			os.write_file(source, prefix + code + '
void probe_fences(void) {
	atomic_thread_fence(memory_order_seq_cst);
	__atomic_thread_fence(memory_order_relaxed);
}
')!
			result := cmdexec.run(tcc, ['-U__linux__', '-U__APPLE__', '-U_WIN32', '-U__FreeBSD__',
				'-U__OpenBSD__', '-D' + platform, '-Werror', '-c', source, '-o',
				os.join_path(dir, 'fence.o')])
			assert result.exit_code == 0, '${platform}, header=${has_header}: ${result.output}'
		}
	}
}
