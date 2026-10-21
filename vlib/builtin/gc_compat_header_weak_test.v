import os
import rand

const vroot = @VEXEROOT

// main_c includes a libc header before V's compat gc.h, as C shims and V's own C do.
const main_c = '#include <stdio.h>
#include "gc.h"
void v_gc_compat_force_strong(void);
int main(void) {
	v_gc_compat_force_strong();
	GC_noop1_ptr(0);
	return 0;
}
'

// strong_c stands in for a libgc.a that defines GC_noop1_ptr, like current bdwgc does.
// v_gc_compat_force_strong makes the linker load this archive member.
const strong_c = '#include <stdio.h>
void v_gc_compat_force_strong(void) {}
void GC_noop1(unsigned long x) { (void)x; }
void GC_noop1_ptr(volatile void *p) { (void)p; puts("strong GC_noop1_ptr"); }
'

fn run_checked(label string, args []string) os.Result {
	res := os.exec(args)
	assert res.exit_code == 0, '${label} failed:\ncmd: ${args.join(' ')}\noutput:\n${res.output}'
	assert !res.output.contains('defined twice'), '${label} reported a duplicate definition:\n${res.output}'
	return res
}

// test_compat_gc_h_fallback_stays_weak_with_tcc_after_libc_headers checks that the
// GC_noop1_ptr fallback in thirdparty/libgc/include/gc.h stays weak when TinyCC compiles it
// after a glibc header. glibc's <sys/cdefs.h> removes `__attribute__` for TinyCC, which does
// not define __GNUC__ on Linux; the fallback then became a strong definition, and linking it
// against a libgc.a that defines GC_noop1_ptr failed with TinyCC e23dabed and later.
fn test_compat_gc_h_fallback_stays_weak_with_tcc_after_libc_headers() {
	$if !linux {
		return
	}
	tcc_dir := os.join_path(vroot, 'thirdparty', 'tcc')
	tcc := os.join_path(tcc_dir, 'tcc.exe')
	tcc_lib := os.join_path(tcc_dir, 'lib', 'tcc')
	if !os.is_file(tcc) || !os.is_dir(tcc_lib) {
		eprintln('skipping: no bundled Linux TinyCC in ${tcc_dir}')
		return
	}
	if os.exec([tcc, '-v']).exit_code != 0 {
		eprintln('skipping: ${tcc} does not run on this host')
		return
	}
	workdir := os.join_path(os.vtmp_dir(), 'v_gc_compat_weak_${rand.ulid()}')
	os.mkdir_all(workdir) or { panic(err) }
	defer {
		os.rmdir_all(workdir) or {}
	}
	main_src := os.join_path(workdir, 'main.c')
	strong_src := os.join_path(workdir, 'strong.c')
	strong_obj := os.join_path(workdir, 'strong.o')
	strong_lib := os.join_path(workdir, 'libstrong.a')
	exe := os.join_path(workdir, 'main')
	os.write_file(main_src, main_c) or { panic(err) }
	os.write_file(strong_src, strong_c) or { panic(err) }
	tcc_flags := ['-B${tcc_lib}', '-I${os.join_path(tcc_lib, 'include')}', '-L${tcc_lib}']
	run_checked('compiling the strong definition', [tcc, ...tcc_flags, '-c', '-o', strong_obj,
		strong_src])
	run_checked('archiving the strong definition', [tcc, '-ar', 'rcs', strong_lib, strong_obj])
	run_checked('linking the compat gc.h fallback against the archive', [tcc, ...tcc_flags,
		'-DGC_BUILTIN_ATOMIC=1', '-I${os.join_path(vroot, 'thirdparty', 'libgc', 'include')}',
		'-o', exe, main_src, strong_lib])
	res := run_checked('running the linked program', [exe])
	assert res.output.trim_space() == 'strong GC_noop1_ptr'
}
