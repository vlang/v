import os

// Every `#flag -D...` of a V program also reaches the C build of the bundled
// libgc. Programs use these macros to trim `windows.h` (for example raylib
// bindings, whose names clash with the Win32 API), so `gc.c` must compile with them.
const windows_header_macro_sets = [['-DNOUSER'], ['-DNOMSG'], ['-DNOUSER', '-DNOGDI']]

fn windows_target_gcc() ?string {
	name := $if windows { 'gcc' } $else { 'x86_64-w64-mingw32-gcc' }
	return os.find_abs_path_of_executable(name) or { return none }
}

fn test_bundled_libgc_compiles_with_windows_header_exclusion_macros() {
	gcc := windows_target_gcc() or { return }
	libgc_dir := os.join_path(@VEXEROOT, 'thirdparty', 'libgc')
	gc_source := os.join_path(libgc_dir, 'gc.c')
	if !os.is_file(gc_source) {
		return
	}
	include_dir := os.join_path(libgc_dir, 'include')
	// The defines that builtin_d_gcboehm.c.v passes for a Windows build with gcc.
	gc_defines := '-DGC_THREADS=1 -DTHREAD_LOCAL_ALLOC=1 -DGC_NOT_DLL=1 -DGC_WIN32_THREADS=1 -DNO_MSGBOX_ON_ERROR=1 -DCONSOLE_LOG=1 -DGC_BUILTIN_ATOMIC=1 -DALL_INTERIOR_POINTERS=1'
	for macros in windows_header_macro_sets {
		cmd := '${os.quoted_path(gcc)} -fsyntax-only -w ${gc_defines} -I ${os.quoted_path(include_dir)} ${macros.join(' ')} ${os.quoted_path(gc_source)}'
		res := os.exec([gcc, '-fsyntax-only', '-w', ...(os.split_args(gc_defines) or { panic(err) }),
			'-I', include_dir, ...macros, gc_source])
		assert res.exit_code == 0, '${cmd}\n${res.output}'
	}
}
