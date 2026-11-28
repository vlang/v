import os

// A `readdir` that clears what the listing says an entry is, as a file system does that does not
// say it. ext4 and tmpfs say it for every entry, so no test would meet such a listing otherwise.
const no_kinds_wrapper = '#define _GNU_SOURCE
#include <dirent.h>
#include <dlfcn.h>
struct dirent *readdir(DIR *d) {
	static struct dirent *(*real)(DIR *);
	if (!real) real = dlsym(RTLD_NEXT, "readdir");
	struct dirent *e = real(d);
	if (e) e->d_type = DT_UNKNOWN;
	return e;
}
struct dirent64 *readdir64(DIR *d) {
	static struct dirent64 *(*real)(DIR *);
	if (!real) real = dlsym(RTLD_NEXT, "readdir64");
	struct dirent64 *e = real(d);
	if (e) e->d_type = DT_UNKNOWN;
	return e;
}
'

// What walk_linux_test.c.v prints first when it runs with this variable set and sees that no
// entry of a listing has a kind.
const no_kinds_env = 'V_WALK_TEST_NO_KINDS'
const no_kinds_marker = 'the listings of this run say no kind'

// The tests of the walk pass as well where no listing says what an entry is, which is where a
// walk on Linux looks every entry up by name. They are built and run again for that, with a
// `readdir` that clears the kind of every entry.
fn test_the_walk_tests_pass_where_no_listing_says_a_kind() {
	$if !linux {
		eprintln('> skipping ${@FN}: it is for the walk on Linux')
		return
	}
	cc := os.find_abs_path_of_executable('cc') or {
		eprintln('> skipping ${@FN}: there is no C compiler `cc` to build the wrapper with')
		return
	}
	folder := os.join_path(os.vtmp_dir(), 'walk_no_kinds_${os.getpid()}')
	os.rmdir_all(folder) or {}
	os.mkdir_all(folder)!
	defer {
		os.rmdir_all(folder) or {}
	}
	source := os.join_path_single(folder, 'no_kinds.c')
	wrapper := os.join_path_single(folder, 'no_kinds.so')
	os.write_file(source, no_kinds_wrapper)!
	built := os.exec([cc, '-shared', '-fPIC', '-o', wrapper, source, '-ldl'])
	if built.exit_code != 0 {
		eprintln('> skipping ${@FN}: ${cc} does not build a shared library: ${built.output}')
		return
	}
	for name in ['walk_linux_test.c.v', 'walk_links_test.v'] {
		exe := os.join_path_single(folder, name + '.bin')
		build := os.exec([@VEXE, '-o', exe, os.join_path(@VEXEROOT, 'vlib', 'os', name)])
		assert build.exit_code == 0, '${name}: ${build.output}'
		run := os.exec(['/usr/bin/env', '${no_kinds_env}=1', 'LD_PRELOAD=${wrapper}', exe])
		if name == 'walk_linux_test.c.v' && !run.output.contains(no_kinds_marker) {
			// The wrapper is not loaded into a program that is linked statically, or against
			// another C library than the one `cc` uses.
			eprintln('> skipping ${@FN}: the wrapper has no effect on this build: ${run.output}')
			return
		}
		assert run.exit_code == 0, '${name}: ${run.output}'
	}
}
