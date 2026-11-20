module ecdsa

import os
import rand

// A program that brings its own C (here a shim reporting which Mbed TLS
// headers it compiled against) next to an installed system Mbed TLS must
// not see V's vendored 3.6 headers just because it imports crypto.ecdsa
// (#32422). The fake "system" tree below stands in for /usr/include on
// hosts without one: the vendored include dirs used to come first, so the
// shim reported the vendored major (3); now the program's own `-I` is the
// only Mbed TLS search dir for its C, and the shim reports 99. Building
// and running this program also proves the vendored backend itself still
// links and works (PrivateKey.new) without any global `-I`.
fn test_user_c_compiles_against_system_mbedtls_headers() ! {
	workdir := os.join_path(os.vtmp_dir(), 'ecdsa_include_scope_${rand.ulid()}')
	os.mkdir_all(workdir)!
	defer {
		os.rmdir_all(workdir) or {}
	}
	sysinclude := os.join_path(workdir, 'sysinclude', 'mbedtls')
	os.mkdir_all(sysinclude)!
	os.write_file(os.join_path(sysinclude, 'build_info.h'), '#ifndef FAKE_SYSTEM_MBEDTLS_BUILD_INFO_H
#define FAKE_SYSTEM_MBEDTLS_BUILD_INFO_H
#define MBEDTLS_VERSION_MAJOR 99
#endif
')!
	shim := os.join_path(workdir, 'shim.c')
	os.write_file(shim, '#include <mbedtls/build_info.h>
int shim_mbedtls_major(void) { return MBEDTLS_VERSION_MAJOR; }
')!
	source := os.join_path(workdir, 'main.v')
	os.write_file(source, 'module main

import crypto.ecdsa

#flag -I @DIR/sysinclude
#flag @DIR/shim.c

fn C.shim_mbedtls_major() int

fn main() {
	k := ecdsa.PrivateKey.new() or { panic(err) }
	k.free()
	println(C.shim_mbedtls_major())
}
')!
	exe := os.join_path(workdir, 'probe' + $if windows { '.exe' } $else { '' })
	// The link-plan fast path must be exercised: tracing bypasses it, so a
	// warmed plan would never be read with V3_CACHE_TRACE set. A stale plan
	// from before the user-plan policy (v5) must be ignored on upgrade; the
	// second build below reuses the warmed v6 plan for the same exe, sources,
	// flags and shared cache and must still report 99.
	old_trace := os.getenv_opt('V3_CACHE_TRACE')
	os.unsetenv('V3_CACHE_TRACE')
	defer {
		if old := old_trace {
			os.setenv('V3_CACHE_TRACE', old, true)
		}
	}
	for _ in 0 .. 2 {
		build := os.exec([@VEXE, '-gc', 'none', '-o', exe, source])
		assert build.exit_code == 0, build.output
		run := os.exec([exe])
		assert run.exit_code == 0, run.output
		assert run.output.trim_space() == '99', run.output
	}
}

// An app-owned tree that happens to live under `thirdparty/mbedtls` is
// user code, not V's vendored copy: its `-I` dir must survive the vendored
// filter. Before the filter was anchored to V's own root, this program
// failed with `review_unique_mbedtls.h: No such file or directory`.
fn test_user_c_keeps_app_owned_mbedtls_tree() ! {
	workdir := os.join_path(os.vtmp_dir(), 'ecdsa_app_tree_${rand.ulid()}')
	os.mkdir_all(workdir)!
	defer {
		os.rmdir_all(workdir) or {}
	}
	app_include := os.join_path(workdir, 'thirdparty', 'mbedtls', 'include')
	os.mkdir_all(app_include)!
	os.write_file(os.join_path(app_include, 'review_unique_mbedtls.h'), '#ifndef REVIEW_UNIQUE_MBEDTLS_H
#define REVIEW_UNIQUE_MBEDTLS_H
#define REVIEW_UNIQUE_MBEDTLS_VALUE 41
#endif
')!
	shim := os.join_path(workdir, 'shim.c')
	os.write_file(shim, '#include "review_unique_mbedtls.h"
int shim_app_value(void) { return REVIEW_UNIQUE_MBEDTLS_VALUE; }
')!
	source := os.join_path(workdir, 'main.v')
	os.write_file(source, 'module main

import crypto.ecdsa

#flag -I @DIR/thirdparty/mbedtls/include
#flag @DIR/shim.c

fn C.shim_app_value() int

fn main() {
	k := ecdsa.PrivateKey.new() or { panic(err) }
	k.free()
	println(C.shim_app_value())
}
')!
	exe := os.join_path(workdir, 'probe' + $if windows { '.exe' } $else { '' })
	build := os.exec([@VEXE, '-gc', 'none', '-o', exe, source])
	assert build.exit_code == 0, build.output
	run := os.exec([exe])
	assert run.exit_code == 0, run.output
	assert run.output.trim_space() == '41', run.output
}
