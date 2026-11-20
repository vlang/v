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
	build := os.exec([@VEXE, '-gc', 'none', '-o', exe, source])
	assert build.exit_code == 0, build.output
	run := os.exec([exe])
	assert run.exit_code == 0, run.output
	assert run.output.trim_space() == '99', run.output
}
