module c

import os
import v.cmdexec
import v.flat
import v.pref

fn test_mmap_header_ownership_is_limited_to_supported_shipped_bindings() {
	mut g := FlatGen.new()
	g.compiler_vroot = '/vroot'
	source := '/vroot/vlib/os/mmap_nix.c.v'
	for target in ['macos', 'linux', 'freebsd', 'openbsd', 'dragonfly'] {
		g.set_target(pref.target_from(target, 'amd64')!)
		assert g.core_native_header_is_redeclared(source, '<sys/mman.h>'), target
		assert !g.core_native_header_is_redeclared(source, '<custom.h>'), target
		assert !g.core_native_header_is_redeclared('/project/os/mmap_nix.c.v', '<sys/mman.h>'), target
	}
	for target in ['macos', 'linux', 'freebsd', 'openbsd', 'dragonfly'] {
		for arch in ['x86', 'arm32'] {
			g.set_target(pref.target_from(target, arch)!)
			assert !g.core_native_header_is_redeclared(source, '<sys/mman.h>'), '${target}/${arch}'
		}
	}
	for target in ['qnx', 'netbsd', 'solaris', 'haiku', 'windows'] {
		g.set_target(pref.target_from(target, 'amd64')!)
		assert !g.core_native_header_is_redeclared(source, '<sys/mman.h>'), target
	}
	g.set_target(pref.target_from('macos', 'arm64')!)
	mut a := flat.FlatAst{}
	a.cached_header_sources['/cache/os.vh'] = source
	g.a = &a
	assert g.core_native_header_is_redeclared('/cache/os.vh', '<sys/mman.h>')
	a.cached_header_sources['/cache/os.vh'] = '/vroot/vlib/os/args.v'
	a.cached_header_module_sources['/cache/os.vh'] = ['/vroot/vlib/os/args.v', source]
	assert g.core_native_header_is_redeclared('/cache/os.vh', '<sys/mman.h>')
	a.cached_header_sources['/cache/user.vh'] = '/project/os/args.v'
	a.cached_header_module_sources['/cache/user.vh'] = ['/project/os/args.v', '/project/os/mmap_nix.c.v']
	assert !g.core_native_header_is_redeclared('/cache/user.vh', '<sys/mman.h>')
	g.set_target(pref.target_from('linux', 'arm32')!)
	assert !g.core_native_header_is_redeclared('/cache/os.vh', '<sys/mman.h>')
	g.set_target(pref.target_from('macos', 'arm64')!)
	g.no_builtin = true
	assert !g.core_native_header_is_redeclared(source, '<sys/mman.h>')
	g.no_builtin = false
	g.output_cross_c = true
	assert !g.core_native_header_is_redeclared(source, '<sys/mman.h>')
	g.output_cross_c = false
	g.object_file_mode = true
	assert !g.core_native_header_is_redeclared(source, '<sys/mman.h>')
	g.object_file_mode = false
	g.target_libc_headers = true
	assert !g.core_native_header_is_redeclared(source, '<sys/mman.h>')
}

// The header of a cached module stands for all of the module's sources. A build
// that reads `os` from it has to keep <sys/mman.h> out of its C as the build that
// parsed mmap_nix.c.v did, though the header is mapped to another source of `os`:
// otherwise the two generate different C, and never find each other's objects.
fn test_mmap_header_ownership_holds_for_a_cached_header_of_the_whole_module() {
	mut g := FlatGen.new()
	g.compiler_vroot = '/vroot'
	g.set_target(pref.target_from('macos', 'arm64')!)
	mut a := flat.FlatAst{}
	a.cached_header_sources['/cache/os.vh'] = '/vroot/vlib/os/args.v'
	a.cached_header_module_sources['/cache/os.vh'] = ['/vroot/vlib/os/args.v',
		'/vroot/vlib/os/mmap_nix.c.v', '/vroot/vlib/os/os.v']
	a.cached_header_sources['/cache/other.vh'] = '/vroot/vlib/os/args.v'
	a.cached_header_module_sources['/cache/other.vh'] = ['/vroot/vlib/os/args.v', '/vroot/vlib/os/os.v']
	a.cached_header_sources['/cache/project.vh'] = '/project/os/args.v'
	a.cached_header_module_sources['/cache/project.vh'] = ['/project/os/args.v',
		'/project/os/mmap_nix.c.v']
	g.a = &a
	assert g.core_native_header_is_redeclared('/cache/os.vh', '<sys/mman.h>')
	assert !g.core_native_header_is_redeclared('/cache/os.vh', '<custom.h>')
	assert !g.core_native_header_is_redeclared('/cache/other.vh', '<sys/mman.h>')
	assert !g.core_native_header_is_redeclared('/cache/project.vh', '<sys/mman.h>')
	// v.cmdexec's header is mapped to cmdexec.v, but its cmdexec_poll.h comes from
	// cmdexec_pipe.c.v.
	poll_h := '"/vroot/vlib/v/cmdexec/cmdexec_poll.h"'
	a.cached_header_sources['/cache/cmdexec.vh'] = '/vroot/vlib/v/cmdexec/cmdexec.v'
	a.cached_header_module_sources['/cache/cmdexec.vh'] = [
		'/vroot/vlib/v/cmdexec/cmdexec.v',
		'/vroot/vlib/v/cmdexec/cmdexec_pipe.c.v',
	]
	assert g.core_native_header_is_redeclared('/cache/cmdexec.vh', poll_h)
	assert !g.core_native_header_is_redeclared('/cache/cmdexec.vh', '<poll.h>')
	// The SDK modes keep every include of a cached header too.
	g.no_builtin = true
	assert !g.core_native_header_is_redeclared('/cache/os.vh', '<sys/mman.h>')
	assert !g.core_native_header_is_redeclared('/cache/cmdexec.vh', poll_h)
	g.no_builtin = false
	g.output_cross_c = true
	assert !g.core_native_header_is_redeclared('/cache/os.vh', '<sys/mman.h>')
	assert !g.core_native_header_is_redeclared('/cache/cmdexec.vh', poll_h)
	g.output_cross_c = false
	g.object_file_mode = true
	assert !g.core_native_header_is_redeclared('/cache/os.vh', '<sys/mman.h>')
	assert !g.core_native_header_is_redeclared('/cache/cmdexec.vh', poll_h)
	g.object_file_mode = false
	g.target_libc_headers = true
	assert !g.core_native_header_is_redeclared('/cache/os.vh', '<sys/mman.h>')
	assert !g.core_native_header_is_redeclared('/cache/cmdexec.vh', poll_h)
	g.target_libc_headers = false
	g.set_target(pref.target_from('macos', 'arm32')!)
	assert !g.core_native_header_is_redeclared('/cache/os.vh', '<sys/mman.h>')
}

fn test_mmap_sync_constants_follow_each_platform_abi() {
	$if !macos && !linux {
		return
	}
	mut g := FlatGen.new()
	g.headerless_platform_constants()
	root := os.join_path(os.vtmp_dir(), 'mmap_constants_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	source := os.join_path(root, 'constants.c')
	os.write_file(source, g.sb.str() + '\nint mmap_flags[] = {MAP_SHARED, MS_SYNC};\n')!
	for macro, value in {
		'__APPLE__':     '0x0010'
		'__FreeBSD__':   '0x0'
		'__OpenBSD__':   '0x2'
		'__NetBSD__':    '0x4'
		'__DragonFly__': '0x0'
		'__sun':         '0x4'
		'__linux__':     '0x4'
	} {
		preprocessed := cmdexec.run('cc', ['-E', '-P', '-undef', '-D${macro}', source])
		assert preprocessed.exit_code == 0, preprocessed.output
		assert preprocessed.output.contains('int mmap_flags[] = {0x0001, ${value}};'), '${macro}: ${preprocessed.output}'
	}
}

fn test_mmap_constants_match_the_native_sdk() {
	$if !macos && !linux {
		return
	}
	mut g := FlatGen.new()
	g.headerless_mmap_constants($if macos { '0x1000' } $else { '0x20' }, $if macos { '0x0010' } $else { '0x4' })
	mut code := '#define _GNU_SOURCE 1\n#include <sys/mman.h>\nenum { sdk_shared = MAP_SHARED, sdk_sync = MS_SYNC, sdk_anon = MAP_ANONYMOUS };\n'
	for name in ['PROT_READ', 'PROT_WRITE', 'PROT_EXEC', 'MAP_SHARED', 'MAP_PRIVATE', 'MAP_FIXED',
		'MAP_ANONYMOUS', 'MAP_ANON', 'MAP_FAILED', 'MS_SYNC'] {
		code += '#undef ${name}\n'
	}
	code += g.sb.str()
	code += '_Static_assert(MAP_SHARED == sdk_shared, "MAP_SHARED");\n_Static_assert(MS_SYNC == sdk_sync, "MS_SYNC");\n_Static_assert(MAP_ANONYMOUS == sdk_anon, "MAP_ANONYMOUS");\n'
	code += '_Static_assert(__builtin_types_compatible_p(__typeof__(&msync), int (*)(void*, size_t, int)), "msync prototype");\n'
	root := os.join_path(os.vtmp_dir(), 'mmap_sdk_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	source := os.join_path(root, 'sdk.c')
	os.write_file(source, code)!
	compiled := cmdexec.run('cc', ['-std=c11', '-Werror', '-fsyntax-only', source])
	assert compiled.exit_code == 0, compiled.output
}

fn test_os_mmap_runs_headerless_and_preserves_explicit_sdk_ownership() {
	$if !macos && !linux {
		return
	}
	root := os.join_path(os.vtmp_dir(), 'mmap_headerless_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	source := os.join_path(root, 'main.c.v')
	c_file := os.join_path(root, 'main.c')
	executable := os.join_path(root, 'main')
	data_file := os.join_path(root, 'mapped')
	program := 'import os
fn main() {
	mut mapping := os.mmap_create(os.args[1], 4096) or { panic(err) }
	mut view := unsafe { mapping.bytes() }
	view[0] = 77
	view[4095] = 91
	mapping.sync() or { panic(err) }
	mapping.close()
	data := os.read_bytes(os.args[1]) or { panic(err) }
	assert data.len == 4096
	assert data[0] == 77 && data[4095] == 91
	mut reopened := os.mmap_open(os.args[1], .read_only) or { panic(err) }
	read_view := unsafe { reopened.bytes() }
	assert read_view[0] == 77 && read_view[4095] == 91
	reopened.close()
	println("mapped and synced")
}
'
	for with_header in [false, true] {
		os.write_file(source, if with_header {
			'#include <sys/mman.h>\n' + program
		} else {
			program
		})!
		generated := cmdexec.run(@VEXE, ['-nocache', '-gc', 'none', '-o', c_file, source])
		assert generated.exit_code == 0, generated.output
		code := os.read_file(c_file)!
		if with_header {
			assert code.contains('#include <sys/mman.h>')
			assert !code.contains('i32 msync('), 'SDK owns the msync prototype'
		} else {
			assert code.split_into_lines().all(!it.starts_with('#include') || it == '#include "${os.join_path(@VEXEROOT, 'vlib', 'builtin', 'track_heap_checks.h').replace('\\', '/')}"'), 'native mmap must preserve the standalone preamble'
			os.write_file(c_file, code + '\n_Static_assert(__builtin_types_compatible_p(__typeof__(&msync), int (*)(void*, size_t, int)), "native msync ABI");\n')!
			abi := cmdexec.run('clang', ['-fsyntax-only', '-Werror=implicit-function-declaration',
				c_file])
			assert abi.exit_code == 0, abi.output
		}
		compiled := cmdexec.run(@VEXE, ['-nocache', '-gc', 'none', '-cc', 'clang', '-cflags',
			'-Werror=implicit-function-declaration', '-o', executable, source])
		assert compiled.exit_code == 0, compiled.output
		run := cmdexec.run(executable, [data_file])
		assert run.exit_code == 0, run.output
		assert run.output == 'mapped and synced\n', run.output
	}
}

fn test_32_bit_bsd_mmap_uses_the_native_sdk_offset_type() {
	$if !macos && !linux {
		return
	}
	root := os.join_path(os.vtmp_dir(), 'mmap_bsd32_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	source := os.join_path(root, 'main.v')
	c_file := os.join_path(root, 'main.c')
	os.write_file(source, 'import os\nfn main() {\n mut mapping := os.mmap_create("mapped", 4096) or { panic(err) }\n mapping.sync() or { panic(err) }\n mapping.close()\n}\n')!
	for target in ['freebsd', 'openbsd', 'dragonfly'] {
		generated := cmdexec.run(@VEXE, ['-nocache', '-gc', 'none', '-os', target, '-arch', 'x86',
			'-o', c_file, source])
		assert generated.exit_code == 0, generated.output
		code := os.read_file(c_file)!
		assert code.contains('#include <sys/mman.h>'), '${target}: the SDK must own the 64-bit off_t argument'
		assert !code.contains('void* mmap('), '${target}: do not emit the pointer-width V fallback beside the SDK'
	}
}

fn test_headerless_32_bit_bootstrap_takes_mmap_from_the_sdk() {
	$if !macos && !linux {
		return
	}
	clang := os.find_abs_path_of_executable('clang') or { return }
	root := os.join_path(os.vtmp_dir(), 'mmap_headerless32_${os.getpid()}')
	os.mkdir_all(os.join_path(root, 'sdk', 'sys'))!
	defer { os.rmdir_all(root) or {} }
	source := os.join_path(root, 'main.v')
	c_file := os.join_path(root, 'main.c')
	os.write_file(source, 'import os\nfn main() {\n mut mapping := os.mmap_create(os.args[1], 4096) or { panic(err) }\n mapping.close()\n}\n')!
	for arch in ['amd64', 'arm32'] {
		generated := cmdexec.run(@VEXE, ['-nocache', '-gc', 'none', '-os', 'linux', '-arch', arch,
			'-cross', '-d', 'v3_headerless_bootstrap', '-o', c_file, source])
		assert generated.exit_code == 0, generated.output
		code := os.read_file(c_file)!
		if arch == 'amd64' {
			assert !code.contains('#include <sys/mman.h>'), 'a 64-bit snapshot keeps its own mmap ABI'
			assert code.contains('void* mmap(')
		} else {
			assert code.contains('#include <sys/mman.h>')
			assert !code.contains('void* mmap('), 'the SDK owns the 32-bit off_t argument'
		}
	}
	// glibc spells the 32-bit offset `long`, musl always uses 64 bits. Either SDK
	// prototype must be the only declaration of mmap.
	for libc, off_t in {
		'glibc': 'long'
		'musl':  'long long'
	} {
		guard := if libc == 'glibc' { '__off_t_defined' } else { '__DEFINED_off_t' }
		os.write_file(os.join_path(root, 'sdk', 'sys', 'mman.h'), '#define ${guard}
typedef ${off_t} off_t;
void *mmap(void *addr, __SIZE_TYPE__ len, int prot, int flags, int fd, off_t offset);
int munmap(void *addr, __SIZE_TYPE__ len);
int msync(void *addr, __SIZE_TYPE__ len, int flags);
')!
		for target in ['armv7-unknown-linux-gnueabihf', 'i686-unknown-linux-gnu'] {
			checked := cmdexec.run(clang, ['-target', target, '-std=c99', '-fsyntax-only',
				'-Wno-everything', '-Werror=implicit-function-declaration',
				'-I${os.join_path(root, 'sdk')}', c_file])
			assert checked.exit_code == 0, '${libc}/${target}: ${checked.output}'
		}
	}
}
