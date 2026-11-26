import os

const bare_hook_vexe = @VEXE

// builtin's `-freestanding` branches call hooks that only a bare builtin
// implementation defines. The generated C must declare each one before its
// first call, or clang 16+ and gcc 14 reject it as an implicit declaration.
fn test_freestanding_output_declares_bare_hooks_before_use() {
	root := os.join_path(os.vtmp_dir(), 'v3_freestanding_bare_hooks_${os.getpid()}')
	os.rmdir_all(root) or {}
	os.mkdir_all(root) or { panic(err) }
	defer {
		os.rmdir_all(root) or {}
	}
	src := os.join_path(root, 'main.v')
	os.write_file(src, 'module main

fn main() {
	eprintln("bare")
	buf := unsafe { malloc(16) }
	if buf == unsafe { nil } {
		panic("no memory")
	}
}
') or {
		panic(err)
	}
	c_file := os.join_path(root, 'main.c')
	res := os.exec([bare_hook_vexe, '-gc', 'none', '-freestanding', '-no-std', '-os', 'linux',
		'-o', c_file, '${src}'])
	assert res.exit_code == 0, res.output
	c_source := os.read_file(c_file) or { panic(err) }
	for hook, prototype in {
		'bare_eprint': 'void bare_eprint(u8* buf, u64 len);'
		'bare_panic':  'void bare_panic(string msg);'
		'__malloc':    'void* __malloc(size_t n);'
	} {
		decl_pos := c_source.index(prototype) or {
			assert false, 'missing prototype `${prototype}`'
			return
		}
		call_pos := c_source.index('${hook}(') or {
			assert false, 'expected a call to `${hook}`'
			return
		}
		assert decl_pos <= call_pos, '`${hook}` is called before its prototype'
	}
}

fn test_hosted_output_does_not_declare_bare_hooks() {
	root := os.join_path(os.vtmp_dir(), 'v3_hosted_bare_hooks_${os.getpid()}')
	os.rmdir_all(root) or {}
	os.mkdir_all(root) or { panic(err) }
	defer {
		os.rmdir_all(root) or {}
	}
	src := os.join_path(root, 'main.v')
	os.write_file(src, 'fn main() {\n\teprintln("hosted")\n}\n') or { panic(err) }
	c_file := os.join_path(root, 'main.c')
	res := os.exec([bare_hook_vexe, '-o', c_file, '${src}'])
	assert res.exit_code == 0, res.output
	c_source := os.read_file(c_file) or { panic(err) }
	assert !c_source.contains('bare_eprint')
}

fn test_syscalls_uses_target_write_without_exporting_standalone_allocators() ! {
	root := os.join_path(os.vtmp_dir(), 'syscalls_target_hooks_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	src := os.join_path(root, 'main.v')
	os.write_file(src, "fn main() { eprintln('hello') }")!
	for target in ['freebsd', 'netbsd', 'dragonfly', 'solaris', 'macos'] {
		c_file := os.join_path(root, '${target}.c')
		result := os.exec([bare_hook_vexe, '-new-compiler', '-syscalls', '-gc', 'none', '-os',
			target, '-arch', 'amd64', '-o', c_file, src])
		assert result.exit_code == 0, result.output
		source := os.read_file(c_file)!
		assert source.contains('bf_write(2, buffer'), target
		assert !source.contains('sys_write(2, buffer'), target
		assert !source.contains('init_global_allocator();'), target
		assert !source.contains('.globl _start'), target
	}
}

fn test_freebsd_syscalls_links_with_system_startup_and_allocator() ! {
	$if freebsd && (amd64 || arm64) {
		root := os.join_path(os.vtmp_dir(), 'freebsd_syscalls_startup_${os.getpid()}')
		os.mkdir_all(root)!
		defer { os.rmdir_all(root) or {} }
		src := os.join_path(root, 'main.c.v')
		os.write_file(src, "module main

#include <stdlib.h>

fn C.posix_memalign(&voidptr, uint, uint) i32

fn main() {
	mut block := voidptr(0)
	if C.posix_memalign(&block, 64, 129) != 0 || block == unsafe { nil } || uint(block) % 64 != 0 {
		exit(1)
	}
	unsafe { C.free(block) }
	println('aligned allocation ok')
}
")!
		for mode in ['-syscalls', '-bare'] {
			exe := os.join_path(root, 'probe')
			result := os.exec([bare_hook_vexe, '-new-compiler', '-no-retry-compilation', '-cc',
				'cc', '-gc', 'none', mode, '-o', exe, src])
			assert result.exit_code == 0, result.output
			run := os.exec([exe])
			assert run.exit_code == 0 && run.output.trim_space() == 'aligned allocation ok', run.output
		}
	}
}
