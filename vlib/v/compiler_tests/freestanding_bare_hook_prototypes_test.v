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
