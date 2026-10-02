module c

import os
import v.cmdexec

fn test_c_backed_struct_alias_constants_use_static_brace_initializers() {
	test_dir := os.join_path(os.vtmp_dir(), 'cgen_c_alias_const_${os.getpid()}')
	module_dir := os.join_path(test_dir, 'minmod')
	os.mkdir_all(module_dir) or { panic(err) }
	defer {
		os.rmdir_all(test_dir) or {}
	}
	os.write_file(os.join_path(test_dir, 'v.mod'), "Module { name: 'c_alias_const' }") or {
		panic(err)
	}
	os.write_file(os.join_path(module_dir, 'myctx.h'), 'typedef struct my_ctx_s {
	unsigned int id;
} my_ctx_t;
') or { panic(err) }
	os.write_file(os.join_path(module_dir, 'minmod.c.v'), 'module minmod
#flag -I @VMODROOT/minmod
#include "myctx.h"

@[typedef]
pub struct C.my_ctx_t {
	pub:
	id u32
}

pub type Ctx = C.my_ctx_t
pub type CtxAlias = Ctx

pub const context = Ctx{0x00010001}
pub const named = CtxAlias{id: 0x00030003}
pub const empty = Ctx{}
') or { panic(err) }
	v_file := os.join_path(test_dir, 'main.v')
	c_file := os.join_path(test_dir, 'main.c')
	os.write_file(v_file, 'import minmod

struct Plain {
	id u32
}

const plain = Plain{42}

fn main() {
	local := minmod.Ctx{0x00020002}
	assert minmod.context.id == 65537
	assert minmod.named.id == 196611
	assert minmod.empty.id == 0
	assert plain.id == 42
	assert local.id == 131074
	println("OK")
}
') or { panic(err) }
	generated := cmdexec.run(@VEXE, ['-gc', 'none', '-cc', 'msvc', '-o', c_file, v_file])
	assert generated.exit_code == 0, generated.output
	source := os.read_file(c_file) or { panic(err) }
	assert source.contains('const my_ctx_t minmod__context = {.id = 0x00010001};')
	assert source.contains('const my_ctx_t minmod__named = {.id = 0x00030003};')
	assert source.contains('const main__Plain main__plain = {.id = 42};')
	assert source.contains('my_ctx_t local = (my_ctx_t){.id = 0x00020002};')
	compatible := msvc_compat_c_source(source)
	assert compatible.contains('const my_ctx_t minmod__context = {.id = 0x00010001};')
	assert compatible.contains('const my_ctx_t minmod__empty = {0};')
	assert compatible.contains('my_ctx_t local = (my_ctx_t){.id = 0x00020002};')
	// Native runtime coverage also checks that the brace form keeps aggregate values intact.
	compiler := os.find_abs_path_of_executable('clang') or {
		os.find_abs_path_of_executable('gcc') or { return }
	}
	executed := cmdexec.run(@VEXE, ['-gc', 'none', '-cc', compiler, 'run', v_file])
	assert executed.exit_code == 0, executed.output
	assert executed.output == 'OK\n'
}
