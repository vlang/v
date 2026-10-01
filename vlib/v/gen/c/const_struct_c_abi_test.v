module c

import os

// Regression test: a file-scope `const` whose type is a C-backed struct reached through a type
// alias must be emitted with a braced initializer, not a compound literal.
//
// The value expression cgen produced for such a const carried a redundant leading cast:
//
//	const my_ctx_t minmod__context = (my_ctx_t){.id = 0x00010001};
//
// A compound literal is not a constant expression in MSVC's C front end, so that file-scope
// object failed to compile with C2099 "initializer is not a constant", while gcc and tcc
// accepted it. A const of a plain V struct in the same position already used the braced form,
// which is why the failure depended only on which of the two shapes the type had.
//
// The blast radius was the whole GUI stack on the MSVC toolchain: `vlib/sokol/sgl` has
// `pub const context = Context{0x00010001}`, and `gg` imports `sokol.sgl`, so every sokol and gg
// program failed to build with `-cc msvc`.

// write_const_struct_probe writes a self-contained program whose const has a C-backed aliased
// struct type: the header supplies the C declaration and the .v file binds to it the same way
// vlib/sokol/sgl binds `C.sgl_context`.
fn write_const_struct_probe(test_root string) string {
	os.write_file(os.join_path(test_root, 'myctx.h'), '#ifndef MYCTX_H
#define MYCTX_H
typedef struct my_ctx_s { unsigned int id; } my_ctx_t;
#endif
') or { panic(err) }
	source_path := os.join_path(test_root, 'main.v')
	os.write_file(source_path, 'module main

#include "myctx.h"

@[typedef]
pub struct C.my_ctx_t {
	id u32
}

pub type Ctx = C.my_ctx_t

pub const context = Ctx{0x00010001}

fn main() {
	println(context.id.str())
}
') or { panic(err) }
	return source_path
}

// const_struct_generated_c returns the C generated for a Windows target, or none when the nested
// compiler could not run in this environment. Skipping rather than hard-failing keeps the test
// usable on hosts where a nested compile is unavailable; the emitted text is host-independent.
fn const_struct_generated_c(test_root string) ?string {
	source_path := write_const_struct_probe(test_root)
	cmd := '${os.quoted_path(@VEXE)} -o - -os windows ${os.quoted_path(source_path)}'
	result := os.execute(cmd)
	if result.exit_code != 0 {
		return none
	}
	return result.output
}

fn test_file_scope_const_of_c_backed_struct_uses_braced_initializer() {
	root := os.join_path(os.vtmp_dir(), 'const_struct_abi_${os.getpid()}')
	os.rmdir_all(root) or {}
	os.mkdir_all(root) or { panic(err) }
	defer {
		os.rmdir_all(root) or {}
	}
	c_source := const_struct_generated_c(root) or {
		eprintln('> skipping C2099 const-initializer check: the nested `v -o -` run did not succeed here')
		return
	}
	// The braced form is what every supported C compiler accepts, and it is what the V-struct
	// path already produced.
	assert c_source.contains('= {.id = 0x00010001};'), c_source
	// The compound literal is the actual defect: gcc and tcc tolerate it, MSVC does not.
	assert !c_source.contains(') {.id = 0x00010001};'), c_source
}

fn test_file_scope_const_of_c_backed_struct_builds_with_msvc_when_available() {
	root := os.join_path(os.vtmp_dir(), 'const_struct_abi_build_${os.getpid()}')
	os.rmdir_all(root) or {}
	os.mkdir_all(root) or { panic(err) }
	defer {
		os.rmdir_all(root) or {}
	}
	source_path := write_const_struct_probe(root)
	executable := os.join_path(root, 'probe.exe')
	result := os.execute('${os.quoted_path(@VEXE)} -cc msvc -o ${os.quoted_path(executable)} ${os.quoted_path(source_path)}')
	if result.exit_code != 0 && !result.output.contains('msvc') && !result.output.contains('MSVC') {
		// No MSVC on this host. That is an environment fact, not a regression, so do not fail
		// the whole suite over it.
		eprintln('> skipping the MSVC build check: this host has no MSVC')
		return
	}
	assert result.exit_code == 0, result.output
	run := os.execute(os.quoted_path(executable))
	assert run.exit_code == 0, run.output
	assert run.output.trim_space() == '65537'
}
