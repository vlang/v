import os

const struct_alias_vexe = @VEXE
const struct_alias_tests_dir = os.dir(@FILE)
const struct_alias_v3_dir = os.dir(struct_alias_tests_dir)
const struct_alias_vlib_dir = os.dir(struct_alias_v3_dir)
const struct_alias_v3_src = os.join_path(struct_alias_v3_dir, 'v3.v')

fn struct_alias_build_v3() string {
	v3_bin := os.join_path(os.temp_dir(), 'v3_struct_alias_test_${os.getpid()}')
	os.rm(v3_bin) or {}
	build :=
		os.execute('${struct_alias_vexe} -gc none -path "${struct_alias_vlib_dir}|@vlib|@vmodules" -o ${v3_bin} ${struct_alias_v3_src}')
	assert build.exit_code == 0, build.output
	return v3_bin
}

fn struct_alias_write_project() string {
	root := os.join_path(os.temp_dir(), 'v3_struct_alias_${os.getpid()}')
	os.rmdir_all(root) or {}
	gg_dir := os.join_path(root, 'gg')
	sgl_dir := os.join_path(root, 'sgl_like')
	os.mkdir_all(gg_dir) or { panic(err) }
	os.mkdir_all(sgl_dir) or { panic(err) }
	os.write_file(os.join_path(root, 'v.mod'), 'Module {
	name: "v3_struct_alias_fixture"
}
') or {
		panic(err)
	}
	os.write_file(os.join_path(gg_dir, 'gg.v'), 'module gg

pub struct Context {
pub:
	render_text int
	foreign_only int = 99
}
') or {
		panic(err)
	}
	os.write_file(os.join_path(sgl_dir, 'sgl_context.h'), '#ifndef V3_STRUCT_ALIAS_SGL_CONTEXT_H
#define V3_STRUCT_ALIAS_SGL_CONTEXT_H
struct sgl_context {
	unsigned int id;
};
#endif
') or {
		panic(err)
	}
	os.write_file(os.join_path(sgl_dir, 'sgl_like.v'), 'module sgl_like

import gg

#include "@DIR/sgl_context.h"

pub struct C.sgl_context {
pub:
	id u32
}

pub type Context = C.sgl_context

pub struct LocalBase {
pub:
	id u32
}

pub type LocalContext = LocalBase

pub const context = Context{
	id: 0x00010001
}
pub const local_context = LocalContext{
	id: 7
}

pub fn context_score() int {
	return int(context.id + local_context.id) + sizeof(gg.Context) - sizeof(gg.Context)
}
') or {
		panic(err)
	}
	os.write_file(os.join_path(root, 'main.v'), 'module main

import sgl_like

struct Context {
	render_text int = 11
	main_only int = 12
}

fn main() {
	println(int_str(sgl_like.context_score()))
}
') or {
		panic(err)
	}
	return root
}

fn test_struct_init_uses_local_alias_authority_before_short_name_fallback() {
	v3_bin := struct_alias_build_v3()
	root := struct_alias_write_project()
	bin := os.join_path(root, 'out')
	compile := os.execute('${v3_bin} ${root} -b c -o ${bin}')
	assert compile.exit_code == 0, compile.output
	run := os.execute(bin)
	assert run.exit_code == 0, run.output
	assert run.output.trim_space() == '65544'
	generated := os.read_file(bin + '.c') or { panic(err) }
	assert generated.contains('const struct sgl_context sgl_like__context = (struct sgl_context){'), generated
	assert generated.contains('.id = (u32)(0x00010001)') || generated.contains('.id = 0x00010001'), generated

	assert generated.contains('const sgl_like__LocalBase sgl_like__local_context = (sgl_like__LocalBase){'), generated

	assert !generated.contains('sgl_like__context = (main__Context){'), generated
	assert !generated.contains('(gg__Context){'), generated
	assert !generated.contains('.render_text = 0x00010001'), generated
	assert !generated.contains('.render_text = (u32)(0x00010001)'), generated
	assert !generated.contains('.main_only = 0x00010001'), generated
	assert !generated.contains('.main_only = (u32)(0x00010001)'), generated
}
