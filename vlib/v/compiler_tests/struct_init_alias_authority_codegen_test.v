import os
import v.tempname

fn struct_alias_write_project(root string) {
	gg_dir := os.join_path(root, 'gg')
	sgl_dir := os.join_path(root, 'sgl_like')
	common_dir := os.join_path(root, 'common')
	data_dir := os.join_path(root, 'data')
	os.mkdir_all(gg_dir) or { panic(err) }
	os.mkdir_all(sgl_dir) or { panic(err) }
	os.mkdir_all(common_dir) or { panic(err) }
	os.mkdir_all(data_dir) or { panic(err) }
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
	os.write_file(os.join_path(common_dir, 'common.v'), 'module common

pub struct ID {
pub:
	value int
}

pub interface Translation {
	locale_id() ID
}
') or {
		panic(err)
	}
	os.write_file(os.join_path(data_dir, 'data.v'), 'module data

import common

pub type ID = common.ID

fn check_translation_locale_ids(translations []common.Translation) ! {
	for translation in translations {
		if translation.locale_id().value < 0 {
			return error("invalid locale")
		}
	}
}

pub struct CategoryTranslationParams {
pub:
	locale_id ID
}

fn (p CategoryTranslationParams) locale_id() ID {
	return p.locale_id
}

pub struct CategoryCreateParams {
pub:
	translations ?[]CategoryTranslationParams
}

pub fn (p CategoryCreateParams) check() ! {
	if translations := p.translations {
		check_translation_locale_ids(translations)!
	}
}
') or {
		panic(err)
	}
	os.write_file(os.join_path(root, 'main.v'), 'module main

import data
import sgl_like

struct Context {
	render_text int = 11
	main_only int = 12
}

fn main() {
	data.CategoryCreateParams{
		translations: [
			data.CategoryTranslationParams{
				locale_id: data.ID{
					value: 1
				}
			},
		]
	}.check()!
	println(int_str(sgl_like.context_score()))
}
') or {
		panic(err)
	}
}

fn test_struct_init_uses_local_alias_authority_before_short_name_fallback() {
	root := os.join_path(os.vtmp_dir(), 'v3_struct_alias_${tempname.unique_token()}')
	os.mkdir(root)!
	defer {
		os.rmdir_all(root) or {}
	}
	struct_alias_write_project(root)
	bin := os.join_path(root, 'out')
	compile := os.exec([@VEXE, root, '-b', 'c', '-o', bin])
	assert compile.exit_code == 0, compile.output
	run := os.exec([bin])
	assert run.exit_code == 0, run.output
	assert run.output.trim_space() == '65544'
	generated := os.read_file(bin + '.c') or { panic(err) }
	// Static struct constants use plain brace initializers (025a92ef15), so the C type that
	// the alias resolved to is the one in the declaration.
	assert generated.contains('const struct sgl_context sgl_like__context = {'), generated
	assert generated.contains('.id = (u32)(0x00010001)') || generated.contains('.id = 0x00010001'), generated

	assert generated.contains('const sgl_like__LocalBase sgl_like__local_context = {'), generated

	assert !generated.contains('main__Context sgl_like__context'), generated
	assert !generated.contains('gg__Context sgl_like__context'), generated
	assert !generated.contains('sgl_like__context = (main__Context){'), generated
	assert !generated.contains('(gg__Context){'), generated
	assert !generated.contains('.render_text = 0x00010001'), generated
	assert !generated.contains('.render_text = (u32)(0x00010001)'), generated
	assert !generated.contains('.main_only = 0x00010001'), generated
	assert !generated.contains('.main_only = (u32)(0x00010001)'), generated
}
