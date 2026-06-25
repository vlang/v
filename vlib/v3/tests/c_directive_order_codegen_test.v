import os

const directive_order_vexe = @VEXE
const directive_order_tests_dir = os.dir(@FILE)
const directive_order_v3_dir = os.dir(directive_order_tests_dir)
const directive_order_vlib_dir = os.dir(directive_order_v3_dir)
const directive_order_v3_src = os.join_path(directive_order_v3_dir, 'v3.v')

fn directive_order_build_v3() string {
	v3_bin := os.join_path(os.temp_dir(), 'v3_c_directive_order_test')
	build :=
		os.execute('${directive_order_vexe} -gc none -path "${directive_order_vlib_dir}|@vlib|@vmodules" -o ${v3_bin} ${directive_order_v3_src}')
	assert build.exit_code == 0, build.output
	return v3_bin
}

fn directive_order_write_file(root string, rel string, src string) {
	path := os.join_path(root, rel)
	os.mkdir_all(os.dir(path)) or { panic(err) }
	os.write_file(path, src) or { panic(err) }
}

fn directive_order_gen_c(v3_bin string) string {
	root := os.join_path(os.temp_dir(), 'v3_c_directive_order_project')
	os.rmdir_all(root) or {}
	os.mkdir_all(root) or { panic(err) }
	directive_order_write_file(root, 'v.mod', "Module { name: 'directive_order' }\n")
	directive_order_write_file(root, 'main.v', 'module main

import sokol.f as _

fn main() {}
')
	directive_order_write_file(root, 'sokol/c/c.v', 'module c

#define SOKOL_GFX_IMPL
#include "sokol_gfx.h"
')
	directive_order_write_file(root, 'sokol/f/f.v', 'module f

import sokol.c as _

#define SOKOL_FONTSTASH_IMPL
#include "util/sokol_fontstash.h"
#include "sokol_gfx.h"
#ifdef KEEP_DUPLICATE_INCLUDE
#include "sokol_gfx.h"
#endif
')
	c_out := os.join_path(os.temp_dir(), 'v3_c_directive_order.c')
	os.rm(c_out) or {}
	result := os.execute('${v3_bin} ${os.join_path(root, 'main.v')} -b c -o ${c_out}')
	assert result.exit_code == 0, result.output
	return os.read_file(c_out) or { panic(err) }
}

fn directive_order_index(c_code string, needle string) int {
	return c_code.index(needle) or { -1 }
}

fn directive_order_count(c_code string, needle string) int {
	mut count := 0
	mut rest := c_code
	for {
		idx := rest.index(needle) or { break }
		count++
		rest = rest[idx + needle.len..]
	}
	return count
}

fn test_c_directives_follow_import_dependency_order() {
	c_code := directive_order_gen_c(directive_order_build_v3())
	gfx_define := directive_order_index(c_code, '#define SOKOL_GFX_IMPL')
	gfx_include := directive_order_index(c_code, '#include "sokol_gfx.h"')
	fontstash_define := directive_order_index(c_code, '#define SOKOL_FONTSTASH_IMPL')
	fontstash_include := directive_order_index(c_code, '#include "util/sokol_fontstash.h"')
	block_start := directive_order_index(c_code, '#ifdef KEEP_DUPLICATE_INCLUDE')
	assert gfx_define >= 0, c_code
	assert gfx_include >= 0, c_code
	assert fontstash_define >= 0, c_code
	assert fontstash_include >= 0, c_code
	assert block_start >= 0, c_code
	assert gfx_define < gfx_include, c_code
	assert gfx_include < fontstash_define, c_code
	assert fontstash_define < fontstash_include, c_code
	assert directive_order_count(c_code, '#include "sokol_gfx.h"') == 2, c_code
	block_tail := c_code[block_start..]
	block_include := directive_order_index(block_tail, '#include "sokol_gfx.h"')
	block_end := directive_order_index(block_tail, '#endif')
	assert block_include >= 0, c_code
	assert block_end > block_include, c_code
}
