import os

const inline_header_vexe = @VEXE
const inline_header_tests_dir = os.dir(@FILE)
const inline_header_v3_dir = os.dir(inline_header_tests_dir)
const inline_header_vlib_dir = os.dir(inline_header_v3_dir)
const inline_header_v3_src = os.join_path(inline_header_v3_dir, 'v3.v')

fn inline_header_build_v3() string {
	v3_bin := os.join_path(os.temp_dir(), 'v3_inline_header_context_test_${os.getpid()}')
	os.rm(v3_bin) or {}
	build :=
		os.execute('${inline_header_vexe} -gc none -path "${inline_header_vlib_dir}|@vlib|@vmodules" -o ${v3_bin} ${inline_header_v3_src}')
	assert build.exit_code == 0, build.output
	return v3_bin
}

fn inline_header_write_file(root string, rel string, src string) {
	path := os.join_path(root, rel)
	os.mkdir_all(os.dir(path)) or { panic(err) }
	os.write_file(path, src) or { panic(err) }
}

fn include_occurrences_are_under_guard(c_code string, include_text string, guard_text string) bool {
	mut stack := []string{}
	for line in c_code.split_into_lines() {
		clean := line.trim_space()
		if clean.starts_with('#if ') || clean.starts_with('#ifdef ')
			|| clean.starts_with('#ifndef ') {
			stack << clean
		} else if clean == '#endif' || clean.starts_with('#endif ') {
			if stack.len > 0 {
				stack.delete_last()
			}
		}
		if clean.contains(include_text) {
			mut guarded := false
			for item in stack {
				if item.contains(guard_text) {
					guarded = true
					break
				}
			}
			if !guarded {
				return false
			}
		}
	}
	return true
}

fn test_nested_local_header_text_stays_under_parent_preprocessor_context() {
	v3_bin := inline_header_build_v3()
	root := os.join_path(os.temp_dir(), 'v3_inline_header_context_project_${os.getpid()}')
	os.rmdir_all(root) or {}
	os.mkdir_all(root) or { panic(err) }
	inline_header_write_file(root, 'v.mod', "Module { name: 'inline_header_context' }\n")
	inline_header_write_file(root, 'main.v', 'module main

import wrapper

fn main() {
	println(int_str(wrapper.touch()))
}
')
	inline_header_write_file(root, 'wrapper/wrapper.v', 'module wrapper

#insert "outer.h"

pub fn touch() int {
	return 1
}
')
	inline_header_write_file(root, 'wrapper/outer.h', '/*
This comment should stay a comment; its directive-looking text must not be
scanned as an active include:
#include "comment_only.h"
*/
#if defined(USE_OPTIONAL_BACKEND)
#include "optional_backend.h"
#endif
')
	inline_header_write_file(root, 'wrapper/optional_backend.h', '#ifndef OPTIONAL_BACKEND_H
#define OPTIONAL_BACKEND_H
#include "missing_optional_backend.h"
static inline int optional_backend_value(void) { return 9; }
#endif
')
	c_out := os.join_path(root, 'out.c')
	result := os.execute('${v3_bin} ${os.join_path(root, 'main.v')} -b c -o ${c_out}')
	assert result.exit_code == 0, result.output
	c_code := os.read_file(c_out) or { panic(err) }
	assert c_code.contains('#include "comment_only.h"'), c_code
	assert c_code.contains('#include "missing_optional_backend.h"'), c_code
	assert include_occurrences_are_under_guard(c_code, '#include "missing_optional_backend.h"',
		'USE_OPTIONAL_BACKEND'), c_code
}
