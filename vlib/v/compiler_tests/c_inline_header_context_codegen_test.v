import os
import v.tempname

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
	root := os.join_path(os.vtmp_dir(), 'v3_inline_header_context_project_${tempname.unique_token()}')
	os.mkdir(root)!
	defer { os.rmdir_all(root) or {} }
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
	// The C preprocessor handles comments and inactive nested includes, including for -cross.
	bin_out := os.join_path(root, 'out')
	result := os.exec([@VEXE, '-cross', '-b', 'c', '-o', bin_out, os.join_path(root, 'main.v')])
	assert result.exit_code == 0, result.output
	run := os.exec([bin_out])
	assert run.exit_code == 0, run.output
	assert run.output.trim_space() == '1', run.output
	c_code := os.read_file(bin_out + '.c') or { panic(err) }
	outer := os.real_path(os.join_path(root, 'wrapper', 'outer.h')).replace('\\', '/')
	assert c_code.contains('#include "${outer}"')
	assert !c_code.contains('#include "comment_only.h"')
	assert !c_code.contains('#include "missing_optional_backend.h"')
}
