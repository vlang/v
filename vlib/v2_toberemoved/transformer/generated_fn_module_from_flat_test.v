// Copyright (c) 2026 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
// vtest build: macos
//
// Bit-equality pin for s164: `generated_fn_module_from_flat` (FlatAst input)
// must produce the same string as `generated_fn_module` (legacy []ast.File
// input) across the routing prefixes: __sort_cmp_, Array_, generic
// module-prefixed names, and the non-module fallback set.
module transformer

import v2.ast

fn make_files_for_routing_test() []ast.File {
	return [
		ast.File{
			name: 'main.v'
			mod:  'main'
		},
		ast.File{
			name: 'time_lib.v'
			mod:  'time'
		},
		ast.File{
			name: 'os_lib.v'
			mod:  'os'
		},
	]
}

fn make_flat_for_routing_test() ast.FlatAst {
	return ast.flatten_files(make_files_for_routing_test())
}

fn test_generated_fn_module_from_flat_matches_legacy_sort_cmp_module() {
	files := make_files_for_routing_test()
	flat := make_flat_for_routing_test()
	name := '__sort_cmp_time__FormatDate'
	assert generated_fn_module(name, files) == 'time'
	assert generated_fn_module_from_flat(name, &flat) == 'time'
}

fn test_generated_fn_module_from_flat_matches_legacy_sort_cmp_unknown() {
	files := make_files_for_routing_test()
	flat := make_flat_for_routing_test()
	// 'somethingelse' is not a registered file mod — both paths return ''
	name := '__sort_cmp_somethingelse__Frob'
	assert generated_fn_module(name, files) == ''
	assert generated_fn_module_from_flat(name, &flat) == ''
}

fn test_generated_fn_module_from_flat_matches_legacy_array_prefix_module() {
	files := make_files_for_routing_test()
	flat := make_flat_for_routing_test()
	name := 'Array_os__File_str'
	assert generated_fn_module(name, files) == 'os'
	assert generated_fn_module_from_flat(name, &flat) == 'os'
}

fn test_generated_fn_module_from_flat_matches_legacy_module_prefixed() {
	files := make_files_for_routing_test()
	flat := make_flat_for_routing_test()
	name := 'time__FormatDate__str'
	assert generated_fn_module(name, files) == 'time'
	assert generated_fn_module_from_flat(name, &flat) == 'time'
}

fn test_generated_fn_module_from_flat_matches_legacy_no_module_prefix() {
	files := make_files_for_routing_test()
	flat := make_flat_for_routing_test()
	// fn name with no __ separator falls through to ''
	name := 'plainname'
	assert generated_fn_module(name, files) == ''
	assert generated_fn_module_from_flat(name, &flat) == ''
}

fn test_generated_fn_module_from_flat_matches_legacy_filtered_prefix() {
	files := make_files_for_routing_test()
	flat := make_flat_for_routing_test()
	// 'int' is in the filtered prefix set — both paths skip and return ''
	name := 'int__custom_helper'
	assert generated_fn_module(name, files) == ''
	assert generated_fn_module_from_flat(name, &flat) == ''
}

fn test_generated_fn_module_from_flat_matches_legacy_main_excluded() {
	files := make_files_for_routing_test()
	flat := make_flat_for_routing_test()
	// 'main' is excluded from prefix module routing — both paths return ''
	name := 'main__Helper__str'
	assert generated_fn_module(name, files) == ''
	assert generated_fn_module_from_flat(name, &flat) == ''
}

fn test_generated_fn_module_from_flat_matches_legacy_unknown_module() {
	files := make_files_for_routing_test()
	flat := make_flat_for_routing_test()
	// 'nonexistent' isn't a registered file mod — both paths return ''
	name := 'nonexistent__Type__str'
	assert generated_fn_module(name, files) == ''
	assert generated_fn_module_from_flat(name, &flat) == ''
}
