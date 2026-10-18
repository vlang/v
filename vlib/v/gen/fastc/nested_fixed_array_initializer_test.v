module fastc

import os
import v.cmdexec
import v.flat
import v.pref

fn test_fastc_fixed_array_markers_require_identifier_boundaries() {
	mut types := flat.TypeTextBindings{}
	fastc_collect_referenced_fixed_array_types('NotFixedArray_99_FASTC_ARRAY_OF_u8 ' +
		'FixedArray_2_FASTC_ARRAY_OF_FixedArray_3_FASTC_ARRAY_OF_u8 data; FixedArray_5_u8; FixedAr', mut types)
	mut values := []string{}
	for name in types.keys() {
		values << types.value(name)
	}
	assert values.len == 2
	assert 'FixedArray_2_FASTC_ARRAY_OF_FixedArray_3_FASTC_ARRAY_OF_u8' in values
	assert 'FixedArray_3_FASTC_ARRAY_OF_u8' in values
}

fn test_fastc_nested_fixed_array_initializer_preserves_function_scope() {
	$if !macos && !linux {
		return
	}
	root := os.join_path(os.vtmp_dir(), 'fastc_nested_fixed_array_initializer_${os.getpid()}')
	os.mkdir_all(root) or { panic(err) }
	defer { os.rmdir_all(root) or {} }
	os.write_file(os.join_path(root, 'main.v'), r'module main
struct FieldRows {
	rows [3][2]i16
}
struct SignedRows {
	rows [3][2]i32
}
fn signature_rows(rows [3][2]i32) [3][2]i32 {
	assert sizeof(rows) == 24
	return rows
}
@[direct_array_access]
fn fill(mut state [16]u8) {
	mut lanes := [8]u32{}; mut powers := [8][8]u32{}; mut result := [8]u32{}
	lanes[0] = 3
	powers[0] = lanes
	powers[1][2] = 7
	result = powers[0]
	state[0] = u8(result[0] + powers[1][2])
	mut cubes := [2][3][4]u16{}
	cubes[1][2][3] = 41
	state[1] = u8(cubes[1][2][3])
	state[2] = u8(cubes[0][0][0])
}
fn main() {
	mut state := [16]u8{}
	fill(mut state)
	assert state[0] == 10 && state[1] == 41 && state[2] == 0
	mut subsequent := [2][2]u8{}
	subsequent[1][1] = 9
	assert subsequent[1][1] == 9 && subsequent[0][0] == 0
	mut non_square := [3][2]u64{}
	non_square[2] = [u64(5), u64(7)]!
	copied_row := non_square[2]
	non_square[0][1] = 11
	assert copied_row[0] == 5 && copied_row[1] == 7 && non_square[0][1] == 11
	fields := FieldRows{}
	assert sizeof(FieldRows) == 12 && sizeof(fields) == 12
	signed := SignedRows{}
	signed_copy := signature_rows(signed.rows)
	assert sizeof(signed_copy) == 24 && signed_copy[2][1] == 0
}
') or { panic(err) }
	mut prefs := pref.new_preferences()
	prefs.vroot = @VEXEROOT
	prefs.building_v = true
	prefs.backend = 'fastc'
	prefs.ccompiler = 'clang'
	prefs.user_defines = ['gc_none', 'no_backtrace', 'v3_backend']
	c_source := generate_files([root], prefs) or { panic(err) }
	c_file := os.join_path(root, 'main.c')
	program := os.join_path(root, 'program')
	os.write_file(c_file, c_source) or { panic(err) }
	build := cmdexec.run('cc', ['-std=gnu11', '-O2', '-o', program, c_file])
	assert build.exit_code == 0, build.output
	run := cmdexec.run(program, [])
	assert run.exit_code == 0, run.output
}
