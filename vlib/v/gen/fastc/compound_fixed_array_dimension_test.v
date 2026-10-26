module fastc

import os
import v.cmdexec
import v.flat
import v.pref
import v.scanner
import v.token

fn test_fastc_compound_fixed_array_dimensions_preserve_native_layout() {
	$if !macos && !linux {
		return
	}
	root := os.join_path(os.vtmp_dir(), 'fastc_compound_dimensions_${os.getpid()}')
	os.mkdir_all(os.join_path(root, 'sizing')) or { panic(err) }
	defer { os.rmdir_all(root) or {} }
	os.write_file(os.join_path(root, 'v.mod'), 'Module { name: "compound_dimensions" }') or { panic(err) }
	os.write_file(os.join_path(root, 'sizing/sizing.v'), 'module sizing\npub const width = 3\n') or { panic(err) }
	os.write_file(os.join_path(root, 'main.v'), r'module main
import sizing
const nodes = 4
__global grid [nodes * nodes]u64
struct Layout {
mut:
	wide [nodes * nodes]u64
	numeric_wide [16]u64
	numeric_nested [5][6]u8
	narrow [(nodes + 1) * 2]u16
	nested [nodes + 1][sizing.width * 2]u8
	precedence [nodes | 2 + 2]u8
	hexadecimal [0x1e + 2]u8
	cast_sum [u8(255) + u8(2)]u8
	cast_value [u8(256)]u8
}
fn identity(values [nodes * nodes]u64) [nodes * nodes]u64 { return values }
fn mutate(mut values [nodes * nodes]u64, value u64) { values[15] = value }
fn nested_value(values [nodes + 1][sizing.width * 2]u8) u8 { return values[4][5] }
fn mutate_nested(mut values [nodes + 1][sizing.width * 2]u8, value u8) { values[4][5] = value }
fn main() {
	grid[15] = u64(1) << 50
	mut local := [nodes * nodes]u64{}
	mutate(mut local, grid[15])
	assert local.len == 16 && sizeof(local) == 128
	assert local[0] == 0 && local[15] == u64(1) << 50
	copied := identity(local)
	assert copied[15] == local[15]
	mut numeric := [16]u64{}
	mutate(mut numeric, u64(1) << 51)
	numeric_copy := identity(numeric)
	assert numeric_copy[15] == u64(1) << 51
	assert identity([16]u64{})[15] == 0
	mut layout := Layout{}
	layout.wide[15] = copied[15]
	mutate(mut layout.numeric_wide, u64(1) << 52)
	assert identity(layout.numeric_wide)[15] == u64(1) << 52
	mutate_nested(mut layout.numeric_nested, 33)
	assert nested_value(layout.numeric_nested) == 33
	layout.narrow[9] = 513
	layout.nested[4][5] = 77
	layout.precedence[7] = 9
	layout.hexadecimal[31] = 19
	assert sizeof(layout.wide) == 128
	assert sizeof(layout.narrow) == 20
	assert sizeof(layout.nested) == 30
	assert sizeof(layout.precedence) == 8
	assert sizeof(layout.hexadecimal) == 32
	assert sizeof(layout.cast_sum) == 257 && sizeof(layout.cast_value) == 256
	assert layout.wide[15] == u64(1) << 50
	assert layout.narrow[9] == 513 && nested_value(layout.nested) == 77
	assert layout.precedence[7] == 9 && layout.hexadecimal[31] == 19
	mut nested := [nodes + 1][sizing.width * 2]u8{}
	nested[4][5] = 23
	assert nested_value(nested) == 23
	mut numeric_nested := [5][6]u8{}
	numeric_nested[4][5] = 31
	assert nested_value(numeric_nested) == 31
	mutate_nested(mut numeric_nested, 41)
	assert numeric_nested[4][5] == 41
	println("compound dimensions work")
}
') or { panic(err) }
	ordinary := os.join_path(root, 'ordinary')
	ordinary_build := cmdexec.run_in_merged_with_env(@VEXE, ['-b', 'c', '-nocache', '-gc', 'none',
		'-enable-globals', '-o', ordinary, root], @VEXEROOT, {
		'VEXE':                   @VEXE
		'VFLAGS':                 ''
		'VOSARGS':                ''
		'V_MACOS_V3_NO_FALLBACK': '1'
	})
	assert ordinary_build.exit_code == 0, ordinary_build.output
	ordinary_run := cmdexec.run(ordinary, [])
	assert ordinary_run.exit_code == 0, ordinary_run.output
	mut prefs := pref.new_preferences()
	prefs.vroot = @VEXEROOT
	prefs.building_v = true
	prefs.target_libc_headers = true
	prefs.target = pref.target_from('vinix', pref.host_arch()) or { panic(err) }
	prefs.backend = 'fastc'
	prefs.ccompiler = 'clang'
	prefs.user_defines = ['gc_none', 'no_backtrace', 'v3_backend']
	prefs.exclude = [os.join_path(@VEXEROOT, 'vlib/builtin/backtraces_nix.c.v')]
	generated := generate_files([root], prefs) or { panic(err) }
	assert generated.contains('FixedArray_EXPRHEX')
	assert !generated.contains('FixedArray_(')
	c_path := os.join_path(root, 'main.c')
	program := os.join_path(root, 'fastc')
	os.write_file(c_path, generated) or { panic(err) }
	build := cmdexec.run('cc', ['-std=gnu11', '-O2', '-fno-strict-aliasing', '-D__vinix__', '-o',
		program, c_path])
	assert build.exit_code == 0, build.output
	run := cmdexec.run(program, [])
	assert run.exit_code == 0, run.output
	assert run.output == ordinary_run.output, run.output
	assert run.output == 'compound dimensions work\n', run.output
}

fn test_fastc_compound_fixed_array_dimensions_keep_identifier_safe_markers() {
	for dimension in ['16', 'main__count', '(main__count * main__count)', '((main__count | 2) + 2)',
		'(0x1e + 2)'] {
		typ := fastc_fixed_array_type(dimension, 'u64')
		assert typ.bytes().all(fastc_identifier_byte(it))
		assert fastc_fixed_array_length(typ) or { '' } == dimension
		initializer := fastc_array_initializer_c_type(flat.source_type_id(typ))
		assert initializer.bytes().all(fastc_identifier_byte(it))
	}
}

fn test_fastc_compound_fixed_array_dimension_scanner_keeps_type_boundaries() {
	prefs := pref.new_preferences()
	for source in ['[4]u64', '[count]u64', '[4 * 4]u64', '[(4 + 1) * 2]u8', '[4 * 4][2 + 1]u8',
		r"[$d('capacity', 8)]u64", '[]u64'] {
		file := token.File.unindexed('dimension', source.len)
		mut scan := scanner.new_scanner(prefs, .normal)
		scan.init(file, source)
		first := scan.scan()
		type_id, next := fastc_scan_type(mut scan, first, 'dimension', 'main', map[string]string{},
			map[string]bool{}, false) or { panic(err) }
		assert next in [.eof, .semicolon]
		if source == '[]u64' {
			assert type_id == flat.source_type_id('Array_u64')
		} else {
			typ := flat.source_type_name(type_id)
			assert typ.starts_with('FixedArray_')
			assert typ.bytes().all(fastc_identifier_byte(it))
		}
	}
	for source in ['[true]u64', '[4 > 2]u64', '[4 && 2]u64', '[call()]u64', '[4 +]u64', '[(4 + 1]u64'] {
		file := token.File.unindexed('dimension', source.len)
		mut scan := scanner.new_scanner(prefs, .normal)
		scan.init(file, source)
		first := scan.scan()
		if _, _ := fastc_scan_type(mut scan, first, 'dimension', 'main', map[string]string{},
			map[string]bool{}, false) {
			assert false, source
		}
	}
}
