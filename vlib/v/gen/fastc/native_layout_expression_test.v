module fastc

import os
import v.cmdexec
import v.pref

fn test_fastc_target_layout_operators_and_hex_token_boundaries_execute() {
	$if !macos && !linux {
		return
	}
	root := os.join_path(os.vtmp_dir(), 'fastc_native_layout_expression_${os.getpid()}')
	os.mkdir_all(root) or { panic(err) }
	defer { os.rmdir_all(root) or {} }
	os.write_file(os.join_path(root, 'main.v'), r'module main
struct Packet { prefix u8 length u64 tail u8 }
fn check_offset(actual u64, expected u64) bool { return actual == expected }
fn check_size(actual int, expected int) bool { return actual == expected }
@[export: "native_layout_entry"]
fn entry() i32 {
    assert check_offset(7 + u64(__offsetof(Packet, length)), 15)
    assert check_size(2 * int(sizeof([1522]u8)), 3044)
    assert sizeof([3][2]u16) == 12
    assert sizeof([2][3][4]u8) == 24
    mut bytes := [4096]u8{}
    index := 3
    bytes[0xb5e + index] = 41
    assert bytes[0xb61] == 41
    assert 0xbe - index == 187
    assert 0x1e + index == 33
    return 42
}
fn main() {}
') or { panic(err) }
	mut prefs := pref.new_preferences()
	prefs.vroot = @VEXEROOT
	prefs.building_v = true
	prefs.target_libc_headers = true
	prefs.user_defines = ['gc_none', 'no_backtrace', 'v3_backend']
	c_source := generate_files([root], prefs) or { panic(err) }
	program_c := os.join_path(root, 'program.c')
	native_c := os.join_path(root, 'native.c')
	os.write_file(program_c, c_source) or { panic(err) }
	os.write_file(native_c, r'#include <stdint.h>
#include <stdlib.h>
extern int32_t native_layout_entry(void);
__attribute__((constructor)) static void verify_layout(void) {
    if (native_layout_entry() != 42) abort();
}
') or { panic(err) }
	program := os.join_path(root, 'program')
	build := cmdexec.run('cc', ['-std=gnu11', '-Werror=implicit-function-declaration', program_c,
		native_c, '-o', program])
	assert build.exit_code == 0, build.output
	run := cmdexec.run(program, [])
	assert run.exit_code == 0, run.output
}
