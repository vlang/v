module fastc

import os
import v.cmdexec
import v.pref

fn test_fastc_optional_callback_alias_parameters_and_nested_fields_execute() {
	$if !macos && !linux {
		return
	}
	root := os.join_path(os.vtmp_dir(), 'fastc_optional_callback_alias_${os.getpid()}')
	os.mkdir_all(root) or { panic(err) }
	defer { os.rmdir_all(root) or {} }
	os.write_file(os.join_path(root, 'main.v'), r'module main
type Read = fn (voidptr) ?i64
type IO = fn (voidptr, voidptr, u64, u64) ?i64
type ResultRead = fn (voidptr) !i64
struct Zone { read Read context voidptr }
struct Record { zone Zone }
fn present(_ voidptr) ?i64 { return 42 }
fn missing(_ voidptr) ?i64 { return none }
fn io_present(_ voidptr, _ voidptr, _ u64, count u64) ?i64 { return i64(count) }
fn io_missing(_ voidptr, _ voidptr, _ u64, _ u64) ?i64 { return none }
fn result_present(_ voidptr) !i64 { return 42 }
fn result_missing(_ voidptr) !i64 { return error("missing") }
fn read_value(read Read, context voidptr) i64 { return read(context) or { -1 } }
fn read_guard(record &Record) i64 {
    if value := record.zone.read(record.zone.context) { return value }
    return -1
}
fn io_value(io IO, context voidptr) ?i64 {
    ret := io(context, context, 0, 42) or { return none }
    if io(context, context, 0, 42)? != i64(42) { return none }
    return ret
}
fn read_comparison(read Read, context voidptr) ?bool { return read(context)? == i64(42) }
fn result_value(read ResultRead, context voidptr) i64 { return read(context) or { -2 } }
fn result_comparison(read ResultRead, context voidptr) !bool { return read(context)! == i64(42) }
@[export: "native_optional_callback_entry"]
fn entry() i32 {
    context := unsafe { nil }
    assert read_value(present, context) == 42
    assert read_value(missing, context) == -1
    good := Record{zone: Zone{read: present, context: context}}
    bad := Record{zone: Zone{read: missing, context: context}}
    assert read_guard(&good) == 42 && read_guard(&bad) == -1
    assert io_value(io_present, context) or { -1 } == 42
    assert io_value(io_missing, context) or { -1 } == -1
    assert read_comparison(present, context) or { false }
    assert !(read_comparison(missing, context) or { false })
    assert result_value(result_present, context) == 42
    assert result_value(result_missing, context) == -2
    assert result_comparison(result_present, context) or { false }
    assert !(result_comparison(result_missing, context) or { false })
    return 42
}
fn main() {}
') or { panic(err) }
	mut prefs := pref.new_preferences()
	prefs.vroot = @VEXEROOT
	prefs.building_v = true
	prefs.user_defines = ['gc_none', 'no_backtrace', 'v3_backend']
	for target_libc in [false, true] {
		prefs.target_libc_headers = target_libc
		c_source := generate_files([root], prefs) or { panic(err) }
		program_c := os.join_path(root, 'program.c')
		native_c := os.join_path(root, 'native.c')
		os.write_file(program_c, c_source) or { panic(err) }
		os.write_file(native_c, r'#include <stdint.h>
#include <stdlib.h>
extern int32_t native_optional_callback_entry(void);
__attribute__((constructor)) static void verify_callbacks(void) {
    if (native_optional_callback_entry() != 42) abort();
}
') or { panic(err) }
		program := os.join_path(root, 'program')
		build := cmdexec.run('cc', ['-std=gnu11', '-Werror=implicit-function-declaration', program_c,
			native_c, '-o', program])
		assert build.exit_code == 0, build.output
		run := cmdexec.run(program, [])
		assert run.exit_code == 0, run.output
	}
}
