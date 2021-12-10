import os
import v.pref

struct TestStruct {
	test string
}

fn (mut t TestStruct) test_struct() {
	assert @STRUCT == 'TestStruct'
}

fn (mut t TestStruct) test_struct_w_return() string {
	assert @STRUCT == 'TestStruct'
	return t.test
}

fn (mut t TestStruct) test_struct_w_high_order(cb fn (int) string) string {
	assert @STRUCT == 'TestStruct'
	return 'test' + cb(2)
}

struct Abc {
}

fn (a Another) method() string {
	println(@STRUCT)
	return @STRUCT
}

struct Another {
}

fn (a Abc) method() string {
	println(@STRUCT)
	return @STRUCT
}

fn test_at_struct_ordering() {
	a := Abc{}
	assert a.method() == 'Abc'
	b := Another{}
	assert b.method() == 'Another'
}

struct TestFn {
}

fn (mut t TestFn) tst_1() {
	assert @FN == 'tst_1'
}

fn (mut t TestFn) tst_2(cb fn (int)) {
	assert @FN == 'tst_2'
	assert @METHOD == 'TestFn.tst_2'
	cb(1)
}

fn fn_name_mod_level() {
	assert @FN == 'fn_name_mod_level'
	assert @METHOD == 'fn_name_mod_level'
}

fn fn_name_mod_level_high_order(cb fn (int)) {
	assert @FN == 'fn_name_mod_level_high_order'
	cb(1)
}

fn test_at_file() {
	// Test @FILE
	f := os.file_name(@FILE)
	assert f == 'comptime_at_test.v'
}

fn test_at_fn() {
	// Test @FN
	assert @FN == 'test_at_fn'
	fn_name_mod_level()
	fn_name_mod_level_high_order(fn (i int) {
		t := i + 1
		assert t == 2
	})
	mut tfn := TestFn{}
	tfn.tst_1()
	tfn.tst_2(fn (i int) {
		t := i + 1
		assert t == 2
	})
}

fn test_at_mod() {
	// Test @MOD
	assert @MOD == 'main'
}

fn test_at_struct() {
	// Test @STRUCT
	assert @STRUCT == ''
	mut ts := TestStruct{
		test: 'test'
	}
	ts.test_struct()
	r1 := ts.test_struct_w_return()
	r2 := ts.test_struct_w_high_order(fn (i int) string {
		assert @STRUCT == ''
		return i.str()
	})
	assert r1 == 'test'
	assert r2 == 'test2'
}

fn test_vmod_file() {
	content := @VMOD_FILE
	assert content.len > 0
	assert content.contains('Module {')
	assert content.contains('name:')
	assert content.contains('version:')
	assert content.contains('description:')
}

fn test_comptime_at() {
	assert @VEXE == pref.vexe_path()
}
