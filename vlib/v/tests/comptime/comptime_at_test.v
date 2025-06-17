import os
import time
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

fn TestStruct.static_method() string {
	assert @STRUCT == 'TestStruct'
	return @STRUCT
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

fn TestFn.static_fn() {
	assert @FN == 'static_fn'
	assert @METHOD == 'TestFn.static_fn'
	assert @STRUCT == 'TestFn'
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

fn test_at_dir() {
	// Test @DIR
	f := os.file_name(@DIR)
	assert f == 'comptime'
	assert os.dir(@FILE) == @DIR
	d := @DIR
	assert d.len > 0
	assert !d.ends_with('.v')
}

fn test_at_file_len() {
	// Test @FILE_LINE
	line1, line2 := '${@LINE}', '${@FILE_LINE}'
	assert os.file_name(@FILE) + ':' + line1.str() == line2
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
	TestFn.static_fn()
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
	assert TestStruct.static_method() == 'TestStruct'
	assert @STRUCT == ''
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

// Reasons for assertions that are not literal:
// to prevent assertion invalidation due to "line" changes in subsequent code changes
fn test_line_number_last_token() {
	line1, line2, line3 := @LINE, @LINE, @LINE
	assert line1 == line2
	assert line1 == line3
}

fn abc() {
	assert @LOCATION.contains('comptime_at_test.v:')
	assert @LOCATION.ends_with(', main.abc')
}

struct MyStruct {
}

fn MyStruct.new() MyStruct {
	assert @LOCATION.ends_with('main.MyStruct.new (static)')
	return MyStruct{}
}

fn (s MyStruct) mymethod() {
	assert @LOCATION.contains('comptime_at_test.v:')
	assert @LOCATION.ends_with('main.MyStruct{}.mymethod')
}

fn test_at_location() {
	abc()
	MyStruct.new().mymethod()
	assert @LOCATION.contains('comptime_at_test.v:')
	assert @LOCATION.ends_with('main.test_at_location')
}

fn test_at_build_date_time_timestamp() {
	bd := dump(@BUILD_DATE)
	bt := dump(@BUILD_TIME)
	bts := dump(@BUILD_TIMESTAMP)
	assert bd.len > 0
	assert bt.len > 0
	assert bd.count('-') == 2
	assert bt.count(':') == 2
	assert bts.len > 0
	assert bts.i64() > 1_000_000_000
	now_utc := dump(time.utc().unix())
	assert now_utc >= bts.i64()
}
