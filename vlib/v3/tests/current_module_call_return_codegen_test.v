import os

const vexe = @VEXE
const tests_dir = os.dir(@FILE)
const v3_dir = os.dir(tests_dir)
const vlib_dir = os.dir(v3_dir)
const v3_src = os.join_path(v3_dir, 'v3.v')

fn build_v3() string {
	v3_bin := os.join_path(os.temp_dir(), 'v3_current_module_call_return_test_${os.getpid()}')
	os.rm(v3_bin) or {}
	build :=
		os.execute('${vexe} -gc none -path "${vlib_dir}|@vlib|@vmodules" -o ${v3_bin} ${v3_src}')
	assert build.exit_code == 0, build.output
	return v3_bin
}

fn write_project() string {
	root := os.join_path(os.temp_dir(), 'v3_current_module_call_return_${os.getpid()}')
	os.rmdir_all(root) or {}
	os.mkdir_all(os.join_path(root, 'clock')) or { panic(err) }
	os.mkdir_all(os.join_path(root, 'zz_other')) or { panic(err) }
	os.write_file(os.join_path(root, 'v.mod'), "Module { name: 'current_module_call_return' }\n") or {
		panic(err)
	}
	os.write_file(os.join_path(root, 'clock/clock.v'), 'module clock

pub struct Time {
pub mut:
	value int
}

pub fn new(t Time) Time {
	return t
}

pub fn (mut t Time) add_seconds(n int) {
	t.value += n
}

pub fn make() int {
	mut time_to_be_returned := new(Time{
		value: 2
	})
	time_to_be_returned.add_seconds(3)
	return time_to_be_returned.value
}
') or {
		panic(err)
	}
	os.write_file(os.join_path(root, 'zz_other/zz_other.v'), 'module zz_other

pub struct Other {
pub mut:
	value int
}

pub fn new(seed int) Other {
	return Other{
		value: seed + 100
	}
}

pub fn (mut o Other) add_seconds(n int) {
	o.value += n + 100
}
') or {
		panic(err)
	}
	os.write_file(os.join_path(root, 'main.v'), 'module main

import clock
import zz_other

fn main() {
	assert clock.make() == 5
	_ := zz_other.new(1)
}
') or {
		panic(err)
	}
	return os.join_path(root, 'main.v')
}

fn test_plain_call_prefers_current_module_return_authority() {
	v3_bin := build_v3()
	main_path := write_project()
	out := os.join_path(os.temp_dir(), 'v3_current_module_call_return_out_${os.getpid()}')
	compile := os.execute('${v3_bin} ${main_path} -b c -o ${out}')
	assert compile.exit_code == 0, compile.output
	run := os.execute(out)
	assert run.exit_code == 0, run.output
	generated := os.read_file(out + '.c') or { panic(err) }
	assert generated.contains('clock__Time time_to_be_returned = clock__new'), generated
	assert generated.contains('clock__Time__add_seconds(&time_to_be_returned'), generated
	assert !generated.contains('zz_other__Other time_to_be_returned = clock__new'), generated
	assert !generated.contains('zz_other__Other__add_seconds(&time_to_be_returned'), generated
}
