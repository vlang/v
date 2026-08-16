import os

const default_clone_vexe = @VEXE
const default_clone_tests_dir = os.dir(@FILE)
const default_clone_v3_dir = os.dir(default_clone_tests_dir)
const default_clone_vlib_dir = os.dir(default_clone_v3_dir)
const default_clone_v3_src = os.join_path(default_clone_v3_dir, 'v3.v')

fn test_compiler_default_clone_uses_the_aggregate_type() {
	pid := os.getpid()
	v3_bin := os.join_path(os.temp_dir(), 'v3_default_clone_${pid}')
	src := os.join_path(os.temp_dir(), 'v3_default_clone_${pid}.v')
	out := os.join_path(os.temp_dir(), 'v3_default_clone_program_${pid}')
	defer {
		os.rm(v3_bin) or {}
		os.rm(src) or {}
		os.rm(out) or {}
		os.rm(out + '.c') or {}
	}
	build :=
		os.execute('${default_clone_vexe} -gc none -d ownership -path "${default_clone_vlib_dir}|@vlib|@vmodules" -o ${v3_bin} ${default_clone_v3_src}')
	assert build.exit_code == 0, build.output
	os.write_file(src, 'module main

struct Item implements IClone {
	value int
}

fn copy_item(item Item) Item {
	return item.clone()
}

fn main() {
	item := Item{value: 7}
	copy := copy_item(item)
	assert copy.value == 7
}
') or {
		panic(err)
	}
	compile := os.execute('${v3_bin} ${src} -d ownership -b c -o ${out}')
	assert compile.exit_code == 0, compile.output
	c_source := os.read_file(out + '.c') or { panic(err) }
	assert !c_source.contains('string__clone(item)'), c_source
	run := os.execute(out)
	assert run.exit_code == 0, run.output
}
