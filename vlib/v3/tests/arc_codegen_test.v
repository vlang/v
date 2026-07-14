import os

const arc_codegen_vexe = @VEXE
const arc_codegen_tests_dir = os.dir(@FILE)
const arc_codegen_v3_dir = os.dir(arc_codegen_tests_dir)
const arc_codegen_vlib_dir = os.dir(arc_codegen_v3_dir)
const arc_codegen_v3_src = os.join_path(arc_codegen_v3_dir, 'v3.v')

fn test_arc_clone_shares_payload_and_drops_it_once() {
	pid := os.getpid()
	v3_bin := os.join_path(os.temp_dir(), 'v3_arc_codegen_${pid}')
	src := os.join_path(os.temp_dir(), 'v3_arc_codegen_${pid}.v')
	out := os.join_path(os.temp_dir(), 'v3_arc_codegen_program_${pid}')
	defer {
		os.rm(v3_bin) or {}
		os.rm(src) or {}
		os.rm(out) or {}
		os.rm(out + '.c') or {}
	}
	build :=
		os.execute('${arc_codegen_vexe} -gc none -d ownership -path "${arc_codegen_vlib_dir}|@vlib|@vmodules" -o ${v3_bin} ${arc_codegen_v3_src}')
	assert build.exit_code == 0, build.output
	os.write_file(src, 'module main

import sync.arc

struct Config implements IClone {
	replacement arc.Arc[?[]u8]
}

struct Resource implements Drop {
	id int
}

fn (mut resource Resource) drop() {
	println("drop \${resource.id}")
}

struct ResourceConfig implements IClone {
	resource arc.Arc[Resource]
}

type ResourceSum = Resource | int

fn exercise_resource() {
	original := ResourceConfig{
		resource: arc.new(Resource{id: 7})
	}
	cloned := original.clone()
	assert original.resource.strong_count() == 2
	assert arc.ptr_eq(&original.resource, &cloned.resource)
}

fn replace_resource() {
	mut value := arc.new(Resource{id: 1})
	value = arc.new(Resource{id: 2})
	assert value.strong_count() == 1
}

fn replace_config_resource() {
	mut config := ResourceConfig{
		resource: arc.new(Resource{id: 3})
	}
	config.resource = arc.new(Resource{id: 4})
	assert config.resource.strong_count() == 1
}

fn clone_assign_resource() {
	mut value := arc.new(Resource{id: 5})
	value = value.clone()
	assert value.strong_count() == 1
}

fn exercise_sum_resource() {
	value := arc.new(ResourceSum(Resource{id: 6}))
	assert value.strong_count() == 1
}

fn exercise_map_resource() {
	value := arc.new({
		"resource": Resource{id: 8}
	})
	assert value.strong_count() == 1
}

fn main() {
	exercise_resource()
	replace_resource()
	replace_config_resource()
	clone_assign_resource()
	exercise_sum_resource()
	exercise_map_resource()
	original := Config{
		replacement: arc.new(?[]u8([u8(1), 2, 3]))
	}
	assert original.replacement.strong_count() == 1
	cloned := original.clone()
	assert arc.ptr_eq(&original.replacement, &cloned.replacement)
	assert original.replacement.strong_count() == 2
	bytes := (*cloned.replacement.get()) or { panic("missing replacement") }
	assert bytes == [u8(1), 2, 3]
}
') or {
		panic(err)
	}
	compile := os.execute('${v3_bin} ${src} -d ownership -b c -o ${out}')
	assert compile.exit_code == 0, compile.output
	c_source := os.read_file(out + '.c') or { panic(err) }
	assert c_source.contains('arc__Arc_')
	assert c_source.contains('__clone')
	run := os.execute(out)
	assert run.exit_code == 0, run.output
	assert run.output == 'drop 7\ndrop 1\ndrop 2\ndrop 3\ndrop 4\ndrop 5\ndrop 6\ndrop 8\n', run.output
}
