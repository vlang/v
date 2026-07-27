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
	nonownership_src := os.join_path(os.temp_dir(), 'v3_arc_codegen_nonownership_${pid}.v')
	nonownership_out := os.join_path(os.temp_dir(), 'v3_arc_codegen_nonownership_program_${pid}')
	defer {
		os.rm(v3_bin) or {}
		os.rm(src) or {}
		os.rm(out) or {}
		os.rm(out + '.c') or {}
		os.rm(nonownership_src) or {}
		os.rm(nonownership_out) or {}
		os.rm(nonownership_out + '.c') or {}
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

struct ContainerConfig implements IClone {
	resources []arc.Arc[Resource]
	by_name map[string]arc.Arc[Resource]
}

struct FactoryConfig implements IClone {
	first arc.Arc[Resource]
	second arc.Arc[Resource]
}

struct FixedArrayConfig implements IClone {
	resources [2]arc.Arc[Resource]
}

struct OptionalConfig implements IClone {
	resources ?[]arc.Arc[Resource]
}

struct MapKeyConfig implements IClone {
	values map[arc.Arc[Resource]]int
}

struct MapResource {
mut:
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

fn exercise_container_clone() {
	original := ContainerConfig{
		resources: [arc.new(Resource{id: 9})]
		by_name: {
			"resource": arc.new(Resource{id: 10})
		}
	}
	cloned := original.clone()
	assert original.resources[0].strong_count() == 2
	assert cloned.resources[0].strong_count() == 2
	assert original.by_name["resource"].strong_count() == 2
	assert cloned.by_name["resource"].strong_count() == 2
	assert arc.ptr_eq(&original.resources[0], &cloned.resources[0])
}

fn make_factory_config() FactoryConfig {
	println("make factory")
	return FactoryConfig{
		first: arc.new(Resource{id: 11})
		second: arc.new(Resource{id: 12})
	}
}

fn exercise_unstable_clone_receiver() {
	cloned := make_factory_config().clone()
	assert cloned.first.strong_count() == 1
	assert cloned.second.strong_count() == 1
	assert cloned.first.get().id == 11
	assert cloned.second.get().id == 12
}

fn replace_indexed_resources() {
	mut resources := [arc.new(Resource{id: 13})]
	resources[0] = arc.new(Resource{id: 14})
	assert resources[0].strong_count() == 1
	mut by_name := {
		"resource": arc.new(Resource{id: 15})
	}
	by_name["resource"] = arc.new(Resource{id: 16})
	assert by_name["resource"].strong_count() == 1
}

fn exercise_array_clone() {
	original := [arc.new(Resource{id: 17})]
	cloned := original.clone()
	assert original[0].strong_count() == 2
	assert cloned[0].strong_count() == 2
	assert arc.ptr_eq(&original[0], &cloned[0])
}

fn exercise_fixed_array_field_clone() {
	original := FixedArrayConfig{
		resources: [arc.new(Resource{id: 18}), arc.new(Resource{id: 19})]!
	}
	cloned := original.clone()
	for i in 0 .. 2 {
		assert original.resources[i].strong_count() == 2
		assert cloned.resources[i].strong_count() == 2
		assert arc.ptr_eq(&original.resources[i], &cloned.resources[i])
	}
}

fn exercise_none_optional_clone() {
	original := OptionalConfig{}
	cloned := original.clone()
	if _ := cloned.resources {
		assert false
	}
}

fn exercise_map_key_clone() {
	key := arc.new(Resource{id: 20})
	original := MapKeyConfig{
		values: {
			key.clone(): 1
		}
	}
	cloned := original.clone()
	assert key.strong_count() == 3
	assert cloned.values.len == 1
}

fn replace_map_nested_resources() {
	mut values := {
		"resource": MapResource{
			resource: arc.new(Resource{id: 21})
		}
	}
	values["resource"].resource = arc.new(Resource{id: 22})
	assert values["resource"].resource.strong_count() == 1
	mut nested := {
		"outer": {
			"inner": arc.new(Resource{id: 23})
		}
	}
	nested["outer"]["inner"] = arc.new(Resource{id: 24})
	assert nested["outer"]["inner"].strong_count() == 1
}

fn main() {
	exercise_resource()
	replace_resource()
	replace_config_resource()
	clone_assign_resource()
	exercise_sum_resource()
	exercise_map_resource()
	exercise_container_clone()
	exercise_unstable_clone_receiver()
	replace_indexed_resources()
	exercise_array_clone()
	exercise_fixed_array_field_clone()
	exercise_none_optional_clone()
	exercise_map_key_clone()
	replace_map_nested_resources()
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
	lines := run.output.trim_space().split_into_lines()
	assert lines.count(it == 'make factory') == 1, run.output
	for id in 1 .. 25 {
		assert lines.count(it == 'drop ${id}') == 1, run.output
	}
	assert lines.len == 25, run.output
	os.write_file(nonownership_src, 'module main

import sync.arc

fn main() {
	value := arc.new(7)
	cloned := value.clone()
	assert cloned.strong_count() == 2
}
') or {
		panic(err)
	}
	nonownership_compile := os.execute('${v3_bin} ${nonownership_src} -b c -o ${nonownership_out}')
	assert nonownership_compile.exit_code != 0, nonownership_compile.output
	assert nonownership_compile.output.contains('sync.arc requires ownership mode (`-d ownership`) so Arc handles are released at scope exit'), nonownership_compile.output
}
