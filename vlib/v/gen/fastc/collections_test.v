module fastc

import os
import v.cmdexec
import v.pref

fn test_ordinary_collection_literals_and_membership() {
	source := "module main

struct Inventory {
	counts map[string]int
}

fn contains(values []int, needle int) bool {
	return needle in values
}

fn has_name(counts map[string]int, name string) bool {
	return name in counts
}

fn make_map() map[string]int {
	return {'x': 1}
}

fn has_inventory(inv Inventory, name string) bool {
	return name in inv.counts
}

fn main() {
	arr := [1, 2, 3]
	println(2 in arr)
	println(4 !in arr)
	println(arr[1])
	println(2 in [1, 2, 3])
	names := ['hello', 'world']
	println('hel' + 'lo' in names)
	println('missing' !in names)
	println('ell' in 'hello')
	m := {'a': 1, 'b': 2, 'a': 3}
	println(m['a'])
	println(m['missing'])
	println('a' in m)
	println('missing' !in m)
	if 'b' in m && 2 in arr {
		println('ok')
	}
	println(('missing' in m) == false)
	mut wide := 2147483647
	wide += 1
	large := [wide]
	println(large[0])
	println(wide in large)
	large_map := {wide: wide}
	println(large_map[wide])
	println(wide in large_map)
	numbers := {1: 'one', 2: 'two'}
	println(numbers[2])
	println(1 in numbers)
	println(3 !in numbers)
	println(contains(arr, 2))
	println(has_name(m, 'a'))
	println(m.len)
	println('x' in make_map())
	println(has_inventory(Inventory{}, 'a'))
	s := 'abc'
	println(s[0])
}
"
	prefs := pref.new_preferences()
	c_source := generate(source, 'ordinary_collections.v', prefs) or { panic(err) }
	assert c_source.contains('builtin__new_array_from_c_array(3, 3'), c_source
	assert c_source.contains('builtin__map_set('), c_source
	assert c_source.contains('builtin__array_get(arr'), c_source
	root := os.join_path(os.vtmp_dir(), 'fastc_collections_${os.getpid()}')
	os.mkdir_all(root) or { panic(err) }
	defer {
		os.rmdir_all(root) or {}
	}
	c_file := os.join_path(root, 'program.c')
	bin_file := os.join_path(root, 'program')
	os.write_file(c_file, c_source) or { panic(err) }
	tcc := os.join_path(prefs.vroot, 'thirdparty', 'tcc', 'tcc.exe')
	compiled := cmdexec.run(tcc, ['-std=gnu11', '-o', bin_file, c_file])
	assert compiled.exit_code == 0, compiled.output
	run := cmdexec.run(bin_file, [])
	assert run.exit_code == 0, run.output
	assert run.output == 'true\ntrue\n2\ntrue\ntrue\ntrue\ntrue\n3\n0\ntrue\ntrue\nok\ntrue\n2147483648\ntrue\n2147483648\ntrue\ntwo\ntrue\ntrue\ntrue\ntrue\n2\ntrue\nfalse\n97\n', run.output
}

fn test_ordinary_nested_array_membership_reads_stored_element_width() {
	source := 'fn main() {
	needle := [1, 2]
	values := [[1, 3]]
	println(needle in values)
	println(needle !in values)
	println(needle in [[1, 3], [1, 2]])
	println([1] in values)
	mut wide := 2147483647
	wide += 1
	wide_needle := [wide, 2]
	wide_values := [[wide, 3]]
	println(wide_needle in wide_values)
	println(wide_needle in [[wide, 2]])
}
'
	prefs := pref.new_preferences()
	c_source := generate(source, 'ordinary_nested_array_membership.v', prefs) or { panic(err) }
	for side in ['l', 'r'] {
		assert c_source.contains('((${fastc_platform_int_c_type} *)__vf_meq_${side}.data)[__vf_meq_k]'), c_source
	}
	mut selfhost_prefs := pref.new_preferences()
	selfhost_prefs.building_v = true
	selfhost_fixture := 'fn contains_nested(needle []int, values [][]int) bool {
	return needle in values
}

fn main() {
	needle := [1, 2]
	values := [[1, 3]]
	found := contains_nested(needle, values)
}
'
	selfhost_source := generate(selfhost_fixture, 'selfhost_nested_array_membership.v', selfhost_prefs) or { panic(err) }
	for side in ['l', 'r'] {
		assert selfhost_source.contains('((int *)__vf_meq_${side}.data)[__vf_meq_k]'), selfhost_source
	}
	root := os.join_path(os.vtmp_dir(), 'fastc_nested_array_membership_${os.getpid()}')
	os.mkdir_all(root) or { panic(err) }
	defer {
		os.rmdir_all(root) or {}
	}
	c_file := os.join_path(root, 'program.c')
	bin_file := os.join_path(root, 'program')
	os.write_file(c_file, c_source) or { panic(err) }
	tcc := os.join_path(prefs.vroot, 'thirdparty', 'tcc', 'tcc.exe')
	compiled := cmdexec.run(tcc, ['-std=gnu11', '-o', bin_file, c_file])
	assert compiled.exit_code == 0, compiled.output
	run := cmdexec.run(bin_file, [])
	assert run.exit_code == 0, run.output
	assert run.output == 'false\ntrue\ntrue\nfalse\nfalse\ntrue\n', run.output
}

fn test_ordinary_float_map_callbacks_use_full_key_width() {
	source := 'type Float = f64
type Single = f32

fn main() {
	bare := {1.0: 10, 2.0: 20}
	println(bare.len)
	println(bare[1.0])
	println(bare[2.0])
	println(3.0 in bare)
	doubles := {Float(1.0): 10, Float(2.0): 20}
	println(doubles.len)
	println(doubles[Float(1.0)])
	println(doubles[Float(2.0)])
	println(Float(1.0) in doubles)
	println(Float(3.0) in doubles)
	println(Float(3.0) !in doubles)
	println(doubles[Float(3.0)])
	singles := {Single(1.0): 30, Single(2.0): 40}
	println(singles.len)
	println(singles[Single(1.0)])
	println(singles[Single(2.0)])
	println(Single(3.0) !in singles)
	bare_singles := {f32(1.0): 50, f32(2.0): 60}
	println(bare_singles.len)
	println(bare_singles[f32(1.0)])
	println(bare_singles[f32(2.0)])
	println(f32(3.0) in bare_singles)
}
'
	prefs := pref.new_preferences()
	c_source := generate(source, 'ordinary_float_map_key_storage.v', prefs) or { panic(err) }
	for key_type in ['f64', 'Float'] {
		assert c_source.contains('builtin__new_map(sizeof(${key_type}), sizeof(${fastc_platform_int_c_type}), &builtin__map_hash_int_8, &builtin__map_eq_int_8, &builtin__map_clone_int_8,'), c_source
	}
	for key_type in ['f32', 'Single'] {
		assert c_source.contains('builtin__new_map(sizeof(${key_type}), sizeof(${fastc_platform_int_c_type}), &builtin__map_hash_int_4, &builtin__map_eq_int_4, &builtin__map_clone_int_4,'), c_source
	}
	root := os.join_path(os.vtmp_dir(), 'fastc_float_map_key_storage_${os.getpid()}')
	os.mkdir_all(root) or { panic(err) }
	defer {
		os.rmdir_all(root) or {}
	}
	c_file := os.join_path(root, 'program.c')
	bin_file := os.join_path(root, 'program')
	os.write_file(c_file, c_source) or { panic(err) }
	tcc := os.join_path(prefs.vroot, 'thirdparty', 'tcc', 'tcc.exe')
	compiled := cmdexec.run(tcc, ['-std=gnu11', '-o', bin_file, c_file])
	assert compiled.exit_code == 0, compiled.output
	run := cmdexec.run(bin_file, [])
	assert run.exit_code == 0, run.output
	assert run.output == '2\n10\n20\nfalse\n2\n10\n20\ntrue\nfalse\ntrue\n0\n2\n30\n40\ntrue\n2\n50\n60\nfalse\n', run.output
}

fn test_ordinary_array_index_is_bounds_checked() {
	prefs := pref.new_preferences()
	for index in ['-1', '3'] {
		c_source := generate('fn main() {\n\tvalues := [1, 2, 3]\n\tprintln(values[${index}])\n}\n',
			'collection_bounds.v', prefs) or { panic(err) }
		root := os.join_path(os.vtmp_dir(), 'fastc_collection_bounds_${os.getpid()}')
		os.mkdir_all(root) or { panic(err) }
		c_file := os.join_path(root, 'program.c')
		bin_file := os.join_path(root, 'program')
		os.write_file(c_file, c_source) or { panic(err) }
		tcc := os.join_path(prefs.vroot, 'thirdparty', 'tcc', 'tcc.exe')
		compiled := cmdexec.run(tcc, ['-std=gnu11', '-o', bin_file, c_file])
		assert compiled.exit_code == 0, compiled.output
		run := cmdexec.run(bin_file, [])
		assert run.exit_code != 0
		assert run.output.contains('array index out of range'), run.output
		os.rmdir_all(root) or {}
	}
}

fn test_ordinary_collection_reads_inside_arithmetic_and_calls() {
	source := "module main

fn next(value int) int {
	return value + 1
}

fn main() {
	values := [1, 2, 3]
	counts := {'a': 4}
	sum := values[0] + values[1]
	incremented := counts['a'] + 1
	println(sum)
	println(incremented)
	println(values[0] + values[1])
	println(counts['a'] + 1)
	println(next(values[0] + counts['a']))
	println(values[counts['a'] - 3] + counts['missing'])
	if values[0] + counts['a'] == 5 && counts['missing'] == 0 {
		println('ok')
	}
}
"
	prefs := pref.new_preferences()
	c_source := generate(source, 'ordinary_collection_arithmetic.v', prefs) or { panic(err) }
	root := os.join_path(os.vtmp_dir(), 'fastc_collection_arithmetic_${os.getpid()}')
	os.mkdir_all(root) or { panic(err) }
	defer {
		os.rmdir_all(root) or {}
	}
	c_file := os.join_path(root, 'program.c')
	bin_file := os.join_path(root, 'program')
	os.write_file(c_file, c_source) or { panic(err) }
	tcc := os.join_path(prefs.vroot, 'thirdparty', 'tcc', 'tcc.exe')
	compiled := cmdexec.run(tcc, ['-std=gnu11', '-o', bin_file, c_file])
	assert compiled.exit_code == 0, compiled.output
	run := cmdexec.run(bin_file, [])
	assert run.exit_code == 0, run.output
	assert run.output == '3\n5\n3\n5\n6\n2\nok\n', run.output
}

fn test_ordinary_map_callbacks_match_enum_and_alias_key_storage() {
	source := 'module main

enum Color { red green blue }
type Shade = Color
type Identifier = int
type Counter = Identifier
@[flag]
enum Access { read write }

fn main() {
	colors := {Color.red: 10, Color.blue: 20, Color.red: 30}
	println(Color.red in colors)
	println(colors[Color.red])
	println(colors[Color.blue])
	println(colors[Color.green])
	println(Color.green !in colors)
	println(colors.len)

	shades := {Shade(Color.red): 40, Shade(Color.blue): 50}
	println(shades[Shade(Color.blue)])
	println(Shade(Color.red) in shades)
	println(Shade(Color.green) !in shades)

	ids := {Identifier(1): 60, Identifier(2): 70, Identifier(1): 80}
	println(ids[Identifier(1)])
	println(ids[Identifier(3)])
	println(Identifier(2) in ids)
	println(Identifier(3) !in ids)
	println(ids.len)
	counters := {Counter(1): 90, Counter(2): 100}
	println(counters[Counter(2)])
	println(Counter(1) in counters)
	println(Counter(3) !in counters)

	permissions := {Access.read: 110, Access.write: 120}
	println(permissions[Access.write])
	println(Access.read in permissions)
}
'
	prefs := pref.new_preferences()
	c_source := generate(source, 'ordinary_map_key_storage.v', prefs) or { panic(err) }
	for key_type in ['Color', 'Shade', 'Identifier', 'Counter'] {
		assert c_source.contains('builtin__new_map(sizeof(${key_type}), sizeof(${fastc_platform_int_c_type}), &builtin__map_hash_int_4, &builtin__map_eq_int_4, &builtin__map_clone_int_4,'), c_source
	}
	assert c_source.contains('builtin__new_map(sizeof(Access), sizeof(${fastc_platform_int_c_type}), &builtin__map_hash_int_8, &builtin__map_eq_int_8, &builtin__map_clone_int_8,'), c_source
	root := os.join_path(os.vtmp_dir(), 'fastc_map_key_storage_${os.getpid()}')
	os.mkdir_all(root) or { panic(err) }
	defer {
		os.rmdir_all(root) or {}
	}
	c_file := os.join_path(root, 'program.c')
	bin_file := os.join_path(root, 'program')
	os.write_file(c_file, c_source) or { panic(err) }
	tcc := os.join_path(prefs.vroot, 'thirdparty', 'tcc', 'tcc.exe')
	compiled := cmdexec.run(tcc, ['-std=gnu11', '-o', bin_file, c_file])
	assert compiled.exit_code == 0, compiled.output
	run := cmdexec.run(bin_file, [])
	assert run.exit_code == 0, run.output
	assert run.output == 'true\n30\n20\n0\ntrue\n2\n50\ntrue\ntrue\n80\n0\ntrue\ntrue\n2\n100\ntrue\ntrue\n120\ntrue\n', run.output
}
