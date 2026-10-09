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
