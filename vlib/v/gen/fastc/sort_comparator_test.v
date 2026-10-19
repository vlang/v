module fastc

import os
import v.cmdexec
import v.pref

// `.sort(cond)` lowers to a generated comparator that is passed to
// `sort_with_compare` as a `FnSortCB`, whose V return type is the
// platform-width `int`. A comparator declared with C `int` returns -1 in the
// low 32 bits only, so the 64-bit caller reads it as a positive value and the
// sort reverses its input instead of ordering it.
fn test_fastc_sort_comparator_returns_the_platform_int() {
	$if !macos && !linux {
		return
	}
	root := os.join_path(os.vtmp_dir(), 'fastc_sort_comparator_${os.getpid()}')
	os.mkdir_all(root) or { panic(err) }
	defer { os.rmdir_all(root) or {} }
	os.write_file(os.join_path(root, 'main.v'), r"module main
struct Item {
    id   int
    name string
}
fn main() {
    mut items := [Item{3, 'c'}, Item{1, 'a'}, Item{2, 'b'}, Item{5, 'e'}, Item{4, 'd'}, Item{0, ''}]
    items.sort(a.id < b.id)
    for i, item in items {
        assert item.id == i, 'ascending by field'
    }
    items.sort(a.name > b.name)
    for i, item in items {
        assert item.id == 5 - i, 'descending by string field'
    }
    mut values := [5, 3, 9, 1, 7, 2, 8]
    values.sort(a > b)
    assert values == [9, 8, 7, 5, 3, 2, 1], 'descending ints'
    println('42')
}
") or { panic(err) }
	mut prefs := pref.new_preferences()
	prefs.vroot = @VEXEROOT
	prefs.building_v = true
	prefs.target = pref.host_target()
	prefs.user_defines = ['gc_none', 'no_backtrace', 'v3_backend']
	c_source := generate_files([root], prefs) or { panic(err) }
	assert c_source.contains('${fastc_platform_int_c_type} v_fastc_sort_Item_'), c_source
	c_path := os.join_path(root, 'program.c')
	program := os.join_path(root, 'program')
	os.write_file(c_path, c_source) or { panic(err) }
	build := cmdexec.run('cc', ['-std=gnu11', '-Werror=implicit-function-declaration', c_path,
		'-o', program])
	assert build.exit_code == 0, build.output
	run := cmdexec.run(program, [])
	assert run.exit_code == 0, run.output
	assert run.output.trim_space() == '42', run.output
}
