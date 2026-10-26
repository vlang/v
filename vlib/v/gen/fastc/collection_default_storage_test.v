module fastc

import os
import v.cmdexec
import v.pref

fn test_fastc_collection_field_defaults_preserve_storage_width() {
	$if !macos && !linux {
		return
	}
	root := os.join_path(os.vtmp_dir(), 'fastc_collection_default_storage_${os.getpid()}')
	os.mkdir_all(root) or { panic(err) }
	defer { os.rmdir_all(root) or {} }
	os.write_file(os.join_path(root, 'main.c.v'), r"module main
@[c_extern]
fn C.alias_map_value_size(voidptr) uint
type Counts = map[string]int
type ScopeStack = []int
struct Tracker {
mut:
    counts map[string]int
    aliased_counts Counts
    wide map[int]int
    undos []string
    scopes []int
    aliased_scopes ScopeStack
}
fn (mut t Tracker) begin() { t.scopes << t.undos.len }
fn (mut t Tracker) bind(name string) {
    t.undos << name
    t.counts[name] = (t.counts[name] or { 0 }) + 1
}
fn (mut t Tracker) end() {
    start := t.scopes.pop()
    for i := t.undos.len - 1; i >= start; i-- {
        name := t.undos[i]
        count := t.counts[name] or { 0 }
        if count <= 1 { t.counts.delete(name) } else { t.counts[name] = count - 1 }
    }
    t.undos.trim(start)
}
fn (t &Tracker) contains(name string) bool { return (t.counts[name] or { 0 }) > 0 }
fn optional(value int) ?int { return value }
fn main() {
    mut t := Tracker{}
    for _ in 0 .. 6 {
        t.begin()
        assert !t.contains('i'), 'new outer scope'
        t.bind('i')
        t.begin()
        t.bind('j')
        t.end()
        assert !t.contains('j'), 'inner scope cleanup'
        assert t.contains('i'), 'outer scope retained'
        t.end()
        assert !t.contains('i'), 'outer scope cleanup'
    }
    key := int(0x100000002)
    value := int(0x200000003)
    t.wide[key] = value
    t.wide[2] = -7
    assert (t.wide[key] or { -1 }) == value, 'wide key and value'
    assert (t.wide[2] or { -1 }) == -7, 'small key remains distinct'
    assert (t.wide[99] or { value }) == value, 'wide missing fallback'
    t.wide.delete(key)
    assert (t.wide[key] or { -1 }) == -1, 'wide key deletion'
    assert (t.wide[2] or { -1 }) == -7, 'other key survives deletion'
    assert C.alias_map_value_size(voidptr(&t.aliased_counts)) == uint(sizeof(value)), 'aliased map storage size'
    t.aliased_scopes << value
    assert t.aliased_scopes[0] == value, 'aliased array storage'
    assert (optional(value) or { -1 }) == value, 'wide optional payload'
    if found := optional(value) {
        assert found == value, 'wide optional guard'
    } else { assert false }
    selected := if found := optional(value) { found } else { -1 }
    assert selected == value, 'wide optional guard expression'
    println('42')
}
") or { panic(err) }
	mut prefs := pref.new_preferences()
	prefs.vroot = @VEXEROOT
	prefs.building_v = true
	prefs.target = pref.host_target()
	prefs.user_defines = ['gc_none', 'no_backtrace', 'v3_backend']
	c_source := generate_files([root], prefs) or { panic(err) }
	c_path := os.join_path(root, 'program.c')
	program := os.join_path(root, 'program')
	os.write_file(c_path, c_source + '\nsize_t alias_map_value_size(void *value) { return ((map *)value)->data->value_bytes; }\n') or {
		panic(err)
	}
	build := cmdexec.run('cc', ['-std=gnu11', '-Werror=implicit-function-declaration', c_path,
		'-o', program])
	assert build.exit_code == 0, build.output
	run := cmdexec.run(program, [])
	assert run.exit_code == 0, run.output
	assert run.output.trim_space() == '42', run.output
}
