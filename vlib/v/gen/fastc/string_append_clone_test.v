module fastc

import os
import v.cmdexec
import v.pref

fn test_fastc_string_appends_copy_borrowed_bytes_and_preserve_bulk_semantics() {
	$if !macos && !linux {
		return
	}
	root := os.join_path(os.vtmp_dir(), 'fastc_string_append_clone_${os.getpid()}')
	os.mkdir_all(root) or { panic(err) }
	defer { os.rmdir_all(root) or {} }
	os.write_file(os.join_path(root, 'optional.h'), r'static unsigned char optional_bytes[] = "optional";
static unsigned char *optional_storage(void) { return optional_bytes; }
') or { panic(err) }
	source_path := os.join_path(root, 'main.v')
	os.write_file(source_path, r'module main
#include "@DIR/optional.h"
type Text = string
struct Holder {
mut:
	text string
	items []string
}
struct Counter {
mut:
	value int
}
fn mutate_original(value string) {
	// Keep the allocation alive and change its bytes to detect accidental sharing.
	unsafe { value.str[0] = `X` }
}
fn returned(mut calls Counter) string {
	calls.value++
	return "returned".clone()
}
fn C.optional_storage() &u8
fn optional() ?string { return unsafe { tos(C.optional_storage(), 8) } }
fn buffer(value string) voidptr { return unsafe { value.str } }
fn main() {
	local := "local".clone()
	mut values := []string{}
	values << local
	mutate_original(local)
	assert local == "Xocal"
	assert values[0] == "local", "local string append"
	mut holder := Holder{text: "field".clone()}
	values << holder.text
	mutate_original(holder.text)
	assert values[1] == "field", "field string append"
	mut source := ["index".clone()]
	values << source[0]
	mutate_original(source[0])
	assert values[2] == "index", "indexed string append"
	mut slice_source := ["sliced".clone()]
	mut slice_values := []string{}
	slice_values << slice_source[..][0]
	mutate_original(slice_source[0])
	assert slice_values[0] == "sliced", "slice-indexed string append"
	member := "member".clone()
	holder.items << member
	mutate_original(member)
	assert holder.items[0] == "member", "member array target"
	map_value := "mapped".clone()
	mut mapped := map[string][]string{}
	mapped["out"] = []string{}
	mapped["out"] << map_value
	mutate_original(map_value)
	assert mapped["out"][0] == "mapped", "map array target"
	nested_value := "nested".clone()
	mut nested := [][]string{len: 1}
	nested[0] << nested_value
	mutate_original(nested_value)
	assert nested[0][0] == "nested", "nested array target"
	mut incoming := ["bulk".clone()]
	mut bulk := []string{}
	bulk << incoming
	mutate_original(incoming[0])
	// The C backend copies descriptors for bulk append; it does not clone each string.
	assert bulk[0] == "Xulk", "bulk append keeps descriptor sharing"
	alias_value := Text("alias".clone())
	mut aliases := []Text{}
	aliases << alias_value
	mutate_original(alias_value)
	assert aliases[0] == Text("alias"), "string alias append"
	mut calls := Counter{}
	values << returned(mut calls)
	values << "literal"
	assert calls.value == 1 && values[3] == "returned" && values[4] == "literal"
	mut optional_values := []string{}
	optional_values << (optional() or { panic("missing optional string") })
	assert optional_values[0] == "optional"
	assert buffer(optional_values[0]) == voidptr(C.optional_storage()), "optional call transfers its buffer"
}
') or { panic(err) }
	baseline := os.join_path(root, 'baseline')
	baseline_build := cmdexec.run_in_merged_with_env(@VEXE, ['-b', 'c', '-gc', 'none', '-o', baseline,
		source_path], @VEXEROOT, {
		'VEXE':                   @VEXE
		'VFLAGS':                 ''
		'VOSARGS':                ''
		'V_MACOS_V3_NO_FALLBACK': '1'
	})
	assert baseline_build.exit_code == 0, baseline_build.output
	baseline_run := cmdexec.run(baseline, [])
	assert baseline_run.exit_code == 0, baseline_run.output
	mut prefs := pref.new_preferences()
	prefs.vroot = @VEXEROOT
	prefs.building_v = true
	prefs.backend = 'fastc'
	prefs.ccompiler = 'clang'
	prefs.user_defines = ['gc_none', 'no_backtrace', 'v3_backend']
	for target_headers in [false, true] {
		prefs.target_libc_headers = target_headers
		source := generate_files([root], prefs) or { panic(err) }
		c_path := os.join_path(root, 'main.c')
		program := os.join_path(root, 'program')
		os.write_file(c_path, source) or { panic(err) }
		build := cmdexec.run('cc', ['-std=gnu11', '-O2', '-o', program, c_path])
		assert build.exit_code == 0, build.output
		run := cmdexec.run(program, [])
		assert run.exit_code == 0, 'target headers ${target_headers}: ${run.output}'
	}
}
