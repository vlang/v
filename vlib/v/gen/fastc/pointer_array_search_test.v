module fastc

import os
import v.cmdexec
import v.pref

fn test_fastc_pointer_array_searches_compare_pointer_values() {
	$if !macos && !linux {
		return
	}
	root := os.join_path(os.vtmp_dir(), 'fastc_pointer_array_search_${os.getpid()}')
	os.mkdir_all(root) or { panic(err) }
	defer { os.rmdir_all(root) or {} }
	os.write_file(os.join_path(root, 'native.h'), r'#ifndef FASTC_POINTER_ARRAY_SEARCH_H
#define FASTC_POINTER_ARRAY_SEARCH_H
#include <stdint.h>
static uint64_t search_records[4][2] = {
    { UINT64_C(0x1122334455667788), 11 },
    { UINT64_C(0x2233445566778899), 22 },
    { UINT64_C(0x33445566778899aa), 33 },
    { 0, 44 }
};
static void *search_record(int index) {
    /* A pointee comparison falsely matches record 0 when record 3 is absent. */
    search_records[3][0] = (uint64_t)(uintptr_t)search_records[0];
    return search_records[index];
}
#endif
') or { panic(err) }
	source_path := os.join_path(root, 'main.v')
	os.write_file(source_path, r'module main

#include "@DIR/native.h"

struct Record {
	first_word u64
	marker u64
}

type RecordRef = &Record
type NestedRecordRef = RecordRef
type RecordRefs = []&Record

struct Holder {
mut:
	items []&Record
}

struct Counter {
mut:
	calls int
}

fn C.search_record(index int) voidptr

fn next_pointer(mut counter Counter, pointer &Record) &Record {
	counter.calls++
	return pointer
}

fn main() {
	first := unsafe { &Record(C.search_record(0)) }
	middle := unsafe { &Record(C.search_record(1)) }
	last := unsafe { &Record(C.search_record(2)) }
	missing := unsafe { &Record(C.search_record(3)) }
	assert first.marker == 11 && middle.marker == 22 && last.marker == 33
	assert missing.first_word == unsafe { u64(first) }
	mut values := [first, middle, last, middle]
	assert values.index(first) == 0, "first pointer"
	assert values.index(middle) == 1, "middle pointer"
	assert values.index(last) == 2, "last distinct pointer"
	assert values.index(missing) == -1, "missing pointer with matching pointee bytes"
	assert values.last_index(middle) == 3, "last duplicate pointer"
	assert values.last_index(first) == 0
	assert values.last_index(missing) == -1
	raw_values := [voidptr(first), voidptr(middle), voidptr(last), voidptr(middle)]
	assert raw_values.index(voidptr(middle)) == 1
	assert raw_values.last_index(voidptr(middle)) == 3
	assert raw_values.index(voidptr(missing)) == -1
	nil_pointer := unsafe { &Record(nil) }
	nil_values := [first, nil_pointer, middle, nil_pointer]
	assert nil_values.index(nil_pointer) == 1
	assert nil_values.last_index(nil_pointer) == 3
	empty := []&Record{}
	assert empty.index(first) == -1 && empty.last_index(first) == -1
	aliases := [RecordRef(first), RecordRef(middle), RecordRef(last), RecordRef(middle)]
	assert aliases.index(RecordRef(middle)) == 1
	assert aliases.last_index(RecordRef(middle)) == 3
	assert aliases.index(RecordRef(missing)) == -1
	nested_aliases := [NestedRecordRef(first), NestedRecordRef(middle)]
	assert nested_aliases.index(NestedRecordRef(middle)) == 1
	assert nested_aliases.last_index(NestedRecordRef(first)) == 0
	array_alias := RecordRefs([first, middle, last, middle])
	assert array_alias.index(middle) == 1
	assert array_alias.last_index(middle) == 3
	mut holder := Holder{items: [first, middle, last, middle]}
	assert holder.items.index(middle) == 1
	assert holder.items.last_index(middle) == 3
	holder_pointer := &holder
	assert holder_pointer.items.index(last) == 2
	array_pointer := &values
	assert array_pointer.index(middle) == 1
	assert array_pointer.last_index(middle) == 3
	member_pointer := &holder.items
	assert member_pointer.index(last) == 2
	mut calls := Counter{}
	assert values.index(next_pointer(mut calls, middle)) == 1
	assert calls.calls == 1, "index argument evaluated once"
	assert values.last_index(next_pointer(mut calls, middle)) == 3
	assert calls.calls == 2, "last_index argument evaluated once"
	mut removed := [first, middle, last]
	removed.delete(removed.index(middle))
	assert removed.len == 2 && removed[0] == first && removed[1] == last
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
