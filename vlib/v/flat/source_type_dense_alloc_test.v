module flat

import os

fn test_source_type_dense_allocation_failures_use_entries_and_pages() {
	root := os.join_path(os.vtmp_dir(), 'source_type_dense_alloc_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	previous_failure := os.getenv('V_FLAT_TEST_DENSE_FAILURE')
	defer {
		if previous_failure == '' {
			os.unsetenv('V_FLAT_TEST_DENSE_FAILURE')
		} else {
			os.setenv('V_FLAT_TEST_DENSE_FAILURE', previous_failure, true)
		}
	}
	source := os.join_path(root, 'main.c.v')
	os.write_file(source, r'import v.flat

#include "@VEXEROOT/vlib/v/flat/testdata/source_type_dense_alloc_failure.h"

fn C.v_flat_test_dense_attempts() int

fn intern_types(worker int) {
	for i in 0 .. 1100 {
		text := "[]DenseAllocation${worker}.Item${i}"
		id := flat.source_type_id(text)
		assert (flat.source_type_existing_id(text) or { panic("missing interned spelling") }) == id
		assert flat.source_type_name(id) == text
		assert flat.source_type_shape(id).has(.array_type)
		assert flat.source_type_query(id).has(.complete)
		syntax := flat.source_type_syntax(id)
		assert syntax.kind() == .array
		assert syntax.trimmed() == id
		assert flat.source_type_syntax(id) == syntax
	}
}

fn main() {
	assert flat.source_type_shape(0) == unsafe { flat.SourceTypeShape(0) }
	assert flat.source_type_query(0).has(.complete)
	empty := flat.source_type_syntax(0)
	assert empty.kind() == .empty
	assert empty.trimmed() == 0
	mut workers := []thread{}
	for i in 0 .. 3 { workers << spawn intern_types(i) }
	workers.wait()
	assert flat.source_type_syntax(0) == empty
	// A refused optional reservation is attempted once, rather than per entry.
	assert C.v_flat_test_dense_attempts() == 2
}
')!
	for preallocated in [false, true] {
		binary := os.join_path(root, if preallocated { 'preallocated' } else { 'regular' })
		mut args := [@VEXE, '-gc', 'none', '-cc', @CCOMPILER, '-nocache']
		if preallocated {
			args << '-prealloc'
		}
		args << ['-o', binary, source]
		build := os.exec(args)
		assert build.exit_code == 0, build.output
		for failure in ['flags', 'syntax', 'both'] {
			os.setenv('V_FLAT_TEST_DENSE_FAILURE', failure, true)
			result := os.exec([binary])
			assert result.exit_code == 0, '${preallocated}, ${failure}: ${result.output}'
		}
	}
}
