module driver

import os

fn C.v_native_object_cache_rename_noreplace(const_source &char, const_destination &char) i32

fn native_cache_rename_worker(root string, index int, gate chan bool) int {
	temporary := os.join_path(root, 'writer_${index}.o')
	os.write_file(temporary, 'object from writer ${index}') or { return -1 }
	defer { os.rm(temporary) or {} }
	destination := os.join_path(root, 'published.o')
	_ := <-gate
	return C.v_native_object_cache_rename_noreplace(&char(temporary.str), &char(destination.str))
}

fn test_native_object_no_replace_rename_keeps_the_first_complete_object() ! {
	$if !(linux || android || termux) {
		return
	}
	root := os.join_path(os.vtmp_dir(), 'native_cache_rename_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	gate := chan bool{cap: 16}
	mut publishers := []thread int{}
	for index in 0 .. 16 {
		publishers << spawn native_cache_rename_worker(root, index, gate)
	}
	for _ in 0 .. 16 { gate <- true }
	results := publishers.wait()
	assert results.filter(it == 0).len == 1, results.str()
	assert results.filter(it == C.EEXIST).len == 15, results.str()
	destination := os.join_path(root, 'published.o')
	first := os.read_file(destination)!
	assert first.starts_with('object from writer ')
	late := os.join_path(root, 'late.o')
	os.write_file(late, 'different LTO symbol IDs')!
	assert C.v_native_object_cache_rename_noreplace(&char(late.str), &char(destination.str)) == C.EEXIST
	assert os.read_file(destination)! == first
	assert os.read_file(late)! == 'different LTO symbol IDs'
	os.rm(late)!
	assert os.ls(root)! == ['published.o']
}

fn test_native_object_no_replace_rename_preserves_real_errors() ! {
	$if !(linux || android || termux) {
		return
	}
	root := os.join_path(os.vtmp_dir(), 'native_cache_rename_error_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	temporary := os.join_path(root, 'temporary.o')
	os.write_file(temporary, 'complete object')!
	destination := os.join_path(root, 'missing', 'published.o')
	assert C.v_native_object_cache_rename_noreplace(&char(temporary.str), &char(destination.str)) == C.ENOENT
	assert os.read_file(temporary)! == 'complete object'
	assert !os.exists(destination)
}
