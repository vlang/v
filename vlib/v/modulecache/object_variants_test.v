module modulecache

import os
import time

const object_lease_test_dir = 'V3_TEST_OBJECT_LEASE_DIR'

fn testsuite_begin() {
	root := os.getenv(object_lease_test_dir)
	if root == '' {
		return
	}
	manager := Manager{ dir: root, enabled: true }
	mut lease := manager.begin_object_cache_build() or { exit(2) }
	os.write_file(os.join_path(root, 'reader-ready'), '') or { exit(3) }
	for _ in 0 .. 3000 {
		if os.exists(os.join_path(root, 'release-reader')) {
			lease.release()
			exit(0)
		}
		time.sleep(10 * time.millisecond)
	}
	exit(4)
}

fn test_retired_objects_are_reclaimed_after_the_last_reader_releases_them() ! {
	root := os.join_path(os.vtmp_dir(), 'v3_object_variant_gc_${os.getpid()}')
	os.rmdir_all(root) or {}
	defer { os.rmdir_all(root) or {} }
	manager := Manager{ dir: root, enabled: true }
	mut writer := manager.begin_object_cache_build()!
	source := os.join_path(root, 'foo.v')
	os.write_file(source, 'module foo\n')!
	mut entries := []Entry{}
	for i in 0 .. max_object_variants + 3 {
		header := os.join_path(root, 'header_${i}.h')
		os.write_file(header, '#define VALUE ${i}\n')!
		inputs := {
			header: file_signature(header)
		}
		entry := manager.object_publish_entry('foo', [source], 'flags', inputs)
		os.write_file(entry.object, 'object ${i}')!
		os.write_file(entry.c_source, 'source ${i}')!
		manager.write_object_stamp(entry, 'foo', [source], inputs, 'flags')!
		entries << entry
	}
	assert manager.object_variant_keys('foo', [source], 'flags').len == max_object_variants

	mut reader := os.new_process(os.executable())
	mut environment := os.environ()
	environment[object_lease_test_dir] = root
	reader.set_environment(environment)
	reader.run()
	defer {
		if reader.is_alive() {
			reader.signal_kill()
		}
		reader.close()
	}
	ready := os.join_path(root, 'reader-ready')
	for _ in 0 .. 500 {
		if os.exists(ready) || !reader.is_alive() {
			break
		}
		time.sleep(10 * time.millisecond)
	}
	assert os.exists(ready)
	writer.release()
	mut next_build := manager.begin_object_cache_build()!
	assert os.exists(entries[0].object)
	next_build.release()

	os.write_file(os.join_path(root, 'release-reader'), '')!
	reader.wait()
	assert reader.code == 0
	mut final_build := manager.begin_object_cache_build()!
	defer { final_build.release() }
	for entry in entries[..3] {
		assert !os.exists(entry.object)
		assert !os.exists(entry.object_stamp)
		assert !os.exists(entry.c_source)
	}
	for entry in entries[3..] {
		assert os.exists(entry.object)
		assert os.exists(entry.object_stamp)
		assert os.exists(entry.c_source)
	}
}
