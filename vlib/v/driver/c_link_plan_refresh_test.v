module driver

import os
import time
import v.modulecache

// A plan whose dependency metadata is stale (another compiler build wrote it)
// still validates by the dependency contents, and is written back with the
// current metadata, so that the next build skips reading the dependencies.
fn test_valid_c_link_plan_refreshes_stale_dependency_metadata() {
	root := os.join_path(os.vtmp_dir(), 'v3_link_plan_refresh_${os.getpid()}')
	os.rmdir_all(root) or {}
	os.mkdir_all(root)!
	defer {
		os.rmdir_all(root) or {}
	}
	dependency := os.join_path(root, 'dependency.h')
	os.write_file(dependency, '#define X 1\n')!
	object := os.join_path(root, 'input.o')
	os.write_file(object, 'object')!
	mut signatures := CObjectCacheStats{}
	signature := c_object_file_signature(dependency, false, mut signatures)
	plan_path := os.join_path(root, 'link.manifest')
	os.write_file(plan_path, 'format=v3-c-link-plan-v7\nrequests=1\ndirect_objects=0\ndependency_files=1\nflag=${object}\nobject=${object}\ndependency=${dependency}\tstale:metadata\t${signature}\ncomplete=1\n')!
	mut first := CObjectCacheStats{}
	plan := valid_c_link_plan(plan_path, mut first) or { panic('the stale plan does not validate') }
	assert plan.requests == 1
	assert plan.flags == [object]
	assert first.dependency_file_reads == 1
	if modulecache.file_metadata_signature(dependency).len > 0 {
		mut second := CObjectCacheStats{}
		refreshed := valid_c_link_plan(plan_path, mut second) or {
			panic('the refreshed plan does not validate')
		}
		assert refreshed.flags == [object]
		assert second.dependency_file_reads == 0
	}
	// Changed contents still invalidate the plan.
	// Step past the filesystem timestamp granularity before a same-size rewrite.
	time.sleep(100 * time.millisecond)
	os.write_file(dependency, '#define X 2\n')!
	mut third := CObjectCacheStats{}
	if _ := valid_c_link_plan(plan_path, mut third) {
		assert false, 'a changed dependency validated'
	}
}

fn test_link_plan_carries_header_lookup_paths_into_the_receipt() {
	$if windows {
		return
	}
	root := os.join_path(os.vtmp_dir(), 'v3_link_symlink_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	object := os.join_path(root, 'value.o')
	one := os.join_path(root, 'one.h')
	two := os.join_path(root, 'two.h')
	header := os.join_path(root, 'value.h')
	missing := os.join_path(root, 'earlier.h')
	os.write_file(object, 'object')!
	os.write_file(one, '#define VALUE 1\n')!
	os.write_file(two, '#define VALUE 2\n')!
	os.symlink(one, header)!
	mut written := CObjectCacheStats{
		requests:         1
		dependency_files: 1
	}
	_ := c_object_file_signature(header, false, mut written)
	written.missing_candidates[missing] = true
	path := os.join_path(root, 'plan')
	write_c_link_plan(path, [object], &written)!
	mut loaded := CObjectCacheStats{}
	assert valid_c_link_plan(path, mut loaded) != none
	assert loaded.file_signatures.keys() == [header]
	assert loaded.missing_candidates[missing]
	os.rm(header)!
	os.symlink(two, header)!
	mut after := CObjectCacheStats{}
	assert valid_c_link_plan(path, mut after) == none
}
