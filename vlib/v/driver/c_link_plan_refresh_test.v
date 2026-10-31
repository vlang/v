module driver

import os
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
	os.write_file(plan_path, 'format=v3-c-link-plan-v5\nrequests=1\ndirect_objects=0\ndependency_files=1\nflag=${object}\nobject=${object}\ndependency=${dependency}\tstale:metadata\t${signature}\ncomplete=1\n')!
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
	os.write_file(dependency, '#define X 2\n')!
	mut third := CObjectCacheStats{}
	if _ := valid_c_link_plan(plan_path, mut third) {
		assert false, 'a changed dependency validated'
	}
}
