module types

import os
import v.flat
import v.parser
import v.pref

// The transform stage memoizes type parses into its own cache, in front of the frozen cache of
// the check. A per-file resolution view made before the transform kept writing into the frozen
// cache, while the transform's disposable arena was current: a map of that cache that grew there
// was left pointing into freed memory (issue #30211).
fn test_resolution_views_made_before_the_transform_use_its_type_cache() {
	path := os.join_path(os.vtmp_dir(), 'v3_transform_resolution_views_${os.getpid()}.v')
	os.write_file(path, 'module shapes
pub struct Circle {
	r int
}
pub struct Square {
	side int
}
pub fn area() int {
	return 0
}
')!
	defer {
		os.rm(path) or {}
	}
	mut p := parser.Parser.new(pref.new_preferences())
	a := p.parse_file(path)
	assert p.diagnostics.len == 0, p.diagnostics.str()
	mut tc := TypeChecker.new(a)
	tc.collect(a)
	tc.enable_scoped_parallel_workers()
	tc.reset_resolution_type_view_cache()
	// The check parses through a view of this file, which stays cached for the file.
	circles := tc.parse_resolution_type_in_file(flat.source_type_id('[]Circle'), path)
	assert circles.name() == '[]shapes.Circle'
	check_cache := tc.type_cache
	check_entries := check_cache.parse_entries.len
	tc.begin_sparse_transform_node_caches(a.nodes.len)
	assert voidptr(tc.pre_transform_type_cache) == voidptr(check_cache)
	assert voidptr(tc.type_cache) != voidptr(check_cache)
	squares := tc.parse_resolution_type_in_file(flat.source_type_id('[]Square'), path)
	assert squares.name() == '[]shapes.Square'
	assert check_cache.parse_entries.len == check_entries
	assert tc.type_cache.parse_entries.len > 0
}
