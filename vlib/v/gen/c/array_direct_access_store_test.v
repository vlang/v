module c

import os
import v.markused
import v.parser
import v.pref
import v.transform
import v.types

// An element store in a function marked @[direct_array_access] has to go through
// the array data pointer, the same way an element load does. When it did not, a
// hot loop that writes array elements compiled to an array__set() call per
// element while the reads next to it were inlined, which cost 2x to 11x on
// write-heavy loops. The load below the store is direct in both functions, so
// only the store is different.
fn test_direct_array_access_makes_element_stores_direct() {
	path := os.join_path(os.vtmp_dir(), 'direct_array_store_${os.getpid()}.c.v')
	os.write_file(path, '@[direct_array_access]
fn direct_store(mut a []u64, n int) {
	for i in 0 .. n {
		a[i] = u64(i)
	}
}

@[direct_array_access]
fn direct_add(mut a []u64, n int) {
	for i in 0 .. n {
		a[i] += 1
	}
}

fn checked_store(mut a []u64, n int) {
	for i in 0 .. n {
		a[i] = u64(i)
	}
}

fn checked_add(mut a []u64, n int) {
	for i in 0 .. n {
		a[i] += 1
	}
}

fn main() {
	mut a := []u64{len: 2}
	direct_store(mut a, 2)
	direct_add(mut a, 2)
	checked_store(mut a, 2)
	checked_add(mut a, 2)
}
')!
	defer {
		os.rm(path) or {}
	}
	mut prefs := pref.new_preferences()
	mut p := parser.Parser.new(prefs)
	mut a := p.parse_file(path)
	mut tc := types.TypeChecker.new(a)
	tc.collect(a)
	tc.check_semantics()
	assert tc.errors.len == 0, tc.errors.str()
	transform.transform(mut a, &tc)
	tc.annotate_types()
	used := markused.mark_used(a, tc)
	mut g := FlatGen.new()
	g.set_target(pref.target_from('linux', 'amd64') or { panic(err) })
	generated := g.gen_with_used_options(a, used, &tc, true)

	direct_store := generated.all_after('void direct_store(Array* a, i64 n) {').all_before('\n}').trim_space()
	assert direct_store.contains('(*((u64*)((a)->data) + (i))) = '), direct_store
	assert !direct_store.contains('array__set'), direct_store

	direct_add := generated.all_after('void direct_add(Array* a, i64 n) {').all_before('\n}').trim_space()
	assert direct_add.contains('(*((u64*)((a)->data) + (i))) += '), direct_add
	assert !direct_add.contains('array__set'), direct_add

	// Without the attribute the store keeps its bounds check, which is what makes
	// the two functions a pair rather than one behaviour tested twice.
	checked_store := generated.all_after('void checked_store(Array* a, i64 n) {').all_before('\n}').trim_space()
	assert checked_store.contains('array__set('), checked_store

	checked_add := generated.all_after('void checked_add(Array* a, i64 n) {').all_before('\n}').trim_space()
	assert checked_add.contains('array__set('), checked_add
}
