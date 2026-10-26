// vtest vflags: -gc boehm -d no_gc_thread_local_alloc -d use_bundled_libgc
module main

// The collector owns the full statistics layout and its function declaration.
#include <gc.h>

struct C.GC_prof_stats_s {
	bytes_allocd_since_gc  uint
	allocd_bytes_before_gc uint
	gc_no                  uint
}

fn C.GC_get_prof_stats(&C.GC_prof_stats_s, uint) uint

fn map_equality_allocations() (u64, u64) {
	mut stats := C.GC_prof_stats_s{}
	C.GC_get_prof_stats(&stats, sizeof(C.GC_prof_stats_s))
	return u64(stats.allocd_bytes_before_gc) + u64(stats.bytes_allocd_since_gc), u64(stats.gc_no)
}

fn maps_equal[T](left map[string]T, right map[string]T) bool {
	return left == right
}

fn test_map_equality_borrows_string_keys_without_allocating() {
	mut left := map[string]f64{}
	mut right := map[string]f64{}
	mut changed_value := map[string]f64{}
	mut changed_key := map[string]f64{}
	for i in 0 .. 6 {
		key := 'dynamic-key-${i}'
		left[key] = f64(i)
		right[key] = f64(i)
		changed_value[key] = f64(i + 1)
		changed_key['other-key-${i}'] = f64(i)
	}
	// A deleted slot must still be skipped by the borrowed-key iterator.
	left['removed'] = 20
	left.delete('removed')
	mut buffers := [][]u8{cap: 148}
	for _ in 0 .. 128 { buffers << []u8{len: 16} }
	for i in 0 .. 20 {
		before, _ := map_equality_allocations()
		mut allocation := []u8{len: 16}
		allocation[0] = u8(i)
		after, _ := map_equality_allocations()
		assert after > before, 'the GC counter must observe each allocation after warmup'
		buffers << allocation
	}
	assert buffers.len == 148
	assert left == right
	assert maps_equal(left, right)
	before, collections_before := map_equality_allocations()
	for _ in 0 .. 128 {
		assert left == right
		assert !(left != right)
		assert left != changed_value
		assert !(left == changed_key)
		assert maps_equal(left, right)
		assert !maps_equal(left, changed_value)
	}
	after, collections_after := map_equality_allocations()
	assert after == before, 'map comparisons must not clone their temporary string keys'
	assert collections_after == collections_before
}

fn temporary_map(collect bool) map[string]string {
	if collect { gc_collect() }
	return {
		'temporary-${17}': 'value-${23}'
	}
}

struct MapPointerValue {
	value int
}

fn test_map_equality_keeps_temporary_maps_and_pointer_values_alive() {
	assert temporary_map(false) == temporary_map(true)
	left_key := 'pointer-${17}'
	right_key := 'pointer-${17}'
	first := &MapPointerValue{ value: 17 }
	second := &MapPointerValue{ value: 17 }
	left := {
		left_key: first
	}
	right := {
		right_key: first
	}
	different := {
		right_key: second
	}
	assert maps_equal(left, right)
	assert !maps_equal(left, different)
	gc_collect()
	assert (left[left_key] or { panic('missing left value') }).value == 17
	assert (right[right_key] or { panic('missing right value') }).value == 17
	assert left_key == right_key
	pointer_keys_left := {
		voidptr(first): 'same-${17}'
	}
	pointer_keys_right := {
		voidptr(first): 'same-${17}'
	}
	pointer_keys_different := {
		voidptr(second): 'same-${17}'
	}
	assert pointer_keys_left == pointer_keys_right
	assert pointer_keys_left != pointer_keys_different
	gc_collect()
	assert pointer_keys_left[voidptr(first)] == 'same-17'
}

fn test_authored_iteration_with_equality_like_name_keeps_owned_string_keys() {
	mut values := {
		'owned-${17}': 17
	}
	mut retained := []string{}
	for map_eq_key_0, _ in values {
		// The ordinary iterator must still copy the key, even when this local
		// resembles the compiler's equality temporary.
		retained << unsafe { map_eq_key_0 }
		values.delete(map_eq_key_0)
	}
	assert values.len == 0
	gc_collect()
	assert retained == ['owned-17']
}
