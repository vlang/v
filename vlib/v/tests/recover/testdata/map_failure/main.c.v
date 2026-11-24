module main

#flag -I @DIR
#include "deny.h"

fn C.map_fail_on(int)
fn C.wyhash(&u8, u64, u64, &u64) u64

fn insert(mut values map[string]int, key string, fail_at int) bool {
	defer {
		if _ := recover() {
			C.map_fail_on(0)
		}
	}
	C.map_fail_on(fail_at)
	values[key] = 42
	C.map_fail_on(0)
	return true
}

fn reserve(mut values map[string]int, capacity u32, fail_at int) bool {
	defer {
		if _ := recover() {
			C.map_fail_on(0)
		}
	}
	C.map_fail_on(fail_at)
	values.reserve(capacity)
	C.map_fail_on(0)
	return true
}

fn check_insert(seed []string, next string) int {
	mut failures := 0
	for position in 1 .. 24 {
		mut values := map[string]int{}
		for i, key in seed {
			values[key] = i
		}
		succeeded := insert(mut values, next, position)
		if !succeeded {
			failures++
			assert values.len == seed.len
			assert values.keys() == seed
			assert next !in values
		}
		for i, key in seed {
			assert values[key] == i
			assert key in values
		}
		assert insert(mut values, next, 0)
		assert values.len == seed.len + 1
		assert values[next] == 42
		if succeeded {
			return failures
		}
	}
	panic('failed to exercise every allocation site')
}

fn check_reserve(seed []string, remove_first bool) {
	for position in 1 .. 24 {
		mut values := map[string]int{}
		for i, key in seed {
			values[key] = i
		}
		if remove_first {
			values.delete(seed[0])
		}
		keys := values.keys()
		succeeded := reserve(mut values, 256, position)
		assert keys == values.keys()
		for i, key in seed {
			if remove_first && i == 0 {
				assert key !in values
			} else {
				assert key in values
				assert values[key] == i
			}
		}
		assert reserve(mut values, 256, 0)
		for i in 0 .. 256 {
			values['after-${i}'] = i
		}
		assert values.len == keys.len + 256
		if succeeded {
			return
		}
	}
	panic('failed to exercise every reserve allocation site')
}

// This fixture is compiled in debug mode, where V hashes standard string
// keys with wyhash(seed: 0). Select the last bucket in the initial table
// so a short, deterministic collision chain must extend the metadata tail.
fn check_collision_growth() {
	mut keys := []string{}
	for i := 0; keys.len < 8; i++ {
		key := 'collision-${i}'
		h := C.wyhash(key.str, u64(key.len), 0, &u64(voidptr(C._wyp)))
		if h & 30 == 30 {
			keys << key
		}
	}
	mut saw_growth := false
	for i in 1 .. keys.len {
		if check_insert(keys[..i], keys[i]) > 1 {
			saw_growth = true
		}
	}
	assert saw_growth
}

fn main() {
	mut keys := []string{}
	for i in 0 .. 80 {
		keys << 'map-key-${i}'
	}
	// Includes initial allocation, nonliteral string cloning, dense resize,
	// metadata doubling, and additional probe capacity for colliding hashes.
	for i in 0 .. keys.len {
		assert check_insert(keys[..i], keys[i]) >= 1
	}
	check_collision_growth()
	check_reserve([], false)
	check_reserve(keys[..8], true)
}
