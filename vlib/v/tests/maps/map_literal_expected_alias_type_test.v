// A map literal written with an alias of the expected map type, or with keys and
// values that are aliases of the expected ones, is the same map and is accepted.
type MissMap = map[u64]bool
type MissKey = u64
type MissFlag = bool
type MissKeyMap = map[MissKey]bool

struct MissCache {
mut:
	misses  map[u64]bool
	aliased MissMap
	nested  map[string]map[u64]bool
	keyed   map[u64]bool   = map[MissKey]MissFlag{}
	counts  map[string]i64 = {
		'a': 1
	}
}

const wide_key = u64(1) << 40

fn count_misses(misses map[u64]bool) int {
	return misses.len
}

fn new_misses() map[u64]bool {
	return MissMap{}
}

fn new_miss_map() MissMap {
	return map[u64]bool{}
}

fn test_aliased_map_literals_take_the_expected_map_type() {
	mut cache := MissCache{
		misses:  MissMap{}
		aliased: map[u64]bool{}
		nested:  {
			'a': map[u64]bool{}
		}
	}
	cache.misses = MissKeyMap{}
	cache.misses[wide_key] = true
	cache.aliased = map[MissKey]bool{}
	cache.aliased[wide_key + 1] = true
	cache.nested['b'] = MissMap{}
	cache.nested['b'][wide_key + 2] = true
	cache.keyed[wide_key + 3] = true
	assert cache.misses[wide_key]
	assert !cache.misses[wide_key + 1]
	assert cache.aliased[wide_key + 1]
	assert cache.nested['b'][wide_key + 2]
	assert cache.keyed[wide_key + 3]
	assert cache.counts['a'] == 1
	assert count_misses(MissMap{}) + count_misses(map[MissKey]MissFlag{}) == 0
	assert new_misses().len + new_miss_map().len == 0
}

fn test_untyped_map_literals_still_take_the_expected_map_type() {
	mut misses := map[u64]bool{}
	misses = {}
	misses = {
		wide_key: true
	}
	assert misses[wide_key]
	mut counts := map[string]i64{}
	counts = {
		'b': 2
	}
	assert counts['b'] == 2
	assert count_misses({}) == 0
	copy := {
		...misses
		wide_key + 1: true
	}
	assert copy.len == 2
}
