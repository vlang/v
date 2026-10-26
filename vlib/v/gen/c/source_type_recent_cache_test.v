module c

import v.flat
import v.types

fn test_source_type_recent_cache_preserves_zero_keys_and_cached_misses() {
	mut cache := ContextSourceTypeLookupCache{}
	assert cache.get(0) == none
	assert cache.get(1) == none
	cache.put(0, 0)
	cache.put(1, 42)
	assert cache.get(0)? == 0
	assert cache.get(1)? == 42
	cache.put(1, 0)
	cache.put(2, 3)
	assert cache.get(1)? == 0
	assert cache.get(3) == none
	cache.put(0, 19)
	cache.put(2, 3)
	assert cache.get(0)? == 19
}

fn test_source_type_recent_cache_collisions_repopulate_from_the_full_map() {
	mut cache := ContextSourceTypeLookupCache{}
	for i in 0 .. 512 {
		cache.put(u32(i), u32(i * 3))
	}
	for i in 0 .. 512 {
		assert cache.get(u32(i))? == u32(i * 3)
		slot := int(u32(i) & 63)
		assert cache.recent[slot].valid
		assert cache.recent[slot].name == u32(i)
	}
	for _ in 0 .. 4 {
		for name in [u32(0), 64, 128, 192, 256, 320, 384, 448] {
			assert cache.get(name)? == name * 3
		}
	}
	cache.put(64, 0)
	assert cache.get(0)? == 0
	assert cache.get(64)? == 0
	assert cache.get(512) == none
}

fn test_source_type_recent_cache_context_changes_clear_all_cache_layers() {
	mut cache := ContextSourceTypeLookupCache{}
	cache.select_context('first.v', 'first')
	cache.put(0, 0)
	cache.put(1, 42)
	cache.select_context('first.v'.clone(), 'first'.clone())
	assert cache.get(0)? == 0
	assert cache.get(1)? == 42
	for context in [['second.v', 'first'], ['second.v', 'second'], ['first.v', 'first']] {
		cache.select_context(context[0], context[1])
		assert !cache.last_valid
		assert cache.entries.len == 0
		for entry in cache.recent {
			assert !entry.valid
		}
		assert cache.get(0) == none
		assert cache.get(1) == none
		cache.put(0, 0)
		cache.put(1, 43)
	}
}

fn test_source_type_recent_cache_workers_and_scratch_batches_are_private() {
	mut a := flat.FlatAst.new()
	mut tc := types.TypeChecker.new(&a)
	mut g := FlatGen.new()
	g.a = &a
	g.tc = &tc
	g.enum_selector_cache.select_context('same.v', 'main')
	g.enum_selector_cache.put(1, 2)
	mut worker := g.new_parallel_worker(1)
	assert worker.enum_selector_cache != g.enum_selector_cache
	assert worker.enum_selector_cache.get(1) == none
	worker.enum_selector_cache.select_context('same.v', 'main')
	worker.enum_selector_cache.put(1, 3)
	assert g.enum_selector_cache.get(1)? == 2
	assert worker.enum_selector_cache.get(1)? == 3
	own := g.enum_selector_cache
	saved := g.begin_scratch_lookup_caches()
	assert g.enum_selector_cache != own
	assert g.enum_selector_cache.get(1) == none
	g.enum_selector_cache.select_context('same.v', 'main')
	g.enum_selector_cache.put(1, 4)
	g.restore_scratch_lookup_caches(saved)
	assert g.enum_selector_cache == own
	assert g.enum_selector_cache.get(1)? == 2
	g.reset_context_lookup_caches()
	assert g.enum_selector_cache != own
	assert g.enum_selector_cache.get(1) == none
}
