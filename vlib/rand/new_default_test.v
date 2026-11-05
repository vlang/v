import rand
import rand.config

const seed_pair = [u32(0x1234_5678), 0x9abc_def0]

fn test_new_default_with_an_explicit_seed_is_reproducible() {
	seeded := config.PRNGConfigStruct{
		seed_: seed_pair
	}
	mut first := rand.new_default(seeded)
	mut second := rand.new_default(seeded)
	assert first.u32() == second.u32()
	assert first.u32() == second.u32()
	assert first.u32() == second.u32()
}

fn test_new_default_with_distinct_seeds_diverge() {
	mut a := rand.new_default(config.PRNGConfigStruct{ seed_: [u32(1), 0] })
	mut b := rand.new_default(config.PRNGConfigStruct{ seed_: [u32(2), 0] })
	assert a.u32() != b.u32()
}

fn test_new_default_without_a_seed_uses_the_clock() {
	clock_seeded := config.PRNGConfigStruct{}
	assert clock_seeded.seed_.len == 0
	mut rng := rand.new_default(clock_seeded)
	// The seed comes from the clock, so only the shape of the output is
	// checkable.
	assert rng.block_size() > 0
	bytes := rng.bytes(16) or { panic(err) }
	assert bytes.len == 16
}

fn test_set_rng_makes_new_default_the_default_generator() {
	old := rand.get_current_rng()
	mut custom := rand.new_default(config.PRNGConfigStruct{ seed_: seed_pair })
	rand.set_rng(custom)
	rand.seed(seed_pair)
	from_module := rand.u32()
	rand.set_rng(old)
	rand.seed(seed_pair)
	restored := rand.u32()
	assert from_module == restored
}

@[manualfree]
fn test_new_default_borrows_an_explicit_heap_seed() {
	mut seed_data := []u32{len: 2}
	seed_data[0] = seed_pair[0]
	seed_data[1] = seed_pair[1]
	seeded := config.PRNGConfigStruct{
		seed_: seed_data
	}
	mut first := rand.new_default(seeded)
	mut second := rand.new_default(seeded)
	mut expected := rand.new_default(config.PRNGConfigStruct{ seed_: seed_pair })
	// Each RNG copied the values; the caller can change and release its seed storage.
	seed_data[0] = 0
	seed_data[1] = 0
	unsafe { seed_data.free() }
	for _ in 0 .. 8 {
		value := expected.u32()
		assert first.u32() == value
		assert second.u32() == value
	}
}

fn test_new_default_accepts_an_explicit_empty_seed() {
	mut rng := rand.new_default(config.PRNGConfigStruct{ seed_: []u32{} })
	assert rng.block_size() > 0
	assert rng.bytes(16)!.len == 16
}
