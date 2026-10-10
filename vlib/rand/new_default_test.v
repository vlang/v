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
	mut rng := rand.new_default(config.PRNGConfigStruct{})
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
