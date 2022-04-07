module main

import rand
import rand.wyrand
import rand.splitmix64

fn test_deref_mut_interface_in_loop() {
	mut wyrand_rng := &rand.PRNG(&wyrand.WyRandRNG{})
	mut splitmix_rng := &rand.PRNG(&splitmix64.SplitMix64RNG{})

	mut generators := [wyrand_rng, splitmix_rng]
	for mut rng in generators {
		seed_len := rng.block_size() / 32
		dump(seed_len)
		println(rng.string(15))
		assert seed_len == 2
	}
}
