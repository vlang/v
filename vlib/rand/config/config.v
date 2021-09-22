module config

import rand.seed

// PRNGConfigStruct is a configuration struct for creating a new instance of the default RNG.
// Note that the RNGs may have a different number of u32s required for seeding. The default
// generator WyRand used 64 bits, ie. 2 u32s so that is the default. In case your desired generator
// uses a different number of u32s, use the `seed.time_seed_array()` method with the correct
// number of u32s.
[params]
pub struct PRNGConfigStruct {
pub:
	seed_ []u32 = seed.time_seed_array(2)
}
