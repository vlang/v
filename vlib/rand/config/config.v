// Copyright (c) 2019-2022 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
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

// Configuration struct for generating normally distributed floats. The default value for
// `mu` is 0 and the default value for `sigma` is 1.
[params]
pub struct NormalConfigStruct {
pub:
	mu    f64 = 0.0
	sigma f64 = 1.0
}

// Configuration struct for the shuffle functions.
// The start index is inclusive and the end index is exclusive.
// Set the end to 0 to shuffle until the end of the array.
[params]
pub struct ShuffleConfigStruct {
pub:
	start int
	end   int
}

// validate_for is a helper function for validating the configuration struct for the given array.
pub fn (config ShuffleConfigStruct) validate_for<T>(a []T) ? {
	if config.start < 0 || config.start >= a.len {
		return error("argument 'config.start' must be in range [0, a.len)")
	}
	if config.end < 0 || config.end > a.len {
		return error("argument 'config.end' must be in range [0, a.len]")
	}
	if config.end != 0 && config.end <= config.start {
		return error("argument 'config.end' must be greater than 'config.start'")
	}
}
