# Non-Uniform Distribution Functions

This module contains functions for sampling from non-uniform distributions.

All implementations of the `rand.PRNG` interface generate numbers from uniform
distributions. This library exists to allow the generation of pseudorandom numbers
sampled from non-uniform distributions. Additionally, it allows the user to use any
PRNG of their choice. This is because the default RNG can be reassigned to a different
generator. It can either be one of the pre-existing one (which are well-tested and
recommended) or a custom user-defined one. See `rand.set_rng()`.