module misc

import rand.wyrand
import time

const (
	start_time_unix = time.now().unix
)
// random returns a random time struct in *the past*.
pub fn random(mut rng &wyrand.WyRandRNG) time.Time {
	return time.unix(int(rng.u64n(start_time_unix)))
}
