module misc

import rand
import time

const (
	// start_time_unix is set when the program is started
	start_time_unix = time.now().unix
)

// random returns a random time struct in *the past*. It is seeded from the unix time at the start of the program.
pub fn random() time.Time {
	return time.unix(int(rand.u64n(start_time_unix)))
}
