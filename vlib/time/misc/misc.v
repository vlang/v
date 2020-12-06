module misc

import rand
import time

const (
	start_time_unix = time.now().unix
)

// random returns a random time struct in *the past*.
pub fn random() time.Time {
	return time.unix(int(rand.u64n(start_time_unix)))
}
