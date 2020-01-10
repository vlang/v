module misc

import rand
import time

const (
	start_time_unix = time.now().unix
)
// random will return a random time.Time in *the past*
pub fn random() time.Time {
	return time.unix(rand.next(start_time_unix))
}
