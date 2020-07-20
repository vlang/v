module runtime

import os

pub fn nr_cpus() int {
	mut nr := int(C.GetCurrentProcessorNumber())
	if nr == 0 {
		nr = os.getenv('NUMBER_OF_PROCESSORS').int()
	}
	return nr
}
