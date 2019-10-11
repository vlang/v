module runtime

import os

fn C.GetCurrentProcessorNumber() u32

pub fn nr_cpus() int {
	mut nr := int(C.GetCurrentProcessorNumber())
	println(nr)
	if nr == 0 {
		nr = os.getenv('NUMBER_OF_PROCESSORS').int()
	}
	println(nr)
	return nr
}
