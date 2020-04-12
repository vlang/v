module runtime

import os

fn nr_cpus_win() int {
	mut nr := int(C.GetCurrentProcessorNumber())
	if nr == 0 {
		nr = os.getenv('NUMBER_OF_PROCESSORS').int()
	}
	return nr
}

fn nr_cpus_nix() int {
	eprintln('nr_cpus_nix should be callable only for nix platforms')
	return 1
}
