module fastc

import runtime

// fastc_nr_cpus reports the host's online CPU count for parallel generation.
// Windows hosts are never FastC-selfhost-compiled, so the runtime module is
// safe to use here.
fn fastc_nr_cpus() int {
	return runtime.nr_jobs()
}
