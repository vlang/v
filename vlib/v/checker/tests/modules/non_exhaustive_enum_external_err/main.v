module main

import amod

fn test_status(s amod.Status) int {
	match s {
		.ready {
			return 1
		}
		.running {
			return 2
		}
		.failed {
			return 3
		}
	}
	return 0
}

fn test_partial_status(s amod.Status) int {
	return match s {
		.ready { 1 }
	}
}

fn main() {
	println(test_status(.ready))
	println(test_partial_status(.ready))
}
