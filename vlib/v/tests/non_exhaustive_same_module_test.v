@[non_exhaustive]
pub enum LocalStatus {
	ready
	running
	failed
}

fn own_status(s LocalStatus) int {
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

fn test_same_module_non_exhaustive_without_else() {
	assert own_status(.ready) == 1
	assert own_status(.running) == 2
	assert own_status(.failed) == 3
}
