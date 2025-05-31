import os

fn test_data_dir() {
	d := os.data_dir()
	assert d.len > 0
}

fn test_state_dir() {
	d := os.state_dir()
	assert d.len > 0
}

fn test_local_bin_dir() {
	d := os.local_bin_dir()
	assert d.len > 0
}
