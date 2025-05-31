module consts_with_or_blocks_in_different_files

import os

const b_const = os.config_dir() or { panic(err) } + '/b'

// Just to test the two files in this folder are compiled normally
fn test_consts_with_or_blocks_in_different_files() {
	assert true
}
