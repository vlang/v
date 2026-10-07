// vtest vflags: -enable-globals
module main

import globaldata
import constants

fn test_module_global_storage_wins_over_foreign_const() {
	assert globaldata.first() == 10
	assert globaldata.fixed_first() == 30
	dynamic_len, fixed_len := globaldata.lengths()
	assert dynamic_len == 2
	assert fixed_len == 3
	assert constants.first() == 1
	assert constants.dynamic_first() == 4
}
