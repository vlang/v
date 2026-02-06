module main

import testing

fn main() {
	if testing.building_any_v_binaries_failed() {
		exit(1)
	}
}
