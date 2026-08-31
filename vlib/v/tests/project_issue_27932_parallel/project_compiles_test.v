module main

fn test_spawn_option_fixed_array_wrappers_across_files() {
	a := ?[2]int(none)
	assert issue27932_parallel_spawn_first(a) == 0
	assert issue27932_parallel_spawn_second(a) == 0
}
