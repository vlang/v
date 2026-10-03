// vtest vflags: -new-compiler

struct TemporaryMapValue {
	values []int
}

struct TemporaryMapState {
	flags   [2]u8
	known   [2]map[int]bool
	entries map[int]TemporaryMapValue
}

fn temporary_map_state() TemporaryMapState {
	return TemporaryMapState{
		entries: {
			1: TemporaryMapValue{[7, 9]}
		}
	}
}

fn test_map_lookup_on_temporary_struct_fields() {
	assert TemporaryMapState{}.entries[1].values == []int{}
	assert temporary_map_state().entries[1].values == [7, 9]
	assert temporary_map_state().entries[2].values == []int{}
}

fn test_map_equality_with_a_temporary_struct_default() {
	state := TemporaryMapState{}
	assert state.entries == TemporaryMapState{}.entries
	assert state == TemporaryMapState{}
	assert temporary_map_state() != TemporaryMapState{}
}
