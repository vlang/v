enum MapStatus {
	off
	on
}

type MapStatusAlias = MapStatus

fn count_map_statuses(data &map[string]MapStatus) int {
	mut count := 0
	mut references := []&MapStatus{}
	for _, status in data {
		references << status
		if status == .on {
			count++
		}
		assert (.on == status) == (status == MapStatus.on)
		assert (status != .off) == (.off != status)
	}
	assert references.len == data.len
	return count
}

fn test_enum_map_reference_iteration_compares_enum_values() {
	mut data := {
		'a': MapStatus.on
		'b': MapStatus.off
	}
	assert count_map_statuses(&data) == 1
	for _, mut status in data {
		status = .on
	}
	assert count_map_statuses(&data) == 2
}

fn test_enum_reference_identity_stays_pointer_identity() {
	first := MapStatus.on
	second := MapStatus.on
	first_ref := &first
	second_ref := &second
	assert first_ref == first_ref
	assert first_ref != second_ref
	assert first_ref == .on
	assert .on == first_ref
	assert second_ref != .off
}

fn test_alias_enum_map_reference_comparison() {
	data := {
		'a': MapStatusAlias(MapStatus.on)
		'b': MapStatusAlias(MapStatus.off)
	}
	mut count := 0
	for _, status in &data {
		if status == MapStatusAlias(MapStatus.on) {
			count++
		}
		assert (status == .on) == (.on == status)
		assert (status != .off) == (.off != status)
	}
	assert count == 1
}
