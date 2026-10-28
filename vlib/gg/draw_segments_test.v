module gg

fn test_radius_to_segments_swapped_entries_are_monotonic() {
	// The r=7/r=8 entries were swapped (13, 10), which made the segment
	// count dip as the radius grew. The table must not dip there.
	assert radius_to_segments(7) == 10
	assert radius_to_segments(8) == 13
}

fn test_radius_to_segments_non_positive_radius_yields_no_segments() {
	// draw_circle_empty guards on `segments <= 0`, so a zero or negative
	// radius must map to zero (or fewer) segments, never a divisor.
	assert radius_to_segments(0) <= 0
	assert radius_to_segments(-2.5) <= 0
}
