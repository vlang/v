module fastc

fn test_retained_range_search_preserves_spans_and_window_boundaries() {
	// Exercise gaps, adjoining ranges, empty queries, and both outer boundaries.
	for mask in 0 .. 256 {
		mut ranges := []int{}
		for bit in 0 .. 8 {
			if mask & (1 << bit) != 0 {
				ranges << bit * 2
				ranges << bit * 2 + 2
			}
		}
		for start in -1 .. 18 {
			for end in start .. 19 {
				mut kept := false
				mut window := []int{}
				for i := 0; i + 1 < ranges.len; i += 2 {
					if ranges[i] <= start && end <= ranges[i + 1] {
						kept = true
					}
					if ranges[i + 1] > start && ranges[i] < end {
						window << if ranges[i] < start { start } else { ranges[i] }
						window << if ranges[i + 1] > end { end } else { ranges[i + 1] }
					}
				}
				assert fastc_range_is_kept(ranges, start, end) == kept
				assert fastc_window_ranges(ranges, start, end) == window
			}
		}
	}
}
