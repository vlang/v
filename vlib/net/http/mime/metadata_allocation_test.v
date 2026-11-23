// vtest vflags: -nocache -gc boehm
module mime

fn test_repeated_metadata_lookups_allocate_only_requested_extensions() {
	$if gcboehm ? {
		for mt in ['nope/nope', 'application/1d-interleaved-parityfec', 'application/json', 'text/plain'] {
			expected := get_complete_mime_type(mt)
			before := gc_heap_usage().total_bytes
			for _ in 0 .. 1000 {
				assert get_complete_mime_type(mt) == expected
			}
			allocated := gc_heap_usage().total_bytes - before
			if expected.extensions.len == 0 {
				assert allocated == 0, '${mt}: ${allocated} allocated bytes'
			} else {
				// Allow allocation headers and GC rounding, but never a copy of the full table.
				budget := 1000 * (expected.extensions.len * int(sizeof(string)) + 128)
				assert allocated <= budget, '${mt}: ${allocated} allocated bytes exceeds ${budget}'
			}
		}
	}
}
