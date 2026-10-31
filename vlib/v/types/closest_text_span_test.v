// vtest vflags: -prealloc -gc none -d prealloc_stats
module types

fn test_closest_text_span_preserves_nearest_occurrence_and_left_ties() {
	for source in ['foo a foo b foo', 'aaaaa', 'ZList[string] ZList[string]', 'foo foobar barfoo'] {
		for text in ['foo', 'aa', 'ZList[string]', 'missing', '', source, source + 'x'] {
			for anchor in -2 .. source.len + 3 {
				mut expected := -1
				mut distance := source.len + text.len + 10
				if text.len > 0 {
					mut from := 0
					for {
						start := source.index_after(text, from) or { break }
						current := if start > anchor { start - anchor } else { anchor - start }
						if current < distance {
							expected = start
							distance = current
						}
						from = start + 1
					}
				}
				actual := closest_text_span(source, text, anchor, 7)
				if expected < 0 {
					assert actual == none
				} else {
					pos := actual or { panic('missing span') }
					assert pos.offset == expected
					assert pos.end == expected + text.len
					assert pos.id == 7
				}
			}
		}
	}
}

fn test_repeated_type_spans_do_not_copy_source_suffixes() {
	$if prealloc && prealloc_stats ? {
		text := 'ZList[string]'
		line := '\tc &${text}\n'
		source := line.repeat(256)
		before := prealloc_stats_snapshot()
		mut matched := 0
		for i in 0 .. 256 {
			pos := closest_text_span(source, text, i * line.len, 1) or { panic('missing type') }
			if pos.offset == i * line.len + 4 {
				matched++
			}
		}
		after := prealloc_stats_snapshot()
		assert matched == 256
		allocated := after.allocated_bytes - before.allocated_bytes
		assert allocated < 1024 * 1024, 'repeated type spans allocated ${allocated} bytes'
	}
}
