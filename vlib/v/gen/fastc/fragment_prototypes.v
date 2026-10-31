module fastc

import strings

// fastc_deduplicate_fragment_prototypes retains the per-file registration of helper
// declarations when several parsers generate fragments of one source file.
fn fastc_deduplicate_fragment_prototypes(mut outputs []FastcFileGenOutput, sources []FastcSourceFile) {
	mut previous_path := ''
	mut seen := map[string]int{}
	for index, source in sources {
		if source.path != previous_path || source.source_offset == 0 {
			seen.clear()
			previous_path = source.path
		}
		if source.source_offset == 0 && (index + 1 >= sources.len
			|| sources[index + 1].path != source.path
			|| sources[index + 1].source_offset == 0) {
			continue
		}
		prototypes := outputs[index].prototypes
		mut removed := []int{}
		mut filtered := strings.Builder{}
		mut kept_start := 0
		mut start := 0
		for start < prototypes.len {
			mut end := start
			for end < prototypes.len && prototypes[end] != `\n` {
				end++
			}
			if end < prototypes.len {
				end++
			}
			line := unsafe { prototypes[start..end] }
			if previous_fragment := seen[line] {
				if previous_fragment < index {
					if removed.len == 0 {
						filtered = strings.new_builder(prototypes.len)
					}
					filtered.write_string(prototypes[kept_start..start])
					kept_start = end
					removed << start
					removed << end
				}
			} else {
				seen[line] = index
			}
			start = end
		}
		if removed.len == 0 {
			continue
		}
		filtered.write_string(prototypes[kept_start..])
		outputs[index].prototypes = fastc_take_string(mut filtered)
		mut removed_index := 0
		mut removed_bytes := 0
		for span_index, offset in outputs[index].proto_spans {
			for removed_index + 1 < removed.len && removed[removed_index + 1] <= offset {
				removed_bytes += removed[removed_index + 1] - removed[removed_index]
				removed_index += 2
			}
			within_removed := if removed_index < removed.len && removed[removed_index] < offset {
				offset - removed[removed_index]
			} else {
				0
			}
			outputs[index].proto_spans[span_index] = offset - removed_bytes - within_removed
		}
	}
}
