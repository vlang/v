// vtest build: !v3_no_parallel?
module parser

import v.flat

fn test_merge_text_probes_preserve_canonical_storage_across_independent_passes() {
	mut a := flat.FlatAst.new()
	for word in ['alpha', 'bravo', 'cider'] {
		a.intern_text(word)
	}
	mut source := []u8{len: 5}
	borrowed := unsafe { tos(source.data, source.len) }
	for word in ['alpha', 'bravo', 'cider', 'alpha', 'ghost'] {
		for i, ch in word {
			source[i] = ch
		}
		// Reusing one source address across passes must not reuse a previous cache hit.
		mut values := new_parse_merge_text_cache()
		mut types := new_parse_merge_text_cache()
		for attempt in 0 .. 2 {
			if attempt == 1 {
				gc_collect()
			}
			value, hit := a.probe_text_ptr_cached(borrowed, mut values.ptrs, mut values.values)
			id, typ, type_hit := a.probe_type_text_ptr_cached(borrowed, mut types.ptrs,
				mut types.values, mut types.ids)
			assert value == word
			assert typ == word
			assert hit == (word != 'ghost')
			assert type_hit == hit
			if hit {
				assert id > 0
				assert value.str == a.text(flat.TextId(id)).str
				assert typ.str == value.str
			} else {
				assert id == 0
			}
		}
		value, hit := a.probe_text_ptr_cached('', mut values.ptrs, mut values.values)
		assert hit
		assert value == ''
		assert a.text_values.len == 3
		unsafe {
			// The cache buffers own none of the canonical AST strings.
			free(values)
			free(types)
		}
		assert a.text(flat.TextId(1)) == 'alpha'
		assert a.text(flat.TextId(2)) == 'bravo'
		assert a.text(flat.TextId(3)) == 'cider'
	}
}
