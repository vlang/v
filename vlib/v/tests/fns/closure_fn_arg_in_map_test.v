struct Options {
}

fn sort_dictionary_order(mut lines []string, options Options) {
	map_fn := fn (e u8) u8 {
		return if e.is_digit() || e.is_letter() || e == ` ` { e } else { ` ` }
	}
	lines.sort_with_compare(fn [map_fn] (a &string, b &string) int {
		aa := a.bytes().map(map_fn).bytestr()
		bb := b.bytes().map(map_fn).bytestr()
		return compare_strings(aa, bb)
	})
}

fn test_main() {
	mut a := ['a', 'b', 'c'].reverse()
	sort_dictionary_order(mut a, Options{})
	assert dump(a) == ['a', 'b', 'c']
}
