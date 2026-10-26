module transform

// Frozen key sets permit negative constant lookups without constructing
// qualified names. Bits never retain source or disposable worker strings.
struct ConstSelectorNameFilter {
mut:
	const_count  int
	suffix_count int
	bits         [64]u64
}

fn (mut t Transformer) prepare_const_selector_filter() {
	if isnil(t.tc) { return }
	mut filter := &ConstSelectorNameFilter{
		const_count:  t.tc.const_types.len
		suffix_count: t.const_suffixes.len
	}
	for name, _ in t.tc.const_types { filter.add(name) }
	for name, _ in t.const_suffixes { filter.add(name) }
	t.const_selector_filter = filter
}

@[direct_array_access; inline]
fn (mut filter ConstSelectorNameFilter) add(name string) {
	bit := file_import_alias_bit(short_name_view(name))
	filter.bits[bit >> 6] |= u64(1) << (bit & 63)
}

@[direct_array_access; inline]
fn (t &Transformer) const_selector_may_match(name string) bool {
	if isnil(t.tc) || isnil(t.const_selector_filter)
		|| t.const_selector_filter.const_count != t.tc.const_types.len
		|| t.const_selector_filter.suffix_count != t.const_suffixes.len {
		return true
	}
	bit := file_import_alias_bit(short_name_view(name))
	return t.const_selector_filter.bits[bit >> 6] & (u64(1) << (bit & 63)) != 0
}
