module transform

// The enum table is frozen after preparation. Record only bits, so workers do
// not retain source strings or strings owned by a disposable worker arena.
struct EnumSelectorNameFilter {
mut:
	count int
	bits  [64]u64
}

fn (mut t Transformer) prepare_enum_selector_filter() {
	mut filter := &EnumSelectorNameFilter{ count: t.enum_types.len }
	for name, fields in t.enum_types {
		// Qualified type selectors may retain several dotted segments in value.
		// Include every suffix accepted by base + '.' + field_name resolution.
		filter.add(name)
		for i, ch in name {
			if ch == `.` {
				filter.add(unsafe { name.substr_unsafe(i + 1, name.len) })
			}
		}
		for field in fields { filter.add(field) }
	}
	t.enum_selector_filter = filter
}

@[direct_array_access; inline]
fn (mut filter EnumSelectorNameFilter) add(name string) {
	bit := file_import_alias_bit(name)
	filter.bits[bit >> 6] |= u64(1) << (bit & 63)
}

@[direct_array_access; inline]
fn (t &Transformer) enum_selector_may_match(name string) bool {
	if isnil(t.enum_selector_filter) || t.enum_selector_filter.count != t.enum_types.len {
		return true
	}
	bit := file_import_alias_bit(name)
	return t.enum_selector_filter.bits[bit >> 6] & (u64(1) << (bit & 63)) != 0
}
