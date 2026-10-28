module transform

import v.flat

// The enum table is frozen after preparation. Record only bits, so workers do
// not retain source strings or strings owned by a disposable worker arena.
struct EnumSelectorNameFilter {
mut:
	count int
	bits  [64]u64
	// base_bits lists the final segment of every enum and type alias name. A
	// selector base resolves to an enum only through a spelling that keeps its
	// final segment or through an alias with that final segment.
	alias_count int
	base_bits   [64]u64
}

fn (mut t Transformer) prepare_enum_selector_filter() {
	mut filter := &EnumSelectorNameFilter{ count: t.enum_types.len }
	for name_id, fields in t.enum_types {
		name := flat.source_type_name(name_id)
		// Qualified type selectors may retain several dotted segments in value.
		// Include every suffix accepted by base + '.' + field_name resolution.
		filter.add(name)
		for i, ch in name {
			if ch == `.` {
				filter.add(unsafe { name.substr_unsafe(i + 1, name.len) })
			}
		}
		for field in fields { filter.add(field) }
		filter.add_base(name)
	}
	if !isnil(t.tc) {
		filter.alias_count = t.tc.type_aliases.ids.len
		for alias_id, _ in t.tc.type_aliases.ids {
			filter.add_base(flat.source_type_name(alias_id))
		}
	} else {
		filter.alias_count = -1
	}
	t.enum_selector_filter = filter
}

@[direct_array_access; inline]
fn (mut filter EnumSelectorNameFilter) add_base(name string) {
	bit := file_import_alias_bit(short_name_view(name))
	filter.base_bits[bit >> 6] |= u64(1) << (bit & 63)
}

// enum_selector_base_may_match reports whether a selector base spelled `name` can
// name an enum. Plain dotted identifiers keep their final segment through module,
// import and selective-import resolution, so a final segment that names no enum
// and no type alias cannot resolve to an enum. Other spellings are not filtered.
@[direct_array_access]
fn (t &Transformer) enum_selector_base_may_match(name string) bool {
	if isnil(t.enum_selector_filter) || isnil(t.tc)
		|| t.enum_selector_filter.count != t.enum_types.len
		|| t.enum_selector_filter.alias_count != t.tc.type_aliases.ids.len {
		return true
	}
	for ch in name {
		if !(ch.is_letter() || ch.is_digit() || ch == `_` || ch == `.`) {
			return true
		}
	}
	bit := file_import_alias_bit(short_name_view(name))
	return t.enum_selector_filter.base_bits[bit >> 6] & (u64(1) << (bit & 63)) != 0
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
