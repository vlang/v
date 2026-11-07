@[aligned: 64]
struct DefaultAlignedWide {
	value int
}

struct DefaultAlignedSmall {
	value int
}

type DefaultAlignedSum = DefaultAlignedSmall | DefaultAlignedWide

struct DefaultAlignedMap {
mut:
	values map[int]DefaultAlignedSum
}

fn default_aligned_value(value &DefaultAlignedSum) int {
	assert uint(voidptr(value)) % 64 == 0
	return value.value
}

fn test_default_map_field_retains_aligned_constructor() {
	mut holder := DefaultAlignedMap{}
	holder.values[1] = DefaultAlignedSmall{42}
	// The map is unchanged while this checked pointer borrows its entry.
	assert default_aligned_value(unsafe { &holder.values[1] or { panic('missing entry') } }) == 42
}
