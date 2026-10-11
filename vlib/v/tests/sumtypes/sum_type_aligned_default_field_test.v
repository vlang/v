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
	assert usize(voidptr(value)) % 64 == 0
	return value.value
}

fn test_default_map_field_retains_aligned_constructor() {
	mut holder := DefaultAlignedMap{}
	holder.values[1] = DefaultAlignedSmall{42}
	assert default_aligned_value(holder.values[1]) == 42
}
