struct OffsetPair {
	first  int
	second int
}

struct OffsetDistance {
	value u32
}

fn offset_distance(value u32) u32 {
	return value
}

fn test_unsigned_offset_subtraction() {
	mut distance := u32(0)
	distance = __offsetof(OffsetPair, second) - __offsetof(OffsetPair, first)
	assert distance == sizeof(int)
	assert offset_distance(__offsetof(OffsetPair, second) - __offsetof(OffsetPair, first)) == distance
	stored := OffsetDistance{
		value: __offsetof(OffsetPair, second) - __offsetof(OffsetPair, first)
	}
	assert stored.value == distance
	assert (__offsetof(OffsetPair, second) - __offsetof(OffsetPair, first)) == u32(sizeof(int))
	distance = __offsetof(OffsetPair, first) - __offsetof(OffsetPair, second)
	assert distance == u32(0) - u32(sizeof(int))
}

fn test_unsigned_negated_expressions() {
	value := 4
	mut distance := u32(0)
	distance = -value
	assert distance == u32(0) - u32(value)
	distance = -i64(value)
	assert distance == u32(0) - u32(value)
	distance = -(value + 1)
	assert distance == u32(0) - u32(value + 1)
	stored := OffsetDistance{ value: -value }
	assert stored.value == u32(0) - u32(value)
	assert offset_distance(-u32(value)) == stored.value
}
