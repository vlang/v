@[aligned: 64]
struct SumCacheLine {
	value int
}

struct SumSmallValue {
	value int
}

type AlignedSum = SumCacheLine | SumSmallValue

type AlignedSumAlias = AlignedSum

struct AlignedSumHolder {
	value AlignedSum
}

fn test_explicit_sum_references_inherit_variant_alignment() {
	for i in 0 .. 64 {
		value := &AlignedSum(SumSmallValue{i})
		assert usize(voidptr(value)) % 64 == 0
		assert value is SumSmallValue
		assert value.value == i
	}
}

fn test_sum_aliases_and_containing_values_inherit_alignment() {
	value := &AlignedSumAlias(SumSmallValue{5})
	assert usize(voidptr(value)) % 64 == 0
	holder := &AlignedSumHolder{ value: SumSmallValue{7} }
	assert usize(voidptr(holder)) % 64 == 0
	assert holder.value is SumSmallValue
	assert holder.value.value == 7
}

fn test_fixed_arrays_of_sums_inherit_alignment() {
	values := &[2]AlignedSum{init: SumSmallValue{3}}
	assert usize(voidptr(values)) % 64 == 0
}

fn aligned_sum_value(value &AlignedSum) int {
	return match value {
		SumCacheLine { value.value }
		SumSmallValue { value.value }
	}
}

fn test_dynamic_sum_arrays_preserve_alignment_when_reallocated() {
	mut values := []AlignedSum{len: 3, init: SumSmallValue{index}}
	for i in 3 .. 200 {
		values << SumSmallValue{i}
	}
	for i in 0 .. values.len {
		assert aligned_sum_value(values[i]) == i
	}
	mut copied := values[1..3].clone()
	copied << SumSmallValue{3}
	for i in 0 .. copied.len {
		assert aligned_sum_value(copied[i]) == i + 1
	}
	reversed := values.reverse()
	assert aligned_sum_value(reversed[0]) == 199
	pair := values[0..2].clone()
	repeated := pair.repeat(2)
	assert aligned_sum_value(repeated[3]) == 1
	first := AlignedSum(SumSmallValue{9})
	literal := [first, first]
	assert aligned_sum_value(literal[0]) == 9
}

fn test_fixed_sum_array_conversion_preserves_element_alignment() {
	first := AlignedSum(SumSmallValue{12})
	fixed := [first, first]!
	mut values := fixed[..].clone()
	values << SumSmallValue{13}
	assert aligned_sum_value(values[0]) == 12
	assert aligned_sum_value(values[2]) == 13
}

fn test_maps_preserve_sum_alignment_across_storage_operations() {
	mut values := map[int]AlignedSum{}
	for i in 0 .. 40 {
		values[i] = SumSmallValue{i}
	}
	values.reserve(200)
	for i in 0 .. 40 {
		assert aligned_sum_value(values[i]) == i
	}
	mut cloned := values.clone()
	values.clear()
	values[50] = SumSmallValue{50}
	assert aligned_sum_value(values[50]) == 50
	cloned.delete(0)
	exported := cloned.values()
	assert exported.len == 39
	for i in 0 .. exported.len {
		assert aligned_sum_value(exported[i]) == i + 1
	}
	mut empty := map[int]AlignedSum{}
	mut copied_empty := empty.clone()
	copied_empty[1] = SumSmallValue{51}
	assert aligned_sum_value(copied_empty[1]) == 51
	moved := empty.move()
	empty[2] = SumSmallValue{52}
	assert aligned_sum_value(empty[2]) == 52
	mut reserved := map[int]AlignedSum{}
	reserved.reserve(100)
	reserved[1] = SumSmallValue{54}
	assert aligned_sum_value(reserved[1]) == 54
	literal := {
		1: AlignedSum(SumSmallValue{53})
	}
	assert aligned_sum_value(literal[1]) == 53
	unsafe {
		values.free()
		cloned.free()
		exported.free()
		empty.free()
		copied_empty.free()
		moved.free()
		literal.free()
		reserved.free()
	}
}

struct AlignedCollections {
mut:
	values []AlignedSum
	lookup map[int]AlignedSum
}

fn test_default_collections_preserve_sum_alignment() {
	mut holder := AlignedCollections{}
	holder.values << SumSmallValue{61}
	holder.lookup[1] = SumSmallValue{62}
	assert aligned_sum_value(holder.values[0]) == 61
	assert aligned_sum_value(holder.lookup[1]) == 62
}
