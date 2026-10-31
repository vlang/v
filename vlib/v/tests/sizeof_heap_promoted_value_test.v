struct SizeRecord {
mut:
	id     u32
	values [1024]u32
}

type SizeRecordAlias = SizeRecord

fn update_size_record(mut record SizeRecord) {
	record.id = 7
}

fn update_size_record_alias(mut record SizeRecordAlias) {
	record.id = 8
}

fn test_sizeof_heap_promoted_struct_value() {
	mut record := SizeRecord{}
	update_size_record(mut record)
	assert record.id == 7
	assert sizeof(record) == sizeof(SizeRecord)
	assert sizeof(record.values) == sizeof([1024]u32)
	mut alias := SizeRecordAlias{}
	update_size_record_alias(mut alias)
	assert alias.id == 8
	assert sizeof(alias) == sizeof(SizeRecordAlias)
}

fn retain_size_value[T](value &T) &T {
	return value
}

fn test_sizeof_heap_promoted_scalar_array_and_pointer_values() {
	mut scalar := u32(42)
	scalar_ref := retain_size_value(&scalar)
	assert *scalar_ref == 42
	assert sizeof(scalar) == sizeof(u32)
	mut values := [u32(1), 2, 3]!
	array_ref := retain_size_value(&values)
	assert (*array_ref)[2] == 3
	assert sizeof(values) == sizeof([3]u32)
	mut pointer := &scalar
	pointer_ref := retain_size_value(&pointer)
	assert *pointer_ref == pointer
	assert sizeof(pointer) == sizeof(&u32)
}
