module main

@[packed]
struct DataRecord {
	n_seq1        u8
	n_seq2        u8
	n_firstdata_a i32
	n_firstdata_b i32
	n_firstdata_c i32
	n_firstdata_d i32
}

const data_record_size = sizeof(DataRecord)

@[packed]
union DataUnion {
	data_struct DataRecord
	data_bytes  [data_record_size]u8
}

const union_record_size = sizeof(DataUnion)

fn test_sizeof_on_packed_struct_in_union() {
	assert data_record_size == 18
	assert sizeof(DataRecord) == 18
	assert union_record_size == 18
	assert sizeof(DataUnion) == 18

	mut union_record := DataUnion{}
	assert sizeof(union_record) == 18
	unsafe {
		assert sizeof(union_record.data_struct) == 18
		assert sizeof(union_record.data_bytes) == 18
		assert sizeof(union_record.data_struct) == data_record_size
	}
}
