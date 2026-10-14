module orm

import db.sqlite

struct MappingNumbers {
	small    i8
	wide     u64
	rate     f32
	enabled  bool
	optional ?int = 7
}

struct MappingEmbeddedNumbers {
	MappingNumbers
	id      int @[primary]
	total   ?f64
	omitted int = 42
}

struct MappingFlatNumbers {
	id       int @[primary]
	small    i8
	wide     u64
	rate     f32
	enabled  bool
	optional ?int = 7
}

fn test_row_mapping_shared_conversions_for_embedded_and_flat_models() {
	mut db := sqlite.connect(':memory:')!
	defer { db.close() or {} }
	mut embedded := new_query[MappingEmbeddedNumbers](db)
	embedded.config.fields = ['id', 'MappingNumbers.small', 'MappingNumbers.wide', 'MappingNumbers.rate',
		'MappingNumbers.enabled', 'MappingNumbers.optional', 'total', 'omitted']
	embedded.hydration_fields = ['omitted']
	row := [Primitive(i16(1)), i64(-12), u32(4000000000), f64(2.5), f32(-0.5), Null{}, true, 99]
	result := embedded.map_row(row)!
	assert result.id == 1
	assert result.small == -12
	assert result.wide == u64(4000000000)
	assert result.rate == f32(2.5)
	assert result.enabled
	assert result.optional == ?int(7)
	assert result.total == ?f64(1.0)
	assert result.omitted == 42
	mut flat := new_query[MappingFlatNumbers](db)
	flat.config.fields = ['enabled', 'rate', 'small', 'wide', 'optional']
	flat_result := flat.map_row([Primitive(u8(0)), int(3), f64(-12.75), i64(99), Null{}])!
	assert flat_result.id == 0
	assert flat_result.small == -12
	assert flat_result.wide == 99
	assert flat_result.rate == f32(3)
	assert !flat_result.enabled
	assert flat_result.optional == none
}
