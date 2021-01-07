module mysql

pub enum FieldType {
	type_decimal
	type_tiny
	type_short
	type_long
	type_float
	type_double
	type_null
	type_timestamp
	type_longlong
	type_int24
	type_date
	type_time
	type_datetime
	type_year
	type_newdate
	type_varchar
	type_bit
	type_timestamp2
	type_datetime2
	type_time2
	type_json = 245
	type_newdecimal
	type_enum
	type_set
	type_tiny_blob
	type_medium_blob
	type_long_blob
	type_blob
	type_var_string
	type_string
	type_geometry
}

pub fn (f FieldType) str() string {
	return match f {
		.type_decimal { 'decimal' }
		.type_tiny { 'tiny' }
		.type_short { 'short' }
		.type_long { 'long' }
		.type_float { 'float' }
		.type_double { 'double' }
		.type_null { 'null' }
		.type_timestamp { 'timestamp' }
		.type_longlong { 'longlong' }
		.type_int24 { 'int24' }
		.type_date { 'date' }
		.type_time { 'time' }
		.type_datetime { 'datetime' }
		.type_year { 'year' }
		.type_newdate { 'newdate' }
		.type_varchar { 'varchar' }
		.type_bit { 'bit' }
		.type_timestamp2 { 'timestamp2' }
		.type_datetime2 { 'datetime2' }
		.type_time2 { 'time2' }
		.type_json { 'json' }
		.type_newdecimal { 'newdecimal' }
		.type_enum { 'enum' }
		.type_set { 'set' }
		.type_tiny_blob { 'tiny_blob' }
		.type_medium_blob { 'medium_blob' }
		.type_long_blob { 'long_blob' }
		.type_blob { 'blob' }
		.type_var_string { 'var_string' }
		.type_string { 'string' }
		.type_geometry { 'geometry' }
	}
}
