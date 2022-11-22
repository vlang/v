module json2

const (
	string_type_idx              = typeof(<string>).idx
	result_string_type_idx       = typeof(<!string>).idx
	optional_string_type_idx     = typeof(<?string>).idx

	int_type_idx                 = typeof(<int>).idx
	result_int_type_idx          = typeof(<!int>).idx
	optional_int_type_idx        = typeof(<?int>).idx

	int_array_type_idx           = typeof(<[]int>).idx
	result_int_array_type_idx    = typeof(<![]int>).idx
	optional_int_array_type_idx  = typeof(<?[]int>).idx

	byte_array_type_idx          = typeof(<[]byte>).idx
	result_byte_array_type_idx   = typeof(<![]byte>).idx
	optional_byte_array_type_idx = typeof(<?[]byte>).idx
)
