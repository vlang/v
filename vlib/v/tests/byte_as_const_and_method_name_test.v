// `byte` is not a type (use `u8`), so consts, methods and static methods may use it as a name.
const byte = f64(8)

type DataSize = f64

fn (ds DataSize) byte() f64 {
	return f64(ds) / byte
}

fn DataSize.byte() DataSize {
	return DataSize(byte)
}

interface Sizer {
	byte() f64
}

fn size_in_bytes(s Sizer) f64 {
	return s.byte()
}

fn test_byte_as_const_name() {
	assert byte == 8.0
	assert byte * 2 == 16.0
}

fn test_byte_as_method_and_static_method_name() {
	ds := DataSize(32)
	assert ds.byte() == 4.0
	assert DataSize.byte() == DataSize(8)
	assert size_in_bytes(ds) == 4.0
}
