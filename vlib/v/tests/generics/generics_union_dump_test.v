union Convertor[T] {
	value T
	bytes [8]u8
}

fn test_conversion_works() {
	a := Convertor[i64]{
		value: 21474837714
	}
	$if little_endian {
		assert unsafe { a.bytes } == [u8(210), 4, 0, 0, 5, 0, 0, 0]!
	}
}

fn test_dumping_of_a_generic_union_value() {
	dump(Convertor[u8]{
		bytes: [u8(210), 4, 0, 0, 5, 0, 0, 0]!
	})
	dump(Convertor[i16]{
		bytes: [u8(210), 4, 0, 0, 5, 0, 0, 0]!
	})
	dump(Convertor[int]{
		bytes: [u8(210), 4, 0, 0, 5, 0, 0, 0]!
	})
	dump(Convertor[i64]{
		bytes: [u8(210), 4, 0, 0, 5, 0, 0, 0]!
	})
}
