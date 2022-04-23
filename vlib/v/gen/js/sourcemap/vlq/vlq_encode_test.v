module vlq

struct TestData {
	expected string
	data_val i64
}

struct TestWriter {
pub mut:
	bytes []u8
}

fn test_encode_a() ? {
	decode_values := [
		TestData{'A', 0},
		TestData{'C', 1},
		TestData{'D', -1},
		TestData{'2H', 123},
		TestData{'qxmvrH', 123456789},
		TestData{'+/////B', 1073741823} /* 2^30-1 */,
		// TestData{'hgggggggggggI', 9_223_372_036_854_775_808} /* 2^63 */,
	]
	for _, test_data in decode_values {
		mut output := TestWriter{}

		encode(test_data.data_val, mut &output) ?
		// dump(output.bytes)
		assert output.bytes == test_data.expected.bytes()
	}
}

fn (mut w TestWriter) write(buf []u8) ?int {
	w.bytes << buf
	return buf.len
}
