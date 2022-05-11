module vlq

import io

struct TestReader {
pub:
	bytes []u8
mut:
	i int
}

struct TestData {
	decode_val string
	expected   i64
}

type TestDataList = []TestData

fn test_decode_a() ? {
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
		mut input := make_test_reader(test_data.decode_val)

		res := decode(mut &input)?
		assert res == test_data.expected
	}
}

fn (mut b TestReader) read(mut buf []u8) ?int {
	if !(b.i < b.bytes.len) {
		return none
	}
	n := copy(mut buf, b.bytes[b.i..])
	b.i += n
	return n
}

fn make_test_reader(data string) io.Reader {
	buf := &TestReader{
		bytes: data.bytes()
	}
	return io.new_buffered_reader(reader: buf)
}
