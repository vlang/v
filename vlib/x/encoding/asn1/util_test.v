// Copyright (c) 2022, 2024 blackshirt. All rights reserved.
// Use of this source code is governed by a MIT License
// that can be found in the LICENSE file.
module asn1

struct RByteTest {
	inp    []u8
	pos    int
	result u8
	end    bool
}

fn test_simple_read_byte() {
	src := [u8(0x16), 0x06, 0x44, 0x05, 0x09, 0x22]
	start := 0

	mut val, mut pos := read_byte(src, 0)!
	assert val == src[start]
	assert pos == 1

	val, pos = read_byte(src, pos)!
	assert val == src[1]
	assert pos == 2

	val, pos = read_byte(src, pos)!
	assert val == src[2]
	assert pos == 3

	val, pos = read_byte(src, pos)!
	assert val == src[3]
	assert pos == 4

	val, pos = read_byte(src, pos)!
	assert val == src[4]
	assert pos == 5

	// this is the last item in the array
	val, pos = read_byte(src, pos)!
	assert val == src[5] // the last item, 0x22
	assert pos == 6

	// read next byte should error
	val, pos = read_byte(src, pos) or {
		assert err == error('invalid loc or len')
		return
	}
}

fn test_read_digit_outside_digit_iserror() ! {
	src := [u8(0xff)]
	_, _ := read_digit(src, 0) or {
		assert err == error('not digit byte')
		return
	}
}

fn test_simple_read_digit() ! {
	// 0x22 is not digit digit byte
	src := [u8(0x32), 0x33, 0x34, 0x35, 0x39, 0x22]
	start := 0

	mut val, mut pos := read_digit(src, start)!
	assert val == 2 // (0x32-0x30)
	assert pos == 1

	val, pos = read_digit(src, pos)!
	assert val == 3 // 0x33-0x30
	assert pos == 2

	val, pos = read_digit(src, pos)!
	assert val == 4 // 0x34 - 0x30
	assert pos == 3

	val, pos = read_digit(src, pos)!
	assert val == 5 // same principe as above
	assert pos == 4

	val, pos = read_digit(src, pos)!
	assert val == 9
	assert pos == 5

	// the last item was not digit
	val, pos = read_digit(src, pos) or {
		assert err == error('not digit byte')
		return
	}
}

fn test_read_2_digits_single() ! {
	src := [u8(0x35)]
	start := 0
	read_2_digits(src, start) or {
		assert err == error('not enough bytes')
		return
	}
}

fn test_read_2_digits_double() ! {
	src := [u8(0x35), 0x33]
	start := 0
	val, pos := read_2_digits(src, start)!
	assert val == 53 // 5*10+3
	assert pos == 2
}

fn test_read_2_digits_simple() ! {
	// 0x22 is not digit digit byte
	src := [u8(0x30), 0x31, 0x32, 0x33, 0x34, 0x35, 0x36, 0x37, 0x38, 0x39, 0x22] // 48,49,50,51,52,53,54,55,56,57, "
	start := 0

	mut val, mut pos := read_2_digits(src, start)!
	assert val == 1 // 0*10+1
	assert pos == 2

	val, pos = read_2_digits(src, pos)!
	assert val == 23 // 2*10+3
	assert pos == 4

	val, pos = read_2_digits(src, pos)!
	assert val == 45 // 4*10+5
	assert pos == 6

	val, pos = read_2_digits(src, pos)!
	assert val == 67 // 6*10+7
	assert pos == 8

	val, pos = read_2_digits(src, pos)!
	assert val == 89 // 8*10+9
	assert pos == 10

	// last item was 0x22, its not digit and not enough
	val, pos = read_2_digits(src, pos) or {
		assert err == error('not enough bytes')
		return
	}
}

fn test_read_4_digits() ! {
	src := [u8(0x30), 0x31, 0x32, 0x33, 0x34, 0x35, 0x36, 0x37, 0x38, 0x39, 0x35] // 48,49,50,51,52,53,54,55,56,57
	start := 0

	mut val, mut pos := read_4_digits(src, start)!
	assert val == 123 // 0*1000 + 1*100 + 2*10 + 3
	assert pos == 4

	val, pos = read_4_digits(src, pos)!
	assert val == 4567 // 4*1000 + 5*100 + 6*10 + 7
	assert pos == 8

	// last read 4 digit was error
	val, pos = read_4_digits(src, pos) or {
		assert err == error('not enough bytes')
		return
	}
}
