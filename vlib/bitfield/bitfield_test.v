import bitfield

import rand
import time

fn test_bf_new_size() {
	instance := bitfield.new(75)
	assert instance.getsize() == 75
}

fn test_bf_set_clear_toggle_get() {
	mut instance := bitfield.new(75)
	instance.setbit(47)
	assert instance.getbit(47) == 1
	instance.clearbit(47)
	assert instance.getbit(47) == 0
	instance.togglebit(47)
	assert instance.getbit(47) == 1
}

fn test_bf_and_not_or_xor() {
	rand.seed(time.now().unix)
	len := 80
	mut input1 := bitfield.new(len)
	mut input2 := bitfield.new(len)
	mut i := 0
	for i < len {
		if rand.next(2) == 1 {
			input1.setbit(i)
		}
		if rand.next(2) == 1{
			input2.setbit(i)
		}
		i++
	}
	output1 := bitfield.bfxor(input1, input2)
	bfand := bitfield.bfand(input1, input2)
	bfor := bitfield.bfor(input1, input2)
	bfnot := bitfield.bfnot(bfand)
	output2 := bitfield.bfand(bfor, bfnot)
	mut result := 1
	for i < len {
		if output1.getbit(i) != output2.getbit(i) {result = 0}
	}
	assert result == 1
}

fn test_clone_cmp() {
	rand.seed(time.now().unix)
	len := 80
	mut input := bitfield.new(len)
	for i in 0..len {
		if rand.next(2) == 1 {
			input.setbit(i)
		}
	}
	output := bitfield.clone(input)
	assert output.getsize() == len
	assert bitfield.cmp(input, output) == true
}

fn test_slice_join() {
	rand.seed(time.now().unix)
	len := 80
	mut input := bitfield.new(len)
	for i in 0..len {
		if rand.next(2) == 1 {
			input.setbit(i)
		}
	}
	mut result := 1
	for point := 1; point < (len - 1); point++ {
		// divide a bitfield into two subfields
		chunk1 := input.slice(0, point)
		chunk2 := input.slice(point, input.getsize())
		// concatenate them back into one and compare to the original
		output := bitfield.join(chunk1, chunk2)
		if !bitfield.cmp(input, output) {
			result = 0
		}
	}
	assert result == 1
}

fn test_popcount() {
	rand.seed(time.now().unix)
	len := 80
	mut count0 := 0
	mut input := bitfield.new(len)
	for i in 0..len {
		if rand.next(2) == 1 {
			input.setbit(i)
			count0++
		}
	}
	count1 := input.popcount()
	assert count0 == count1
}

fn test_hamming() {
	rand.seed(time.now().unix)
	len := 80
	mut count := 0
	mut input1 := bitfield.new(len)
	mut input2 := bitfield.new(len)
	for i in 0..len {
		match rand.next(4) {
			0, 1 {
				input1.setbit(i)
				count++
			}
			2 {
				input2.setbit(i)
				count++
			}
			3 {
				input1.setbit(i)
				input2.setbit(i)
			}
			else {

			}
		}
	}
	assert count == bitfield.hamming(input1, input2)
}

fn test_bf_from_bytes() {
	input := [byte(0xF0), byte(0x0F), byte(0xF0), byte(0xFF)]
	output := bitfield.from_bytes(input)
	mut result := 1
	for i in 0..input.len * 8 {
		if (input[i / 8] >> (i % 8)) & 1 != output.getbit(i) {
			result = 0
		}
	}
	assert result == 1
}

fn test_bf_from_string() {
	rand.seed(time.now().unix)
	len := 80
	mut input := ''
	for i in 0..len {
		if rand.next(2) == 1 {
			input = input + '1'
		}
		else {
			input = input + '0'
		}
	}
	output := bitfield.from_string(input)
	mut result := 1
	for i in 0..len {
		if input[i] != output.getbit(i) + 48 {
			result = 0
		}
	}
	assert result == 1
}

fn test_bf_bf2str() {
	rand.seed(time.now().unix)
	len := 80
	mut input := bitfield.new(len)
	for i in 0..len {
		if rand.next(2) == 1 {
			input.setbit(i)
		}
	}
	mut check := ''
	for i in 0..len {
		if input.getbit(i) == 1 {
			check = check + '1'
		}
		else {
			check = check + '0'
		}
	}
	output := input.string()
	mut result := 1
	for i in 0..len {
		if check[i] != output[i] {
			result = 0
		}
	}
	assert result == 1
}

fn test_bf_setall() {
		rand.seed(time.now().unix)
	len := 80
	mut input := bitfield.new(len)
	input.setall()
	mut result := 1
	for i in 0..len {
		if input.getbit(i) != 1 {
			result = 0
		}
	}
	assert result == 1
}

fn test_bf_clearall() {
		rand.seed(time.now().unix)
	len := 80
	mut input := bitfield.new(len)
	for i in 0..len {
		if rand.next(2) == 1 {
			input.setbit(i)
		}
	}
	input.clearall()
	mut result := 1
	for i in 0..len {
		if input.getbit(i) != 0 {
			result = 0
		}
	}
	assert result == 1
}

fn test_bf_reverse() {
	rand.seed(time.now().unix)
	len := 80
	mut input := bitfield.new(len)
	for i in 0..len {
		if rand.next(2) == 1 {
			input.setbit(i)
		}
	}
	check := bitfield.clone(input)
	output := input.reverse()
	mut result := 1
	for i in 0..len {
		if output.getbit(i) != check.getbit(len - i - 1) {
			result = 0
		}
	}
	assert result == 1
}

fn test_bf_resize() {
	rand.seed(time.now().unix)
	len := 80
	mut input := bitfield.new(rand.next(len) + 1)
	for i in 0..100 {
		input.resize(rand.next(len) + 1)
		input.setbit(input.getsize() - 1)
	}
	assert input.getbit(input.getsize() - 1) == 1
}

fn test_bf_pos() {
	/**
	 * set haystack size to 80
	 * test different sizes of needle, from 1 to 80
	 * test different positions of needle, from 0 to where it fits
	 * all haystacks here contain exactly one instanse of needle,
	 * so search should return non-negative-values
	**/
	rand.seed(time.now().unix)
	len := 80
	mut result := 1
	for i := 1; i < len; i++ {	// needle size
		for j in 0..len - i {	// needle position in the haystack
			// create the needle
			mut needle := bitfield.new(i)

			// fill the needle with random values
			for k in 0..i {
				if rand.next(2) == 1 {
					needle.setbit(k)
				}
			}

			// make sure the needle contains at least one set bit, selected randomly
			r := rand.next(i)
			needle.setbit(r)

			// create the haystack, make sure it contains the needle
			mut haystack := bitfield.clone(needle)

			// if there is space between the start of the haystack and the sought needle, fill it with zeroes
			if j > 0 {
				start := bitfield.new(j)
				tmp := bitfield.join(start, haystack)
				haystack = tmp
			}

			// if there is space between the sought needle and the end of haystack, fill it with zeroes
			if j + i < len {
				end := bitfield.new(len - j - i)
				tmp2 := bitfield.join(haystack, end)
				haystack = tmp2
			}

			// now let's test
			// the result should be equal to j
			if haystack.pos(needle) != j {
				result = 0
			}
		}
	}
	assert result == 1
}

fn test_bf_rotate() {
	mut result := 1
	len := 80
	for i := 1; i < 80 && result == 1; i++ {
		mut chunk1 := bitfield.new(i)
		chunk2 := bitfield.new(len - i)
		chunk1.setall()
		input := bitfield.join(chunk1, chunk2)
		output := input.rotate(i)
		if output.getbit(len - i - 1) != 0 || output.getbit(len - i) != 1 {
			result = 0
		}
	}
	assert result == 1
}
