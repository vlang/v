import bitfield
import rand

fn test_bf_new_size() {
	instance := bitfield.new(75)
	assert instance.get_size() == 75
}

fn test_bf_set_clear_toggle_get() {
	mut instance := bitfield.new(75)
	instance.set_bit(47)
	assert instance.get_bit(47) == 1
	instance.clear_bit(47)
	assert instance.get_bit(47) == 0
	instance.toggle_bit(47)
	assert instance.get_bit(47) == 1
}

fn test_bf_insert_extract() {
	mut instance := bitfield.new(11)
	instance.set_all()
	instance.insert(2, 9, 3)
	assert instance.extract(2, 1) == 0
	assert instance.extract(2, 8) == 1
	assert instance.extract(10, 1) == 1
	instance.set_all()
	instance.insert_lowest_bits_first(2, 9, 3)
	assert instance.extract_lowest_bits_first(2, 1) == 1
	assert instance.extract_lowest_bits_first(2, 8) == 3
	assert instance.extract_lowest_bits_first(10, 1) == 0
}

fn test_bf_and_not_or_xor() {
	len := 80
	mut input1 := bitfield.new(len)
	mut input2 := bitfield.new(len)
	mut i := 0
	for i < len {
		if rand.intn(2) == 1 {
			input1.set_bit(i)
		}
		if rand.intn(2) == 1 {
			input2.set_bit(i)
		}
		i++
	}
	output1 := bitfield.bf_xor(input1, input2)
	bf_and := bitfield.bf_and(input1, input2)
	bf_or := bitfield.bf_or(input1, input2)
	bf_not := bitfield.bf_not(bf_and)
	output2 := bitfield.bf_and(bf_or, bf_not)
	mut result := 1
	for i < len {
		if output1.get_bit(i) != output2.get_bit(i) {
			result = 0
		}
	}
	assert result == 1
}

fn test_clone_cmp() {
	len := 80
	mut input := bitfield.new(len)
	for i in 0 .. len {
		if rand.intn(2) == 1 {
			input.set_bit(i)
		}
	}
	output := input.clone()
	assert output.get_size() == len
	assert input.cmp(output) == true
}

fn test_slice_join() {
	len := 80
	mut input := bitfield.new(len)
	for i in 0 .. len {
		if rand.intn(2) == 1 {
			input.set_bit(i)
		}
	}
	mut result := 1
	for point := 1; point < (len - 1); point++ {
		// divide a bitfield into two subfields
		chunk1 := input.slice(0, point)
		chunk2 := input.slice(point, input.get_size())
		// concatenate them back into one and compare to the original
		output := bitfield.join(chunk1, chunk2)
		if !input.cmp(output) {
			result = 0
		}
	}
	assert result == 1
}

fn test_pop_count() {
	len := 80
	mut count0 := 0
	mut input := bitfield.new(len)
	for i in 0 .. len {
		if rand.intn(2) == 1 {
			input.set_bit(i)
			count0++
		}
	}
	count1 := input.pop_count()
	assert count0 == count1
}

fn test_hamming() {
	len := 80
	mut count := 0
	mut input1 := bitfield.new(len)
	mut input2 := bitfield.new(len)
	for i in 0 .. len {
		match rand.intn(4) {
			0, 1 {
				input1.set_bit(i)
				count++
			}
			2 {
				input2.set_bit(i)
				count++
			}
			3 {
				input1.set_bit(i)
				input2.set_bit(i)
			}
			else {}
		}
	}
	assert count == bitfield.hamming(input1, input2)
}

fn test_bf_from_bytes() {
	input := [byte(0x01), 0xF0, 0x0F, 0xF0, 0xFF]
	output := bitfield.from_bytes(input).str()
	assert output == '00000001' + '11110000' + '00001111' + '11110000' + '11111111'
	newoutput := bitfield.from_str(output).str()
	assert newoutput == output
}

fn test_bf_from_bytes_lowest_bits_first() {
	input := [byte(0x01), 0xF0]
	output := bitfield.from_bytes_lowest_bits_first(input).str()
	assert output == '10000000' + '00001111'
	newoutput := bitfield.from_str(output).str()
	assert newoutput == output
}

fn test_bf_from_str() {
	len := 80
	mut input := ''
	for _ in 0 .. len {
		if rand.intn(2) == 1 {
			input = input + '1'
		} else {
			input = input + '0'
		}
	}
	output := bitfield.from_str(input)
	mut result := 1
	for i in 0 .. len {
		if input[i] != output.get_bit(i) + 48 {
			result = 0
		}
	}
	assert result == 1
}

fn test_bf_bf2str() {
	len := 80
	mut input := bitfield.new(len)
	for i in 0 .. len {
		if rand.intn(2) == 1 {
			input.set_bit(i)
		}
	}
	mut check := ''
	for i in 0 .. len {
		if input.get_bit(i) == 1 {
			check = check + '1'
		} else {
			check = check + '0'
		}
	}
	output := input.str()
	mut result := 1
	for i in 0 .. len {
		if check[i] != output[i] {
			result = 0
		}
	}
	assert result == 1
}

fn test_bf_set_all() {
	len := 80
	mut input := bitfield.new(len)
	input.set_all()
	mut result := 1
	for i in 0 .. len {
		if input.get_bit(i) != 1 {
			result = 0
		}
	}
	assert result == 1
}

fn test_bf_clear_all() {
	len := 80
	mut input := bitfield.new(len)
	for i in 0 .. len {
		if rand.intn(2) == 1 {
			input.set_bit(i)
		}
	}
	input.clear_all()
	mut result := 1
	for i in 0 .. len {
		if input.get_bit(i) != 0 {
			result = 0
		}
	}
	assert result == 1
}

fn test_bf_reverse() {
	len := 80
	mut input := bitfield.new(len)
	for i in 0 .. len {
		if rand.intn(2) == 1 {
			input.set_bit(i)
		}
	}
	check := input.clone()
	output := input.reverse()
	mut result := 1
	for i in 0 .. len {
		if output.get_bit(i) != check.get_bit(len - i - 1) {
			result = 0
		}
	}
	assert result == 1
}

fn test_bf_resize() {
	len := 80
	mut input := bitfield.new(rand.intn(len) + 1)
	for _ in 0 .. 100 {
		input.resize(rand.intn(len) + 1)
		input.set_bit(input.get_size() - 1)
	}
	assert input.get_bit(input.get_size() - 1) == 1
}

fn test_bf_pos() {
	/*
	*
	 * set haystack size to 80
	 * test different sizes of needle, from 1 to 80
	 * test different positions of needle, from 0 to where it fits
	 * all haystacks here contain exactly one instanse of needle,
	 * so search should return non-negative-values
	*
	*/
	len := 80
	mut result := 1
	for i := 1; i < len; i++ { // needle size
		for j in 0 .. len - i { // needle position in the haystack
			// create the needle
			mut needle := bitfield.new(i)
			// fill the needle with random values
			for k in 0 .. i {
				if rand.intn(2) == 1 {
					needle.set_bit(k)
				}
			}
			// make sure the needle contains at least one set bit, selected randomly
			r := rand.intn(i)
			needle.set_bit(r)
			// create the haystack, make sure it contains the needle
			mut haystack := needle.clone()
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
		chunk1.set_all()
		input := bitfield.join(chunk1, chunk2)
		output := input.rotate(i)
		if output.get_bit(len - i - 1) != 0 || output.get_bit(len - i) != 1 {
			result = 0
		}
	}
	assert result == 1
}

fn test_bf_printing() {
	len := 80
	mut input := bitfield.new(len)
	for i in 0 .. len {
		if rand.intn(2) == 0 {
			input.set_bit(i)
		}
	}
	// the following should convert the bitfield input into a string automatically
	println(input)
	assert true
}
