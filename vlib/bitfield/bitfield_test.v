import bitfield

import rand
import time

fn test_bf_new_size() {
	instance := bf.new(75)
	assert instance.getsize() == 75
}

fn test_bf_set_clear_toggle_get() {
	mut instance := bf.new(75)
	instance.setbit(47)
	assert instance.getbit(47) == 1
	instance.clearbit(47)
	assert instance.getbit(47) == 0
	instance.togglebit(47)
	assert instance.getbit(47) == 1
}

fn test_bf_and_not_or_xor() {
	rand.seed(time.now().uni)
	len := 80
	mut input1 := bf.new(len)
	mut input2 := bf.new(len)
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
	output1 := bf.bfxor(input1, input2)
	bfand := bf.bfand(input1, input2)
	bfor := bf.bfor(input1, input2)
	bfnot := bf.bfnot(bfand)
	output2 := bf.bfand(bfor, bfnot)
	mut result := 1
	for i < len {
		if output1.getbit(i) != output2.getbit(i) {result = 0}
	}
	assert result == 1
}

fn test_clone_cmp() {
	rand.seed(time.now().uni)
	len := 80
	mut input := bf.new(len)
	for i := 0; i < len; i++ {
		if rand.next(2) == 1 {
			input.setbit(i)
		}
	}
	output := bf.clone(input)
	assert output.getsize() == len
	assert bf.cmp(input, output) == true
}

fn test_slice_join() {
	rand.seed(time.now().uni)
	len := 80
	mut input := bf.new(len)
	for i := 0; i < len; i++ {
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
		output := bf.join(chunk1, chunk2)
		if !bf.cmp(input, output) {
			result = 0
		}
	}
	assert result == 1
}

fn test_popcount() {
	rand.seed(time.now().uni)
	len := 80
	mut count0 := 0
	mut input := bf.new(len)
	for i := 0; i < len; i++ {
		if rand.next(2) == 1 {
			input.setbit(i)
			count0++
		}
	}
	count1 := input.popcount()
	assert count0 == count1
}

fn test_hamming() {
	rand.seed(time.now().uni)
	len := 80
	mut count := 0
	mut input1 := bf.new(len)
	mut input2 := bf.new(len)
	for i := 0; i < len; i++ {
		switch rand.next(4) {
			case 0:
			case 1:
				input1.setbit(i)
				count++
			case 2:
				input2.setbit(i)
				count++
			case 3:
				input1.setbit(i)
				input2.setbit(i)
		}
	}
	assert count == bf.hamming(input1, input2)
}

fn test_bf_str2bf() {
	rand.seed(time.now().uni)
	len := 80
	mut input := ''
	for i := 0; i < len; i++ {
		if rand.next(2) == 1 {
			input = input + '1'
		}
		else {
			input = input + '0'
		}
	}
	output := bf.str2bf(input)
	mut result := 1
	for i := 0; i < len; i++ {
		if input[i] != output.getbit(i) + 48 {
			result = 0
		}
	}
	assert result == 1
}

fn test_bf_bf2str() {
	rand.seed(time.now().uni)
	len := 80
	mut input := bf.new(len)
	for i := 0; i < len; i++ {
		if rand.next(2) == 1 {
			input.setbit(i)
		}
	}
	mut check := ''
	for i := 0; i < len; i++ {
		if input.getbit(i) == 1 {
			check = check + '1'
		}
		else {
			check = check + '0'
		}
	}
	output := input.string()
	mut result := 1
	for i := 0; i < len; i++ {
		if check[i] != output[i] {
			result = 0
		}
	}
	assert result == 1
}

fn test_bf_setall() {
		rand.seed(time.now().uni)
	len := 80
	mut input := bf.new(len)
	input.setall()
	mut result := 1
	for i := 0; i < len; i++ {
		if input.getbit(i) != 1 {
			result = 0
		}
	}
	assert result == 1
}

fn test_bf_clearall() {
		rand.seed(time.now().uni)
	len := 80
	mut input := bf.new(len)
	for i := 0; i < len; i++ {
		if rand.next(2) == 1 {
			input.setbit(i)
		}
	}
	input.clearall()
	mut result := 1
	for i := 0; i < len; i++ {
		if input.getbit(i) != 0 {
			result = 0
		}
	}
	assert result == 1
}

fn test_bf_reverse() {
	rand.seed(time.now().uni)
	len := 80
	mut input := bf.new(len)
	for i := 0; i < len; i++ {
		if rand.next(2) == 1 {
			input.setbit(i)
		}
	}
	check := bf.clone(input)
	output := input.reverse()
	mut result := 1
	for i := 0; i < len; i++ {
		if output.getbit(i) != check.getbit(len - i - 1) {
			result = 0
		}
	}
	assert result == 1
}

fn test_bf_resize() {
	rand.seed(time.now().uni)
	len := 80
	mut input := bf.new(rand.next(len) + 1)
	for i := 0; i < 100; i++ {
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
	rand.seed(time.now().uni)
	len := 80
	mut result := 1
	for i := 1; i < len; i++ {	// needle size
		for j := 0; j < len - i; j++ {	// needle position in the haystack
			// create the needle
			mut needle := bf.new(i)

			// fill the needle with random values
			for k := 0; k < i; k++ {
				if rand.next(2) == 1 {
					needle.setbit(k)
				}
			}

			// make sure the needle contains at least one set bit, selected randomly
			r := rand.next(i)
			needle.setbit(r)

			// create the haystack, make sure it contains the needle
			mut haystack := bf.clone(needle)

			// if there is space between the start of the haystack and the sought needle, fill it with zeroes
			if j > 0 {
				start := bf.new(j)
				tmp := bf.join(start, haystack)
				haystack = tmp
			}

			// if there is space between the sought needle and the end of haystack, fill it with zeroes
			if j + i < len {
				end := bf.new(len - j - i)
				tmp2 := bf.join(haystack, end)
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
		mut chunk1 := bf.new(i)
		chunk2 := bf.new(len - i)
		chunk1.setall()
		input := bf.join(chunk1, chunk2)
		output := input.rotate(i)
		if output.getbit(len - i - 1) != 0 || output.getbit(len - i) != 1 {
			result = 0
		}
	}
	assert result == 1
}
