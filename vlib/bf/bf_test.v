import bf

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
