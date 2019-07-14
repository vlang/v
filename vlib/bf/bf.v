module bf

struct BitField {
mut:
	size int
	//field *u32
	field []u32
}

// helper functions  
const (
	SLOT_SIZE = 32
)

fn bitmask(bitnr int) u32 {
	return u32(u32(1) << u32(bitnr % SLOT_SIZE))
}

fn bitslot(size int) int {
	return size / SLOT_SIZE
}

fn bitget(instance BitField, bitnr int) int {
	return (instance.field[bitslot(bitnr)] >> u32(bitnr % SLOT_SIZE)) & 1
}

fn bitset(instance BitField, bitnr int) {
	instance.field[bitslot(bitnr)] = instance.field[bitslot(bitnr)] | bitmask(bitnr)
}

fn bitclear(instance BitField, bitnr int) {
	instance.field[bitslot(bitnr)] = instance.field[bitslot(bitnr)] & ~bitmask(bitnr)
}

fn bittoggle(instance BitField, bitnr int) {
	instance.field[bitslot(bitnr)] = instance.field[bitslot(bitnr)] ^ bitmask(bitnr)
}
/*
#define BITTEST(a, b) ((a)->field[BITSLOT(b)] & BITMASK(b))
*/

fn min(input1 int, input2 int) int {
	if input1 < input2 {
		return input1
	}
	else {
		return input2
	}
}

fn bitnslots(length int) int {
	return (length - 1) / SLOT_SIZE + 1
} 

fn cleartail(instance BitField) {
	tail := instance.size % SLOT_SIZE
	if tail != 0 {
		// create a mask for the tail 
		mask := u32((1 << tail) - 1)
		// clear the extra bits 
		instance.field[bitnslots(instance.size) - 1] = instance.field[bitnslots(instance.size) - 1] & mask
	}
}

// public functions 

pub fn str2bf(input string) BitField {
	mut output := new(input.len)
	for i := 0; i < input.len; i++ {
		if input[i] != 48 {
			output.setbit(i)
		}
	}
	return output
}

pub fn (input BitField) string() string {
	mut output := ''
	for i := 0; i < input.size; i++ {
		if input.getbit(i) == 1 {
			output = output + '1'
		}
		else {
			output = output + '0'
		}
	}
	return output
}

pub fn new(size int) BitField {
	output := BitField{
		size: size 
		//field: *u32(calloc(bitnslots(size) * SLOT_SIZE / 8))
		field: [u32(0); bitnslots(size)]
	}
	return output
}
/*
pub fn del(instance *BitField) {
	free(instance.field)
	free(instance)
}
*/
pub fn (instance BitField) getbit(bitnr int) int {
	if bitnr >= instance.size {return 0}
	return bitget(instance, bitnr)
}

pub fn (instance mut BitField) setbit(bitnr int) {
	if bitnr >= instance.size {return}
	bitset(instance, bitnr)
}

pub fn (instance mut BitField) clearbit(bitnr int) {
	if bitnr >= instance.size {return}
	bitclear(instance, bitnr)
}

pub fn (instance mut BitField) setall() {
	for i := 0; i < bitnslots(instance.size); i++ {
		instance.field[i] = u32(-1)
	}
	cleartail(instance)
}

pub fn (instance mut BitField) clearall() {
	for i := 0; i < bitnslots(instance.size); i++ {
		instance.field[i] = u32(0)
	}
}

pub fn (instance mut BitField) togglebit(bitnr int) {
	if bitnr >= instance.size {return}
	bittoggle(instance, bitnr)
}

pub fn bfand(input1 BitField, input2 BitField) BitField {
	size := min(input1.size, input2.size)
	bitnslots := bitnslots(size)
	mut output := new(size)
	mut i := 0
	for i < bitnslots {
		output.field[i] = input1.field[i] & input2.field[i]
		i++
	}
	cleartail(output)
	return output
}

pub fn bfnot(input BitField) BitField {
	size := input.size
	bitnslots := bitnslots(size)
	mut output := new(size)
	mut i := 0
	for i < bitnslots {
		output.field[i] = ~input.field[i]
		i++
	}
	cleartail(output)
	return output
}

pub fn bfor(input1 BitField, input2 BitField) BitField {
	size := min(input1.size, input2.size)
	bitnslots := bitnslots(size)
	mut output := new(size)
	mut i := 0
	for i < bitnslots {
		output.field[i] = input1.field[i] | input2.field[i]
		i++
	}
	cleartail(output)
	return output
}

pub fn bfxor(input1 BitField, input2 BitField) BitField {
	size := min(input1.size, input2.size)
	bitnslots := bitnslots(size)
	mut output := new(size)
	mut i := 0
	for i < bitnslots {
		output.field[i] = input1.field[i] ^ input2.field[i]
		i++
	}
	cleartail(output)
	return output
}

pub fn join(input1 BitField, input2 BitField) BitField {
	output_size := input1.size + input2.size
	mut output := new(output_size)
	// copy the first input to output as is
	for i := 0; i < bitnslots(input1.size); i++ {
		output.field[i] = input1.field[i]
	}

	// find offset bit and offset slot
	offset_bit := input1.size % SLOT_SIZE
	offset_slot := input1.size / SLOT_SIZE

	for i := 0; i < bitnslots(input2.size); i++ {
		output.field[i + offset_slot] =
		    output.field[i + offset_slot] |
		    u32(input2.field[i] << u32(offset_bit))
	}

	/*
	 * If offset_bit is not zero, additional operations are needed.
	 * Number of iterations depends on the nr of slots in output. Two
	 * options:
	 * (a) nr of slots in output is the sum of inputs' slots. In this
	 * case, the nr of bits in the last slot of output is less than the
	 * nr of bits in second input (i.e. ), OR
	 * (b) nr of slots of output is the sum of inputs' slots less one
	 * (i.e. less iterations needed). In this case, the nr of bits in
	 * the last slot of output is greater than the nr of bits in second
	 * input.
	 * If offset_bit is zero, no additional copies needed.
	 */
	if (output_size - 1) % SLOT_SIZE < (input2.size - 1) % SLOT_SIZE {
		for i := 0; i < bitnslots(input2.size); i++ {
			output.field[i + offset_slot + 1] =
			    output.field[i + offset_slot + 1] |
			    u32(input2.field[i] >> u32(SLOT_SIZE - offset_bit))
		}
	} else if (output_size - 1) % SLOT_SIZE > (input2.size - 1) % SLOT_SIZE {
		for i := 0; i < bitnslots(input2.size) - 1; i++ {
			output.field[i + offset_slot + 1] =
			    output.field[i + offset_slot + 1] |
			    u32(input2.field[i] >> u32(SLOT_SIZE - offset_bit))
		}
	}
	return output
}

pub fn print(instance BitField) {
	mut i := 0
	for i < instance.size {
		if instance.getbit(i) == 1 {
			print('1')
		}
		else {
			print('0')
		}
		i++
	}
}

pub fn (instance BitField) getsize() int {
	return instance.size
}

pub fn clone(input BitField) BitField {
	bitnslots := bitnslots(input.size)
	mut output := new(input.size)
	mut i := 0
	for i < bitnslots {
		output.field[i] = input.field[i]
		i++
	}
	return output
}

pub fn cmp(input1 BitField, input2 BitField) bool {
	if input1.size != input2.size {return false}
	for i := 0; i < bitnslots(input1.size); i++ {
		if input1.field[i] != input2.field[i] {return false}
	}
	return true
}

pub fn (instance BitField) popcount() int {
	size := instance.size
	bitnslots := bitnslots(size)
	tail := size % SLOT_SIZE
	mut count := 0
	for i := 0; i < bitnslots - 1; i++ {
		for j := 0; j < SLOT_SIZE; j++ {
			if u32(instance.field[i] >> u32(j)) & u32(1) == u32(1) {
				count++
			}
		}
	}
	for j := 0; j < tail; j++ {
		if u32(instance.field[bitnslots - 1] >> u32(j)) & u32(1) == u32(1) {
			count++
		}
	}
	return count
}

pub fn hamming (input1 BitField, input2 BitField) int {
	input_xored := bfxor(input1, input2)
	return input_xored.popcount()
}

pub fn (input BitField) slice(_start int, _end int) BitField {
	// boundary checks
	mut start := _start
	mut end := _end
	if end > input.size {
		end = input.size // or panic?
	}
	if start > end {
		start = end // or panic?
	}

	mut output := new(end - start)
	start_offset := start % SLOT_SIZE
	end_offset := (end - 1) % SLOT_SIZE
	start_slot := start / SLOT_SIZE
	end_slot := (end - 1) / SLOT_SIZE
	output_slots := bitnslots(end - start)

	if output_slots > 1 {
		if start_offset != 0 {
			for i := 0; i < output_slots - 1; i++ {
				output.field[i] =
				    u32(input.field[start_slot + i] >> u32(start_offset))
				output.field[i] = output.field[i] |
				    u32(input.field[start_slot + i + 1] <<
				    u32(SLOT_SIZE - start_offset))
			}
		}
		else {
			for i := 0; i < output_slots - 1; i++ {
				output.field[i] =
				    u32(input.field[start_slot + i])
			}
		}
	}

	if start_offset > end_offset {
		output.field[(end - start - 1) / SLOT_SIZE] =
		    u32(input.field[end_slot - 1] >> u32(start_offset))
		mut mask := u32((1 << (end_offset + 1)) - 1)
		mask = input.field[end_slot] & mask
		mask = u32(mask << u32(SLOT_SIZE - start_offset))
		output.field[(end - start - 1) / SLOT_SIZE] =
		    output.field[(end - start - 1) / SLOT_SIZE] | mask
	}
	else if start_offset == 0 {
		mut mask := u32(0)
		if end_offset == SLOT_SIZE - 1 {
			mask = u32(-1)
		}
		else {
			mask = u32(u32(1) << u32(end_offset + 1))
			mask = mask - u32(1)
		}
		output.field[(end - start - 1) / SLOT_SIZE] =
		    (input.field[end_slot] & mask)
	}
	else {
		mut mask := u32(((1 << (end_offset - start_offset + 1)) - 1)  << start_offset)
		mask = input.field[end_slot] & mask
		mask = u32(mask >> u32(start_offset))
		output.field[(end - start - 1) / SLOT_SIZE] =
		    output.field[(end - start - 1) / SLOT_SIZE] | mask
	}
	return output
}
