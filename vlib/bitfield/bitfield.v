module bitfield

/*
bitfield is a module for
manipulating arrays of bits, i.e. series of zeroes and ones spread across an
array of storage units (unsigned 32-bit integers).

BitField structure
------------------

Bit arrays are stored in data structures called 'BitField'. The structure is
'opaque', i.e. its internals are not available to the end user. This module
provides API (functions and methods) for accessing and modifying bit arrays.
*/

pub struct BitField {
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
	return (instance.field[bitslot(bitnr)] >> (bitnr % SLOT_SIZE)) & u32(1)
}

fn bitset(instance mut BitField, bitnr int) {
	instance.field[bitslot(bitnr)] = instance.field[bitslot(bitnr)] | bitmask(bitnr)
}

fn bitclear(instance mut BitField, bitnr int) {
	instance.field[bitslot(bitnr)] = instance.field[bitslot(bitnr)] & ~bitmask(bitnr)
}

fn bittoggle(instance mut BitField, bitnr int) {
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

fn cleartail(instance mut BitField) {
	tail := instance.size % SLOT_SIZE
	if tail != 0 {
		// create a mask for the tail
		mask := u32((1 << tail) - 1)
		// clear the extra bits
		instance.field[bitnslots(instance.size) - 1] = instance.field[bitnslots(instance.size) - 1] & mask
	}
}

// public functions

// from_bytes() converts a byte array into a bitfield.
pub fn from_bytes(input []byte) BitField {
	mut output := new(input.len * 8)
	for i, b in input {
		output.field[i / 4] |= u32(b) << ((i % 4) * 8)
	}
	return output
}

// from_string() converts a string of characters ('0' and '1') to a bit
// array. Any character different from '0' is treated as '1'.

pub fn from_string(input string) BitField {
	mut output := new(input.len)
	for i in 0..input.len {
		if input[i] != 48 {
			output.setbit(i)
		}
	}
	return output
}

// string() converts the bit array to a string of characters ('0' and '1') and
// return the string

pub fn (input BitField) string() string {
	mut output := ''
	for i in 0..input.size {
		if input.getbit(i) == 1 {
			output = output + '1'
		}
		else {
			output = output + '0'
		}
	}
	return output
}

//new() creates an empty bit array of capable of storing 'size' bits.

pub fn new(size int) BitField {
	output := BitField{
		size: size
		//field: *u32(calloc(bitnslots(size) * SLOT_SIZE / 8))
		field: [u32(0)].repeat(bitnslots(size))
	}
	return output
}
/*
pub fn del(instance *BitField) {
	free(instance.field)
	free(instance)
}
*/

// getbit() returns the value (0 or 1) of bit number 'bit_nr' (count from
// 0)

pub fn (instance BitField) getbit(bitnr int) int {
	if bitnr >= instance.size {return 0}
	return bitget(instance, bitnr)
}

// setbit() set bit number 'bit_nr' to 1 (count from 0)

pub fn (instance mut BitField) setbit(bitnr int) {
	if bitnr >= instance.size {return}
	bitset(mut instance, bitnr)
}

// clearbit() clears (sets to zero) bit number 'bit_nr' (count from 0)

pub fn (instance mut BitField) clearbit(bitnr int) {
	if bitnr >= instance.size {return}
	bitclear(mut instance, bitnr)
}

// setall() sets all bits in the array to 1

pub fn (instance mut BitField) setall() {
	for i in 0..bitnslots(instance.size) {
		instance.field[i] = u32(-1)
	}
	cleartail(mut instance)
}

// clearall() clears (sets to zero) all bits in the array

pub fn (instance mut BitField) clearall() {
	for i in 0..bitnslots(instance.size) {
		instance.field[i] = u32(0)
	}
}

// togglebit() change the value (from 0 to 1 or from 1 to 0) of bit
// number 'bit_nr'

pub fn (instance mut BitField) togglebit(bitnr int) {
	if bitnr >= instance.size {return}
	bittoggle(mut instance, bitnr)
}

// bfand() perform logical AND operation on every pair of bits from 'input1'
// and 'input2' and return the result as a new array. If inputs differ in size,
// the tail of the longer one is ignored.

pub fn bfand(input1 BitField, input2 BitField) BitField {
	size := min(input1.size, input2.size)
	bitnslots := bitnslots(size)
	mut output := new(size)
	mut i := 0
	for i < bitnslots {
		output.field[i] = input1.field[i] & input2.field[i]
		i++
	}
	cleartail(mut output)
	return output
}

// bfnot() toggle all bits in a bit array and return the result as a new array

pub fn bfnot(input BitField) BitField {
	size := input.size
	bitnslots := bitnslots(size)
	mut output := new(size)
	mut i := 0
	for i < bitnslots {
		output.field[i] = ~input.field[i]
		i++
	}
	cleartail(mut output)
	return output
}

// bfor() perform logical OR operation on every pair of bits from 'input1' and
// 'input2' and return the result as a new array. If inputs differ in size, the
// tail of the longer one is ignored.

pub fn bfor(input1 BitField, input2 BitField) BitField {
	size := min(input1.size, input2.size)
	bitnslots := bitnslots(size)
	mut output := new(size)
	mut i := 0
	for i < bitnslots {
		output.field[i] = input1.field[i] | input2.field[i]
		i++
	}
	cleartail(mut output)
	return output
}

// bfxor(input1 BitField, input2 BitField) perform logical XOR operation on
// every pair of bits from 'input1' and 'input2' and return the result as a new
// array. If inputs differ in size, the tail of the longer one is ignored.

pub fn bfxor(input1 BitField, input2 BitField) BitField {
	size := min(input1.size, input2.size)
	bitnslots := bitnslots(size)
	mut output := new(size)
	mut i := 0
	for i < bitnslots {
		output.field[i] = input1.field[i] ^ input2.field[i]
		i++
	}
	cleartail(mut output)
	return output
}

// join() concatenates two bit arrays and return the result as a new array.

pub fn join(input1 BitField, input2 BitField) BitField {
	output_size := input1.size + input2.size
	mut output := new(output_size)
	// copy the first input to output as is
	for i in 0..bitnslots(input1.size) {
		output.field[i] = input1.field[i]
	}

	// find offset bit and offset slot
	offset_bit := input1.size % SLOT_SIZE
	offset_slot := input1.size / SLOT_SIZE

	for i in 0..bitnslots(input2.size) {
		output.field[i + offset_slot] |=
		    u32(input2.field[i] << u32(offset_bit))
	}

	/*
	 * If offset_bit is not zero, additional operations are needed.
	 * Number of iterations depends on the nr of slots in output. Two
	 * options:
	 * (a) nr of slots in output is the sum of inputs' slots. In this
	 * case, the nr of bits in the last slot of output is less than the
	 * nr of bits in the second input (i.e. ), OR
	 * (b) nr of slots of output is the sum of inputs' slots less one
	 * (i.e. less iterations needed). In this case, the nr of bits in
	 * the last slot of output is greater than the nr of bits in the second
	 * input.
	 * If offset_bit is zero, no additional copies needed.
	 */
	if (output_size - 1) % SLOT_SIZE < (input2.size - 1) % SLOT_SIZE {
		for i in 0..bitnslots(input2.size) {
			output.field[i + offset_slot + 1] |=
			    u32(input2.field[i] >> u32(SLOT_SIZE - offset_bit))
		}
	} else if (output_size - 1) % SLOT_SIZE > (input2.size - 1) % SLOT_SIZE {
		for i in 0..bitnslots(input2.size) - 1 {
			output.field[i + offset_slot + 1] |=
			    u32(input2.field[i] >> u32(SLOT_SIZE - offset_bit))
		}
	}
	return output
}

// print(instance BitField) send the content of a bit array to stdout as a
// string of characters ('0' and '1').

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

// getsize() returns the number of bits the array can hold

pub fn (instance BitField) getsize() int {
	return instance.size
}

// clone() create a copy of a bit array

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

// cmp() compare two bit arrays bit by bit and return 'true' if they are
// identical by length and contents and 'false' otherwise.

pub fn cmp(input1 BitField, input2 BitField) bool {
	if input1.size != input2.size {return false}
	for i in 0..bitnslots(input1.size) {
		if input1.field[i] != input2.field[i] {return false}
	}
	return true
}

// popcount() returns the number of set bits (ones) in the array

pub fn (instance BitField) popcount() int {
	size := instance.size
	bitnslots := bitnslots(size)
	tail := size % SLOT_SIZE
	mut count := 0
	for i in 0..bitnslots - 1 {
		for j in 0..SLOT_SIZE {
			if u32(instance.field[i] >> u32(j)) & u32(1) == u32(1) {
				count++
			}
		}
	}
	for j in 0..tail {
		if u32(instance.field[bitnslots - 1] >> u32(j)) & u32(1) == u32(1) {
			count++
		}
	}
	return count
}

// hamming () compute the Hamming distance between two bit arrays.

pub fn hamming (input1 BitField, input2 BitField) int {
	input_xored := bfxor(input1, input2)
	return input_xored.popcount()
}

// pos() checks if the array contains a sub-array 'needle' and returns its
// position if it does, -1 if it does not, and -2 on error.

pub fn (haystack BitField) pos(needle BitField) int {
	heystack_size := haystack.size
	needle_size := needle.size
	diff := heystack_size - needle_size

	// needle longer than haystack; return error code -2
	if diff < 0 {
		return -2
	}
	for i := 0; i <= diff; i++ {
		needle_candidate := haystack.slice(i, needle_size + i)
		if cmp(needle_candidate, needle) {
			// needle matches a sub-array of haystack; return starting position of the sub-array
			return i
		}
	}
	// nothing matched; return -1
	return -1
}

// slice() return a sub-array of bits between 'start_bit_nr' (included) and
// 'end_bit_nr' (excluded)

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
			for i in 0..output_slots - 1 {
				output.field[i] =
				    u32(input.field[start_slot + i] >> u32(start_offset))
				output.field[i] = output.field[i] |
				    u32(input.field[start_slot + i + 1] <<
				    u32(SLOT_SIZE - start_offset))
			}
		}
		else {
			for i in 0..output_slots - 1 {
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
		output.field[(end - start - 1) / SLOT_SIZE] |= mask
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
		output.field[(end - start - 1) / SLOT_SIZE] |= mask
	}
	return output
}

// reverse() reverses the order of bits in the array (swap the first with the
// last, the second with the last but one and so on)

pub fn (instance BitField) reverse() BitField {
	size := instance.size
	bitnslots := bitnslots(size)
	mut output := new(size)
	for i:= 0; i < (bitnslots - 1); i++ {
		for j in 0..SLOT_SIZE {
			if u32(instance.field[i] >> u32(j)) & u32(1) == u32(1) {
				bitset(mut output, size - i * SLOT_SIZE - j - 1)
			}
		}
	}
	bits_in_last_input_slot := (size - 1) % SLOT_SIZE + 1
	for j in 0..bits_in_last_input_slot {
		if u32(instance.field[bitnslots - 1] >> u32(j)) & u32(1) == u32(1) {
			bitset(mut output, bits_in_last_input_slot - j - 1)
		}
	}
	return output
}

// resize changes the size of the bit array to 'new_size'
pub fn (instance mut BitField) resize(new_size int) {
	new_bitnslots := bitnslots(new_size)
	old_size := instance.size
	old_bitnslots := bitnslots(old_size)
	mut field := [u32(0)].repeat(new_bitnslots)
	for i := 0; i < old_bitnslots && i < new_bitnslots; i++ {
		field[i] = instance.field[i]
	}
	instance.field = field.clone()
	instance.size = new_size
	if new_size < old_size && new_size % SLOT_SIZE != 0 {
		cleartail(mut instance)
	}
}

// rotate(offset int) circular-shift the bits by 'offset' positions (move
// 'offset' bit to 0, 'offset+1' bit to 1, and so on)

pub fn (instance BitField) rotate(offset int) BitField {
	/**
	 * This function "cuts" the bitfield into two and swaps them.
	 * If the offset is positive, the cutting point is counted from the
	 * beginning of the bit array, otherwise from the end.
	**/
	size := instance.size
	// removing extra rotations

	mut offset_internal := offset % size
	if (offset_internal == 0) {
		// nothing to shift
		return instance
	}
	if offset_internal < 0 {
		offset_internal = offset_internal + size
	}

	first_chunk := instance.slice(0, offset_internal)
	second_chunk := instance.slice(offset_internal, size)
	output := join(second_chunk, first_chunk)
	return output
}
