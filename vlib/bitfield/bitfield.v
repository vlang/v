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
	// field *u32
	field []u32
}

// helper functions
const (
	slot_size = 32
)

// from_bytes converts a byte array into a bitfield.
// [0x0F, 0x01] => 0000 1111 0000 0001
pub fn from_bytes(input []u8) BitField {
	mut output := new(input.len * 8)
	for i, b in input {
		mut ob := u8(0)
		if b & 0b10000000 > 0 {
			ob |= 0b00000001
		}
		if b & 0b01000000 > 0 {
			ob |= 0b00000010
		}
		if b & 0b00100000 > 0 {
			ob |= 0b00000100
		}
		if b & 0b00010000 > 0 {
			ob |= 0b00001000
		}
		if b & 0b00001000 > 0 {
			ob |= 0b00010000
		}
		if b & 0b00000100 > 0 {
			ob |= 0b00100000
		}
		if b & 0b00000010 > 0 {
			ob |= 0b01000000
		}
		if b & 0b00000001 > 0 {
			ob |= 0b10000000
		}
		output.field[i / 4] |= u32(ob) << ((i % 4) * 8)
	}
	return output
}

// from_bytes_lowest_bits_first converts a byte array into a bitfield
// [0x0F, 0x01] => 1111 0000 1000 0000
pub fn from_bytes_lowest_bits_first(input []u8) BitField {
	mut output := new(input.len * 8)
	for i, b in input {
		output.field[i / 4] |= u32(b) << ((i % 4) * 8)
	}
	return output
}

// from_str converts a string of characters ('0' and '1') to a bit
// array. Any character different from '0' is treated as '1'.
pub fn from_str(input string) BitField {
	mut output := new(input.len)
	for i in 0 .. input.len {
		if input[i] != `0` {
			output.set_bit(i)
		}
	}
	return output
}

// str converts the bit array to a string of characters ('0' and '1') and
// return the string
pub fn (input BitField) str() string {
	mut output := ''
	for i in 0 .. input.size {
		if input.get_bit(i) == 1 {
			output = output + '1'
		} else {
			output = output + '0'
		}
	}
	return output
}

// new creates an empty bit array of capable of storing 'size' bits.
pub fn new(size int) BitField {
	output := BitField{
		size: size
		// field: *u32(calloc(zbitnslots(size) * slot_size / 8))
		field: []u32{len: zbitnslots(size)}
	}
	return output
}

// frees the memory allocated for the bitfield instance
[unsafe]
pub fn (instance &BitField) free() {
	unsafe {
		instance.field.free()
	}
}

// get_bit returns the value (0 or 1) of bit number 'bit_nr' (count from 0).
pub fn (instance BitField) get_bit(bitnr int) int {
	if bitnr >= instance.size {
		return 0
	}
	return int((instance.field[bitslot(bitnr)] >> (bitnr % bitfield.slot_size)) & u32(1))
}

// set_bit sets bit number 'bit_nr' to 1 (count from 0).
pub fn (mut instance BitField) set_bit(bitnr int) {
	if bitnr >= instance.size {
		return
	}
	instance.field[bitslot(bitnr)] |= bitmask(bitnr)
}

// clear_bit clears (sets to zero) bit number 'bit_nr' (count from 0).
pub fn (mut instance BitField) clear_bit(bitnr int) {
	if bitnr >= instance.size {
		return
	}
	instance.field[bitslot(bitnr)] &= ~bitmask(bitnr)
}

// extract returns the value converted from a slice of bit numbers
// from 'start' by the length of 'len'.
// 0101 (1, 2) => 0b10
pub fn (instance BitField) extract(start int, len int) u64 {
	// panic?
	if start < 0 {
		return 0
	}
	mut output := u64(0)
	for i in 0 .. len {
		output |= u64(instance.get_bit(start + len - i - 1)) << i
	}
	return output
}

// insert sets bit numbers from 'start' to 'len' length with
// the value converted from the number 'value'.
// 0000 (1, 2, 0b10) => 0100
pub fn (mut instance BitField) insert<T>(start int, len int, _value T) {
	// panic?
	if start < 0 {
		return
	}
	mut value := _value
	for i in 0 .. len {
		pos := start + len - i - 1
		if value & 1 == 1 {
			instance.set_bit(pos)
		} else {
			instance.clear_bit(pos)
		}
		value >>= 1
	}
}

// extract returns the value converted from a slice of bit numbers
// from 'start' by the length of 'len'.
// 0101 (1, 2) => 0b01
pub fn (instance BitField) extract_lowest_bits_first(start int, len int) u64 {
	// panic?
	if start < 0 {
		return 0
	}
	mut output := u64(0)
	for i in 0 .. len {
		output |= u64(instance.get_bit(start + i)) << i
	}
	return output
}

// insert sets bit numbers from 'start' to 'len' length with
// the value converted from the number 'value'.
// 0000 (1, 2, 0b10) => 0010
pub fn (mut instance BitField) insert_lowest_bits_first<T>(start int, len int, _value T) {
	// panic?
	if start < 0 {
		return
	}
	mut value := _value
	for pos in start .. start + len {
		if value & 1 == 1 {
			instance.set_bit(pos)
		} else {
			instance.clear_bit(pos)
		}
		value >>= 1
	}
}

// set_all sets all bits in the array to 1.
pub fn (mut instance BitField) set_all() {
	for i in 0 .. zbitnslots(instance.size) {
		instance.field[i] = u32(-1)
	}
	instance.clear_tail()
}

// clear_all clears (sets to zero) all bits in the array.
pub fn (mut instance BitField) clear_all() {
	for i in 0 .. zbitnslots(instance.size) {
		instance.field[i] = u32(0)
	}
}

// toggle_bit changes the value (from 0 to 1 or from 1 to 0) of bit
// number 'bit_nr'.
pub fn (mut instance BitField) toggle_bit(bitnr int) {
	if bitnr >= instance.size {
		return
	}
	instance.field[bitslot(bitnr)] ^= bitmask(bitnr)
}

// bf_and performs logical AND operation on every pair of bits from 'input1' and
// 'input2' and returns the result as a new array. If inputs differ in size,
// the tail of the longer one is ignored.
pub fn bf_and(input1 BitField, input2 BitField) BitField {
	size := min(input1.size, input2.size)
	bitnslots := zbitnslots(size)
	mut output := new(size)
	for i in 0 .. bitnslots {
		output.field[i] = input1.field[i] & input2.field[i]
	}
	output.clear_tail()
	return output
}

// bf_not toggles all bits in a bit array and returns the result as a new array.
pub fn bf_not(input BitField) BitField {
	size := input.size
	bitnslots := zbitnslots(size)
	mut output := new(size)
	for i in 0 .. bitnslots {
		output.field[i] = ~input.field[i]
	}
	output.clear_tail()
	return output
}

// bf_or performs logical OR operation on every pair of bits from 'input1' and
// 'input2' and returns the result as a new array. If inputs differ in size,
// the tail of the longer one is ignored.
pub fn bf_or(input1 BitField, input2 BitField) BitField {
	size := min(input1.size, input2.size)
	bitnslots := zbitnslots(size)
	mut output := new(size)
	for i in 0 .. bitnslots {
		output.field[i] = input1.field[i] | input2.field[i]
	}
	output.clear_tail()
	return output
}

// bf_xor perform logical XOR operation on every pair of bits from 'input1' and
// 'input2' and returns the result as a new array. If inputs differ in size,
// the tail of the longer one is ignored.
pub fn bf_xor(input1 BitField, input2 BitField) BitField {
	size := min(input1.size, input2.size)
	bitnslots := zbitnslots(size)
	mut output := new(size)
	for i in 0 .. bitnslots {
		output.field[i] = input1.field[i] ^ input2.field[i]
	}
	output.clear_tail()
	return output
}

// join concatenates two bit arrays and return the result as a new array.
pub fn join(input1 BitField, input2 BitField) BitField {
	output_size := input1.size + input2.size
	mut output := new(output_size)
	// copy the first input to output as is
	for i in 0 .. zbitnslots(input1.size) {
		output.field[i] = input1.field[i]
	}
	// find offset bit and offset slot
	offset_bit := input1.size % bitfield.slot_size
	offset_slot := input1.size / bitfield.slot_size
	for i in 0 .. zbitnslots(input2.size) {
		output.field[i + offset_slot] |= u32(input2.field[i] << u32(offset_bit))
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
	if (output_size - 1) % bitfield.slot_size < (input2.size - 1) % bitfield.slot_size {
		for i in 0 .. zbitnslots(input2.size) {
			output.field[i + offset_slot + 1] |= u32(input2.field[i] >> u32(bitfield.slot_size - offset_bit))
		}
	} else if (output_size - 1) % bitfield.slot_size > (input2.size - 1) % bitfield.slot_size {
		for i in 0 .. zbitnslots(input2.size) - 1 {
			output.field[i + offset_slot + 1] |= u32(input2.field[i] >> u32(bitfield.slot_size - offset_bit))
		}
	}
	return output
}

// get_size returns the number of bits the array can hold.
pub fn (instance BitField) get_size() int {
	return instance.size
}

// clone creates a copy of a bit array.
pub fn (instance BitField) clone() BitField {
	bitnslots := zbitnslots(instance.size)
	mut output := new(instance.size)
	for i in 0 .. bitnslots {
		output.field[i] = instance.field[i]
	}
	return output
}

// == compares 2 bitfields, and returns true when they are equal
pub fn (a BitField) == (b BitField) bool {
	if a.size != b.size {
		return false
	}
	for i in 0 .. zbitnslots(a.size) {
		if a.field[i] != b.field[i] {
			return false
		}
	}
	return true
}

// pop_count returns the number of set bits (ones) in the array.
pub fn (instance BitField) pop_count() int {
	size := instance.size
	bitnslots := zbitnslots(size)
	tail := size % bitfield.slot_size
	mut count := 0
	for i in 0 .. bitnslots - 1 {
		for j in 0 .. bitfield.slot_size {
			if u32(instance.field[i] >> u32(j)) & u32(1) == u32(1) {
				count++
			}
		}
	}
	for j in 0 .. tail {
		if u32(instance.field[bitnslots - 1] >> u32(j)) & u32(1) == u32(1) {
			count++
		}
	}
	return count
}

// hamming computes the Hamming distance between two bit arrays.
pub fn hamming(input1 BitField, input2 BitField) int {
	input_xored := bf_xor(input1, input2)
	return input_xored.pop_count()
}

// pos checks if the array contains a sub-array 'needle' and returns its
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
		if needle_candidate == needle {
			// needle matches a sub-array of haystack; return starting position of the sub-array
			return i
		}
	}
	// nothing matched; return -1
	return -1
}

// slice returns a sub-array of bits between 'start_bit_nr' (included) and
// 'end_bit_nr' (excluded).
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
	start_offset := start % bitfield.slot_size
	end_offset := (end - 1) % bitfield.slot_size
	start_slot := start / bitfield.slot_size
	end_slot := (end - 1) / bitfield.slot_size
	output_slots := zbitnslots(end - start)
	if output_slots > 1 {
		if start_offset != 0 {
			for i in 0 .. output_slots - 1 {
				output.field[i] = u32(input.field[start_slot + i] >> u32(start_offset))
				output.field[i] = output.field[i] | u32(input.field[start_slot + i + 1] << u32(bitfield.slot_size - start_offset))
			}
		} else {
			for i in 0 .. output_slots - 1 {
				output.field[i] = u32(input.field[start_slot + i])
			}
		}
	}
	if start_offset > end_offset {
		output.field[(end - start - 1) / bitfield.slot_size] = u32(input.field[end_slot - 1] >> u32(start_offset))
		mut mask := u32((1 << (end_offset + 1)) - 1)
		mask = input.field[end_slot] & mask
		mask = u32(mask << u32(bitfield.slot_size - start_offset))
		output.field[(end - start - 1) / bitfield.slot_size] |= mask
	} else if start_offset == 0 {
		mut mask := u32(0)
		if end_offset == bitfield.slot_size - 1 {
			mask = u32(-1)
		} else {
			mask = u32(u32(1) << u32(end_offset + 1))
			mask = mask - u32(1)
		}
		output.field[(end - start - 1) / bitfield.slot_size] = (input.field[end_slot] & mask)
	} else {
		mut mask := u32(((1 << (end_offset - start_offset + 1)) - 1) << start_offset)
		mask = input.field[end_slot] & mask
		mask = u32(mask >> u32(start_offset))
		output.field[(end - start - 1) / bitfield.slot_size] |= mask
	}
	return output
}

// reverse reverses the order of bits in the array (swap the first with the
// last, the second with the last but one and so on).
pub fn (instance BitField) reverse() BitField {
	size := instance.size
	bitnslots := zbitnslots(size)
	mut output := new(size)
	for i := 0; i < (bitnslots - 1); i++ {
		for j in 0 .. bitfield.slot_size {
			if u32(instance.field[i] >> u32(j)) & u32(1) == u32(1) {
				output.set_bit(size - i * bitfield.slot_size - j - 1)
			}
		}
	}
	bits_in_last_input_slot := (size - 1) % bitfield.slot_size + 1
	for j in 0 .. bits_in_last_input_slot {
		if u32(instance.field[bitnslots - 1] >> u32(j)) & u32(1) == u32(1) {
			output.set_bit(bits_in_last_input_slot - j - 1)
		}
	}
	return output
}

// resize changes the size of the bit array to 'new_size'.
pub fn (mut instance BitField) resize(new_size int) {
	new_bitnslots := zbitnslots(new_size)
	old_size := instance.size
	old_bitnslots := zbitnslots(old_size)
	mut field := []u32{len: new_bitnslots}
	for i := 0; i < old_bitnslots && i < new_bitnslots; i++ {
		field[i] = instance.field[i]
	}
	instance.field = field.clone()
	instance.size = new_size
	if new_size < old_size && new_size % bitfield.slot_size != 0 {
		instance.clear_tail()
	}
}

// rotate circular-shifts the bits by 'offset' positions (move
// 'offset' bit to 0, 'offset+1' bit to 1, and so on).
pub fn (instance BitField) rotate(offset int) BitField {
	/*
	*
	 * This function "cuts" the bitfield into two and swaps them.
	 * If the offset is positive, the cutting point is counted from the
	 * beginning of the bit array, otherwise from the end.
	*
	*/
	size := instance.size
	// removing extra rotations
	mut offset_internal := offset % size
	if offset_internal == 0 {
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

// Internal functions
// clear_tail clears the extra bits that are not part of the bitfield, but yet are allocated
fn (mut instance BitField) clear_tail() {
	tail := instance.size % bitfield.slot_size
	if tail != 0 {
		// create a mask for the tail
		mask := u32((1 << tail) - 1)
		// clear the extra bits
		instance.field[zbitnslots(instance.size) - 1] = instance.field[zbitnslots(instance.size) - 1] & mask
	}
}

// bitmask is the bitmask needed to access a particular bit at offset bitnr
fn bitmask(bitnr int) u32 {
	return u32(u32(1) << u32(bitnr % bitfield.slot_size))
}

// bitslot is the slot index (i.e. the integer) where a particular bit is located
fn bitslot(size int) int {
	return size / bitfield.slot_size
}

// min returns the minimum of 2 integers; it is here to avoid importing math just for that
fn min(input1 int, input2 int) int {
	if input1 < input2 {
		return input1
	} else {
		return input2
	}
}

// zbitnslots returns the minimum number of whole integers, needed to represent a bitfield of size length
fn zbitnslots(length int) int {
	return (length - 1) / bitfield.slot_size + 1
}
