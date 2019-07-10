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
	return u32(1 << (bitnr % SLOT_SIZE))
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

