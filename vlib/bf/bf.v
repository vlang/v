module bf

struct bitfield {
mut:
	size int
	field *u32//[]u32
}

/* helper functions */
const (
	SLOT_SIZE = 32
)

fn bitmask(bitnr int) u32 {
	return u32(1 << (bitnr % SLOT_SIZE))
}

fn bitslot(size int) int {
	return size / SLOT_SIZE
}

fn bitget(instance *bitfield, bitnr int) int {
	return (instance.field[bitslot(bitnr)] >> u32(bitnr % SLOT_SIZE)) & 1
}

fn bitset(instance *bitfield, bitnr int) {
	instance.field[bitslot(bitnr)] = instance.field[bitslot(bitnr)] | bitmask(bitnr)
}

fn bitclear(instance *bitfield, bitnr int) {
	instance.field[bitslot(bitnr)] = instance.field[bitslot(bitnr)] & ~bitmask(bitnr)
}

fn bittoggle(instance *bitfield, bitnr int) {
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

fn cleartail(instance *bitfield) {
	tail := instance.size % SLOT_SIZE
	if tail != 0 {
		/* create a mask for the tail */
		mask := u32((1 << tail) - 1)
		/* clear the extra bits */
		instance.field[bitnslots(instance.size) - 1] = instance.field[bitnslots(instance.size) - 1] & mask
	}
}

/* public functions */

pub fn new(size int) *bitfield {
	output := &bitfield{
		size: size 
		field: *u32(calloc(bitnslots(size) * SLOT_SIZE / 8)) //[u32(0); bitnslots(size)]
	}
	return output
}

pub fn del(instance *bitfield) {
	free(instance.field)
	free(instance)
}

pub fn getbit(instance *bitfield, bitnr int) int {
	if bitnr >= instance.size {return 0}
	return bitget(instance, bitnr)
}

pub fn setbit(instance *bitfield, bitnr int) {
	if bitnr >= instance.size {return}
	bitset(instance, bitnr)
}

pub fn clearbit(instance *bitfield, bitnr int) {
	if bitnr >= instance.size {return}
	bitclear(instance, bitnr)
}

pub fn togglebit(instance *bitfield, bitnr int) {
	if bitnr >= instance.size {return}
	bittoggle(instance, bitnr)
}

pub fn bfand(input1 *bitfield, input2 *bitfield) *bitfield {
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

pub fn bfnot(input *bitfield) *bitfield {
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

pub fn bfor(input1 *bitfield, input2 *bitfield) *bitfield {
	size := min(input1.size, input2.size)
	bitnslots := bitnslots(size)
	mut output := new(size)
	mut i := 0
	for i < bitnslots {
		output.field[i] = input1.field[1] | input2.field[i]
		i++
	}
	cleartail(output)
	return output
}

pub fn bfxor(input1 *bitfield, input2 *bitfield) *bitfield {
	size := min(input1.size, input2.size)
	bitnslots := bitnslots(size)
	mut output := new(size)
	mut i := 0
	for i < bitnslots {
		output.field[i] = input1.field[1] ^ input2.field[i]
		i++
	}
	cleartail(output)
	return output
}

pub fn print(instance *bitfield) {
	mut i := 0
	for i < instance.size {
		if getbit(instance, i) == 1 {
			print('1')
		}
		else {
			print('0')
		}
		i++
	}
}

pub fn size(instance *bitfield) int {
	return instance.size
}

pub fn clone(input *bitfield) *bitfield {
	bitnslots := bitnslots(input.size)
	mut output := new(input.size)
	mut i := 0
	for i < bitnslots {
		output.field[i] = input.field[i]
		i++
	}
	return output
}