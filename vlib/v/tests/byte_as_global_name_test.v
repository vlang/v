@[has_globals]
module main

__global byte int

fn update_byte_global() {
	byte = 8
	byte += 2
	byte++
	byte--
}

fn read_byte_global() int {
	return byte
}

fn test_byte_global_reads_and_writes() {
	assert byte == 0
	update_byte_global()
	assert read_byte_global() == 10
	byte = 12
	assert read_byte_global() == 12
}

fn test_sizeof_byte_global_uses_its_type() {
	assert sizeof(byte) == sizeof(int)
}
