module sha3

fn test_round_constants() {
	assert iota_round_constants[0] == 0x0000000000000001
	assert iota_round_constants[1] == 0x0000000000008082
	assert iota_round_constants[2] == 0x800000000000808A
	assert iota_round_constants[3] == 0x8000000080008000
	assert iota_round_constants[4] == 0x000000000000808B
	assert iota_round_constants[5] == 0x0000000080000001
	assert iota_round_constants[6] == 0x8000000080008081
	assert iota_round_constants[7] == 0x8000000000008009
	assert iota_round_constants[8] == 0x000000000000008A
	assert iota_round_constants[9] == 0x0000000000000088
	assert iota_round_constants[10] == 0x0000000080008009
	assert iota_round_constants[11] == 0x000000008000000A
	assert iota_round_constants[12] == 0x000000008000808B
	assert iota_round_constants[13] == 0x800000000000008B
	assert iota_round_constants[14] == 0x8000000000008089
	assert iota_round_constants[15] == 0x8000000000008003
	assert iota_round_constants[16] == 0x8000000000008002
	assert iota_round_constants[17] == 0x8000000000000080
	assert iota_round_constants[18] == 0x000000000000800A
	assert iota_round_constants[19] == 0x800000008000000A
	assert iota_round_constants[20] == 0x8000000080008081
	assert iota_round_constants[21] == 0x8000000000008080
	assert iota_round_constants[22] == 0x0000000080000001
	assert iota_round_constants[23] == 0x8000000080008008
}
