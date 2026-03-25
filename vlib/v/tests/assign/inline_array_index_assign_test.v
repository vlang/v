fn test_inline_array_index_assign_uses_full_expression() {
	mut ram := []u8{len: 0x10000}
	rom := [u8(0x00), 0x12, 0x00, 0x00]
	pc := 0
	value := u8(0x0f)

	address := u16(rom[pc + 2]) << 8 | u16(rom[pc + 1])
	ram[address] = value
	assert ram[0x0012] == value
	assert ram[0] == 0

	mut ram2 := []u8{len: 0x10000}
	ram2[u16(rom[pc + 2]) << 8 | u16(rom[pc + 1])] = value
	assert ram2[0x0012] == value
	assert ram2[0] == 0
}

fn test_inline_fixed_array_index_assign_uses_full_expression() {
	mut ram := [0x10000]u8{}
	rom := [u8(0x00), 0x12, 0x00, 0x00]!
	pc := 0
	value := u8(0x0f)

	address := u16(rom[pc + 2]) << 8 | u16(rom[pc + 1])
	ram[address] = value
	assert ram[0x0012] == value
	assert ram[0] == 0

	mut ram2 := [0x10000]u8{}
	ram2[u16(rom[pc + 2]) << 8 | u16(rom[pc + 1])] = value
	assert ram2[0x0012] == value
	assert ram2[0] == 0
}

struct MemoryState {
mut:
	a   u8
	pc  int
	ram [0x10000]u8
	rom [4]u8
}

fn (mut state MemoryState) write_with_temp() {
	address := u16(state.rom[state.pc + 2]) << 8 | u16(state.rom[state.pc + 1])
	state.ram[address] = state.a
}

fn (mut state MemoryState) write_inline() {
	state.ram[u16(state.rom[state.pc + 2]) << 8 | u16(state.rom[state.pc + 1])] = state.a
}

fn test_inline_struct_fixed_array_index_assign_uses_full_expression() {
	mut temp_state := MemoryState{
		a:   0x0f
		pc:  0
		rom: [u8(0x00), 0x12, 0x00, 0x00]!
	}
	temp_state.write_with_temp()
	assert temp_state.ram[0x0012] == temp_state.a
	assert temp_state.ram[0] == 0

	mut inline_state := MemoryState{
		a:   0x0f
		pc:  0
		rom: [u8(0x00), 0x12, 0x00, 0x00]!
	}
	inline_state.write_inline()
	assert inline_state.ram[0x0012] == inline_state.a
	assert inline_state.ram[0] == 0
}
