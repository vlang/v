module util

// The tables are the acceptance check for an assembly block's register names, so
// a missing entry is a register that looks misspelled to the user.
fn test_asm_register_names_dispatches_on_the_architecture() {
	assert asm_register_names('amd64') == x86_asm_register_names()
	assert asm_register_names('x86_64') == x86_asm_register_names()
	assert asm_register_names('i386') == x86_asm_register_names()
	assert asm_register_names('arm64') == arm64_asm_register_names()
	assert asm_register_names('aarch64') == arm64_asm_register_names()
	// An architecture with no table here yields nothing, and callers must not
	// treat that as an unknown name.
	assert asm_register_names('riscv64') == []string{}
	assert asm_register_names('wasm32') == []string{}
	assert asm_register_names('') == []string{}
}

// The generated names have to be the ones a user writes in an `asm` block, so the
// numbered series are what is actually being asserted.
fn test_x86_asm_register_names_cover_the_general_and_vector_sets() {
	names := x86_asm_register_names()
	for name in ['al', 'ah', 'ax', 'eax', 'rax', 'bx', 'cx', 'dx', 'sp', 'bp', 'si', 'di', 'ip',
		'eip', 'rip', 'flags', 'eflags', 'rflags', 'cs', 'ss', 'ds', 'es', 'fs', 'gs', 'eiz', 'riz',
		'gdtr', 'idtr', 'cw', 'sw', 'tw', 'fp_ip', 'fp_dp', 'mxcsr'] {
		assert name in names, 'missing x86 register `${name}`'
	}
	for i in 0 .. 8 {
		assert 'k${i}' in names
		assert 'tmm${i}' in names
		assert 'mm${i}' in names
		assert 'st${i}' in names
	}
	for i in 0 .. 4 {
		assert 'bnd${i}' in names
	}
	for i in 8 .. 32 {
		for width in ['', 'b', 'w', 'd'] {
			assert 'r${i}${width}' in names
		}
	}
	for i in 0 .. 16 {
		assert 'cr${i}' in names
		assert 'dr${i}' in names
	}
	for i in 0 .. 32 {
		assert 'xmm${i}' in names
		assert 'ymm${i}' in names
		assert 'zmm${i}' in names
	}
	// No duplicates, or a suggestion built from the table would be ambiguous.
	mut unique := []string{cap: names.len}
	for name in names {
		assert name !in unique, 'duplicate x86 register `${name}`'
		unique << name
	}
}

fn test_arm64_asm_register_names_cover_the_core_and_vector_sets() {
	names := arm64_asm_register_names()
	for name in ['sp', 'wsp', 'lr', 'fp', 'pc', 'xzr', 'wzr', 'nzcv', 'fpcr', 'fpsr', 'daif', 'za',
		'zt0'] {
		assert name in names, 'missing arm64 register `${name}`'
	}
	for i in 0 .. 32 {
		assert 'x${i}' in names
		assert 'w${i}' in names
		assert 'v${i}' in names
		assert 'q${i}' in names
		assert 'd${i}' in names
		assert 's${i}' in names
		assert 'h${i}' in names
		assert 'b${i}' in names
		assert 'z${i}' in names
	}
	for i in 0 .. 16 {
		assert 'p${i}' in names
		assert 'za${i}' in names
		assert 'za${i}h' in names
		assert 'za${i}v' in names
	}
	for i in 8 .. 16 {
		assert 'pn${i}' in names
	}
	mut unique := []string{cap: names.len}
	for name in names {
		assert name !in unique, 'duplicate arm64 register `${name}`'
		unique << name
	}
}
