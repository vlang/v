struct Abc {
	@asm string
}

enum Enum {
	@asm
	end
}

fn test_struct() {
	dump(Abc{})
	assert true
}

fn test_local() {
	@asm := 12
	dump(@asm)
	assert true
}

fn test_enum() {
	dump(unsafe { Enum(0) })
	dump(Enum.@asm)
	dump(Enum.end)
	assert true
}
