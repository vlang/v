module x64

pub struct Aarch64 {
mut:
	// arm64 specific stuff for code generation
	g Gen
}

pub fn (mut x Aarch64) allocate_var(name string, size int, initial_val int) {
	eprintln('TODO: allocating var on arm64 ($name) = $size = $initial_val')
}
