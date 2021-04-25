module x64

pub struct Aarch64 {
	// arm64 specific stuff for code generation
}

pub fn (mut x Aarch64) allocate_var(mut g Gen, name string, size int, initial_val int) {
	eprintln('TODO: allocating var on arm64 ($name) = $size = $initial_val')
}
