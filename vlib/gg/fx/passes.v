module fx

// PassOp is one named post-processing operation in a chain.
pub enum PassOp {
	grayscale
	invert
	brightness
	contrast
	saturate
	vignette
	blur
	tonemap
	bloom
}

// PassChain applies post-processing operations in order, like a small
// EffectComposer for CPU-rendered RGBA8 buffers. It is pure V and therefore
// usable on every backend.
pub struct PassChain {
pub mut:
	ops             []PassOp
	brightness      f32 = 1
	contrast        f32 = 1
	saturate        f32 = 1
	vignette        f32 = 0.5
	blur_radius     int = 1
	bloom_threshold f32 = 0.8
	bloom_strength  f32 = 0.4
}

// new_pass_chain returns an empty chain.
pub fn new_pass_chain() PassChain {
	return PassChain{}
}

// add appends an operation to the chain.
pub fn (mut c PassChain) add(op PassOp) {
	c.ops << op
}

// clear removes all operations.
pub fn (mut c PassChain) clear() {
	c.ops = []
}

// apply runs every operation over pixels, returning a new buffer. The input
// is never modified.
pub fn (c &PassChain) apply(pixels []u8, width int, height int) []u8 {
	mut current := pixels.clone()
	for op in c.ops {
		current = match op {
			.grayscale { post_grayscale(current, width, height) }
			.invert { post_invert(current, width, height) }
			.brightness { post_brightness(current, width, height, c.brightness) }
			.contrast { post_contrast(current, width, height, c.contrast) }
			.saturate { post_saturate(current, width, height, c.saturate) }
			.vignette { post_vignette(current, width, height, c.vignette) }
			.blur { post_box_blur(current, width, height, c.blur_radius) }
			.tonemap { post_tonemap(current, width, height) }
			.bloom {
				post_bloom(current, width, height, c.bloom_threshold, c.bloom_strength, c.blur_radius)
			}
		}
	}
	return current
}
