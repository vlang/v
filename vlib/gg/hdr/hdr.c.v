module hdr

import sokol.gfx

@[heap]
pub struct FloatRenderTarget {
pub mut:
	width           int
	height          int
	format          gfx.PixelFormat
	color           gfx.Image
	depth           gfx.Image
	attachments     gfx.Attachments
	has_attachments bool
	pass            gfx.Pass
}

// is_hdr_format reports whether fmt can store values above 1.
pub fn is_hdr_format(fmt gfx.PixelFormat) bool {
	return match fmt {
		.rgba16f, .rgba32f, .rg11b10f, .rgb9e5 { true }
		else { false }
	}
}

// new_float_render_target creates an offscreen HDR render target. fmt must be
// one of .rgba16f, .rgba32f, .rg11b10f or .rgb9e5. It requires an initialized
// sokol.gfx context, normally set up by gg.new_context.
pub fn new_float_render_target(width int, height int, fmt gfx.PixelFormat) !&FloatRenderTarget {
	if width <= 0 || height <= 0 {
		return error('hdr: invalid float render target size ${width}x${height}')
	}
	if !gfx.is_valid() {
		return error('hdr: sokol.gfx is not initialized')
	}
	if !is_hdr_format(fmt) {
		return error('hdr: unsupported HDR pixel format')
	}
	mut rt := &FloatRenderTarget{
		width:  width
		height: height
		format: fmt
	}
	rt.color = gfx.make_image(&gfx.ImageDesc{
		render_target: true
		width:         width
		height:        height
		num_slices:    1
		num_mipmaps:   1
		pixel_format:  fmt
		sample_count:  1
	})
	rt.depth = gfx.make_image(&gfx.ImageDesc{
		render_target: true
		width:         width
		height:        height
		num_slices:    1
		num_mipmaps:   1
		pixel_format:  .depth
		sample_count:  1
	})
	mut desc := gfx.AttachmentsDesc{}
	desc.colors[0].image = rt.color
	desc.depth_stencil.image = rt.depth
	rt.attachments = gfx.make_attachments(&desc)
	rt.has_attachments = true
	if gfx.query_image_state(rt.color) != .valid || gfx.query_image_state(rt.depth) != .valid
		|| gfx.query_attachments_state(rt.attachments) != .valid {
		rt.free()
		return error('hdr: could not create the float render target')
	}
	rt.pass = gfx.Pass{ attachments: rt.attachments }
	rt.pass.action.colors[0].load_action = .clear
	rt.pass.action.colors[0].store_action = .store
	rt.pass.action.depth.load_action = .clear
	rt.pass.action.depth.store_action = .dontcare
	rt.pass.action.depth.clear_value = 1.0
	return rt
}

// begin starts a render pass into this target. clear may contain values above
// one; they are passed to the float color attachment unchanged.
pub fn (mut rt FloatRenderTarget) begin(clear ColorHDR) {
	rt.pass.action.colors[0].clear_value = gfx.Color{
		r: clear.r
		g: clear.g
		b: clear.b
		a: clear.a
	}
	gfx.begin_pass(&rt.pass)
}

// end finishes the render pass started by begin().
pub fn (mut rt FloatRenderTarget) end() {
	gfx.end_pass()
}

// texture returns the float color image so it can be sampled by a later pass.
pub fn (rt &FloatRenderTarget) texture() gfx.Image {
	return rt.color
}

// free destroys the GPU resources owned by the target. Safe to call twice.
pub fn (mut rt FloatRenderTarget) free() {
	if !gfx.is_valid() {
		return
	}
	if rt.has_attachments {
		gfx.destroy_attachments(rt.attachments)
		rt.has_attachments = false
	}
	if rt.color.id != 0 {
		gfx.destroy_image(rt.color)
		rt.color = gfx.Image{}
	}
	if rt.depth.id != 0 {
		gfx.destroy_image(rt.depth)
		rt.depth = gfx.Image{}
	}
}
