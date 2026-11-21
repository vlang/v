module gg

import sokol.gfx

@[heap]
pub struct RenderTexture2D {
pub:
	// width is the width of the render target in pixels.
	width int
	// height is the height of the render target in pixels.
	height int
mut:
	// color is the sampled RGBA color attachment of the target.
	color gfx.Image
	// depth is the depth attachment used while drawing into the target.
	depth gfx.Image
	// attachments binds color and depth to a render pass.
	attachments gfx.Attachments
	// has_attachments records ownership: the gfx.Attachments handle has a
	// module-private id, so it cannot be checked from here.
	has_attachments bool
	// pass is retained in heap storage: sg_begin_pass copies its descriptor
	// synchronously, so a fresh stack local is not required, but keeping the
	// descriptor here lets begin() update the clear color in place.
	pass gfx.Pass
}

// new_render_texture2d creates an offscreen render target of `width` by
// `height` pixels. It requires an initialized sokol.gfx context. The color
// attachment is a sampleable RGBA8 image, so texture() can be bound to a
// shader, and a matching depth attachment is created for 3D/scissored draws.
// The target is cleared on the first begin() call.
pub fn new_render_texture2d(width int, height int) !&RenderTexture2D {
	if width <= 0 || height <= 0 {
		return error('gg.E0030: invalid render texture size ${width}x${height}')
	}
	if !gfx.is_valid() {
		return error('gg.E0031: sokol.gfx is not initialized; call gfx.setup() first')
	}
	mut rt := &RenderTexture2D{
		width:  width
		height: height
	}
	rt.color = gfx.make_image(&gfx.ImageDesc{
		render_target: true
		width:         width
		height:        height
		num_slices:    1
		num_mipmaps:   1
		pixel_format:  .rgba8
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
		return error('gg: could not create the render texture attachments')
	}
	// A default action, overwritten with the requested clear color by begin().
	rt.pass = gfx.Pass{ attachments: rt.attachments }
	rt.pass.action.colors[0].load_action = .clear
	rt.pass.action.colors[0].store_action = .store
	rt.pass.action.depth.load_action = .clear
	rt.pass.action.depth.store_action = .dontcare
	rt.pass.action.depth.clear_value = 1.0
	return rt
}

// begin starts a render pass that draws into this target. The color
// attachment is cleared to `clear` and the depth attachment to the far plane.
// Issue gg drawing calls between begin() and end().
pub fn (mut rt RenderTexture2D) begin(clear Color) {
	rt.pass.action.colors[0].clear_value = gfx.Color{
		r: f32(clear.r) / 255.0
		g: f32(clear.g) / 255.0
		b: f32(clear.b) / 255.0
		a: f32(clear.a) / 255.0
	}
	gfx.begin_pass(&rt.pass)
}

// end finishes the render pass started by begin(), committing the pixels
// into the color attachment.
pub fn (mut rt RenderTexture2D) end() {
	gfx.end_pass()
}

// texture returns the color image of the target, which can be sampled in a
// later pass (for example with ctx.draw_image after wrapping it in an Image).
pub fn (rt &RenderTexture2D) texture() gfx.Image {
	return rt.color
}

// free destroys the GPU resources owned by the target. It is safe to call
// more than once.
pub fn (mut rt RenderTexture2D) free() {
	if !gfx.is_valid() {
		return
	}
	if rt.has_attachments {
		gfx.destroy_attachments(rt.attachments)
		rt.has_attachments = false
	}
	if rt.depth.id != 0 {
		gfx.destroy_image(rt.depth)
		rt.depth = gfx.Image{}
	}
	if rt.color.id != 0 {
		gfx.destroy_image(rt.color)
		rt.color = gfx.Image{}
	}
}
