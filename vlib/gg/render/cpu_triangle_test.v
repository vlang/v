module render

import sokol.gfx

fn cpu_triangle_frame(vertices []Vertex, reversed bool) Frame {
	ctx := new_context(max_vertices: 16, max_commands: 4)
	defer {
		ctx.destroy()
	}
	ctx.defaults()
	ctx.matrix_mode_projection()
	ctx.ortho(0, 8, 8, 0, -1, 1)
	ctx.load_pipeline(Pipeline{ id: 17, blend_mode: .none })
	ctx.begin_triangles()
	for j in 0 .. vertices.len {
		i := if reversed { vertices.len - 1 - j } else { j }
		v := vertices[i]
		ctx.v2f_t2f_c1i(v.x, v.y, v.u, v.v, v.rgba)
	}
	ctx.end()
	return ctx.render_cpu(8, 8, 0xff010203)
}

fn test_cpu_solid_triangle_color_and_coverage_for_both_windings() {
	vertices := [Vertex{ x: 0, y: 0, rgba: 0xff00ff00 }, Vertex{ x: 4, y: 0, rgba: 0xff00ff00 },
		Vertex{ x: 0, y: 4, rgba: 0xff00ff00 }]
	for reversed in [false, true] {
		frame := cpu_triangle_frame(vertices, reversed)
		for y in 0 .. 8 {
			for x in 0 .. 8 {
				// Pixel centers on the diagonal remain included.
				expected := if x + y <= 3 { u32(0xff00ff00) } else { u32(0xff010203) }
				assert frame.pixels[y * 8 + x] == expected
			}
		}
	}
}

fn test_cpu_triangle_interpolates_rgba_for_both_windings() {
	vertices := [Vertex{ x: 0.5, y: 0.5, rgba: 0xff0000ff },
		Vertex{ x: 4.5, y: 0.5, rgba: 0x0000ff00 }, Vertex{ x: 0.5, y: 4.5, rgba: 0x00ff0000 }]
	for reversed in [false, true] {
		frame := cpu_triangle_frame(vertices, reversed)
		assert frame.pixels[0] == 0xff0000ff
		assert frame.pixels[4] == 0x0000ff00
		assert frame.pixels[4 * 8] == 0x00ff0000
		// These pixel centers have weights (1/2, 1/4, 1/4) and (1/4, 1/2, 1/4).
		assert frame.pixels[1 * 8 + 1] == 0x7f3f3f7f
		assert frame.pixels[1 * 8 + 2] == 0x3f3f7f3f
	}
}

fn test_cpu_degenerate_triangles_leave_pixels_unchanged() {
	for vertices in [
		[Vertex{ x: 1, y: 1, rgba: 0xffffffff }, Vertex{ x: 2, y: 2, rgba: 0xffffffff },
			Vertex{ x: 3, y: 3, rgba: 0xffffffff }],
		[Vertex{ x: 2, y: 2, rgba: 0xffffffff }, Vertex{ x: 2, y: 2, rgba: 0xffffffff },
			Vertex{ x: 2, y: 2, rgba: 0xffffffff }],
	] {
		for reversed in [false, true] {
			frame := cpu_triangle_frame(vertices, reversed)
			for pixel in frame.pixels {
				assert pixel == 0xff010203
			}
		}
	}
}

fn cpu_textured_quad_frame(width int, height int, vertices []Vertex, pixels []u8,
	image_width int, image_height int, reversed bool) Frame {
	ctx := new_context(max_vertices: 32, max_commands: 8)
	defer {
		ctx.destroy()
	}
	ctx.defaults()
	ctx.matrix_mode_projection()
	ctx.ortho(0, f32(width), f32(height), 0, -1, 1)
	ctx.load_pipeline(Pipeline{ id: 17, blend_mode: .alpha })
	ctx.register_image(71, pixels.data, image_width, image_height, 4, 0)
	ctx.texture(gfx.Image{ id: 71 }, gfx.Sampler{})
	ctx.begin_quads()
	for j in 0 .. vertices.len {
		i := if reversed { vertices.len - 1 - j } else { j }
		v := vertices[i]
		ctx.v2f_t2f_c1i(v.x, v.y, v.u, v.v, 0xffffffff)
	}
	ctx.end()
	return ctx.render_cpu(width, height, 0xff010203)
}

fn test_cpu_textured_quad_preserves_opaque_texel_color() {
	vertices := [Vertex{ x: 6, y: 10, u: 0, v: 0 }, Vertex{ x: 12, y: 10, u: 1, v: 0 },
		Vertex{ x: 12, y: 14, u: 1, v: 1 }, Vertex{ x: 6, y: 14, u: 0, v: 1 }]
	pixels := [u8(200), 30, 40, 255]
	for reversed in [false, true] {
		frame := cpu_textured_quad_frame(16, 20, vertices, pixels, 1, 1, reversed)
		assert frame.pixels[11 * 16 + 7] == 0xff281ec8
		assert frame.pixels[11 * 16 + 5] == 0xff010203
		assert frame.pixels[14 * 16 + 7] == 0xff010203
	}
}

fn test_cpu_textured_quad_interpolates_uv_for_both_windings() {
	vertices := [Vertex{ x: 0, y: 0, u: 0, v: 0 }, Vertex{ x: 8, y: 0, u: 1, v: 0 },
		Vertex{ x: 8, y: 8, u: 1, v: 1 }, Vertex{ x: 0, y: 8, u: 0, v: 1 }]
	pixels := [u8(255), 0, 0, 255, 0, 255, 0, 255, 0, 0, 255, 255, 255, 255, 0, 255]
	colors := [u32(0xff0000ff), 0xff00ff00, 0xffff0000, 0xff00ffff]
	for reversed in [false, true] {
		frame := cpu_textured_quad_frame(8, 8, vertices, pixels, 2, 2, reversed)
		for y in 0 .. 8 {
			for x in 0 .. 8 {
				assert frame.pixels[y * 8 + x] == colors[(y / 4) * 2 + x / 4]
			}
		}
	}
}
