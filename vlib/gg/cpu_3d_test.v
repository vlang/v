module gg

fn test_pack_cpu_color() {
	c := pack_cpu_color(gg.Color{10, 20, 30, 40})
	assert c == u32(10) | (u32(20) << 8) | (u32(30) << 16) | (u32(40) << 24)
}

fn test_headless_cpu_context_renders_3d() {
	mut ctx := gg.new_context_cpu()
	cam := gg.Camera3D{ position: gg.Vec3{0, 0, 5}, target: gg.Vec3{0, 0, 0} }
	// Record a near green triangle and a far red triangle through render.
	ctx.r.begin_triangles()
	ctx.r.c3f(1, 0, 0)
	ctx.r.v3f(-1, -1, -10)
	ctx.r.v3f(1, -1, -10)
	ctx.r.v3f(0, 1, -10)
	ctx.r.c3f(0, 1, 0)
	ctx.r.v3f(-1, -1, -2)
	ctx.r.v3f(1, -1, -2)
	ctx.r.v3f(0, 1, -2)
	ctx.r.end()
	frame := ctx.render_cpu_3d(64, 64, gg.black, cam)
	assert frame.pixels.len == 64 * 64
	assert frame.depth.len == 64 * 64
	px := frame.pixels[32 * 64 + 32]
	assert f32(px & 0xff) < 80.0
	assert f32((px >> 8) & 0xff) > 170.0
}

fn test_headless_cpu_context_renders_cube() {
	mut ctx := gg.new_context_cpu()
	cam := gg.Camera3D{ position: gg.Vec3{0, 0, 5}, target: gg.Vec3{0, 0, 0} }
	ctx.draw_cube(gg.Vec3{}, gg.Vec3{2, 2, 2}, gg.red)
	frame := ctx.render_cpu_3d(64, 64, gg.black, cam)
	assert frame.pixels.len == 64 * 64
	assert frame.depth.len == 64 * 64
	px := frame.pixels[32 * 64 + 32]
	assert f32(px & 0xff) > 170.0
}

fn test_headless_cpu_context_renders_textured_mesh() {
	mut ctx := gg.new_context_cpu()
	cam := gg.Camera3D{ position: gg.Vec3{0, 0, 5}, target: gg.Vec3{0, 0, 0} }
	// A distinctive red texel: white, untextured pixels must not pass.
	mut pixels := []u8{len: 4 * 2 * 2, init: 0}
	for i in 0 .. 4 {
		pixels[i * 4] = 220
		pixels[i * 4 + 3] = 255
	}
	img := &gg.Image{
		data:        pixels.data
		width:       2
		height:      2
		nr_channels: 4
		ok:          true
	}
	mat := gg.Material3D{ texture: img }
	mesh := gg.gen_mesh_cube(gg.Vec3{1, 1, 1})
	ctx.draw_material_mesh(&mesh, gg.Vec3{}, gg.Vec3{}, 1.0, &mat, cam)
	frame := ctx.render_cpu_3d(64, 64, gg.black, cam)
	px := frame.pixels[32 * 64 + 32]
	assert (px & 0xff) > 190
	assert ((px >> 8) & 0xff) < 40
	assert ((px >> 16) & 0xff) < 40
}

fn test_headless_cpu_context_keeps_cpu_images_distinct() {
	mut ctx := gg.new_context_cpu()
	cam := gg.Camera3D{ position: gg.Vec3{0, 0, 5}, target: gg.Vec3{0, 0, 0} }
	mesh := gg.gen_mesh_cube(gg.Vec3{1, 1, 1})
	for channel in 0 .. 2 {
		mut pixels := []u8{len: 4, init: 0}
		pixels[channel] = 220
		pixels[3] = 255
		img := &gg.Image{
			data:        pixels.data
			width:       1
			height:      1
			nr_channels: 4
			ok:          true
		}
		mat := gg.Material3D{ texture: img }
		ctx.r.rewind()
		ctx.draw_material_mesh(&mesh, gg.Vec3{}, gg.Vec3{}, 1.0, &mat, cam)
		frame := ctx.render_cpu_3d(64, 64, gg.black, cam)
		px := frame.pixels[32 * 64 + 32]
		assert (px >> (u32(channel) * 8)) & 0xff > 190
		assert (px >> (u32(1 - channel) * 8)) & 0xff < 40
	}
}

fn test_headless_cpu_context_renders_2d() {
	mut ctx := gg.new_context_cpu()
	ctx.r.matrix_mode_projection()
	ctx.r.ortho(0, 40, 40, 0, -1, 1)
	ctx.r.begin_quads()
	ctx.r.c3f(1, 0, 0)
	ctx.r.v2f(5, 5)
	ctx.r.v2f(35, 5)
	ctx.r.v2f(35, 35)
	ctx.r.v2f(5, 35)
	ctx.r.end()
	frame := ctx.render_cpu(40, 40, gg.black)
	assert frame.pixels.len == 40 * 40
	assert frame.depth.len == 0
	px := frame.pixels[20 * 40 + 20]
	assert f32(px & 0xff) > 170.0
}
