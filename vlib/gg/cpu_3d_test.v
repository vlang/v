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
