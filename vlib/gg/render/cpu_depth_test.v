// vtest build: !musl? && !self_ubuntu_musl_ci?
module render

fn test_cpu_depth_frame_has_depth_buffer() {
	ctx := new_context(Desc{ max_vertices: 256, max_commands: 64 })
	ctx.defaults()
	frame := ctx.render_cpu_depth(8, 8, 0xffffffff, 1.0)
	assert frame.depth.len == 64
	for z in frame.depth {
		assert z == 1.0
	}
	assert frame.pixels.len == 64
}

fn test_cpu_painter_frame_has_no_depth_buffer() {
	ctx := new_context(Desc{ max_vertices: 256, max_commands: 64 })
	ctx.defaults()
	frame := ctx.render_cpu(8, 8, 0xffffffff)
	assert frame.depth.len == 0
}

fn test_cpu_depth_keeps_nearest_triangle() {
	mut ctx := new_context(Desc{ max_vertices: 64, max_commands: 8 })
	ctx.defaults()
	ctx.matrix_mode_projection()
	ctx.perspective(1.5707963267948966, 1.0, 0.1, 100)
	ctx.matrix_mode_modelview()
	ctx.load_identity()
	ctx.begin_triangles()
	// Far red triangle, z = -10.
	ctx.c3f(1, 0, 0)
	ctx.v3f(-1, -1, -10)
	ctx.v3f(1, -1, -10)
	ctx.v3f(0, 1, -10)
	// Near green triangle in front, z = -2.
	ctx.c3f(0, 1, 0)
	ctx.v3f(-1, -1, -2)
	ctx.v3f(1, -1, -2)
	ctx.v3f(0, 1, -2)
	ctx.end()
	frame := ctx.render_cpu_depth(64, 64, 0xff000000, 1.0)
	assert frame.depth[32 * 64 + 32] > 0.5 && frame.depth[32 * 64 + 32] < 1.0
	px := frame.pixels[32 * 64 + 32]
	// Green component must clearly dominate: the near triangle won.
	assert f32(px & 0xff) < 80.0
	assert f32((px >> 8) & 0xff) > 170.0
}

fn test_cpu_depth_rejects_far_geometry() {
	mut ctx := new_context(Desc{ max_vertices: 64, max_commands: 8 })
	ctx.defaults()
	ctx.matrix_mode_projection()
	ctx.perspective(1.5707963267948966, 1.0, 0.1, 10)
	ctx.matrix_mode_modelview()
	ctx.load_identity()
	ctx.begin_triangles()
	ctx.c3f(1, 1, 1)
	ctx.v3f(-1, -1, -20) // behind the far plane
	ctx.v3f(1, -1, -20)
	ctx.v3f(0, 1, -20)
	ctx.end()
	frame := ctx.render_cpu_depth(32, 32, 0xff000000, 1.0)
	assert frame.pixels[16 * 32 + 16] == 0xff000000
}
