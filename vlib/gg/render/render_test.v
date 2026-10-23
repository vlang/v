module render

fn test_vertex_size() {
	assert sizeof(Vertex) == 28
}

fn test_mat4_identity() {
	m := mat4_identity()
	for i in 0 .. 4 {
		for j in 0 .. 4 {
			if i == j {
				assert m.v[i][j] == 1.0
			} else {
				assert m.v[i][j] == 0.0
			}
		}
	}
}

fn test_cpu_backend_frame_size() {
	mut ctx := new_context(Desc{ max_vertices: 1024, max_commands: 64 })
	ctx.defaults()
	frame := ctx.render_cpu(64, 48, 0xffffffff)
	assert frame.width == 64
	assert frame.height == 48
	assert frame.pixels.len == 64 * 48
}

fn test_cpu_backend_renders_nonempty() {
	mut ctx := new_context(Desc{ max_vertices: 4096, max_commands: 256 })
	ctx.defaults()
	ctx.matrix_mode_projection()
	ctx.ortho(0, 120, 120, 0, -1, 1)
	ctx.begin_quads()
	ctx.c4b(255, 80, 80, 255)
	ctx.v2f(10, 10)
	ctx.c4b(80, 255, 80, 255)
	ctx.v2f(100, 10)
	ctx.c4b(80, 80, 255, 255)
	ctx.v2f(100, 100)
	ctx.c4b(255, 255, 80, 255)
	ctx.v2f(10, 100)
	ctx.end()
	frame := ctx.render_cpu(120, 120, 0x101018ff)
	mut nonzero := 0
	for p in frame.pixels {
		if p != 0x101018ff {
			nonzero++
		}
	}
	assert nonzero > 100
}

pub fn frame_diff_percent(a Frame, b Frame, tol u8) f64 {
	if a.width != b.width || a.height != b.height {
		return 100.0
	}
	mut diff := 0
	for i in 0 .. a.pixels.len {
		pa := a.pixels[i]
		pb := b.pixels[i]
		mut dr := int(pa & 0xff) - int(pb & 0xff)
		if dr < 0 { dr = -dr }
		mut dg := int((pa >> 8) & 0xff) - int((pb >> 8) & 0xff)
		if dg < 0 { dg = -dg }
		mut db := int((pa >> 16) & 0xff) - int((pb >> 16) & 0xff)
		if db < 0 { db = -db }
		if dr > tol || dg > tol || db > tol {
			diff++
		}
	}
	return f64(diff) * 100.0 / f64(a.pixels.len)
}

fn test_frame_diff_identical() {
	mut ctx := new_context(Desc{ max_vertices: 64, max_commands: 8 })
	f := ctx.render_cpu(8, 8, 0x12345678)
	assert frame_diff_percent(f, f, 0) == 0.0
}
