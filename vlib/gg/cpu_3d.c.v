// Copyright (c) 2019-2026 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license that can be found in the LICENSE file.
module gg

import gg.render

// new_context_cpu creates a minimal Context for headless CPU rendering.
// No window, Sokol context or GPU is required. Use render_cpu() for 2D and
// render_cpu_3d() for depth-tested 3D. The returned Context is not a render
// owner and must not be passed to run().
pub fn new_context_cpu() &Context {
	mut ctx := &Context{
		config: Config{ depth: true }
		r:      render.new_context(render.Desc{
			max_vertices: 1 << 17
			max_commands: 1 << 15
			depth_test:   true
		})
	}
	ctx.r.defaults()
	return ctx
}

// render_cpu draws the current frame's recorded commands into a CPU pixel
// buffer, using painter order. It works without a window or GPU.
pub fn (ctx &Context) render_cpu(width int, height int, clear Color) render.Frame {
	return ctx.r.render_cpu(width, height, pack_cpu_color(clear))
}

// render_cpu_3d draws the current frame's recorded commands with a depth
// buffer using cam. The matrices are set directly from width/height/aspect, so
// this never touches the window system. It works without a window or GPU.
pub fn (ctx &Context) render_cpu_3d(width int, height int, clear Color, cam Camera3D) render.Frame {
	aspect := if height > 0 { f32(width) / f32(height) } else { 1.0 }
	mvp := render.mat4_mul(cam.projection_matrix(aspect), cam.view_matrix())
	return ctx.r.render_cpu_depth_with_mvp(width, height, pack_cpu_color(clear), 1.0, mvp)
}

fn pack_cpu_color(c Color) u32 {
	return u32(c.r) | (u32(c.g) << 8) | (u32(c.b) << 16) | (u32(c.a) << 24)
}
