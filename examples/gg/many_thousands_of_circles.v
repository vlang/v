module main

import gg
import gx
import rand

const max_circles_per_pass = 1000

// prepare some random colors ahead of time
const colors = []gx.Color{len: max_circles_per_pass, init: gx.Color{
	r: u8(index * 0 + rand.u8())
	g: u8(index * 0 + rand.u8())
	b: u8(index * 0 + rand.u8())
}}

fn frame(mut ctx gg.Context) {
	// First pass, just clears the background:
	ctx.begin()
	ctx.end()

	// We want to draw thousands of circles, but sokol has a limit for how
	// many primitives can be in a single pass, and if you reach that limit
	// you will not see *anything at all* for that pass.
	// For the circles below, that limit is ~2520 circles per pass.
	// To overcome this, we will use several passes instead, where each one
	// will draw just 1000 circles.
	// In other words, in total we will have 4 * 1000 = 4000 circles, drawn with
	// 4 passes.
	for i := 0; i < 4 * max_circles_per_pass; i += max_circles_per_pass {
		ctx.begin()
		for c in 0 .. max_circles_per_pass {
			rx := rand.int_in_range(0, ctx.window.width) or { 0 }
			ry := rand.int_in_range(0, ctx.window.height) or { 0 }
			ctx.draw_circle_filled(rx, ry, 10, colors[c])
		}
		ctx.end(how: .passthru)
	}

	// The last pass, is for the fps overlay, that should be *always on top of everything*.
	// Drawing it in a separate pass, guarantees, that it *will* be drawn, even if the drawing
	// of all the other passes fail. Try increasing max_circles_per_pass to 3000 for example.
	ctx.begin()
	ctx.show_fps()
	ctx.end(how: .passthru)
}

fn main() {
	mut ctx := gg.new_context(
		window_title: 'Many Thousands of Circles'
		bg_color:     gx.black
		width:        600
		height:       400
		frame_fn:     frame
	)
	ctx.run()
}
