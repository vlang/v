module main

import gg
import freetype
import gx
import glfw

const (
	win_width = 600
	win_height = 300
	bg_color = gx.white
)


struct Context {
mut:
	gg &gg.GG
	ft &freetype.FreeType
}

fn main() {
	glfw.init_glfw()
	mut ctx := &Context{
		gg: gg.new_context(gg.Cfg {
			width: win_width
			height: win_height
			use_ortho: true // This is needed for 2D drawing
			create_window: true
			window_title: 'Empty window'
			window_user_ptr: ctx
		})
	}
	ctx.gg.window.set_user_ptr(ctx) // TODO remove this when `window_user_ptr:` works
	gg.clear(bg_color)
	// Try to load font
	ctx.ft = freetype.new_context(gg.Cfg{
		width: win_width
		height: win_height
		use_ortho: true
		font_size: 18
		scale: 2
	})
	for {
		gg.clear(bg_color)
		ctx.draw()
		ctx.gg.render()
		if ctx.gg.window.should_close() {
			ctx.gg.window.destroy()
			return
		}
	}
}

fn (ctx Context) draw() {
}

