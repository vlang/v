module main

import gg
import gx
import automaton

const (
	screen_width = 800
	screen_height = 600
	filled_color = gx.blue
)

fn new_graphics() &gg.Context {
	glfw.init_glfw()
	return gg.new_context(gg.Cfg{
		width: screen_width
		height: screen_height
		use_ortho: true
		create_window: true
		resizable: false
		window_title: 'v life (with gg, glfw, gx)'
		window_user_ptr: 0
	})
}

const (
	graphics = new_graphics()
)

[live]
fn print_automaton(a &automaton.Automaton){
	gg.clear(gx.white)
	square_size := 18
	for y := 1; y<a.field.maxy; y++ {
		for x := 1; x<a.field.maxx; x++ {
			cell := a.field.get(x,y)
			if cell == 1 {
				graphics.draw_rect(f32(square_size*x), f32(square_size*y), f32(square_size),
					f32(square_size), filled_color)
			}
		}
	}
}

fn main() {
	mut a := automaton.gun()
	for {
		if graphics.window.should_close() { graphics.window.destroy() break }
		gg.post_empty_event() // needed so the animation does not stop
		///////////////////////////////////////////////
		a.update()
		print_automaton(a)
		graphics.render()
	}
}
