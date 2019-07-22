module main

import gx
import gl 
import gg
import time 
import glfw 
import math 
	
const (
	Size  = 1000
	Scale  = 50.0 
)

struct Context {
	gg *gg.GG
}

fn main() {
	glfw.init()
	ctx:= &Context{ 
		gg: gg.new_context(gg.Cfg {
			width: Size 
			height: Size 
			use_ortho: true 
			create_window: true 
			window_title: 'Graph builder' 
			window_user_ptr: ctx 
			always_on_top: true
		})
	} 
	for { 
		gg.clear(gx.White) 
		ctx.draw()
		ctx.gg.render() 
	}
}

[live] 
fn (ctx &Context) draw() {
	ctx.gg.draw_line(0, Size / 2, Size, Size / 2) // x axis 
	ctx.gg.draw_line(Size / 2, 0, Size / 2, Size) // y axis 
	center := f64(Size / 2) 
	for x := -10.0; x <= 10.0; x += 0.002 {
		y := (x - 1) * (x - 1) - 2 
		ctx.gg.draw_rect(center + x * Scale, 
			center - y * Scale, 1, 1, gx.Black) 
	}
}
