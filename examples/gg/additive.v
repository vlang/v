module main

import os
import gg
import gx
import math

@[heap]
pub struct Window {
pub mut:
	ctx   &gg.Context = unsafe { 0 }
	image gg.Image
}

pub fn (mut window Window) init() {
	logo_path := os.join_path(@VEXEROOT, 'examples/assets/logo.png')
	window.image = window.ctx.create_image(logo_path) or { panic(err) }
}

pub fn (mut window Window) draw(_ voidptr) {
	window.ctx.begin()

	myconfig := gg.DrawImageConfig{
		img:      &window.image
		img_id:   window.image.id
		img_rect: gg.Rect{
			x:      400 - window.image.width / 2
			y:      300 - window.image.height / 2
			width:  window.image.width
			height: window.image.height
		}
		rotation: f32(window.ctx.frame)
		// effect: .alpha <-- this can be omitted completely as it is alpha by default.
	}
	window.ctx.draw_image_with_config(gg.DrawImageConfig{ ...myconfig, flip_x: true })

	// Red
	window.ctx.draw_image_with_config(gg.DrawImageConfig{
		...myconfig
		img_rect: gg.Rect{
			...myconfig.img_rect
			x: myconfig.img_rect.x + f32(math.sin(f32(window.ctx.frame) / 10.0) * 60)
			y: myconfig.img_rect.y + f32(math.cos(f32(window.ctx.frame) / 10.0) * 60)
		}
		color:    gx.Color{255, 0, 0, 255}
		effect:   .add
	})

	// Green
	window.ctx.draw_image_with_config(gg.DrawImageConfig{
		...myconfig
		img_rect: gg.Rect{
			...myconfig.img_rect
			x: myconfig.img_rect.x + f32(math.sin(f32(window.ctx.frame) / 10.0) * 80)
			y: myconfig.img_rect.y + f32(math.cos(f32(window.ctx.frame) / 10.0) * 80)
		}
		color:    gx.Color{0, 255, 0, 255}
		effect:   .add
	})

	// Blue
	window.ctx.draw_image_with_config(gg.DrawImageConfig{
		...myconfig
		img_rect: gg.Rect{
			...myconfig.img_rect
			x: myconfig.img_rect.x + f32(math.sin(f32(window.ctx.frame) / 10.0) * 100)
			y: myconfig.img_rect.y + f32(math.cos(f32(window.ctx.frame) / 10.0) * 100)
		}
		color:    gx.Color{0, 0, 255, 255}
		effect:   .add
	})

	// More examples
	window.ctx.draw_image_with_config(gg.DrawImageConfig{
		...myconfig
		img_rect: gg.Rect{
			...myconfig.img_rect
			x: 50
			y: 0
		}
		color:    gx.Color{255, 0, 0, 255}
		effect:   .add
	})

	window.ctx.draw_image_with_config(gg.DrawImageConfig{
		...myconfig
		img_rect: gg.Rect{
			...myconfig.img_rect
			x: 50
			y: 50
		}
		color:    gx.Color{0, 255, 0, 255}
		effect:   .add
	})

	window.ctx.draw_image_with_config(gg.DrawImageConfig{
		...myconfig
		img_rect: gg.Rect{
			...myconfig.img_rect
			x: 50
			y: 100
		}
		color:    gx.Color{0, 0, 255, 255}
		effect:   .add
	})

	window.ctx.end()
}

fn main() {
	mut window := &Window{}

	window.ctx = gg.new_context(
		window_title: 'Additive colors & image rotation'
		width:        800
		height:       600
		user_data:    window
		bg_color:     gx.gray
		// FNs
		init_fn:  window.init
		frame_fn: window.draw
	)

	window.ctx.run()
}
