import gg

gg.start(
	window_title: 'Hello'
	bg_color:     gg.Color{240, 240, 128, 255}
	width:        320
	height:       240
	frame_fn:     fn (ctx &gg.Context) {
		ctx.begin()
		ctx.draw_text(40, 100, 'GG frame: ${ctx.frame:06}',
			size:  30
			color: gg.Color{50, 50, 255, 255}
		)
		ctx.show_fps()
		ctx.end()
	}
)
