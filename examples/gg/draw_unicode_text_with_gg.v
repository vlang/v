import os
import gg

println('Usage: `v run examples/gg/draw_unicode_text_with_gg.v [FONT_PATH] [TEXT_PATH]`')

font_path := os.args[1] or {
	os.resource_abs_path(os.join_path('..', 'assets', 'fonts', 'RobotoMono-Regular.ttf'))
}
dump(font_path)

text_path := os.args[2] or {
	os.resource_abs_path(os.join_path('..', 'ttf_font/draw_static_text.txt'))
}
dump(text_path)

text := os.read_file(text_path)!
dump(text)

gg.start(
	window_title: 'Draw unicode text with gg'
	bg_color:     gg.Color{155, 155, 128, 255}
	width:        1024
	height:       140
	font_path:    font_path
	frame_fn:     fn [text] (ctx &gg.Context) {
		ctx.begin()
		ctx.draw_text(30, 50, text,
			size:  24
			color: gg.Color{50, 50, 255, 255}
		)
		ctx.end()
	}
)
