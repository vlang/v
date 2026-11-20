module main

// raylib [text] example - inline styling, ported to gg.
// PORT NOTE: gg has no per-glyph background rendering; the [bRRGGBBAA] tag draws
// a translucent rectangle behind the current run and [cRRGGBBAA] sets its color.
import gg
import rand

const (
	raywhite = gg.Color{245, 245, 245, 255}
	black_c  = gg.Color{0, 0, 0, 255}
	green_c  = gg.Color{0, 158, 47, 255}
	red_c    = gg.Color{230, 41, 55, 255}
)

fn hex_byte(s string) u8 {
	mut v := 0
	for i in 0 .. 2 {
		c := s[i]
		d := if c >= `0` && c <= `9` {
			int(c - `0`)
		} else if c >= `a` && c <= `f` {
			10 + int(c - `a`)
		} else {
			10 + int(c - `A`)
		}
		v = v * 16 + d
	}
	return u8(v)
}

fn parse_hex(s string) gg.Color {
	if s.len < 8 { return gg.Color{255, 255, 255, 255} }
	return gg.Color{
		r:      hex_byte(s[0..2])
		g:      hex_byte(s[2..4])
		b:      hex_byte(s[4..6])
		a:      hex_byte(s[6..8])
	}
}

fn draw_styled(ctx &gg.Context, x int, y int, text string, size int, base gg.Color) {
	mut front := base
	mut back := gg.Color{0, 0, 0, 0}
	mut px := x
	mut i := 0
	mut run := ''
	for i < text.len {
		if text[i] == `[` && i + 2 < text.len {
			end := text.index_from(']', i) or { -1 }
			if end > i {
				tag := text[i + 1 .. end]
				if tag == 'r' {
					flush(ctx, px, y, run, size, front, back)
					px += ctx.text_width(run) + 4 * run.len
					run = ''
					front = base
					back = gg.Color{0, 0, 0, 0}
				} else if tag.len >= 9 && (tag[0] == `c` || tag[0] == `b`) {
					flush(ctx, px, y, run, size, front, back)
					px += ctx.text_width(run) + 4 * run.len
					run = ''
					c := parse_hex(tag[1..])
					if tag[0] == `c` { front = c } else { back = c }
				}
				i = end + 1
				continue
			}
		}
		run += text[i..i + 1]
		i++
	}
	flush(ctx, px, y, run, size, front, back)
}

fn flush(ctx &gg.Context, x int, y int, run string, size int, front gg.Color, back gg.Color) {
	if run.len == 0 { return }
	if back.a > 0 {
		ctx.draw_rect_filled(x - 2, y - 2, ctx.text_width(run) + 4, size + 4, back)
	}
	ctx.draw_text(x, y, run, size: size, color: front)
}

struct App {
mut:
	ctx     &gg.Context = unsafe { nil }
	counter int
	color   gg.Color = gg.Color{230, 41, 55, 255}
}

fn on_frame(data voidptr) {
	mut app := unsafe { &App(data) }
	ctx := app.ctx
	ctx.begin()
	draw_styled(ctx, 100, 80, 'This changes the [cFF0000FF]foreground color[r] of provided text!!!', 20, black_c)
	draw_styled(ctx, 100, 120, 'This changes the [bFF00FFFF]background color[r] of provided text!!!', 20, black_c)
	draw_styled(ctx, 100, 160, 'This changes the [c00ff00ff][bff0000ff]foreground and background colors[r]!!!', 20, black_c)
	draw_styled(ctx, 100, 200, 'This changes the [c00ff00ff]alpha[r] relative [cffffffff][b000000ff]from source[r] [cff000088]color[r]!!!', 20, gg.Color{0, 0, 0, 100})
	msg := "Let's be [c${app.color.r:02x}${app.color.g:02x}${app.color.b:02x}FF]CREATIVE[r] !!!"
	draw_styled(ctx, 100, 240, msg, 40, black_c)
	ctx.draw_rect_empty(100, 240, 400, 48, green_c)
	ctx.end()
}

fn on_update(dt f32, data voidptr) {
	mut app := unsafe { &App(data) }
	_ = dt
	app.counter++
	if app.counter % 20 == 0 {
		app.color = gg.Color{u8(rand.intn(256) or { 0 }), u8(rand.intn(256) or { 0 }), u8(rand.intn(256) or { 0 }), 255}
	}
}

fn main() {
	mut app := &App{}
	app.ctx = gg.new_context(
		width:        800
		height:       450
		window_title: 'raylib [text] example - inline styling'
		user_data:    app
		frame_fn:     on_frame
		update_fn:    on_update
		bg_color:     raywhite
	)
	app.ctx.run()
}
