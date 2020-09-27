import time
import term
import term.input as ti

struct App {
mut:
	header_text     []string
	tick            int
	msg             string
	msg_hide_tick   int
	primary_color   Color = Color{200, 30, 0 }
	secondary_color Color = Color{20, 60, 200 }
	drawing         [][]string = [][]string{ len: 255, init: []string{ len: 255, init: ' ' } }
	w               int
	h               int
}

struct Color {
	r byte
	g byte
	b byte
}

fn (c Color) hex() string {
	return '#${c.r.hex()}${c.g.hex()}${c.b.hex()}'
}

// Material design
const (
	colors = [
		Color{239, 154, 154}
		Color{244, 143, 177}
		Color{206, 147, 216}
		Color{179, 157, 219}
		Color{159, 168, 218}
		Color{144, 202, 249}
		Color{129, 212, 250}
		Color{128, 222, 234}
		Color{128, 203, 196}
		Color{165, 214, 167}
		Color{197, 225, 165}
		Color{230, 238, 156}
		Color{255, 245, 157}
		Color{255, 224, 130}
		Color{255, 204, 128}
		Color{255, 171, 145}
		Color{188, 170, 164}
		Color{238, 238, 238}
		Color{176, 190, 197}
		Color{244, 67, 54}
		Color{233, 30, 99}
		Color{156, 39, 176}
		Color{103, 58, 183}
		Color{63, 81, 181}
		Color{33, 150, 243}
		Color{3, 169, 244}
		Color{0, 188, 212}
		Color{0, 150, 136}
		Color{76, 175, 80}
		Color{139, 195, 74}
		Color{205, 220, 57}
		Color{255, 235, 59}
		Color{255, 193, 7}
		Color{255, 152, 0}
		Color{255, 87, 34}
		Color{121, 85, 72}
		Color{158, 158, 158}
		Color{96, 125, 139}
		Color{198, 40, 40}
		Color{173, 20, 87}
		Color{106, 27, 154}
		Color{69, 39, 160}
		Color{40, 53, 147}
		Color{21, 101, 192}
		Color{2, 119, 189}
		Color{0, 131, 143}
		Color{0, 105, 92}
		Color{46, 125, 50}
		Color{85, 139, 47}
		Color{158, 157, 36}
		Color{249, 168, 37}
		Color{255, 143, 0}
		Color{239, 108, 0}
		Color{216, 67, 21}
		Color{78, 52, 46}
		Color{66, 66, 66}
		Color{55, 71, 79}
	]
)

fn main() {
	mut app := App{}
	ti.setup({})
	app.clear_term()
	term.hide_cursor()

	for {
		app.tick()
		time.sleep_ms(10)
	}
}

[inline]
fn (app &App) clear_term() {
	println('\x1b[1;1H\x1b[2J')
	term.erase_del_clear()
}

fn (mut app App) tick() {
	w, h := term.get_terminal_size()
	if w != app.w || h != app.h {
		app.clear_term()
	}
	app.w, app.h = w, h

	event, data := ti.read()
	match event {
		.empty{}
		.mouse_down {
			if app.h - data.y < 5 {
				app.footer_click(data)
			} else {
				app.paint(data)
			}
		} .mouse_drag {
			app.paint(data)
		} .text {
			if data.bytes.len == 1 && data.bytes[0] == `c` {
				app.drawing = [][]string{ len: 255, init: []string{ len: 255, init: ' ' } }
				app.clear_term()
			} else {
				app.show_msg(data.bytes.bytestr(), 1000)
			}
		}
		else {}
	}
	app.render()
	app.tick++
}

fn (mut app App) render() {
	app.draw_content()
	app.draw_header()
	app.draw_footer()

	if app.msg != '' {
		if app.msg_hide_tick > 0 && app.tick > app.msg_hide_tick {
			app.msg = ''
			app.msg_hide_tick = -1
		} else {
			term.set_cursor_position(x: 0, y: 0)
			print(term.bg_white(term.black(app.msg)))
		}
	}
}

fn (mut app App) paint(data ti.EventData) {
	if data.y < 4 || app.h - data.y < 4 { return }
	c := if data.button == .primary { app.primary_color } else { app.secondary_color }
	app.drawing[data.y - 4][data.x - 1] = term.bg_rgb(c.r, c.g, c.b, ' ')
}

fn (mut app App) draw_content() {
	h := app.h - 9
	term.set_cursor_position(x: 0, y: 4)
	for i in 0 .. h {
		line := app.drawing[i]
		print(line[0..app.w].join(''))
	}
}

fn (app &App) draw_header() {
	term.set_cursor_position(x: 4, y: 2)
	println('tick: $app.tick | terminal size: ($app.w, $app.h) | primary color: $app.primary_color.hex() | secondary color: $app.secondary_color.hex()')
	println('⎽'.repeat(app.w))
}

fn (app &App) draw_footer() {
	term.set_cursor_position(x: 0, y: app.h - 5)
	color_map := colors.map(term.bg_rgb(it.r, it.g, it.b, '  '))

	l := colors.len / 3
	mut l1 := color_map[0..l]
	mut l2 := color_map[l..l*2]
	mut l3 := color_map[l*2..l*3]

	if l * 3 + 18 > app.w {
		l1 = l1[0 .. (app.w - 2 - 18) / 3]
		l2 = l2[0 .. (app.w - 2 - 18) / 3]
		l3 = l3[0 .. (app.w - 2 - 18) / 3]
	}

	println('⎽'.repeat(app.w))
	println('')
	println('                  ' + l1.join(' ') + '       ╭────────╮')
	println('   Select color:  ' + l2.join(' ') + '       │  HELP  │')
	println('                  ' + l3.join(' ') + '       ╰────────╯')
}

fn (mut app App) footer_click(data ti.EventData) {
	if data.x > 18 && data.x < colors.len {
		app.show_msg((app.h - data.y).str(), 400)
	}
}

fn (mut app App) select_color(data ti.EventData) {
	color_idx := data.x - 18
	rel_pos := (color_idx % 3)
	app.show_msg(rel_pos.str(), 300)
	if color_idx < 0 || rel_pos !in [1, 2] { return }
	color := colors[color_idx / 4]
	if data.button == .primary {
		app.primary_color = color
	} else {
		app.secondary_color = color
	}
}

fn (mut app App) show_msg(text string, time int) {
	// TODO: Fix message display
	app.msg_hide_tick = if time > 0 { app.tick + time } else { time }
	app.msg = text
}
