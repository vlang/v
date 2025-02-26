// Try commenting or changing the drawing functions, that are at the bottom of the file,
// and that are inside functions marked with @[live] ...
import gg
import math

fn main() {
	mut state := init_state()
	gg.start(
		window_title: 'Tunnel'
		bg_color:     gg.Color{255, 255, 255, 255}
		width:        1024
		height:       768
		frame_fn:     unsafe { state.draw }
		event_fn:     unsafe { state.on_event }
	)
}

struct BigRect {
mut:
	id     int
	t      f32
	x      f32
	y      f32
	w      f32
	bwidth f32
	c      gg.Color
}

struct State {
mut:
	t       f32
	real_t  f32
	drt     f32 = 0.1
	tparams gg.DrawTextParams
	rects   []BigRect
}

const colors = []gg.Color{len: 1024, init: gg.Color{u8(100 +
	100 * math.sin(f32(index) / 50) * math.cos(f32(index) / 50)), u8(100 +
	100 * math.cos(f32(index) / 50)), 100 + u8(index % 50), 255}}

fn init_state() State {
	mut state := State{
		t:       0
		real_t:  120
		drt:     0.1
		tparams: gg.DrawTextParams{
			x:     40
			size:  20
			color: gg.Color{255, 255, 255, 255}
		}
		rects:   [
			BigRect{
				x:      350
				y:      250
				w:      30
				bwidth: 10
				c:      gg.Color{0, 0, 255, 255}
			},
		]
	}
	for i in 1 .. 50 {
		state.rects << BigRect{
			id: i
		}
	}
	return state
}

fn (mut state State) on_event(e &gg.Event, ctx voidptr) {
	if e.typ == .char {
		if e.char_code == `f` {
			gg.toggle_fullscreen()
			return
		}
		if e.char_code == `1` {
			state.real_t -= 100
		}
		if e.char_code == `2` {
			state.real_t += 100
		}
		if e.char_code == `3` {
			state.t -= 100
		}
		if e.char_code == `4` {
			state.t += 100
		}
		if e.char_code == `5` {
			state.rects[0].w += 10
		}
		if e.char_code == `6` {
			state.rects[0].w -= 10
		}
		if e.char_code == `r` {
			state.real_t = 120
			state.t = 0
			state.rects[0].w = 30
		}
		if e.char_code == ` ` {
			state.drt = if state.drt > 0.0001 { f32(0.0) } else { 0.1 }
		}
	}
}

fn (mut state State) draw(ctx &gg.Context) {
	update(mut state)
	ctx.begin()
	mut r0 := &state.rects[0]
	draw_big_rect(ctx, r0)
	for i in 1 .. state.rects.len {
		draw_big_rect(ctx, state.rects[i])
	}
	ctx.draw_text2(gg.DrawTextParams{
		...state.tparams
		y:    0
		text: 'rt[1,2]: ${state.real_t:08.3f}, t[3,4]: ${state.t:08.3f}, r0.x: ${r0.x:07.3f}, r0.y: ${r0.y:07.3f}, r0.w[5,6]: ${r0.w:4.1f}, [f]-toggle fs, [r]-reset, [ ]-pause'
	})
	draw_center_point(ctx, r0, state.t)
	ctx.show_fps()
	ctx.end()
}

@[live]
fn update(mut state State) {
	if state.drt <= 0.0001 {
		return
	}
	state.real_t += state.drt
	state.t += 0.5 * math.sinf(state.real_t / 1000)
	mut r0 := &state.rects[0]
	r0.t = state.t
	r0.x = 550 + 450 * math.sinf(state.t / 10) * math.sinf(state.t / 50)
	r0.y = 350 + 350 * math.cosf(state.t / 20) * math.cosf(-state.t / 50)
	r0.x += 5 * math.sinf(state.t)
	r0.y += 5 * math.cosf(state.t)
	r0.c = colors[int((50000 + state.t) / 200 * (1 + math.sin(state.t / 5))) % colors.len]
	for i := state.rects.len - 1; i > 0; i-- {
		state.rects[i] = state.rects[i - 1]
		state.rects[i].w *= 1.11 + f32(i) / 1000
		state.rects[i].bwidth *= 1.09
	}
}

@[live]
fn draw_center_point(ctx &gg.Context, br BigRect, t f32) {
	b := u8(128 + 128 * math.sin(t / 12))
	c := gg.Color{255 - b, b, b, 55}
	ctx.draw_circle_filled(br.x, br.y, 8, c)
}

@[live]
fn draw_big_rect(ctx &gg.Context, br BigRect) {
	radius := 20
	x := br.x
	y := br.y
	w := br.w
	c := br.c
	bwidth := br.bwidth
	base := gg.DrawRectParams{
		radius:     radius
		is_rounded: true
		color:      c
	}
	rleft := gg.DrawRectParams{
		...base
		x: x - (w / 2)
		y: y - (w / 2)
		w: bwidth
		h: w
	}
	rright := gg.DrawRectParams{
		...base
		x: x + (w / 2) - bwidth
		y: y - w + (w / 2)
		w: bwidth
		h: w
	}
	rtop := gg.DrawRectParams{
		...base
		x: x - (w / 2)
		y: y - (w / 2)
		w: w
		h: bwidth
	}
	rbottom := gg.DrawRectParams{
		...base
		x: x + (w / 2) - w
		y: y + (w / 2) - bwidth
		w: w
		h: bwidth
	}
	border := gg.DrawRectParams{
		x:     x - (w / 2) + bwidth + 1
		y:     y - (w / 2) + bwidth + 2
		w:     w - 2 * bwidth - 2
		h:     w - 2 * bwidth - 1
		color: gg.Color{255, 255, 255, 155}
		style: .stroke
	}
	ctx.draw_rect(rtop)
	ctx.draw_rect(rleft)
	ctx.draw_rect(rright)
	ctx.draw_rect(border)
	ctx.draw_rect(rbottom)
}
