import gg
import math

const pixel_count = 15000
const p = 3
const s = math.pi * 2 / f32(pixel_count)
const color = gg.Color{255, 255, 255, 255}
const background = gg.Color{20, 20, 64, 5}
const colors = [
	gg.Color{255, 255, 255, 255},
	gg.Color{255, 0, 255, 255},
	gg.Color{255, 255, 0, 255},
	gg.Color{0, 255, 255, 255},
	gg.Color{0, 0, 255, 255},
	gg.Color{0, 0, 0, 255},
]!

mut k := 497
mut scale := 200
gg.start(
	window_title: 'Spirograph'
	bg_color:     background
	width:        900
	height:       950
	sample_count: 2
	frame_fn:     fn [mut k, mut scale] (mut ctx gg.Context) {
		wsize := gg.window_size()
		ctx.begin()
		ctx.draw_rect_filled(0, 0, wsize.width, wsize.height, gg.Color{10, 1, 30, 60})
		t := f64(ctx.frame) / 300
		d := math.sin(t)
		if math.abs(d) < 0.0015 {
			k++
		}
		if ctx.pressed_keys[int(gg.KeyCode._0)] {
			ctx.frame = 475
		}
		if ctx.pressed_keys[int(gg.KeyCode.left)] {
			k--
		}
		if ctx.pressed_keys[int(gg.KeyCode.right)] {
			k++
		}
		if ctx.pressed_keys[int(gg.KeyCode.up)] {
			scale++
		}
		if ctx.pressed_keys[int(gg.KeyCode.down)] {
			scale--
		}
		d600 := math.sin(f64(ctx.frame) / 60)
		cd600 := math.cos(d600)
		mut pdj, mut pkj := [p]f64{}, [p]f64{}
		for j := 0; j < p; j++ {
			pdj[j], pkj[j] = math.pow(d, j), math.pow(k, j)
		}
		for i := 0; i < pixel_count; i++ {
			mut x, mut y := 0.0, 0.0
			for j := 0; j < p; j++ {
				a := pkj[j] * i * s
				x += pdj[j] * math.sin(a - t) * cd600
				y += pdj[j] * math.cos(a - t) * cd600
			}
			ctx.draw_rect_filled(f32(wsize.width / 2 + scale * x), f32(wsize.height / 2 + scale * y),
				1, 1, colors[i % colors.len])
		}
		ctx.draw_text(wsize.width / 2 - 170, wsize.height - 15, 'frame: ${ctx.frame:06}, k: ${k:4}, scale: ${scale:4}, arrows to change',
			color: colors[1]
		)
		ctx.end(how: .passthru)
	}
)
