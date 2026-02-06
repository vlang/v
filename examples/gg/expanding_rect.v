import gg

mut r := &gg.Rect{20, 20, 60, 20}
gg.start(
	update_fn: fn [mut r] (dt f32, c &gg.Context) {
		r.width++
		r.height++
	}
	frame_fn:  fn [mut r] (c &gg.Context) {
		c.begin()
		c.draw_rect_filled(r.x, r.y, r.width, r.height, gg.frgb(0.0, 0.4, 0.4))
		c.end()
	}
)
