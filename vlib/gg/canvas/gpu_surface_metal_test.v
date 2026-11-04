// vtest build: macos
module canvas

const test_width = 32
const test_height = 24

fn pixel(px []u8, x int, y int) []u8 {
	i := (y * test_width + x) * 4
	return px[i..i + 4].clone()
}

fn render(mut ctx GpuContext, sample_count int, background []u8) ![]u8 {
	mut s := ctx.new_surface(width: test_width, height: test_height, sample_count: sample_count)!
	defer {
		s.free()
	}
	if background.len > 0 {
		s.write_pixels(background.data, test_width * 4)!
	} else {
		s.clear(Color4f{})
	}
	// A pixel-aligned opaque rect renders without coverage or blending.
	s.draw_rect(Rect.ltrb(4, 4, 12, 10), Paint{
		color: Color4f.from_rgba8(10, 20, 30, 255)
	})!
	s.draw_rrect(RRect.rect_radii(Rect.ltrb(14.5, 3, 30.5, 21), [Point{4, 4}, Point{4, 4}, Point{4, 4},
		Point{4, 4}]!), Paint{
		color:      Color4f.from_rgba8(200, 100, 0, 255)
		anti_alias: true
	})!
	mut px := []u8{len: test_width * test_height * 4}
	s.read_pixels(px.data, test_width * 4)!
	return px
}

fn test_gpu_surface_draws_and_reads_back() {
	mut ctx := new_gpu_context() or {
		eprintln('skipping: ${err}')
		return
	}
	defer {
		ctx.free()
	}
	for samples in [1, 4] {
		px := render(mut ctx, samples, []u8{})!
		// Premultiplied BGRA.
		assert pixel(px, 5, 5) == [u8(30), 20, 10, 255], 'samples ${samples}'
		assert pixel(px, 0, 0) == [u8(0), 0, 0, 0]
		assert pixel(px, 22, 12) == [u8(0), 100, 200, 255]
		// The left edge at x = 14.5 half covers column 14.
		edge := pixel(px, 14, 12)
		assert edge[3] > 96 && edge[3] < 160, 'samples ${samples}: ${edge}'
		assert edge[2] <= edge[3]
	}
	// Uploaded pixels show through where nothing is drawn.
	mut background := []u8{len: test_width * test_height * 4}
	for i in 0 .. background.len / 4 {
		background[i * 4] = 40
		background[i * 4 + 1] = 50
		background[i * 4 + 2] = 60
		background[i * 4 + 3] = 255
	}
	px := render(mut ctx, 1, background)!
	assert pixel(px, 0, 0) == [u8(40), 50, 60, 255]
	assert pixel(px, 5, 5) == [u8(30), 20, 10, 255]
	// Multisampled surfaces do not load existing pixels.
	if _ := render(mut ctx, 4, background) {
		assert false, 'expected an error'
	}
}
