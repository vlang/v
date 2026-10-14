import image
import image.color
import image.draw

fn test_resize_nearest_neighbor_with_nonzero_bounds() {
	mut src := image.new_nrgba(image.rect(3, 4, 5, 5))
	src.set_nrgba(3, 4, color.NRGBA{255, 0, 0, 255})
	src.set_nrgba(4, 4, color.NRGBA{0, 0, 255, 255})
	dst := draw.resize(src, 4, 2, interpolation: .nearest_neighbor)!
	for y in 0 .. 2 {
		assert dst.nrgba_at(0, y) == src.nrgba_at(3, 4)
		assert dst.nrgba_at(1, y) == src.nrgba_at(3, 4)
		assert dst.nrgba_at(2, y) == src.nrgba_at(4, 4)
		assert dst.nrgba_at(3, y) == src.nrgba_at(4, 4)
	}
}

fn test_resize_bilinear_interpolates_premultiplied_alpha() {
	mut src := image.new_nrgba(image.rect(0, 0, 2, 1))
	src.set_nrgba(0, 0, color.NRGBA{255, 0, 0, 255})
	src.set_nrgba(1, 0, color.NRGBA{0, 0, 255, 0})
	dst := draw.resize(src, 1, 1)!
	c := dst.nrgba_at(0, 0)
	assert c.r == 255
	assert c.g == 0
	assert c.b == 0
	assert c.a == 128
}

fn test_resize_rejects_empty_and_invalid_dimensions() {
	src := image.new_nrgba(image.rect(0, 0, 1, 1))
	for dimensions in [[0, 1], [1, -1], [max_i32, max_i32]] {
		if _ := draw.resize(src, dimensions[0], dimensions[1]) {
			assert false, 'accepted invalid dimensions'
		}
	}
	if _ := draw.resize(image.NRGBA{}, 1, 1) {
		assert false, 'accepted an empty source'
	}
}
