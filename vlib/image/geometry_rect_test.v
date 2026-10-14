module image

import image.color

// A rectangle whose origin is not the zero point, so offsets and mod results
// cannot pass by accident on a rect anchored at (0,0).
const geom_rect = rect(3, 5, 11, 13)

fn test_point_vector_arithmetic() {
	assert pt(3, 4).mul(2) == pt(6, 8)
	assert pt(3, 4).mul(-1) == pt(-3, -4)
	assert pt(3, 4).mul(0) == pt(0, 0)
	assert pt(7, -7).div(2) == pt(3, -3)
	assert pt(9, 9).div(3) == pt(3, 3)
	assert pt(3, 4).eq(pt(3, 4))
	assert !pt(3, 4).eq(pt(3, 5))
	assert !pt(3, 4).eq(pt(4, 4))
	assert pt(0, 0).add(pt(0, 0)) == pt(0, 0)
	assert pt(1, 2).sub(pt(1, 2)) == pt(0, 0)
}

fn test_point_mod_maps_any_point_into_the_rectangle() {
	points := [pt(0, 0), pt(-1, -1), pt(11, 13), pt(12, 14), pt(-7, 9), pt(100, -100), geom_rect.min,
		geom_rect.max]
	for p in points {
		q := p.mod(geom_rect)
		assert q.in_rect(geom_rect)
		// mod wraps by the rectangle size, so the result only depends on the
		// offset from the minimum corner.
		assert p.mod(geom_rect).eq(q)
	}
	assert pt(3, 5).mod(geom_rect) == pt(3, 5)
	// The max corner is outside the half-open rectangle, so it wraps to min.
	assert pt(11, 13).mod(geom_rect) == pt(3, 5)
	assert pt(-1, -1).mod(geom_rect) == pt(7, 7)
}

fn test_rectangle_str_and_size() {
	assert geom_rect.str() == '(3,5)-(11,13)'
	assert geom_rect.size() == pt(8, 8)
	assert rect(0, 0, 5, 7).size() == pt(5, 7)
	assert Rectangle{}.str() == '(0,0)-(0,0)'
	assert Rectangle{}.size() == pt(0, 0)
	assert Rectangle{}.dx() == 0
	assert Rectangle{}.dy() == 0
	assert geom_rect.add(pt(1, 1)) == rect(4, 6, 12, 14)
	assert geom_rect.sub(pt(1, 1)) == rect(2, 4, 10, 12)
	assert geom_rect.add(pt(0, 0)) == geom_rect
}

fn test_rectangle_canon_orders_min_before_max() {
	assert geom_rect.canon() == geom_rect
	assert Rectangle{}.canon() == Rectangle{}
	swapped := Rectangle{
		min: pt(5, 5)
		max: pt(1, 1)
	}
	assert swapped.canon() == rect(1, 1, 5, 5)
	assert !swapped.canon().empty()
	both := Rectangle{
		min: pt(9, 2)
		max: pt(4, 8)
	}
	assert both.canon() == rect(4, 2, 9, 8)
	assert both.canon().dx() == 5
	assert both.canon().dy() == 6
}

fn test_rectangle_inset_grows_and_collapses() {
	assert geom_rect.inset(0) == geom_rect
	assert geom_rect.inset(2) == rect(5, 7, 9, 11)
	assert geom_rect.inset(-2) == rect(1, 3, 13, 15)

	// Inset past half the width collapses the axis onto its midpoint.
	collapsed := geom_rect.inset(100)
	assert collapsed.dx() == 0
	assert collapsed.dy() == 0
	assert collapsed.min == pt(7, 9)
	assert collapsed.max == pt(7, 9)

	// An inset rectangle of a non-empty rectangle stays inside it.
	assert geom_rect.inset(3).inside(geom_rect)
	// ... and a negative inset contains it.
	assert geom_rect.inside(geom_rect.inset(-3))
}

fn test_rectangle_inset_collapses_one_axis_at_a_time() {
	wide := rect(0, 0, 10, 2)
	// 2n == dy exactly, so the height collapses while the width survives.
	inset := wide.inset(1)
	assert inset.dx() == 8
	assert inset.dy() == 0
	assert inset.min.y == 1
	assert inset.max.y == 1

	tall := rect(0, 0, 2, 10)
	inset_tall := tall.inset(1)
	assert inset_tall.dx() == 0
	assert inset_tall.dy() == 8
}

fn test_rectangle_is_itself_an_image() {
	assert geom_rect.bounds() == geom_rect
	assert geom_rect.color_model() == color.alpha16_model
	assert geom_rect.at(3, 5) == color.opaque
	assert geom_rect.at(10, 12) == color.opaque
	assert geom_rect.at(2, 5) == color.transparent
	assert geom_rect.at(11, 13) == color.transparent
	assert geom_rect.at(-1, -1) == color.transparent

	assert geom_rect.rgba64_at(3, 5) == color.RGBA64{
		r: 0xffff
		g: 0xffff
		b: 0xffff
		a: 0xffff
	}
	assert geom_rect.rgba64_at(11, 13) == color.RGBA64{}
	assert geom_rect.rgba64_at(-1, -1) == color.RGBA64{}
}

fn test_empty_rectangle_relations() {
	assert Rectangle{}.inside(geom_rect)
	assert geom_rect.inside(geom_rect)
	assert !geom_rect.inside(Rectangle{})
	assert !Rectangle{}.overlaps(geom_rect)
	assert !geom_rect.overlaps(Rectangle{})
	// Sharing only an edge is not an overlap.
	assert !geom_rect.overlaps(rect(11, 5, 14, 13))
	assert !geom_rect.overlaps(rect(0, 0, 3, 13))
	assert geom_rect.overlaps(rect(10, 12, 20, 20))
	assert !geom_rect.overlaps(geom_rect.add(pt(100, 0)))
	// Equality treats every empty rectangle as equal to every other.
	assert Rectangle{
		min: pt(1, 1)
		max: pt(1, 1)
	}.eq(Rectangle{
		min: pt(9, 9)
		max: pt(8, 8)
	})
	assert !Rectangle{
		min: pt(1, 1)
		max: pt(1, 1)
	}.eq(geom_rect)
}

fn test_rectangle_self_operations_are_the_identity() {
	assert geom_rect.intersect(geom_rect) == geom_rect
	assert geom_rect.union(geom_rect) == geom_rect
	assert geom_rect.intersect(geom_rect.add(pt(100, 0))) == Rectangle{}
	assert geom_rect.union(rect(0, 0, 1, 1)) == rect(0, 0, 11, 13)
	assert Rectangle{}.union(geom_rect) == geom_rect
	assert geom_rect.union(Rectangle{}) == geom_rect
	assert Rectangle{}.intersect(geom_rect) == Rectangle{}
}

fn test_point_in_rect_half_open_on_both_axes() {
	assert pt(3, 5).in_rect(geom_rect)
	assert pt(10, 12).in_rect(geom_rect)
	assert !pt(11, 12).in_rect(geom_rect)
	assert !pt(10, 13).in_rect(geom_rect)
	assert !pt(2, 5).in_rect(geom_rect)
	assert !pt(3, 4).in_rect(geom_rect)
	assert !pt(3, 5).in_rect(Rectangle{})
}
