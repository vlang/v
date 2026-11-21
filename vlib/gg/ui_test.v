module gg

fn test_theme_defaults() {
	t := default_ui_theme()
	assert t.padding > 0
	assert t.accent.a == 255
}

fn test_ui_hit_and_slider() {
	r := Rect{10, 10, 100, 20}
	assert ui_hit(r, Vec2{20, 20})
	assert !ui_hit(r, Vec2{5, 20})
	assert ui_slider_value(r, 10, 0, 100) == 0
	assert ui_slider_value(r, 60, 0, 100) == 50
	assert ui_slider_value(r, 110, 0, 100) == 100
	assert ui_slider_value(r, 60, 5, 5) == 5
}

fn test_ui_grid() {
	cells := ui_grid_cells(Rect{0, 0, 100, 100}, 40, 20, 5)
	assert cells.len == 5
	assert cells[0].x == 0 && cells[0].y == 0
	assert cells[1].x == 40 && cells[1].y == 0
	assert cells[2].x == 0 && cells[2].y == 20
	assert ui_grid_cells(Rect{0, 0, 0, 0}, 40, 20, 5).len == 0
}

fn test_chart_math() {
	norm := chart_normalize([0, 5, 10], 0, 10)
	assert norm.len == 3
	assert norm[0] == 0 && (norm[1] - 0.5) < 0.0001 && norm[2] == 1
	assert chart_normalize([], 0, 1).len == 0
	lo, hi := chart_min_max([3, 1, 2])
	assert lo == 1 && hi == 3
	a, b := chart_min_max([2, 2])
	assert a == 2 && b == 3
	x, y := chart_min_max([])
	assert x == 0 && y == 1
}
