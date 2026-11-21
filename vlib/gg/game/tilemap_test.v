module game

import gg

fn test_tilemap_access() {
	mut m := new_tilemap([1, 0, 0, 2], 2, 2, 16)
	assert m.at(0, 0) == 1
	assert m.at(1, 1) == 2
	assert m.at(5, 5) == -1
	assert m.is_solid(0, 0)
	assert !m.is_solid(1, 0)
	m.set(1, 0, 3)
	assert m.at(1, 0) == 3
	m.set(-1, 0, 9)
	assert m.at(0, 0) == 1
}

fn test_tilemap_collision() {
	m := new_tilemap([0, 0, 0, 0, 0, 0, 0, 1, 0], 3, 3, 16)
	assert !m.collides_rect(gg.Rect{0, 0, 16, 16})
	assert m.collides_rect(gg.Rect{20, 36, 8, 8})
	assert !m.collides_circle(Circle{8, 8, 4})
	assert m.collides_circle(Circle{26, 38, 4})
}
