module gg

fn test_tween_f32() {
	mut t := new_tween(f32(0), f32(10), 2, lerp_f32)
	assert t.value() == 0
	assert !t.update(1)
	assert t.value() - 5 < 0.0001
	assert t.update(1)
	assert t.value() == 10
	t.reset()
	assert t.value() == 0
}

fn test_tween_looping() {
	mut t := new_tween(f32(0), f32(10), 1, lerp_f32)
	t.looping = true
	assert !t.update(2.5)
	assert t.value() - 5 < 0.0001
}

fn test_tween_color() {
	mut t := new_tween(rgb(0, 0, 0), rgb(255, 255, 255), 1, lerp_color)
	assert !t.update(0.5)
	c := t.value()
	assert c.r == 127 || c.r == 128
	assert c.a == 255
}

fn test_timeline() {
	mut tl := new_timeline()
	tl.add(0.5, 'a')
	tl.add(1.5, 'b')
	assert tl.update(0.4).len == 0
	first := tl.update(0.2)
	assert first.len == 1 && first[0] == 'a'
	second := tl.update(1.0)
	assert second.len == 1 && second[0] == 'b'
	assert tl.update(1.0).len == 0
	tl.reset()
	assert tl.update(0.6).len == 1
}

fn test_spring() {
	mut s := new_spring(0, 10)
	for _ in 0 .. 240 {
		s.update(1.0 / 60.0)
	}
	assert s.value > 9.5 && s.value < 10.5
	assert s.velocity > -1 && s.velocity < 1
}

fn test_lerp_helpers() {
	assert lerp_f32(0, 10, 0.5) == 5
	p := lerp_vec2(Vec2{0, 0}, Vec2{10, 20}, 0.5)
	assert p.x == 5 && p.y == 10
	q := lerp_vec3(Vec3{0, 0, 0}, Vec3{10, 20, 30}, 0.5)
	assert q.x == 5 && q.y == 10 && q.z == 15
}
