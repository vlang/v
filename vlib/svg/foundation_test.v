module svg

fn test_scan_number() {
	n := scan_number('10-20abc', 0) or {
		assert false
		return
	}
	assert n.value == 10
	assert n.consumed == 2
	n2 := scan_number('-3.5e2', 0) or {
		assert false
		return
	}
	assert n2.value == -350
	assert n2.consumed == 6
}

fn test_colors() {
	c := parse_color('#f00')!
	assert c.r == 255 && c.g == 0 && c.b == 0 && c.a == 255
	c2 := parse_color('rgb(10, 20, 30)')!
	assert c2.r == 10 && c2.g == 20 && c2.b == 30
	c3 := parse_color('rgba(0,0,0,0.5)')!
	assert c3.a == 128
	c4 := parse_color('rebeccapurple')!
	assert c4.r == 102 && c4.g == 51 && c4.b == 153
	c5 := parse_color('hsl(0, 100%, 50%)')!
	assert c5.r == 255 && c5.g == 0 && c5.b == 0
	assert parse_color('none')!.kind == .none
	assert parse_color('currentColor')!.kind == .current_color
}

fn test_transform() {
	t := parse_transform('translate(10,20) scale(2)')!
	p := t.apply(vec2(1, 1))
	assert p.x == 12 && p.y == 22
	inv := t.inverse() or {
		assert false
		return
	}
	back := inv.apply(p)
	assert back.x == 1 && back.y == 1
}

fn test_length_angle_viewbox() {
	l := parse_length('50%')!
	assert l.to_px(96, 16, 200) == 100
	a := parse_angle('0.5turn')!
	assert a.to_deg() == 180
	vb := parse_viewbox('0 0 100 50')!
	m := vb.matrix_for(rect(0, 0, 200, 200), AspectRatio{})
	assert m.apply(vec2(100, 50)).x == 200
}

fn test_path_parse_compact() {
	p := parse_path('M10-20A5.5.3-4 110-.1')!
	assert p.cmds.len == 2
	assert p.cmds[1].kind == .a_arc
	assert p.cmds[1].vals[0] == 5.5
	assert p.cmds[1].vals[1] == 0.3
	assert p.cmds[1].vals[5] == 0
	assert p.cmds[1].vals[6] == -0.1
}

fn test_path_implicit() {
	p := parse_path('M0 0 L10 0 10 10 0 10 Z')!
	assert p.cmds.len == 5
	assert p.cmds[4].kind == .close
}

fn test_path_bounds_flatten() {
	p := parse_path('M0 0 L10 0 L10 10 Z')!
	b := p.bounds()
	assert b.x == 0 && b.y == 0 && b.w == 10 && b.h == 10
	lines := p.flatten(0.1)
	assert lines.len == 1
	assert lines[0].len >= 3
}
