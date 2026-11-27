module svg

fn test_builder_roundtrip() {
	mut d := new(200, 100)
	d.circle(50, 50, 20, fill('#f00'), stroke('black'), stroke_width(2))
	d.rect(0, 0, 10, 10)
	d.text(5, 15, 'Hi', font_size(12))
	d.set_style('.x', 'fill', 'blue')
	out := d.to_svg(WriteOptions{ minify: true })
	assert out.contains('<circle')
	assert out.contains('fill: #f00')
	assert out.contains('>Hi</text>')
	parsed := parse_svg(out) or {
		assert false
		return
	}
	assert parsed.root.children.len == 4
	assert parsed.root.children[0].tag == 'style'
	assert parsed.root.children[1].tag == 'circle'
	assert parsed.css.len == 1
}

fn test_parse_css_and_cascade() {
	doc := parse_svg('<svg xmlns="http://www.w3.org/2000/svg"><style>.a{fill:red}.b{fill:blue}</style><rect class="a b" id="r"/></svg>') or {
		assert false
		return
	}
	assert doc.css.len == 2
	r := doc.root.children[1]
	style := cascade_style(&r, doc.css)
	assert style.get('fill') == 'blue'
	r2 := parse_svg('<svg><style>#r{fill:green}</style><rect id="r"/></svg>') or {
		assert false
		return
	}
	el := r2.root.children[1]
	assert cascade_style(&el, r2.css).get('fill') == 'green'
}

fn test_gradient_midpoint_is_interpolated() {
	mut g := Gradient{}
	g.stops << Stop{ offset: 0, color: rgb(255, 0, 0) }
	g.stops << Stop{ offset: 1, color: rgb(0, 0, 255) }
	mid := gradient_color(g, 0.5)
	assert mid.r > 100 && mid.r < 160
	assert mid.b > 100 && mid.b < 160
	assert mid.r == mid.b
}

fn test_group_clip_and_dash_are_inherited() {
	doc := parse_svg('<svg><defs><clipPath id="c"><circle cx="50" cy="50" r="20"/></clipPath></defs><g clip-path="url(#c)"><line x1="0" y1="0" x2="100" y2="100" stroke-dasharray="20 10" stroke="black"/></g></svg>') or {
		assert false
		return
	}
	scene := flatten_document(&doc, FlattenOptions{}) or {
		assert false
		return
	}
	assert scene.items.len == 1
	assert scene.items[0].clip_ids.len == 1
	assert scene.items[0].clip_ids[0] == 'c'
	assert scene.items[0].dash_array.len == 2
}

fn test_escape() {
	assert escape_attr('a&b"c') == 'a&amp;b&quot;c'
	assert escape_text('<x>') == '&lt;x&gt;'
}
