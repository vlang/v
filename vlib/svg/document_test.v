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

fn test_escape() {
	assert escape_attr('a&b"c') == 'a&amp;b&quot;c'
	assert escape_text('<x>') == '&lt;x&gt;'
}
