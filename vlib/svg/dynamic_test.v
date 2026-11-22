module svg

fn test_animation_snapshot() {
	doc := parse_svg('<svg xmlns="http://www.w3.org/2000/svg" width="100" height="100"><rect id="r" width="10" height="10" fill="red"><animate attributeName="fill" from="red" to="blue" dur="2s" fill="freeze"/></rect></svg>') or {
		assert false
		return
	}
	anims := collect_animations(&doc)
	assert anims.len == 1
	assert anims[0].target_id == 'r'
	mid := snapshot_at(&doc, 1) or {
		assert false
		return
	}
	r := mid.root.find('r') or {
		assert false
		return
	}
	c := parse_color(r.get_attr('fill')) or {
		assert false
		return
	}
	assert c.r > 0 && c.r < 255
	assert c.b > 0 && c.b < 255
}

fn test_animation_eval_discrete() {
	a := Animation{
		attr:      'fill'
		dur:       1
		calc_mode: 'discrete'
		values:    ['red', 'green', 'blue']
	}
	v := eval_animation(a, 0.5) or {
		assert false
		return
	}
	assert v == 'green'
	v2 := eval_animation(a, 0.8) or {
		assert false
		return
	}
	assert v2 == 'blue'
}

fn test_discrete_animation_without_values() {
	a := Animation{
		attr:      'fill'
		dur:       1
		calc_mode: 'discrete'
		from:      'red'
		to:        'blue'
	}
	v := eval_animation(a, 0.5) or {
		assert false
		return
	}
	assert v == 'blue'

	empty := Animation{ attr: 'fill', calc_mode: 'discrete' }
	eval_animation(empty, 0.5) or {
		assert err.msg().contains('no values')
		return
	}
	assert false
}

fn test_filter_parse() {
	doc := parse_svg('<svg><defs><filter id="blur"><feGaussianBlur stdDeviation="2"/></filter></defs><rect width="10" height="10" filter="url(#blur)"/></svg>') or {
		assert false
		return
	}
	fdef := doc.root.find('blur') or {
		assert false
		return
	}
	f := parse_filter(fdef) or {
		assert false
		return
	}
	assert f.primitives.len == 1
	assert f.primitives[0].kind == .gaussian_blur
	assert filter_is_supported(f)
	assert supported_filters().len >= 5
}

fn test_text_render() {
	mut d := new(120, 30)
	d.text(2, 2, 'ABC', fill('#000000'), font_size(15))
	img := d.rasterize(RasterOptions{ width: 120, height: 30 }) or {
		assert false
		return
	}
	mut inked := 0
	for i in 3 .. img.pix.len / 4 {
		if img.pix[i * 4 + 3] > 0 {
			inked++
		}
	}
	assert inked > 0
}

fn test_keyframes_parse() {
	frames := parse_keyframes('@keyframes fade { from { opacity: 0 } to { opacity: 1 } }')
	assert 'fade' in frames
	assert frames['fade'].len == 2
}
