module svg

fn test_fast_parse_entities() {
	doc := parse_svg('<svg xmlns="http://www.w3.org/2000/svg"><text>A&amp;B &lt;C &gt;D &quot;E&quot; &#65; &#x42;</text></svg>') or {
		assert false
		return
	}
	t := doc.root.children[0]
	assert t.text == 'A&B <C >D "E" A B'
}

fn test_fast_parse_cdata_comments_doctype() {
	src := '<?xml version="1.0"?><!DOCTYPE svg><!-- skip --><svg><style><![CDATA[.a{fill:red}]]></style><rect/></svg>'
	doc := parse_svg(src) or {
		assert false
		return
	}
	assert doc.root.tag == 'svg'
	assert doc.root.children.len == 2
	assert doc.css.len == 1
	assert doc.root.children[0].text == '.a{fill:red}'
}

fn test_fast_parse_style_and_href() {
	doc := parse_svg('<svg><rect style="fill:red; stroke: blue"/><use xlink:href="#sym"/><use xlink:href="other.svg#sym"/></svg>') or {
		assert false
		return
	}
	r := doc.root.children[0]
	assert r.style.get('fill') == 'red'
	assert r.style.get('stroke') == 'blue'
	u1 := doc.root.children[1]
	assert u1.get_attr('xlink:href') == '#sym'
	u2 := doc.root.children[2]
	assert u2.get_attr('href') == 'other.svg#sym'
}

fn test_fast_parse_fallback() {
	// An unclosed attribute makes the fast scanner reject the input; the
	// general XML parser then reports the error instead of hanging.
	if _ := parse_svg('<svg><rect x="10></svg>') {
		assert false
	}
}
