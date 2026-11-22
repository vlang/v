import os
import svg

fn main() {
	mut doc := svg.new(480, 240)
	doc.rect(0, 0, 480, 240, svg.fill('#101820'))
	doc.circle(110, 120, 70,
		svg.fill('#ff6b6b'),
		svg.stroke('#ffffff'),
		svg.stroke_width(3))
	doc.ellipse(280, 120, 80, 45,
		svg.fill('#4ecdc4'))
	doc.line(30, 210, 450, 210,
		svg.stroke('#f7d794'),
		svg.stroke_width(4))
	doc.polygon([
		svg.vec2(360, 40)
		svg.vec2(430, 40)
		svg.vec2(395, 95),
	], svg.fill('#f7d794'))
	doc.text(20, 28, 'SVG IN PURE V', svg.fill('#ffffff'), svg.font_size(18))

	os.write_file('svg_basic.svg', doc.to_svg(svg.WriteOptions{})) or { panic(err.msg()) }
	println('wrote svg_basic.svg')
}
