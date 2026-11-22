import image.png
import os
import svg

fn main() {
	mut doc := svg.new(200, 200)
	doc.rect(0, 0, 200, 200, svg.fill('#f5f5f5'))
	doc.circle(100, 100, 70, svg.fill('#3498db'))
	doc.text(30, 30, 'RASTER', svg.fill('#222222'), svg.font_size(20))

	img := doc.rasterize(svg.RasterOptions{
		width:   400
		height:  400
		samples: 4
	}) or { panic(err.msg()) }
	data := png.encode_to_bytes(img) or { panic(err.msg()) }
	os.write_file('svg_raster.png', data.bytestr()) or { panic(err.msg()) }
	println('wrote svg_raster.png')
}
