module canvas

fn test_unsupported_text_platform_returns_errors_without_drawing() {
	$if macos || linux {
		return
	}
	if _ := Typeface.from_data([u8(1)]) {
		assert false, 'non-macOS typeface creation succeeded'
	} else {
		assert err.msg().contains('macOS CoreText')
	}
	if _ := Typeface.from_file('font.ttf') {
		assert false, 'non-macOS typeface loading succeeded'
	} else {
		assert err.msg().contains('macOS CoreText')
	}
	if _ := Typeface.from_ct_font(unsafe { nil }) {
		assert false, 'non-macOS CTFont wrapping succeeded'
	} else {
		assert err.msg().contains('macOS CoreText')
	}
	mut s := new_raster_surface(width: 4, height: 4)!
	s.clear(0xff123456)
	s.draw_glyphs(Font{}, [u16(1)], [Point{}], Point{}, Paint{}) or {
		assert err.msg().contains('macOS CoreText')
		assert s.pixels.all(it == 0xff123456)
		return
	}
	assert false, 'unsupported text draw succeeded'
}
