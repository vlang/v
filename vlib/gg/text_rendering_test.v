module gg

import fontstash

const embedded_test_mono_font = $embed_file('@VEXEROOT/examples/assets/fonts/RobotoMono-Regular.ttf')

fn new_test_text_context() (&Context, &fontstash.Context) {
	mut fons := fontstash.create_internal(&C.FONSparams{
		width:  512
		height: 512
		flags:  0
	})
	assert fons != unsafe { nil }
	font_id := fons.add_font_mem('mono', embedded_test_mono_font.to_bytes().clone(), true)
	assert font_id != fontstash.invalid
	fons.set_font(font_id)
	fons.set_size(16)
	return &Context{
		ft:          &FT{
			fons: fons
		}
		font_inited: true
		scale:       1.0
	}, fons
}

fn text_advance_and_bounds_width(fons &fontstash.Context, text string) (f32, f32) {
	mut bounds := [4]f32{}
	advance := fons.text_bounds(0, 0, text, &bounds[0])
	return advance, bounds[2] - bounds[0]
}

fn test_text_width_uses_font_advance_for_monospaced_fonts() {
	ctx, fons := new_test_text_context()
	defer {
		fontstash.delete_internal(fons)
	}
	i_advance, i_bounds_width := text_advance_and_bounds_width(fons, 'i')
	m_advance, m_bounds_width := text_advance_and_bounds_width(fons, 'm')
	assert int(i_advance / ctx.scale) == int(m_advance / ctx.scale)
	assert int(i_bounds_width / ctx.scale) != int(m_bounds_width / ctx.scale)
	assert ctx.text_width('i') == int(i_advance / ctx.scale)
	assert ctx.text_width('m') == int(m_advance / ctx.scale)
	width, height := ctx.text_size('i')
	assert width == ctx.text_width('i')
	assert height > 0
}

fn test_text_width_counts_trailing_space_advance() {
	ctx, fons := new_test_text_context()
	defer {
		fontstash.delete_internal(fons)
	}
	advance, bounds_width := text_advance_and_bounds_width(fons, 'a ')
	assert int(advance / ctx.scale) > int(bounds_width / ctx.scale)
	assert ctx.text_width('a ') == int(advance / ctx.scale)
	width, _ := ctx.text_size('a ')
	assert width == int(advance / ctx.scale)
}
