// vtest build: !musl?
import gg
import gx

struct Game {
mut:
	gg ?gg.Context
}

fn test_c_struct_with_reserved_field_name() {
	mut game := Game{
		gg: none
	}
	mut cont := gg.new_context(
		bg_color:     gx.rgb(174, 198, 255)
		width:        600
		height:       400
		window_title: 'Polygons'
	)
	game.gg = cont
	game.gg?.str()
	assert true
}
