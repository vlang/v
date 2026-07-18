module main

import gg

fn main() {
	mut context := gg.new_context(
		width:        32
		height:       32
		window_title: 'legacy no-flag isolation'
		frame_fn:     fn (mut context gg.Context) {
			context.begin()
			context.end()
		}
	)
	_ = context
}
