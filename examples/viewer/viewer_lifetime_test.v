module main

import image.decoders.gif
import image.decoders.webp
import os

fn test_repeated_animation_cleanup() ! {
	for _ in 0 .. 3 {
		bytes := os.read_bytes(@VEXEROOT + '/vlib/image/decoders/gif/testdata/animated_2frame.gif')!
		mut gif_player := gif.new_gif_player(bytes)!
		_ = gif_player.step()!
		mut app := App{
			player:     gif_player
			anim_bytes: bytes
			is_anim:    true
		}
		app.clear_image()
		app.clear_image()
		assert !app.is_anim
		assert app.anim_bytes.len == 0
		webp_bytes := os.read_bytes(@VEXEROOT + '/vlib/image/decoders/webp/testdata/animated_blend.webp')!
		mut webp_player := webp.new_webp_player(webp_bytes)!
		unsafe { webp_bytes.free() }
		_ = webp_player.step()!
		app.player = webp_player
		app.is_anim = true
		app.clear_image()
		app.clear_image()
		assert !app.is_anim
	}
}
