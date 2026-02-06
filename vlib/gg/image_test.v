// vtest build: !docker-ubuntu-musl // needs GL/gl.h
import gg

fn test_get_cached_image_idx_bounds_checking() {
	mut ctx := gg.new_context(width: 100)
	assert ctx.get_cached_image_by_idx(-1).ok == false
	assert ctx.get_cached_image_by_idx(1).ok == false
}

fn test_remove_cached_image_remove_and_get() {
	mut ctx := gg.new_context(width: 100)
	image := gg.Image{
		ok: true
	}
	idx := ctx.cache_image(image)
	assert ctx.get_cached_image_by_idx(idx).ok == true
	ctx.remove_cached_image_by_idx(idx)
	assert ctx.get_cached_image_by_idx(idx).ok == false
}
