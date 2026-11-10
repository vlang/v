// vtest build: !docker-ubuntu-musl && !(tinyc && arm64)
// vtest vflags: -gc boehm -d no_gc_thread_local_alloc
// Precise counters rebuild libgc, whose atomics ARM64 TCC cannot compile.
module gg

import sokol.gfx

$if gcboehm ? {
	fn C.GC_get_total_bytes() uint
	fn C.GC_get_gc_no() uint
}

fn test_context_frame_pass_refreshes_action_and_swapchain_in_retained_storage() {
	mut ctx := &Context{
		bg_color:                    Color{ r: 20, g: 30, b: 40, a: 128 }
		clear_pass:                  gfx.PassAction{
			colors: [
				gfx.ColorAttachmentAction{ load_action: .clear, clear_value: gfx.Color{0.2, 0.3, 0.4, 0.5} },
				gfx.ColorAttachmentAction{},
				gfx.ColorAttachmentAction{},
				gfx.ColorAttachmentAction{},
			]!
		}
		translucent_bg_seed_pending: true
	}
	address := unsafe { voidptr(&ctx.frame_pass) }
	first := gfx.Swapchain{ width: 320, height: 200, sample_count: 1, color_format: .rgba8 }
	ctx.prepare_end_pass(EndOptions{}, first)
	assert ctx.frame_pass.action.colors[0].load_action == .clear
	assert ctx.frame_pass.action.colors[0].clear_value.a == f32(0.5)
	assert !ctx.translucent_bg_seed_pending
	assert ctx.frame_pass.swapchain.width == 320
	second := gfx.Swapchain{ width: 640, height: 400, sample_count: 4, color_format: .bgra8 }
	ctx.prepare_end_pass(EndOptions{}, second)
	assert ctx.frame_pass.action.colors[0].load_action == .load
	assert ctx.frame_pass.swapchain.width == 640 && ctx.frame_pass.swapchain.sample_count == 4
	assert ctx.frame_pass.swapchain.color_format == .bgra8
	ctx.prepare_end_pass(EndOptions{ how: .passthru }, first)
	assert ctx.frame_pass.action.colors[0].load_action == .dontcare
	ctx.bg_color.a = 255
	ctx.clear_pass.colors[0].clear_value.r = 0.75
	ctx.prepare_end_pass(EndOptions{}, second)
	assert ctx.frame_pass.action.colors[0].load_action == .clear
	assert ctx.frame_pass.action.colors[0].clear_value.r == f32(0.75)
	assert unsafe { voidptr(&ctx.frame_pass) } == address
	$if gcboehm ? {
		before_bytes := C.GC_get_total_bytes()
		before_collections := C.GC_get_gc_no()
		for _ in 0 .. 256 {
			ctx.prepare_end_pass(EndOptions{}, first)
			ctx.prepare_end_pass(EndOptions{ how: .passthru }, second)
		}
		assert C.GC_get_total_bytes() == before_bytes
		assert C.GC_get_gc_no() == before_collections
	}
}
