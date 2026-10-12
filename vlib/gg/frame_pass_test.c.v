// vtest build: !docker-ubuntu-musl
// vtest vflags: -gc boehm -d no_gc_thread_local_alloc
module gg

import sokol.gfx
import os
import v.cmdexec

$if gcboehm ? {
	fn C.GC_get_total_bytes() usize
	fn C.GC_get_gc_no() usize
}

fn test_context_end_does_not_heap_copy_a_frame_local_pass() {
	root := os.join_path(os.vtmp_dir(), 'gg_retained_pass_${os.getpid()}')
	os.mkdir_all(root) or { panic(err) }
	defer { os.rmdir_all(root) or {} }
	source := os.join_path(root, 'main.v')
	output := os.join_path(root, 'main.c')
	os.write_file(source, 'import gg
fn main() {
	ctx := &gg.Context{}
	ctx.end()
}
') or { panic(err) }
	compiled := cmdexec.run(@VEXE, ['-b', 'c', '-nocache', '-o', output, source])
	assert compiled.exit_code == 0, compiled.output
	generated := os.read_file(output) or { panic(err) }
	body := generated.all_after_last('void gg__Context__end(').all_after('{').all_before('\n}')
	assert body.contains('gfx__begin_pass'), body
	assert !body.contains('memdup'), body
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
