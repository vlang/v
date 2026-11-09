// vtest build: linux
module canvas

import sokol.gfx

fn C.eglQueryAPI() u32
fn C.eglGetCurrentDisplay() voidptr
fn C.eglGetCurrentContext() voidptr
fn C.eglGetCurrentSurface(which int) voidptr
fn C.eglMakeCurrent(display voidptr, draw voidptr, read voidptr, context voidptr) u32

struct CanvasEglState {
	api     u32
	display voidptr
	context voidptr
	draw    voidptr
	read    voidptr
}

fn canvas_egl_state() CanvasEglState {
	return CanvasEglState{
		api:     C.eglQueryAPI()
		display: C.eglGetCurrentDisplay()
		context: C.eglGetCurrentContext()
		draw:    C.eglGetCurrentSurface(C.EGL_DRAW)
		read:    C.eglGetCurrentSurface(C.EGL_READ)
	}
}

fn draw_with_owned_egl_context() ! {
	mut ctx := new_gpu_context()!
	defer { ctx.free() }
	assert ctx.owns_gfx
	assert ctx.native_context != unsafe { nil }
	assert linux_gpu_has_storage_buffers()
	mut surface := ctx.new_surface(width: 3, height: 2)!
	defer { surface.free() }
	surface.clear(Color4f{0, 1, 0, 1})
	mut pixels := []u8{len: 3 * 2 * 4}
	surface.read_pixels(pixels.data, 3 * 4)!
	assert pixels[..4] == [u8(0), 255, 0, 255]
}

fn draw_and_restore_existing_egl_context(current bool) ! {
	external := new_egl_gpu_context()!
	defer { free_egl_gpu_context(external) }
	prior := canvas_egl_state()
	assert prior.context != unsafe { nil }
	assert !gfx.is_valid()
	if !current {
		assert C.eglMakeCurrent(prior.display, unsafe { nil }, unsafe { nil }, unsafe { nil }) != 0
	}
	expected := canvas_egl_state()
	draw_with_owned_egl_context()!
	assert !gfx.is_valid()
	assert canvas_egl_state() == expected
	if !current {
		// An EGL context can remain owned by an application without being
		// current. Canvas must not terminate its display when it finishes.
		assert C.eglMakeCurrent(prior.display, prior.draw, prior.read, prior.context) != 0
	}
	assert canvas_egl_state() == prior
}

fn test_owned_canvas_context_restores_existing_egl_state() {
	original := canvas_egl_state()
	draw_and_restore_existing_egl_context(true)!
	assert canvas_egl_state() == original
}

fn test_owned_canvas_context_preserves_noncurrent_egl_resources() {
	original := canvas_egl_state()
	draw_and_restore_existing_egl_context(false)!
	assert canvas_egl_state() == original
}
