module canvas

#include "@DIR/gpu_egl_linux.h"

fn C.v_canvas_egl_create(message &&char) voidptr
fn C.v_canvas_egl_destroy(context voidptr)
fn C.v_canvas_gl_has_storage_buffers() int

const linux_gpu_requirements = 'gg.canvas: GPU rendering requires OpenGL 4.3 with vertex and fragment storage buffers and at least 16 storage bindings'

fn linux_gpu_has_storage_buffers() bool {
	return C.v_canvas_gl_has_storage_buffers() != 0
}

fn new_egl_gpu_context() !voidptr {
	mut message := &char(unsafe { nil })
	context := C.v_canvas_egl_create(&message)
	if context == unsafe { nil } {
		return error('gg.canvas: ${unsafe { message.vstring() }}')
	}
	// Check before sokol.gfx setup, which initializes all of its SSBO slots.
	if !linux_gpu_has_storage_buffers() {
		C.v_canvas_egl_destroy(context)
		return error(linux_gpu_requirements)
	}
	return context
}

fn free_egl_gpu_context(context voidptr) {
	C.v_canvas_egl_destroy(context)
}
