module multiwindow

const win32_d3d11_readback_status_ready = 1
const win32_d3d11_readback_status_failed = 2

struct Win32D3D11ReadbackNativeResult {
	request             u64
	state_identity      u64
	renderer_generation u64
	window_slot         u64
	window_generation   u32
	submitted_frame     u64
	status              int
	width               u32
	height              u32
	stride              usize
	byte_length         usize
}

struct Win32D3D11ReadbackBridgeResult {
	request             u64
	state_identity      u64
	renderer_generation u64
	window_slot         u64
	window_generation   u32
	submitted_frame     u64
	status              int
	width               u32
	height              u32
	stride              usize
	byte_length         usize
}

struct Win32D3D11ReadbackBridge {
mut:
	native voidptr
}

type Win32D3D11ReadbackTakeFn = fn (voidptr) (bool, Win32D3D11ReadbackNativeResult)

type Win32D3D11ReadbackReleaseFn = fn (voidptr, u64) bool

$if windows && sokol_d3d11 ? {
	#flag windows -ld3d11
	#flag windows -ldxgi
	#insert "@VMODROOT/vlib/x/multiwindow/win32_readback_d3d11_native.h"

	@[typedef]
	struct C.VMultiwindowWin32ReadbackResult {
	mut:
		request             u64
		state_identity      u64
		renderer_generation u64
		window_slot         u64
		window_generation   u32
		submitted_frame     u64
		status              int
		width               u32
		height              u32
		stride              usize
		byte_length         usize
	}

	fn C.v_multiwindow_win32_d3d11_readback_native_create(device_identity u64, context_identity u64, state_identity u64, renderer_generation u64) voidptr
	fn C.v_multiwindow_win32_d3d11_readback_native_stage_window(state voidptr, swapchain_identity u64, request u64, state_identity u64, renderer_generation u64, window_slot u64, window_generation u32, x u32, y u32, width u32, height u32, producing_frame u64, resize_pending int) int
	fn C.v_multiwindow_win32_d3d11_readback_native_stage_image(state voidptr, texture_identity u64, request u64, state_identity u64, renderer_generation u64, window_slot u64, window_generation u32, x u32, y u32, width u32, height u32, producing_frame u64, resize_pending int) int
	fn C.v_multiwindow_win32_d3d11_readback_native_resolve(state voidptr, window_slot u64, window_generation u32, producing_frame u64, succeeded int, failure_origin int) int
	fn C.v_multiwindow_win32_d3d11_readback_native_resize_submitted(state voidptr, window_slot u64, window_generation u32) int
	fn C.v_multiwindow_win32_d3d11_readback_native_notify_failure(state voidptr, state_identity u64, renderer_generation u64, failure_origin int) int
	fn C.v_multiwindow_win32_d3d11_readback_native_poll(state voidptr) int
	fn C.v_multiwindow_win32_d3d11_readback_native_take(state voidptr, result &C.VMultiwindowWin32ReadbackResult) int
	fn C.v_multiwindow_win32_d3d11_readback_native_copy(state voidptr, request u64, pixels &u8, capacity usize) int
	fn C.v_multiwindow_win32_d3d11_readback_native_release(state voidptr, request u64) int
	fn C.v_multiwindow_win32_d3d11_readback_native_cancel(state voidptr, request u64) int
	fn C.v_multiwindow_win32_d3d11_readback_native_quiesce_window(state voidptr, window_slot u64, window_generation u32) int
	fn C.v_multiwindow_win32_d3d11_readback_native_restart(state voidptr, state_identity u64, renderer_generation u64) int
	fn C.v_multiwindow_win32_d3d11_readback_native_stop(state voidptr) int
	fn C.v_multiwindow_win32_d3d11_readback_native_active(state voidptr) int
	fn C.v_multiwindow_win32_d3d11_readback_native_last_hresult(state voidptr) i32
	fn C.v_multiwindow_win32_d3d11_readback_native_device_removed_reason(state voidptr) i32
	fn C.v_multiwindow_win32_d3d11_readback_native_destroy(state voidptr) int
}

fn win32_d3d11_readback_native_take(context voidptr) (bool, Win32D3D11ReadbackNativeResult) {
	$if windows && sokol_d3d11 ? {
		mut native_result := C.VMultiwindowWin32ReadbackResult{}
		if C.v_multiwindow_win32_d3d11_readback_native_take(context, &native_result) != 1 {
			return false, Win32D3D11ReadbackNativeResult{}
		}
		return true, Win32D3D11ReadbackNativeResult{
			request:             native_result.request
			state_identity:      native_result.state_identity
			renderer_generation: native_result.renderer_generation
			window_slot:         native_result.window_slot
			window_generation:   native_result.window_generation
			submitted_frame:     native_result.submitted_frame
			status:              native_result.status
			width:               native_result.width
			height:              native_result.height
			stride:              native_result.stride
			byte_length:         native_result.byte_length
		}
	} $else {
		_ = context
		return false, Win32D3D11ReadbackNativeResult{}
	}
}

fn win32_d3d11_readback_native_release(context voidptr, request u64) bool {
	$if windows && sokol_d3d11 ? {
		return C.v_multiwindow_win32_d3d11_readback_native_release(context, request) == 1
	} $else {
		_ = context
		_ = request
		return false
	}
}

fn win32_d3d11_readback_bridge_result(raw Win32D3D11ReadbackNativeResult) (bool, Win32D3D11ReadbackBridgeResult) {
	base_valid := raw.request != 0 && raw.state_identity != 0 && raw.renderer_generation != 0
		&& raw.window_slot != 0 && raw.window_generation != 0
	if raw.status == win32_d3d11_readback_status_ready {
		ready_valid := base_valid && raw.submitted_frame != 0 && raw.width != 0 && raw.height != 0
			&& raw.stride == usize(raw.width) * 4
			&& u64(raw.stride) * u64(raw.height) == u64(raw.byte_length)
		return ready_valid, Win32D3D11ReadbackBridgeResult{
			request:             raw.request
			state_identity:      raw.state_identity
			renderer_generation: raw.renderer_generation
			window_slot:         raw.window_slot
			window_generation:   raw.window_generation
			submitted_frame:     raw.submitted_frame
			status:              raw.status
			width:               raw.width
			height:              raw.height
			stride:              raw.stride
			byte_length:         raw.byte_length
		}
	}
	if raw.status == win32_d3d11_readback_status_failed {
		failed_valid := base_valid && raw.submitted_frame == 0 && raw.stride == 0
			&& raw.byte_length == 0
		return failed_valid, Win32D3D11ReadbackBridgeResult{
			request:             raw.request
			state_identity:      raw.state_identity
			renderer_generation: raw.renderer_generation
			window_slot:         raw.window_slot
			window_generation:   raw.window_generation
			status:              raw.status
		}
	}
	return false, Win32D3D11ReadbackBridgeResult{}
}

fn win32_d3d11_readback_bridge_take_with_ops(context voidptr, take_fn Win32D3D11ReadbackTakeFn, release_fn Win32D3D11ReadbackReleaseFn) (bool, Win32D3D11ReadbackBridgeResult) {
	taken, raw := take_fn(context)
	if !taken {
		return false, Win32D3D11ReadbackBridgeResult{}
	}
	valid, result := win32_d3d11_readback_bridge_result(raw)
	if valid {
		return true, result
	}
	if raw.request != 0 {
		release_fn(context, raw.request)
	}
	return false, Win32D3D11ReadbackBridgeResult{}
}

fn new_win32_d3d11_readback_bridge(device voidptr, context voidptr, state_identity u64, renderer_generation u64) !Win32D3D11ReadbackBridge {
	$if windows && sokol_d3d11 ? {
		native := C.v_multiwindow_win32_d3d11_readback_native_create(native_identity(device),
			native_identity(context), state_identity, renderer_generation)
		if native == unsafe { nil } {
			return error('D3D11 readback adapter initialization failed')
		}
		return Win32D3D11ReadbackBridge{
			native: native
		}
	} $else {
		_ = device
		_ = context
		_ = state_identity
		_ = renderer_generation
		return error('D3D11 readback adapter is unavailable')
	}
}

fn (bridge &Win32D3D11ReadbackBridge) initialized() bool {
	return bridge.native != unsafe { nil }
}

fn (bridge &Win32D3D11ReadbackBridge) stage_window(swapchain voidptr, request u64, state_identity u64, renderer_generation u64, window_slot u64, window_generation u32, x u32, y u32, width u32, height u32, producing_frame u64, resize_pending bool) bool {
	if !bridge.initialized() {
		return false
	}
	$if windows && sokol_d3d11 ? {
		return C.v_multiwindow_win32_d3d11_readback_native_stage_window(bridge.native,
			native_identity(swapchain), request, state_identity, renderer_generation, window_slot,
			window_generation, x, y, width, height, producing_frame, if resize_pending {
			1
		} else {
			0
		}) == 1
	} $else {
		return false
	}
}

fn (bridge &Win32D3D11ReadbackBridge) stage_image(texture voidptr, request u64, state_identity u64, renderer_generation u64, window_slot u64, window_generation u32, x u32, y u32, width u32, height u32, producing_frame u64, resize_pending bool) bool {
	if !bridge.initialized() {
		return false
	}
	$if windows && sokol_d3d11 ? {
		return C.v_multiwindow_win32_d3d11_readback_native_stage_image(bridge.native,
			native_identity(texture), request, state_identity, renderer_generation, window_slot,
			window_generation, x, y, width, height, producing_frame, if resize_pending {
			1
		} else {
			0
		}) == 1
	} $else {
		return false
	}
}

fn (bridge &Win32D3D11ReadbackBridge) resolve(window_slot u64, window_generation u32, producing_frame u64, succeeded bool, failure_origin int) bool {
	if !bridge.initialized() {
		return false
	}
	$if windows && sokol_d3d11 ? {
		return C.v_multiwindow_win32_d3d11_readback_native_resolve(bridge.native, window_slot,
			window_generation, producing_frame, if succeeded { 1 } else { 0 }, failure_origin) == 1
	} $else {
		return false
	}
}

fn (bridge &Win32D3D11ReadbackBridge) resize_submitted(window_slot u64, window_generation u32) bool {
	if !bridge.initialized() {
		return false
	}
	$if windows && sokol_d3d11 ? {
		return C.v_multiwindow_win32_d3d11_readback_native_resize_submitted(bridge.native,
			window_slot, window_generation) == 1
	} $else {
		return false
	}
}

fn (bridge &Win32D3D11ReadbackBridge) notify_failure(state_identity u64, renderer_generation u64, failure_origin int) bool {
	if !bridge.initialized() {
		return false
	}
	$if windows && sokol_d3d11 ? {
		return C.v_multiwindow_win32_d3d11_readback_native_notify_failure(bridge.native,
			state_identity, renderer_generation, failure_origin) == 1
	} $else {
		return false
	}
}

fn (bridge &Win32D3D11ReadbackBridge) poll() int {
	if !bridge.initialized() {
		return 0
	}
	$if windows && sokol_d3d11 ? {
		return C.v_multiwindow_win32_d3d11_readback_native_poll(bridge.native)
	} $else {
		return 0
	}
}

fn (bridge &Win32D3D11ReadbackBridge) take() (bool, Win32D3D11ReadbackBridgeResult) {
	if !bridge.initialized() {
		return false, Win32D3D11ReadbackBridgeResult{}
	}
	return win32_d3d11_readback_bridge_take_with_ops(bridge.native,
		win32_d3d11_readback_native_take, win32_d3d11_readback_native_release)
}

fn (bridge &Win32D3D11ReadbackBridge) copy(request u64, mut pixels []u8) bool {
	if !bridge.initialized() || pixels.len == 0 {
		return false
	}
	$if windows && sokol_d3d11 ? {
		return C.v_multiwindow_win32_d3d11_readback_native_copy(bridge.native, request,
			pixels.data, usize(pixels.len)) == 1
	} $else {
		return false
	}
}

fn (bridge &Win32D3D11ReadbackBridge) release(request u64) bool {
	if !bridge.initialized() {
		return false
	}
	$if windows && sokol_d3d11 ? {
		return C.v_multiwindow_win32_d3d11_readback_native_release(bridge.native, request) == 1
	} $else {
		return false
	}
}

fn (bridge &Win32D3D11ReadbackBridge) cancel(request u64) bool {
	if !bridge.initialized() {
		return false
	}
	$if windows && sokol_d3d11 ? {
		return C.v_multiwindow_win32_d3d11_readback_native_cancel(bridge.native, request) == 1
	} $else {
		return false
	}
}

fn (bridge &Win32D3D11ReadbackBridge) quiesce_window(window_slot u64, window_generation u32) int {
	if !bridge.initialized() {
		return 0
	}
	$if windows && sokol_d3d11 ? {
		return C.v_multiwindow_win32_d3d11_readback_native_quiesce_window(bridge.native,
			window_slot, window_generation)
	} $else {
		return 0
	}
}

fn (bridge &Win32D3D11ReadbackBridge) restart(state_identity u64, renderer_generation u64) bool {
	if !bridge.initialized() {
		return false
	}
	$if windows && sokol_d3d11 ? {
		return C.v_multiwindow_win32_d3d11_readback_native_restart(bridge.native, state_identity,
			renderer_generation) == 1
	} $else {
		return false
	}
}

fn (bridge &Win32D3D11ReadbackBridge) stop() bool {
	if !bridge.initialized() {
		return false
	}
	$if windows && sokol_d3d11 ? {
		return C.v_multiwindow_win32_d3d11_readback_native_stop(bridge.native) == 1
	} $else {
		return false
	}
}

fn (bridge &Win32D3D11ReadbackBridge) active() int {
	if !bridge.initialized() {
		return 0
	}
	$if windows && sokol_d3d11 ? {
		return C.v_multiwindow_win32_d3d11_readback_native_active(bridge.native)
	} $else {
		return 0
	}
}

fn (bridge &Win32D3D11ReadbackBridge) last_hresult() i32 {
	if !bridge.initialized() {
		return 0
	}
	$if windows && sokol_d3d11 ? {
		return C.v_multiwindow_win32_d3d11_readback_native_last_hresult(bridge.native)
	} $else {
		return 0
	}
}

fn (bridge &Win32D3D11ReadbackBridge) device_removed_reason() i32 {
	if !bridge.initialized() {
		return 0
	}
	$if windows && sokol_d3d11 ? {
		return C.v_multiwindow_win32_d3d11_readback_native_device_removed_reason(bridge.native)
	} $else {
		return 0
	}
}

fn (mut bridge Win32D3D11ReadbackBridge) destroy() bool {
	if !bridge.initialized() {
		return false
	}
	$if windows && sokol_d3d11 ? {
		if C.v_multiwindow_win32_d3d11_readback_native_destroy(bridge.native) != 1 {
			return false
		}
		bridge.native = unsafe { nil }
		return true
	} $else {
		return false
	}
}
