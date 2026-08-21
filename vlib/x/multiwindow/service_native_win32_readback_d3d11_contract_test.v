module multiwindow

import os

#include "@VMODROOT/vlib/x/multiwindow/win32_readback_d3d11_diagnostics.h"

fn C.v_multiwindow_win32_readback_record_device_removed_reason(primary_hresult i32, device_removed_reason &i32, observed_reason i32)

const win32_d3d11_contract_e_fail = i32(-2147467259)
const win32_d3d11_contract_device_removed = i32(-2005270523)

struct Win32D3D11ReadbackTakeProbe {
mut:
	available     bool
	released      bool
	take_calls    int
	release_calls int
}

fn win32_d3d11_readback_take_probe(context voidptr) (bool, Win32D3D11ReadbackNativeResult) {
	mut probe := unsafe { &Win32D3D11ReadbackTakeProbe(context) }
	probe.take_calls++
	if !probe.available {
		return false, Win32D3D11ReadbackNativeResult{}
	}
	probe.available = false
	return true, Win32D3D11ReadbackNativeResult{
		request:             107
		state_identity:      109
		renderer_generation: 113
		window_slot:         127
		window_generation:   131
		submitted_frame:     137
		status:              win32_d3d11_readback_status_ready
		width:               3
		height:              2
		stride:              16
		byte_length:         32
	}
}

fn win32_d3d11_readback_release_probe(context voidptr, request u64) bool {
	mut probe := unsafe { &Win32D3D11ReadbackTakeProbe(context) }
	if request != 107 || probe.released {
		return false
	}
	probe.released = true
	probe.release_calls++
	return true
}

fn win32_d3d11_readback_native_contract_function(source string, signature string, next_signature string) !string {
	start := source.index(signature) or {
		return error('missing native contract signature `${signature}`')
	}
	end := source.index_after(next_signature, start + signature.len) or {
		return error('missing native contract boundary `${next_signature}`')
	}
	return source[start..end]
}

fn test_win32_d3d11_readback_bridge_validates_ready_and_failed_results() {
	ready_ok, ready := win32_d3d11_readback_bridge_result(Win32D3D11ReadbackNativeResult{
		request:             7
		state_identity:      11
		renderer_generation: 13
		window_slot:         17
		window_generation:   19
		submitted_frame:     23
		status:              win32_d3d11_readback_status_ready
		width:               3
		height:              2
		stride:              12
		byte_length:         24
	})
	assert ready_ok
	assert ready.request == 7
	assert ready.submitted_frame == 23
	assert ready.width == 3
	assert ready.height == 2
	assert ready.stride == 12
	assert ready.byte_length == 24

	failed_ok, failed := win32_d3d11_readback_bridge_result(Win32D3D11ReadbackNativeResult{
		request:             29
		state_identity:      31
		renderer_generation: 37
		window_slot:         41
		window_generation:   43
		status:              win32_d3d11_readback_status_failed
	})
	assert failed_ok
	assert failed.request == 29
	assert failed.submitted_frame == 0
	assert failed.width == 0
	assert failed.height == 0
	assert failed.stride == 0
	assert failed.byte_length == 0

	malformed_ok, _ := win32_d3d11_readback_bridge_result(Win32D3D11ReadbackNativeResult{
		request:             47
		state_identity:      53
		renderer_generation: 59
		window_slot:         61
		window_generation:   67
		submitted_frame:     71
		status:              win32_d3d11_readback_status_ready
		width:               3
		height:              2
		stride:              11
		byte_length:         22
	})
	assert !malformed_ok

	too_large_ok, _ := win32_d3d11_readback_bridge_result(Win32D3D11ReadbackNativeResult{
		request:             73
		state_identity:      79
		renderer_generation: 83
		window_slot:         89
		window_generation:   97
		submitted_frame:     101
		status:              win32_d3d11_readback_status_ready
		width:               3
		height:              2
		stride:              16
		byte_length:         32
	})
	assert !too_large_ok
}

fn test_win32_d3d11_readback_bridge_is_inert_without_native_state() {
	mut bridge := Win32D3D11ReadbackBridge{}
	assert !bridge.initialized()
	assert bridge.poll() == 0
	assert bridge.active() == 0
	assert bridge.last_hresult() == 0
	assert bridge.device_removed_reason() == 0
	assert !bridge.cancel(1)
	assert !bridge.stop()
	assert !bridge.destroy()
}

fn test_win32_d3d11_readback_bridge_releases_rejected_taken_result_once() {
	mut probe := Win32D3D11ReadbackTakeProbe{
		available: true
	}
	accepted, _ := win32_d3d11_readback_bridge_take_with_ops(voidptr(&probe),
		win32_d3d11_readback_take_probe, win32_d3d11_readback_release_probe)
	assert !accepted
	assert probe.take_calls == 1
	assert probe.release_calls == 1
	assert probe.released

	second_accepted, _ := win32_d3d11_readback_bridge_take_with_ops(voidptr(&probe),
		win32_d3d11_readback_take_probe, win32_d3d11_readback_release_probe)
	assert !second_accepted
	assert probe.take_calls == 2
	assert probe.release_calls == 1
}

fn test_win32_d3d11_readback_native_owner_guard_precedes_swapchain_access() {
	source := os.read_file(os.join_path(@VMODROOT, 'vlib', 'x', 'multiwindow',
		'win32_readback_d3d11_native.h'))!
	body := win32_d3d11_readback_native_contract_function(source,
		'static int v_multiwindow_win32_d3d11_readback_native_stage_window(',
		'static int v_multiwindow_win32_d3d11_readback_native_stage_image(')!
	owner_guard := body.index('v_multiwindow_win32_readback_is_owner(native->machine)') or {
		assert false, 'stage_window has no owner-thread guard'
		return
	}
	swapchain_access := body.index('v_multiwindow_win32_readback_swapchain_buffer(') or {
		assert false, 'stage_window has no swapchain GetBuffer path'
		return
	}
	assert owner_guard < swapchain_access
}

fn test_win32_d3d11_readback_reason_s_ok_preserves_primary_hresult() {
	mut primary := win32_d3d11_contract_e_fail
	mut reason := i32(-1)
	C.v_multiwindow_win32_readback_record_device_removed_reason(primary, &reason, 0)
	assert primary == win32_d3d11_contract_e_fail
	assert reason == 0
}

fn test_win32_d3d11_readback_device_loss_reason_is_separate_from_primary_hresult() {
	mut primary := win32_d3d11_contract_e_fail
	mut reason := i32(0)
	C.v_multiwindow_win32_readback_record_device_removed_reason(primary, &reason,
		win32_d3d11_contract_device_removed)
	assert primary == win32_d3d11_contract_e_fail
	assert reason == win32_d3d11_contract_device_removed
	assert reason != primary
}

fn test_win32_d3d11_readback_native_records_reason_without_replacing_primary_hresult() {
	source := os.read_file(os.join_path(@VMODROOT, 'vlib', 'x', 'multiwindow',
		'win32_readback_d3d11_native.h'))!
	body := win32_d3d11_readback_native_contract_function(source,
		'static int v_multiwindow_win32_readback_native_device_removed_reason(',
		'static const VMultiwindowWin32ReadbackOps')!
	assert source.contains('int32_t device_removed_reason;')
	assert source.contains('v_multiwindow_win32_d3d11_readback_native_device_removed_reason(')
	assert body.contains('v_multiwindow_win32_readback_record_device_removed_reason(')
	assert body.contains('native->last_hresult, &native->device_removed_reason')
	assert !body.contains('native->last_hresult =')
}
