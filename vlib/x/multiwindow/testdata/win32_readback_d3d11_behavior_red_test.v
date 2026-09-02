module main

#include "@VMODROOT/vlib/x/multiwindow/testdata/win32_readback_d3d11_fake_com.h"

fn C.v_multiwindow_win32_d3d11_readback_probe_addref_release() int
fn C.v_multiwindow_win32_d3d11_readback_probe_query_progression() int
fn C.v_multiwindow_win32_d3d11_readback_probe_row_pitch_rgba() int
fn C.v_multiwindow_win32_d3d11_readback_probe_producing_frame() int
fn C.v_multiwindow_win32_d3d11_readback_probe_two_windows() int
fn C.v_multiwindow_win32_d3d11_readback_probe_resize_pending() int
fn C.v_multiwindow_win32_d3d11_readback_probe_device_lost() int
fn C.v_multiwindow_win32_d3d11_readback_probe_poll_before_resolve() int
fn C.v_multiwindow_win32_d3d11_readback_probe_map_still_drawing() int
fn C.v_multiwindow_win32_d3d11_readback_probe_rgba_passthrough() int
fn C.v_multiwindow_win32_d3d11_readback_probe_destroy_direct() int
fn C.v_multiwindow_win32_d3d11_readback_probe_targeted_resolve() int
fn C.v_multiwindow_win32_d3d11_readback_probe_operation_journal() int
fn C.v_multiwindow_win32_d3d11_readback_probe_subregion_canaries() int
fn C.v_multiwindow_win32_d3d11_readback_probe_resize_after_submit() int
fn C.v_multiwindow_win32_d3d11_readback_probe_device_loss_latch() int
fn C.v_multiwindow_win32_d3d11_readback_probe_lifecycle_matrix() int
fn C.v_multiwindow_win32_d3d11_readback_probe_shared_machine() int
fn C.v_multiwindow_win32_d3d11_readback_probe_restart_teardown() int

fn test_d3d11_readback_retains_and_releases_com_sources_exactly_once() {
	assert C.v_multiwindow_win32_d3d11_readback_probe_addref_release() == 1, 'D3D11 readback must retain through staging, release the source before public delivery, and free every native object once'
}

fn test_d3d11_readback_waits_for_s_ok_after_s_false_without_mapping_early() {
	assert C.v_multiwindow_win32_d3d11_readback_probe_query_progression() == 1, 'D3D11_QUERY_EVENT must remain pending on S_FALSE and map only after S_OK'
}

fn test_d3d11_readback_uses_row_pitch_and_compacts_odd_widths_to_rgba8() {
	assert C.v_multiwindow_win32_d3d11_readback_probe_row_pitch_rgba() == 1, 'padded RowPitch for widths 1, 3, and 257 must become compact top-left RGBA8'
}

fn test_d3d11_readback_reports_the_exact_producing_frame() {
	assert C.v_multiwindow_win32_d3d11_readback_probe_producing_frame() == 1, 'only the matching successful submission may publish its producing frame'
}

fn test_d3d11_readback_keeps_two_windows_and_generations_isolated() {
	assert C.v_multiwindow_win32_d3d11_readback_probe_two_windows() == 1, 'two windows must retain separate source, generation, result, and pixels'
}

fn test_d3d11_readback_rejects_resize_pending_and_quiesces_stale_generation() {
	assert C.v_multiwindow_win32_d3d11_readback_probe_resize_pending() == 1, 'resize-pending windows cannot admit capture and old generation work must be reaped'
}

fn test_d3d11_readback_device_loss_is_single_failed_result_for_each_boundary() {
	assert C.v_multiwindow_win32_d3d11_readback_probe_device_lost() == 1, 'Present, ResizeBuffers, GetData, and Map device loss must each fail once with frame zero'
}

fn test_d3d11_readback_never_polls_gpu_before_targeted_resolve() {
	assert C.v_multiwindow_win32_d3d11_readback_probe_poll_before_resolve() == 1, 'stage alone must not call GetData or Map or expose a result before targeted resolve'
}

fn test_d3d11_readback_retries_map_was_still_drawing_without_terminalizing() {
	assert C.v_multiwindow_win32_d3d11_readback_probe_map_still_drawing() == 1, 'DXGI_ERROR_WAS_STILL_DRAWING must remain pending until a later Map succeeds'
}

fn test_d3d11_readback_preserves_rgba_sources_without_channel_permutation() {
	assert C.v_multiwindow_win32_d3d11_readback_probe_rgba_passthrough() == 1, 'RGBA8 input must remain RGBA8 while only BGRA8 input is channel-swapped'
}

fn test_d3d11_readback_direct_destroy_reaps_active_pending_and_ready_once() {
	assert C.v_multiwindow_win32_d3d11_readback_probe_destroy_direct() == 1, 'direct destroy must release active, pending, and ready native resources exactly once'
}

fn test_d3d11_readback_resolve_is_scoped_to_window_and_generation() {
	assert C.v_multiwindow_win32_d3d11_readback_probe_targeted_resolve() == 1, 'same-frame success and failure must be independently resolved for each window generation'
}

fn test_d3d11_readback_native_operation_order_is_stable() {
	assert C.v_multiwindow_win32_d3d11_readback_probe_operation_journal() == 1, 'retain, describe, copy, query, poll, map, unmap, and release must preserve their native order'
}

fn test_d3d11_readback_subregion_copy_is_complete_bounded_and_retryable() {
	assert C.v_multiwindow_win32_d3d11_readback_probe_subregion_canaries() == 1, 'non-zero x/y readback must preserve all pixels, reject short output, and keep canaries intact'
}

fn test_d3d11_readback_submitted_copy_survives_resize_while_pending() {
	assert C.v_multiwindow_win32_d3d11_readback_probe_resize_after_submit() == 1, 'a staging copy pending on S_FALSE must survive ResizeBuffers and later become ready'
}

fn test_d3d11_readback_device_loss_latches_per_renderer_until_restart() {
	assert C.v_multiwindow_win32_d3d11_readback_probe_device_loss_latch() == 1, 'device loss must fail all local requests, reject admission until restart, and not contaminate another renderer'
}

fn test_d3d11_readback_cancel_quiesce_destroy_and_stop_cover_every_phase() {
	assert C.v_multiwindow_win32_d3d11_readback_probe_lifecycle_matrix() == 1, 'cancel, quiesce, destroy, and stop must reap active, pending, and ready records exactly once'
}

fn test_d3d11_readback_test_wrappers_use_the_production_state_machine() {
	assert C.v_multiwindow_win32_d3d11_readback_probe_shared_machine() == 1, 'fake-COM wrappers must expose the same state-machine identity used by native production wrappers'
}

fn test_d3d11_readback_restart_destroy_and_stop_reap_exactly_once() {
	assert C.v_multiwindow_win32_d3d11_readback_probe_restart_teardown() == 1, 'restart and stop must quiesce old work without a native cancellation terminal'
}
