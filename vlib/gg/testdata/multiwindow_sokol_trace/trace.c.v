module multiwindow_sokol_trace

import sokol.gfx

#insert "@VMODROOT/vlib/gg/testdata/multiwindow_sokol_trace/trace_helpers.h"

fn C.v_multiwindow_trace_install() int
fn C.v_multiwindow_trace_install_owned() u64
fn C.v_multiwindow_trace_uninstall()
fn C.v_multiwindow_trace_uninstall_owned(generation u64) int
fn C.v_multiwindow_trace_logical_release_owned(generation u64) int
fn C.v_multiwindow_trace_reset()
fn C.v_multiwindow_trace_snapshot() &char
fn C.v_multiwindow_typed_trace_snapshot_count() usize
fn C.v_multiwindow_typed_trace_snapshot_overflow() int
fn C.v_multiwindow_trace_active_install_generation() u64
fn C.v_multiwindow_typed_trace_snapshot_operation(index usize) int
fn C.v_multiwindow_typed_trace_snapshot_identity(index usize) u32
fn C.v_multiwindow_typed_trace_snapshot_value(index usize) i64
fn C.v_multiwindow_typed_trace_snapshot_sequence(index usize) u64
fn C.v_multiwindow_typed_trace_snapshot_swapchain_identity(index usize) u64
fn C.v_multiwindow_typed_trace_snapshot_width(index usize) int
fn C.v_multiwindow_typed_trace_snapshot_height(index usize) int
fn C.v_multiwindow_typed_trace_snapshot_sample_count(index usize) int
fn C.v_multiwindow_typed_trace_snapshot_color_format(index usize) int
fn C.v_multiwindow_typed_trace_snapshot_depth_format(index usize) int

pub enum Operation {
	invalid
	make_buffer
	make_image
	make_sampler
	make_shader
	make_pipeline
	make_attachments
	destroy_buffer
	destroy_image
	destroy_sampler
	destroy_shader
	destroy_pipeline
	destroy_attachments
	update_buffer
	append_buffer
	update_image
	begin_swapchain_pass
	begin_offscreen_pass
	end_pass
	commit
}

pub struct Record {
pub:
	operation          Operation
	identity           u32
	value              i64
	sequence           u64
	swapchain_identity u64
	width              int
	height             int
	sample_count       int
	color_format       int
	depth_format       int
}

pub struct TypedSnapshot {
pub:
	records            []Record
	count              usize
	overflow           bool
	install_generation u64
}

pub fn install() ! {
	_ = gfx.is_valid()
	if C.v_multiwindow_trace_install() == 0 {
		return error('multi-window Sokol trace hooks are unavailable')
	}
}

pub fn install_generation() !u64 {
	_ = gfx.is_valid()
	generation := C.v_multiwindow_trace_install_owned()
	if generation == 0 {
		return error('multi-window Sokol trace hooks are unavailable or already owned')
	}
	return generation
}

pub fn uninstall() {
	C.v_multiwindow_trace_uninstall()
}

pub fn uninstall_generation(generation u64) ! {
	if C.v_multiwindow_trace_uninstall_owned(generation) == 0 {
		return error('multi-window Sokol trace generation is stale')
	}
}

pub fn try_uninstall_generation(generation u64) bool {
	return C.v_multiwindow_trace_uninstall_owned(generation) != 0
}

// logical_release_generation_after_shutdown drops only test trace ownership.
// The caller has already shut gfx down, so reinstalling Sokol hooks is illegal.
pub fn logical_release_generation_after_shutdown(generation u64) ! {
	if C.v_multiwindow_trace_logical_release_owned(generation) == 0 {
		return error('multi-window Sokol trace generation is stale')
	}
}

pub fn active_generation() u64 {
	return C.v_multiwindow_trace_active_install_generation()
}

pub fn reset() {
	C.v_multiwindow_trace_reset()
}

pub fn snapshot() []string {
	value := unsafe { cstring_to_vstring(C.v_multiwindow_trace_snapshot()) }
	if value == '' {
		return []string{}
	}
	return value.split_into_lines()
}

pub fn typed_snapshot() TypedSnapshot {
	count := C.v_multiwindow_typed_trace_snapshot_count()
	mut records := []Record{cap: int(count)}
	for index in 0 .. count {
		records << Record{
			operation:          unsafe { Operation(C.v_multiwindow_typed_trace_snapshot_operation(index)) }
			identity:           C.v_multiwindow_typed_trace_snapshot_identity(index)
			value:              C.v_multiwindow_typed_trace_snapshot_value(index)
			sequence:           C.v_multiwindow_typed_trace_snapshot_sequence(index)
			swapchain_identity: C.v_multiwindow_typed_trace_snapshot_swapchain_identity(index)
			width:              C.v_multiwindow_typed_trace_snapshot_width(index)
			height:             C.v_multiwindow_typed_trace_snapshot_height(index)
			sample_count:       C.v_multiwindow_typed_trace_snapshot_sample_count(index)
			color_format:       C.v_multiwindow_typed_trace_snapshot_color_format(index)
			depth_format:       C.v_multiwindow_typed_trace_snapshot_depth_format(index)
		}
	}
	return TypedSnapshot{
		records:            records
		count:              count
		overflow:           C.v_multiwindow_typed_trace_snapshot_overflow() != 0
		install_generation: C.v_multiwindow_trace_active_install_generation()
	}
}
