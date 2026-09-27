module multiwindow

enum RenderWindowRuntimeStatus {
	invalid
	alive
	preparing_destroy
	sealed_destroy
	destroyed
	exhausted
}

// RenderBlockReason records the backend fact which withheld a ready credit.
pub enum RenderBlockReason {
	none
	no_workload
	not_configured
	frame_callback_pending
	hidden
	minimized
	occluded
	unmapped
	not_viewable
	zero_sized
	resize_pending
	drawable_unavailable
	backend_unavailable
	renderer_failed
}

pub enum RenderAcquireStatus {
	ready
	transient_unavailable
}

// RenderMetricsSnapshot contains only backend-observed values. Conversion is
// unavailable unless the backend can reproduce it for metrics_sequence.
pub struct RenderMetricsSnapshot {
pub:
	logical_width        f32
	logical_height       f32
	framebuffer_width    int
	framebuffer_height   int
	dpi_scale            f32
	metrics_sequence     u64
	metrics_available    bool
	conversion_available bool
}

pub struct RenderTargetSnapshot {
pub:
	target_identity u64
	color_format    int
	depth_format    int
	sample_count    int
}

struct BackendRenderUpdate {
	window       WindowId
	sequence     u64
	ready_credit bool
	block_reason RenderBlockReason
	metrics      RenderMetricsSnapshot
	target       RenderTargetSnapshot
}

struct RenderWindowRuntime {
mut:
	id                      WindowId
	status                  RenderWindowRuntimeStatus
	redraw_mode             RenderRedrawMode
	has_workload            bool
	pending_admission       bool
	pending_admission_id    u64
	pending_admission_epoch u64
	dirty_epoch             u64
	consumed_dirty_epoch    u64
	claimed_dirty_epoch     u64
	frame_serial            u64
	submitted_frame         u64
	lease_epoch             u64
	in_frame                bool
	batch_epoch             u64
	candidate_active        bool
	candidate_batch_epoch   u64
	candidate_frame_serial  u64
	candidate_metrics       RenderMetricsSnapshot
	candidate_target        RenderTargetSnapshot
	eligibility_sequence    u64
	ready_credit            bool
	ready_credit_consumed   bool
	block_reason            RenderBlockReason
	focus_known             bool
	focused                 bool
	minimized_known         bool
	minimized               bool
	metrics                 RenderMetricsSnapshot
	target                  RenderTargetSnapshot
}

struct RenderRuntimeState {
mut:
	windows                []RenderWindowRuntime
	next_epoch             u64 = 1
	next_admission_id      u64 = 1
	next_lease_epoch       u64 = 1
	next_batch_epoch       u64 = 1
	next_destroy_serial    u64 = 1
	next_teardown_sequence u64 = 1
	active_batch_epoch     u64
	batch_active           bool
	renderer_terminal      string
}

// RenderWindowSnapshot is immutable scheduler and backend state. Batch
// candidates expose the checked next serial without mutating persistent state.
pub struct RenderWindowSnapshot {
pub:
	window               WindowId
	redraw_mode          RenderRedrawMode
	dirty_epoch          u64
	consumed_epoch       u64
	frame_serial         u64
	submitted_frame      u64
	metrics              RenderMetricsSnapshot
	target               RenderTargetSnapshot
	eligibility_sequence u64
	block_reason         RenderBlockReason
	focus_known          bool
	focused              bool
	minimized_known      bool
	minimized            bool
	batch_epoch          u64
}

// RenderBatchLease is opaque authority for one owner-thread transaction.
pub struct RenderBatchLease {
	app_instance u64
	epoch        u64
	include_all  bool
}

// epoch_for_gg exposes immutable validation data without exposing scheduler
// mutation or finish authority.
pub fn (lease RenderBatchLease) epoch_for_gg() u64 {
	return lease.epoch
}

// RenderTargetLease identifies one acquired target without exposing its
// swapchain, drawable, command buffer, framebuffer, or native handles.
pub struct RenderTargetLease {
	app_instance u64
	batch_epoch  u64
	target_epoch u64
	window_epoch u64
	window       WindowId
}

pub struct RenderTargetAcquisition {
pub:
	status       RenderAcquireStatus
	lease        RenderTargetLease
	snapshot     RenderWindowSnapshot
	block_reason RenderBlockReason
}

pub struct RenderBatchOutcome {
	suppressed_callback_target  RenderTargetLease
	suppressed_callback_outcome NativeRenderResult
	suppressed_callback_message string
pub:
	batch_epoch           u64
	committed             bool
	had_gpu_work          bool
	completed_user_passes int
	finalized_submissions int
	error                 string
}

pub type RenderBatchFn = fn (RenderBatchLease, []RenderWindowSnapshot) !

pub type RenderPassFn = fn () !

enum WindowDestroyStage {
	none
	prepared
	sealed
	finished
}

// WindowDestroyTicket is opaque and app/generation/serial checked at every
// stage. Only an unsealed ticket can be rolled back.
pub struct WindowDestroyTicket {
	app_instance u64
	window       WindowId
	serial       u64
}

pub struct RenderTeardownNotice {
pub:
	window   WindowId
	snapshot RenderWindowSnapshot
	ticket   WindowDestroyTicket
}

pub struct AppStopTicket {
	app_instance u64
	serial       u64
}

// BackendTeardownNotice is a private backend-event ingress record. App never
// consumes it out of band; poll_queued_events converts it into the same ordered
// event stream used by native backends before durable slot acceptance.
struct BackendTeardownNotice {
	window WindowId
}
