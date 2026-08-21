module gg

import sokol.gfx

// WindowRedrawMode controls when a window is eligible for a render frame.
pub enum WindowRedrawMode {
	on_demand
	continuous
}

// WindowCleanupReason identifies why a window render lifetime is ending.
pub enum WindowCleanupReason {
	requested
	native_closed
	init_failed
	app_stop
	renderer_lost
}

// WindowReadbackStatus is the terminal state of an asynchronous readback.
pub enum WindowReadbackStatus {
	ready
	cancelled
	failed
}

// WindowLogicalSize is a drawable size in logical coordinates.
pub struct WindowLogicalSize {
pub:
	width  f32
	height f32
}

// WindowPixelSize is a drawable size in framebuffer pixels.
pub struct WindowPixelSize {
pub:
	width  int
	height int
}

// WindowLogicalRect is a region in logical coordinates.
pub struct WindowLogicalRect {
pub:
	x      f32
	y      f32
	width  f32
	height f32
}

// WindowPixelRect is a region in framebuffer coordinates.
pub struct WindowPixelRect {
pub:
	x      int
	y      int
	width  int
	height int
}

// WindowReadbackCapabilities reports current per-window path availability.
// Each request revalidates app ownership, same-window image scope,
// render-target/sample eligibility, and rectangle bounds.
pub struct WindowReadbackCapabilities {
pub:
	offscreen_image bool
	window_capture  bool
}

// WindowMetrics is an immutable accepted logical/framebuffer metrics snapshot.
pub struct WindowMetrics {
pub:
	logical_size     WindowLogicalSize
	framebuffer_size WindowPixelSize
	dpi_scale        f32
	metrics_sequence u64
	submitted_frame  u64
}

// WindowRenderTargetInfo describes a managed window target without exposing it.
pub struct WindowRenderTargetInfo {
pub:
	color_format gfx.PixelFormat
	depth_format gfx.PixelFormat
	sample_count int
}

// WindowFrameInfo binds one callback to a window, frame, metrics, and target snapshot.
pub struct WindowFrameInfo {
pub:
	window          WindowId
	frame_serial    u64
	submitted_frame u64
	metrics         WindowMetrics
	target          WindowRenderTargetInfo
}

// WindowBufferId is a generation-checked managed buffer identity.
pub struct WindowBufferId {
	app_instance u64
	slot         int
	generation   u32
	window       WindowId
}

// WindowImageId is a generation-checked managed image identity.
pub struct WindowImageId {
	app_instance u64
	slot         int
	generation   u32
	window       WindowId
}

// WindowSamplerId is a generation-checked managed sampler identity.
pub struct WindowSamplerId {
	app_instance u64
	slot         int
	generation   u32
	window       WindowId
}

// WindowShaderId is a generation-checked managed shader identity.
pub struct WindowShaderId {
	app_instance u64
	slot         int
	generation   u32
	window       WindowId
}

// WindowPipelineId is a generation-checked managed pipeline identity.
pub struct WindowPipelineId {
	app_instance u64
	slot         int
	generation   u32
	window       WindowId
}

// WindowAttachmentsId is a generation-checked managed attachments identity.
pub struct WindowAttachmentsId {
	app_instance u64
	slot         int
	generation   u32
	window       WindowId
}

// WindowSglPipelineId is a generation-checked managed SGL pipeline identity.
pub struct WindowSglPipelineId {
	app_instance u64
	slot         int
	generation   u32
	window       WindowId
}

// WindowReadbackId identifies one asynchronous terminal readback request.
pub struct WindowReadbackId {
	app_instance u64
	slot         int
	generation   u32
	window       WindowId
	serial       u64
}

// WindowAttachmentsConfig names managed image ids used by an offscreen pass.
pub struct WindowAttachmentsConfig {
pub:
	colors        []WindowImageId
	resolves      []WindowImageId
	depth_stencil ?WindowImageId
}

// WindowBufferBinding binds a managed buffer at one graphics slot.
pub struct WindowBufferBinding {
pub:
	slot   int
	buffer WindowBufferId
	offset int
}

// WindowImageBinding binds a managed image at one graphics slot.
pub struct WindowImageBinding {
pub:
	slot  int
	image WindowImageId
}

// WindowSamplerBinding binds a managed sampler at one graphics slot.
pub struct WindowSamplerBinding {
pub:
	slot    int
	sampler WindowSamplerId
}

// WindowStageBindings groups managed resource bindings for one shader stage.
pub struct WindowStageBindings {
pub:
	images          []WindowImageBinding
	samplers        []WindowSamplerBinding
	storage_buffers []WindowBufferBinding
}

// WindowBindings is the managed binding set accepted by WindowPassContext.
pub struct WindowBindings {
pub:
	vertex_buffers []WindowBufferBinding
	index_buffer   ?WindowBufferBinding
	vs             WindowStageBindings
	fs             WindowStageBindings
}

// WindowReadbackConfig selects a framebuffer-pixel region. A missing rect
// requests the full target; a present rect must be positive and fully contained.
pub struct WindowReadbackConfig {
pub:
	rect ?WindowPixelRect
}

// WindowReadbackResult is admitted and enqueued once with a terminal status.
// Dispatch can replay it until callback acknowledgment, so handlers must be
// idempotent. Ready results own top-left RGBA8 bytes and an explicit stride.
pub struct WindowReadbackResult {
pub:
	id              WindowReadbackId
	window          WindowId
	status          WindowReadbackStatus
	submitted_frame u64
	width           int
	height          int
	stride          int
	pixels_rgba8    []u8
	error           string
}

enum MultiWindowRenderPhase {
	invalid
	init
	frame
	cleanup
	native
	text
}

enum MultiWindowResourceScope {
	window
	app
}

enum MultiWindowResourceOperation {
	create
	update
	append
	replace
	retire
}

// WindowInitContext is a callback-bounded window initialization lease.
pub struct WindowInitContext {
	app          &App = unsafe { nil }
	app_instance u64
	lease_epoch  u64
	info         WindowFrameInfo
}

// WindowContext is a callback-bounded window frame lease.
pub struct WindowContext {
	app                        &App = unsafe { nil }
	app_instance               u64
	lease_epoch                u64
	info                       WindowFrameInfo
	compatibility_capabilities Capabilities
}

// WindowCleanupContext is a callback-bounded window cleanup lease.
pub struct WindowCleanupContext {
	app            &App = unsafe { nil }
	app_instance   u64
	lease_epoch    u64
	info           WindowFrameInfo
	cleanup_reason WindowCleanupReason
	has_graphics   bool
}

// WindowResourceContext bounds managed resource operations to a callback lease.
pub struct WindowResourceContext {
	app          &App = unsafe { nil }
	app_instance u64
	window       WindowId
	lease_epoch  u64
	batch_epoch  u64
	phase        MultiWindowRenderPhase
	scope        MultiWindowResourceScope
}

// WindowPassContext bounds draw recording to one managed pass.
pub struct WindowPassContext {
	app          &App = unsafe { nil }
	app_instance u64
	window       WindowId
	lease_epoch  u64
	pass_epoch   u64
	info         WindowFrameInfo
}

// WindowSglContext bounds immediate-mode recording to one managed pass.
pub struct WindowSglContext {
	app          &App = unsafe { nil }
	app_instance u64
	window       WindowId
	lease_epoch  u64
	pass_epoch   u64
	target_key   string
}

// AppResourceContext uses the same managed resource operations with app scope.
pub type AppResourceContext = WindowResourceContext

// NativeWindowLease is an opaque, callback-bounded native-service seam.
pub struct NativeWindowLease {
	app          &App = unsafe { nil }
	app_instance u64
	window       WindowId
	lease_epoch  u64
	backend      MultiWindowBackend
	primary      voidptr
	secondary    u64
}

// WindowInitFn initializes window-scoped managed resources.
pub type WindowInitFn = fn (mut WindowInitContext) !

// WindowFrameFn records one managed window frame.
pub type WindowFrameFn = fn (mut WindowContext) !

// WindowCleanupFn releases window-scoped resources during terminal cleanup.
pub type WindowCleanupFn = fn (mut WindowCleanupContext) !

// AppResourceInitFn initializes app-scoped managed resources.
pub type AppResourceInitFn = fn (mut AppResourceContext) !

// AppResourceFrameFn updates app-scoped managed resources during a batch.
pub type AppResourceFrameFn = fn (mut AppResourceContext) !

// AppResourceCleanupFn releases app-scoped managed resources.
pub type AppResourceCleanupFn = fn (mut AppResourceContext) !

// WindowReadbackFn receives ordered terminal readback results from App.run.
pub type WindowReadbackFn = fn (WindowReadbackResult, mut App) !

// WindowResourceFn receives callback-bounded managed resource authority.
pub type WindowResourceFn = fn (mut WindowResourceContext) !

// WindowPassFn records commands in one callback-bounded managed pass.
pub type WindowPassFn = fn (mut WindowPassContext) !

// WindowSglFn records SGL commands in one callback-bounded managed pass.
pub type WindowSglFn = fn (mut WindowSglContext) !

// NativeWindowBorrowFn receives a native lease that expires with the callback.
pub type NativeWindowBorrowFn = fn (mut NativeWindowLease) !
