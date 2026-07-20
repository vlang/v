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

pub struct WindowLogicalSize {
pub:
	width  f32
	height f32
}

pub struct WindowPixelSize {
pub:
	width  int
	height int
}

pub struct WindowLogicalRect {
pub:
	x      f32
	y      f32
	width  f32
	height f32
}

pub struct WindowPixelRect {
pub:
	x      int
	y      int
	width  int
	height int
}

pub struct WindowReadbackCapabilities {
pub:
	offscreen_image bool
	window_capture  bool
}

pub struct WindowMetrics {
pub:
	logical_size     WindowLogicalSize
	framebuffer_size WindowPixelSize
	dpi_scale        f32
	metrics_sequence u64
	submitted_frame  u64
}

pub struct WindowRenderTargetInfo {
pub:
	color_format gfx.PixelFormat
	depth_format gfx.PixelFormat
	sample_count int
}

pub struct WindowFrameInfo {
pub:
	window          WindowId
	frame_serial    u64
	submitted_frame u64
	metrics         WindowMetrics
	target          WindowRenderTargetInfo
}

pub struct WindowBufferId {
	app_instance u64
	slot         int
	generation   u32
	window       WindowId
}

pub struct WindowImageId {
	app_instance u64
	slot         int
	generation   u32
	window       WindowId
}

pub struct WindowSamplerId {
	app_instance u64
	slot         int
	generation   u32
	window       WindowId
}

pub struct WindowShaderId {
	app_instance u64
	slot         int
	generation   u32
	window       WindowId
}

pub struct WindowPipelineId {
	app_instance u64
	slot         int
	generation   u32
	window       WindowId
}

pub struct WindowAttachmentsId {
	app_instance u64
	slot         int
	generation   u32
	window       WindowId
}

pub struct WindowSglPipelineId {
	app_instance u64
	slot         int
	generation   u32
	window       WindowId
}

pub struct WindowReadbackId {
	app_instance u64
	slot         int
	generation   u32
	window       WindowId
}

pub struct WindowAttachmentsConfig {
pub:
	colors        []WindowImageId
	resolves      []WindowImageId
	depth_stencil ?WindowImageId
}

pub struct WindowBufferBinding {
pub:
	slot   int
	buffer WindowBufferId
	offset int
}

pub struct WindowImageBinding {
pub:
	slot  int
	image WindowImageId
}

pub struct WindowSamplerBinding {
pub:
	slot    int
	sampler WindowSamplerId
}

pub struct WindowStageBindings {
pub:
	images          []WindowImageBinding
	samplers        []WindowSamplerBinding
	storage_buffers []WindowBufferBinding
}

pub struct WindowBindings {
pub:
	vertex_buffers []WindowBufferBinding
	index_buffer   ?WindowBufferBinding
	vs             WindowStageBindings
	fs             WindowStageBindings
}

pub struct WindowReadbackConfig {
pub:
	rect ?WindowPixelRect
}

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
}

pub type WindowInitFn = fn (mut WindowInitContext) !

pub type WindowFrameFn = fn (mut WindowContext) !

pub type WindowCleanupFn = fn (mut WindowCleanupContext) !

pub type AppResourceInitFn = fn (mut AppResourceContext) !

pub type AppResourceFrameFn = fn (mut AppResourceContext) !

pub type AppResourceCleanupFn = fn (mut AppResourceContext) !

pub type WindowReadbackFn = fn (WindowReadbackResult, mut App) !

pub type WindowResourceFn = fn (mut WindowResourceContext) !

pub type WindowPassFn = fn (mut WindowPassContext) !

pub type WindowSglFn = fn (mut WindowSglContext) !

pub type NativeWindowBorrowFn = fn (mut NativeWindowLease) !
