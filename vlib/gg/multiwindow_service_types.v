module gg

// WindowSupportLevel reports whether a native operation is usable for one
// live window on the selected backend.
pub enum WindowSupportLevel {
	unsupported
	available
	conditional
}

// WindowObservedBool distinguishes an observed false value from unavailable
// native state.
pub enum WindowObservedBool {
	unknown
	off
	on
}

// WindowMappingState reports whether a native window is attached to the
// platform's visible window hierarchy.
pub enum WindowMappingState {
	unknown
	unmapped
	mapped
}

// WindowVisibilityState is the latest native visibility observation.
pub enum WindowVisibilityState {
	unknown
	hidden
	visible
	occluded
}

// WindowOperation identifies a runtime native-window service.
pub enum WindowOperation {
	show
	hide
	focus
	raise
	position
	minimize
	maximize
	restore
	fullscreen
	clipboard_read
	clipboard_write
	portal_parent
	native_borrow
	mouse_lock
	titlebar_appearance
	image_readback
	window_capture
}

// WindowServiceEventKind selects the payload carried by WindowServiceEvent.
pub enum WindowServiceEventKind {
	state
	metrics
	capability
	monitor
	clipboard
	portal_parent
}

// WindowServiceStatus is the terminal status of an asynchronous service request.
pub enum WindowServiceStatus {
	ready
	cancelled
	failed
}

// WindowTitlebarAppearance requests the platform default or a light/dark
// native titlebar where the backend exposes such an operation.
pub enum WindowTitlebarAppearance {
	system
	light
	dark
}

// WindowOperationCapability is the authoritative per-window runtime answer for
// one optional operation. asynchronous does not promise a later queued result;
// state_observable says whether callers can rely on a resulting state
// observation. Conditional operations can still require user action.
pub struct WindowOperationCapability {
pub:
	support              WindowSupportLevel
	asynchronous         bool
	requires_user_action bool
	state_observable     bool
}

// WindowMonitorId is an opaque generation-checked monitor identity.
pub struct WindowMonitorId {
	app_instance u64
	slot         int
	generation   u32
}

// str returns a diagnostic monitor identity without exposing mutable fields.
pub fn (id WindowMonitorId) str() string {
	return 'WindowMonitorId(${id.app_instance}:${id.slot}:${id.generation})'
}

// WindowPosition carries a native position only when known is true.
pub struct WindowPosition {
pub:
	known bool
	x     int
	y     int
}

// WindowRect is an integer native monitor rectangle.
pub struct WindowRect {
pub:
	x      int
	y      int
	width  int
	height int
}

// WindowKnownRect distinguishes an unavailable monitor rectangle from zeroes.
pub struct WindowKnownRect {
pub:
	known bool
	value WindowRect
}

// WindowKnownScale distinguishes an unavailable scale from zero.
pub struct WindowKnownScale {
pub:
	known bool
	value f32
}

// WindowMonitorInfo is an immutable observation for one monitor generation.
// Names are descriptive; WindowMonitorId is the identity.
pub struct WindowMonitorInfo {
pub:
	id        WindowMonitorId
	name      string
	geometry  WindowKnownRect
	work_area WindowKnownRect
	scale     WindowKnownScale
	primary   WindowObservedBool
	available bool
	sequence  u64
}

// WindowState is the latest native observation for one live window. Unknown
// fields were not reported by the backend; sequence orders accepted updates.
pub struct WindowState {
pub:
	mapping      WindowMappingState
	visibility   WindowVisibilityState
	active       WindowObservedBool
	focused      WindowObservedBool
	minimized    WindowObservedBool
	maximized    WindowObservedBool
	fullscreen   WindowObservedBool
	mouse_locked WindowObservedBool
	position     WindowPosition
	monitor_ids  []WindowMonitorId
	// monitor_membership_observed distinguishes an observed empty membership
	// from a partial state observation which did not report monitors.
	monitor_membership_observed bool
	sequence                    u64
}

// ClipboardRequestId identifies one accepted asynchronous clipboard request.
pub struct ClipboardRequestId {
	app_instance u64
	serial       u64
}

// str returns a diagnostic clipboard request identity.
pub fn (id ClipboardRequestId) str() string {
	return 'ClipboardRequestId(${id.app_instance}:${id.serial})'
}

// PortalParentRequestId identifies one accepted native-parent request.
pub struct PortalParentRequestId {
	app_instance u64
	serial       u64
}

// str returns a diagnostic portal-parent request identity.
pub fn (id PortalParentRequestId) str() string {
	return 'PortalParentRequestId(${id.app_instance}:${id.serial})'
}

// PortalParentLeaseId identifies a portal parent export until release or
// window/app teardown invalidates it.
pub struct PortalParentLeaseId {
	app_instance u64
	serial       u64
}

// str returns a diagnostic portal-parent lease identity.
pub fn (id PortalParentLeaseId) str() string {
	return 'PortalParentLeaseId(${id.app_instance}:${id.serial})'
}

// ClipboardResult is the terminal result matched to ClipboardRequestId.
pub struct ClipboardResult {
pub:
	id     ClipboardRequestId
	window WindowId
	status WindowServiceStatus
	text   string
	error  string
}

// PortalParentResult is the terminal result matched to PortalParentRequestId.
// A ready event already queued before teardown can carry a stale lease.
pub struct PortalParentResult {
pub:
	id         PortalParentRequestId
	window     WindowId
	status     WindowServiceStatus
	lease      PortalParentLeaseId
	identifier string
	error      string
}

// WindowServiceEvent carries one native service observation or terminal result.
// kind selects the meaningful payload field.
pub struct WindowServiceEvent {
pub:
	kind          WindowServiceEventKind
	window        WindowId
	sequence      u64
	state         WindowState
	metrics       WindowMetrics
	operation     WindowOperation
	capability    WindowOperationCapability
	monitor       WindowMonitorInfo
	monitors      []WindowMonitorInfo
	clipboard     ClipboardResult
	portal_parent PortalParentResult
}

// WindowQueuedEventKind identifies one of the four canonical queue families.
pub enum WindowQueuedEventKind {
	lifecycle
	input
	service
	readback
}

// WindowQueuedEvent is the canonical ordered delivery envelope for lifecycle,
// input, service, and readback events. sequence preserves global admission order.
pub struct WindowQueuedEvent {
pub:
	kind      WindowQueuedEventKind
	sequence  u64
	lifecycle WindowEvent
	input     WindowInputEvent
	service   WindowServiceEvent
	readback  WindowReadbackResult
}

// Win32NativeWindowFn receives a borrowed HWND valid only for the callback.
pub type Win32NativeWindowFn = fn (hwnd voidptr) !

// AppKitNativeWindowFn receives a borrowed NSWindow pointer valid only for the callback.
pub type AppKitNativeWindowFn = fn (ns_window voidptr) !

// X11NativeWindowFn receives borrowed Display and Window handles valid only for the callback.
pub type X11NativeWindowFn = fn (display voidptr, window u64) !

// WaylandNativeWindowFn receives borrowed wl_display and wl_surface pointers valid only for the callback.
pub type WaylandNativeWindowFn = fn (display voidptr, surface voidptr) !

// WindowServiceFn is called by App.run for ordered native service events.
pub type WindowServiceFn = fn (event WindowServiceEvent, mut app App) !
