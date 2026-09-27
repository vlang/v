module multiwindow

// ServiceSupportLevel reports whether one runtime service can be attempted.
pub enum ServiceSupportLevel {
	unsupported
	available
	conditional
}

// ServiceObservedBool distinguishes an unreported value from observed false/true.
pub enum ServiceObservedBool {
	unknown
	off
	on
}

// ServiceMappingState reports native window-hierarchy mapping.
pub enum ServiceMappingState {
	unknown
	unmapped
	mapped
}

// ServiceVisibilityState is the latest native visibility observation.
pub enum ServiceVisibilityState {
	unknown
	hidden
	visible
	occluded
}

// ServiceOperation identifies an optional runtime native-window service.
pub enum ServiceOperation {
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

// ServiceEventKind selects the meaningful payload in ServiceEvent.
pub enum ServiceEventKind {
	state
	metrics
	capability
	monitor
	clipboard
	portal_parent
}

// ServiceStatus is the terminal state of an asynchronous service request.
pub enum ServiceStatus {
	ready
	cancelled
	failed
}

// ServiceTitlebarAppearance requests a system, light, or dark native titlebar.
pub enum ServiceTitlebarAppearance {
	system
	light
	dark
}

// ServiceOperationCapability is the authoritative per-window runtime result for
// one operation. asynchronous does not promise a later queued result;
// state_observable says whether callers can rely on a resulting state
// observation.
pub struct ServiceOperationCapability {
pub:
	support              ServiceSupportLevel
	asynchronous         bool
	requires_user_action bool
	state_observable     bool
}

// ServiceMonitorId is an opaque generation-checked monitor identity.
pub struct ServiceMonitorId {
	app_instance u64
	slot         int
	generation   u32
}

// str returns a diagnostic representation of a ServiceMonitorId.
pub fn (id ServiceMonitorId) str() string {
	return 'ServiceMonitorId(${id.app_instance}:${id.slot}:${id.generation})'
}

// app_instance_for_gg is an internal gg-facade bridge; not user API.
pub fn (id ServiceMonitorId) app_instance_for_gg() u64 {
	return id.app_instance
}

// slot_for_gg is an internal gg-facade bridge; not user API.
pub fn (id ServiceMonitorId) slot_for_gg() int {
	return id.slot
}

// generation_for_gg is an internal gg-facade bridge; not user API.
pub fn (id ServiceMonitorId) generation_for_gg() u32 {
	return id.generation
}

// service_monitor_id_from_gg is an internal gg-facade bridge; not user API.
pub fn service_monitor_id_from_gg(app_instance u64, slot int, generation u32) ServiceMonitorId {
	return ServiceMonitorId{
		app_instance: app_instance
		slot:         slot
		generation:   generation
	}
}

// ServicePosition carries coordinates only when known is true.
pub struct ServicePosition {
pub:
	known bool
	x     int
	y     int
}

// ServiceRect is an integer native monitor rectangle.
pub struct ServiceRect {
pub:
	x      int
	y      int
	width  int
	height int
}

// ServiceKnownRect distinguishes unavailable geometry from zeroes.
pub struct ServiceKnownRect {
pub:
	known bool
	value ServiceRect
}

// ServiceKnownScale distinguishes an unavailable scale from zero.
pub struct ServiceKnownScale {
pub:
	known bool
	value f32
}

enum ServiceMonitorNativeKind {
	invalid
	mock
	x11_atom
	wayland_global
	appkit_display
	win32_device
}

struct ServiceMonitorNativeKey {
	kind    ServiceMonitorNativeKind
	numeric u64
	text    string
}

// ServiceMonitorInfo is one immutable monitor-generation observation. name is
// descriptive; id is the identity and sequence orders accepted updates.
pub struct ServiceMonitorInfo {
	native_key ServiceMonitorNativeKey
pub:
	id        ServiceMonitorId
	name      string
	geometry  ServiceKnownRect
	work_area ServiceKnownRect
	scale     ServiceKnownScale
	primary   ServiceObservedBool
	available bool
	sequence  u64
}

// ServiceWindowState is the latest observed state for one live window. Unknown
// values were not reported by the backend.
pub struct ServiceWindowState {
pub:
	mapping      ServiceMappingState
	visibility   ServiceVisibilityState
	active       ServiceObservedBool
	focused      ServiceObservedBool
	minimized    ServiceObservedBool
	maximized    ServiceObservedBool
	fullscreen   ServiceObservedBool
	mouse_locked ServiceObservedBool
	position     ServicePosition
	monitor_ids  []ServiceMonitorId
	// monitor_membership_observed distinguishes an observed empty membership
	// from a partial state observation which did not report monitors.
	monitor_membership_observed bool
	sequence                    u64
}

// ServiceRequestId identifies one accepted asynchronous service request.
pub struct ServiceRequestId {
	app_instance u64
	serial       u64
}

// str returns a diagnostic representation of a ServiceRequestId.
pub fn (id ServiceRequestId) str() string {
	return 'ServiceRequestId(${id.app_instance}:${id.serial})'
}

// app_instance_for_gg is an internal gg-facade bridge; not user API.
pub fn (id ServiceRequestId) app_instance_for_gg() u64 {
	return id.app_instance
}

// serial_for_gg is an internal gg-facade bridge; not user API.
pub fn (id ServiceRequestId) serial_for_gg() u64 {
	return id.serial
}

// ServicePortalLeaseId owns a ready portal export until release or window/app
// teardown invalidates it.
pub struct ServicePortalLeaseId {
	app_instance u64
	serial       u64
}

// app_instance_for_gg is an internal gg-facade bridge; not user API.
pub fn (id ServicePortalLeaseId) app_instance_for_gg() u64 {
	return id.app_instance
}

// serial_for_gg is an internal gg-facade bridge; not user API.
pub fn (id ServicePortalLeaseId) serial_for_gg() u64 {
	return id.serial
}

// service_portal_lease_id_from_gg is an internal gg-facade bridge; not user API.
pub fn service_portal_lease_id_from_gg(app_instance u64, serial u64) ServicePortalLeaseId {
	return ServicePortalLeaseId{
		app_instance: app_instance
		serial:       serial
	}
}

// ServiceClipboardResult is a terminal clipboard result matched by request id.
pub struct ServiceClipboardResult {
pub:
	id     ServiceRequestId
	window WindowId
	status ServiceStatus
	text   string
	error  string
}

// ServicePortalParentResult is a terminal portal export result. A ready event
// already queued before teardown can carry a stale lease.
pub struct ServicePortalParentResult {
pub:
	id         ServiceRequestId
	window     WindowId
	status     ServiceStatus
	lease      ServicePortalLeaseId
	identifier string
	error      string
}

// ServiceEvent carries one native state/capability/monitor observation or one
// terminal asynchronous service result.
pub struct ServiceEvent {
pub:
	kind          ServiceEventKind
	window        WindowId
	sequence      u64
	state         ServiceWindowState
	metrics       RenderMetricsSnapshot
	operation     ServiceOperation
	capability    ServiceOperationCapability
	monitor       ServiceMonitorInfo
	monitors      []ServiceMonitorInfo
	clipboard     ServiceClipboardResult
	portal_parent ServicePortalParentResult
}

// ServiceReadbackStatus is the terminal state of one readback request.
pub enum ServiceReadbackStatus {
	ready
	cancelled
	failed
}

// ServiceReadbackId identifies one window-scoped asynchronous readback.
pub struct ServiceReadbackId {
	app_instance u64
	serial       u64
	window       WindowId
}

// app_instance_for_gg is an internal gg-facade bridge; not user API.
pub fn (id ServiceReadbackId) app_instance_for_gg() u64 {
	return id.app_instance
}

// serial_for_gg is an internal gg-facade bridge; not user API.
pub fn (id ServiceReadbackId) serial_for_gg() u64 {
	return id.serial
}

// window_for_gg is an internal gg-facade bridge; not user API.
pub fn (id ServiceReadbackId) window_for_gg() WindowId {
	return id.window
}

// ServiceReadbackResult is admitted and enqueued once. Dispatch can replay it
// until callback acknowledgment, so handlers must be idempotent. Ready results
// own top-left RGBA8 bytes and an explicit stride; cancelled/failed results
// carry no pixels.
pub struct ServiceReadbackResult {
pub:
	id              ServiceReadbackId
	window          WindowId
	status          ServiceReadbackStatus
	submitted_frame u64
	width           int
	height          int
	stride          int
	pixels_rgba8    []u8
	error           string
}

// NativeWindowBackend identifies the native handle shape in a scoped borrow.
pub enum NativeWindowBackend {
	mock
	x11
	wayland
	appkit
	win32
}

// NativeWindowBorrow is valid only inside the gg facade's synchronous borrow
// callback. Its handles and epoch must never escape that callback.
pub struct NativeWindowBorrow {
	app_instance u64
	window       WindowId
	epoch        u64
	backend      NativeWindowBackend
	primary      voidptr
	secondary    u64
}

// app_instance_for_gg is an internal gg-facade bridge; not user API.
pub fn (borrow NativeWindowBorrow) app_instance_for_gg() u64 {
	return borrow.app_instance
}

// window_for_gg is an internal gg-facade bridge; not user API.
pub fn (borrow NativeWindowBorrow) window_for_gg() WindowId {
	return borrow.window
}

// epoch_for_gg is an internal gg-facade bridge; not user API.
pub fn (borrow NativeWindowBorrow) epoch_for_gg() u64 {
	return borrow.epoch
}

// backend_for_gg is an internal gg-facade bridge; not user API.
pub fn (borrow NativeWindowBorrow) backend_for_gg() NativeWindowBackend {
	return borrow.backend
}

// primary_for_gg is an internal gg-facade bridge; not user API.
pub fn (borrow NativeWindowBorrow) primary_for_gg() voidptr {
	return borrow.primary
}

// secondary_for_gg is an internal gg-facade bridge; not user API.
pub fn (borrow NativeWindowBorrow) secondary_for_gg() u64 {
	return borrow.secondary
}

// NativeWindowBorrowCallback is the callback-bounded facade bridge for a borrow.
pub type NativeWindowBorrowCallback = fn (NativeWindowBorrow) !

fn service_window_state_with_sequence(state ServiceWindowState, sequence u64) ServiceWindowState {
	return ServiceWindowState{
		mapping:                     state.mapping
		visibility:                  state.visibility
		active:                      state.active
		focused:                     state.focused
		minimized:                   state.minimized
		maximized:                   state.maximized
		fullscreen:                  state.fullscreen
		mouse_locked:                state.mouse_locked
		position:                    state.position
		monitor_ids:                 state.monitor_ids.clone()
		sequence:                    sequence
		monitor_membership_observed: state.monitor_membership_observed
	}
}

fn service_window_state_with_observed_monitor_membership(state ServiceWindowState) ServiceWindowState {
	return ServiceWindowState{
		...state
		monitor_ids:                 state.monitor_ids.clone()
		monitor_membership_observed: true
	}
}

fn service_window_state_observes_monitor_membership(state ServiceWindowState) bool {
	return state.monitor_membership_observed || state.monitor_ids.len != 0
}

fn service_monitor_info_with_sequence(info ServiceMonitorInfo, sequence u64) ServiceMonitorInfo {
	return ServiceMonitorInfo{
		native_key: info.native_key
		id:         info.id
		name:       info.name
		geometry:   info.geometry
		work_area:  info.work_area
		scale:      info.scale
		primary:    info.primary
		available:  info.available
		sequence:   sequence
	}
}

fn service_event_with_sequence(event ServiceEvent, sequence u64) ServiceEvent {
	mut monitors := []ServiceMonitorInfo{cap: event.monitors.len}
	for monitor in event.monitors {
		monitors << service_monitor_info_with_sequence(monitor, sequence)
	}
	return ServiceEvent{
		kind:          event.kind
		window:        event.window
		sequence:      sequence
		state:         service_window_state_with_sequence(event.state, sequence)
		metrics:       RenderMetricsSnapshot{
			...event.metrics
			metrics_sequence: sequence
		}
		operation:     event.operation
		capability:    event.capability
		monitor:       service_monitor_info_with_sequence(event.monitor, sequence)
		monitors:      monitors
		clipboard:     event.clipboard
		portal_parent: event.portal_parent
	}
}
