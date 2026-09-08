module multiwindow

const service_clipboard_payload_capacity = u64(16 * 1024 * 1024)
const service_readback_payload_capacity = u64(256 * 1024 * 1024)
const service_clipboard_pending_capacity = 64
const service_readback_pending_capacity = 64
const service_portal_lease_capacity = 64

enum PendingServiceKind {
	clipboard_read
	clipboard_write
	portal_parent
}

struct ServiceWindowRecord {
	id WindowId
mut:
	owner         ?WindowId
	modal         bool
	state         ServiceWindowState
	metrics       RenderMetricsSnapshot
	borrow_epochs []u64
}

struct PendingServiceRequest {
	id     ServiceRequestId
	window WindowId
	kind   PendingServiceKind
mut:
	terminal      bool
	payload_bytes u64
}

struct PendingReadbackRequest {
	id ServiceReadbackId
mut:
	terminal      bool
	payload_bytes u64
}

struct ServicePortalLease {
	id     ServicePortalLeaseId
	window WindowId
}

struct ServiceRegistry {
mut:
	app_instance            u64
	backend                 BackendKind
	windows                 []ServiceWindowRecord
	monitors                []ServiceMonitorInfo
	next_request            u64 = 1
	next_borrow_epoch       u64 = 1
	clipboard_text          string
	pending                 []PendingServiceRequest
	readbacks               []PendingReadbackRequest
	portal_leases           []ServicePortalLease
	clipboard_payload_bytes u64
	readback_payload_bytes  u64
}

fn payload_resize_fits(total u64, current u64, desired u64, capacity u64) bool {
	if current > total || desired > capacity {
		return false
	}
	if desired <= current {
		return true
	}
	return total <= capacity - (desired - current)
}

fn (registry &ServiceRegistry) clipboard_pending_count() int {
	mut count := 0
	for request in registry.pending {
		if request.kind in [.clipboard_read, .clipboard_write] {
			count++
		}
	}
	return count
}

fn (mut registry ServiceRegistry) resize_pending_service_payload(index int, desired u64) bool {
	if index < 0 || index >= registry.pending.len {
		return false
	}
	current := registry.pending[index].payload_bytes
	if !payload_resize_fits(registry.clipboard_payload_bytes, current, desired,
		service_clipboard_payload_capacity) {
		return false
	}
	if desired >= current {
		registry.clipboard_payload_bytes += desired - current
	} else {
		registry.clipboard_payload_bytes -= current - desired
	}
	registry.pending[index].payload_bytes = desired
	return true
}

fn (mut registry ServiceRegistry) resize_pending_readback_payload(index int, desired u64) bool {
	if index < 0 || index >= registry.readbacks.len {
		return false
	}
	current := registry.readbacks[index].payload_bytes
	if !payload_resize_fits(registry.readback_payload_bytes, current, desired,
		service_readback_payload_capacity) {
		return false
	}
	if desired >= current {
		registry.readback_payload_bytes += desired - current
	} else {
		registry.readback_payload_bytes -= current - desired
	}
	registry.readbacks[index].payload_bytes = desired
	return true
}

fn (mut registry ServiceRegistry) release_pending_service_payload(index int) {
	if index < 0 || index >= registry.pending.len {
		return
	}
	bytes := registry.pending[index].payload_bytes
	if bytes > registry.clipboard_payload_bytes {
		// A corrupt per-request charge must fail closed. Keeping the aggregate
		// occupied is safer than reopening capacity that may still own storage.
		return
	}
	registry.clipboard_payload_bytes -= bytes
	registry.pending[index].payload_bytes = 0
}

fn (mut registry ServiceRegistry) release_pending_readback_payload(index int) {
	if index < 0 || index >= registry.readbacks.len {
		return
	}
	bytes := registry.readbacks[index].payload_bytes
	if bytes > registry.readback_payload_bytes {
		// Preserve the aggregate charge on invariant failure for the same
		// fail-closed reason as clipboard payload accounting.
		return
	}
	registry.readback_payload_bytes -= bytes
	registry.readbacks[index].payload_bytes = 0
}

fn new_service_registry(app_instance u64, backend BackendKind) ServiceRegistry {
	if backend != .mock {
		return ServiceRegistry{
			app_instance: app_instance
			backend:      backend
		}
	}
	monitor_id := ServiceMonitorId{
		app_instance: app_instance
		slot:         0
		generation:   1
	}
	return ServiceRegistry{
		app_instance: app_instance
		backend:      backend
		monitors:     [
			ServiceMonitorInfo{
				native_key: ServiceMonitorNativeKey{
					kind: .mock
					text: 'mock-primary'
				}
				id:         monitor_id
				name:       'mock-primary'
				geometry:   ServiceKnownRect{
					known: true
					value: ServiceRect{
						width:  1920
						height: 1080
					}
				}
				work_area:  ServiceKnownRect{
					known: true
					value: ServiceRect{
						width:  1920
						height: 1040
					}
				}
				scale:      ServiceKnownScale{
					known: true
					value: 1.0
				}
				primary:    .on
				available:  true
			},
		]
	}
}

fn (registry &ServiceRegistry) window_index(id WindowId) !int {
	if id.app_instance != registry.app_instance {
		return error(err_app_identity_mismatch)
	}
	for index, record in registry.windows {
		if record.id == id {
			return index
		}
	}
	return error(err_stale_window)
}

fn (registry &ServiceRegistry) monitor_index(id ServiceMonitorId) !int {
	if id.app_instance != registry.app_instance {
		return error(err_app_identity_mismatch)
	}
	for index, monitor in registry.monitors {
		if monitor.id == id {
			return index
		}
	}
	return error(err_service_request_stale)
}

fn (mut registry ServiceRegistry) replace_monitors(monitors []ServiceMonitorInfo) {
	if registry.monitor_snapshot_identity_valid(monitors) {
		registry.monitors = monitors.clone()
	}
}

fn (key ServiceMonitorNativeKey) valid() bool {
	return match key.kind {
		.mock { key.numeric == 0 && key.text == 'mock-primary' }
		.x11_atom, .wayland_global, .appkit_display { key.numeric != 0 && key.text == '' }
		.win32_device { key.numeric == 0 && key.text != '' }
		.invalid { false }
	}
}

fn service_monitor_native_kind_for_backend(backend BackendKind) ServiceMonitorNativeKind {
	return match backend {
		.mock { .mock }
		.x11 { .x11_atom }
		.wayland { .wayland_global }
		.appkit { .appkit_display }
		.win32 { .win32_device }
		.auto { .invalid }
	}
}

fn service_monitor_snapshot_identity_valid(snapshot []ServiceMonitorInfo, backend BackendKind, app_instance u64) bool {
	if backend == .auto || app_instance == 0 {
		return false
	}
	expected_kind := service_monitor_native_kind_for_backend(backend)
	for index, candidate in snapshot {
		if !candidate.native_key.valid() || candidate.native_key.kind != expected_kind
			|| candidate.id.app_instance != app_instance || candidate.id.slot < 0
			|| candidate.id.generation == 0 {
			return false
		}
		for previous in 0 .. index {
			if snapshot[previous].native_key == candidate.native_key
				|| snapshot[previous].id == candidate.id
				|| snapshot[previous].id.slot == candidate.id.slot {
				return false
			}
		}
	}
	return true
}

fn (registry &ServiceRegistry) monitor_snapshot_identity_valid(snapshot []ServiceMonitorInfo) bool {
	return service_monitor_snapshot_identity_valid(snapshot, registry.backend,
		registry.app_instance)
}

fn service_monitor_info_for_slot(info ServiceMonitorInfo, app_instance u64, slot int, generation u32, available bool, sequence u64) ServiceMonitorInfo {
	return ServiceMonitorInfo{
		native_key: info.native_key
		id:         ServiceMonitorId{
			app_instance: app_instance
			slot:         slot
			generation:   generation
		}
		name:       info.name
		geometry:   info.geometry
		work_area:  info.work_area
		scale:      info.scale
		primary:    info.primary
		available:  available
		sequence:   sequence
	}
}

fn (mut registry ServiceRegistry) reconcile_monitor_snapshot(snapshot []ServiceMonitorInfo, sequence u64) ?[]ServiceMonitorInfo {
	if !registry.monitor_snapshot_identity_valid(snapshot) || snapshot.any(!it.available) {
		return none
	}
	mut staged := registry.monitors.clone()
	mut seen := []bool{len: staged.len}
	mut record_indices := []int{len: snapshot.len, init: -1}
	for snapshot_index, candidate in snapshot {
		for index, current in staged {
			if !seen[index] && current.native_key == candidate.native_key && current.available {
				if registry.backend != .x11 && candidate.id != current.id {
					return none
				}
				record_indices[snapshot_index] = index
				seen[index] = true
				break
			}
		}
	}
	for snapshot_index, candidate in snapshot {
		if record_indices[snapshot_index] >= 0 {
			continue
		}
		mut exact_reusable := false
		for index, current in staged {
			if !seen[index] && current.native_key == candidate.native_key && !current.available
				&& current.id.generation < max_u32 {
				exact_reusable = true
				expected := ServiceMonitorId{
					app_instance: registry.app_instance
					slot:         current.id.slot
					generation:   current.id.generation + 1
				}
				if registry.backend == .x11 || candidate.id == expected {
					record_indices[snapshot_index] = index
					seen[index] = true
					break
				}
			}
		}
		if registry.backend != .x11 && exact_reusable && record_indices[snapshot_index] < 0 {
			return none
		}
	}
	for snapshot_index, candidate in snapshot {
		if record_indices[snapshot_index] >= 0 {
			continue
		}
		if registry.backend == .x11 {
			for index, current in staged {
				if !seen[index] && !current.available && current.id.generation < max_u32 {
					record_indices[snapshot_index] = index
					seen[index] = true
					break
				}
			}
			continue
		}
		for index, current in staged {
			if !seen[index] && !current.available && current.id.generation < max_u32
				&& candidate.id.slot == current.id.slot
				&& candidate.id.generation == current.id.generation + 1 {
				record_indices[snapshot_index] = index
				seen[index] = true
				break
			}
		}
		if record_indices[snapshot_index] < 0 {
			known_slot := staged.any(it.id.slot == candidate.id.slot)
			known_key := staged.any(it.native_key == candidate.native_key)
			if known_slot
				|| (candidate.id.generation != 1 && (registry.backend != .wayland || known_key)) {
				return none
			}
		}
	}
	for snapshot_index, candidate in snapshot {
		mut record_index := record_indices[snapshot_index]
		if record_index < 0 {
			mut public_slot := candidate.id.slot
			mut generation := candidate.id.generation
			if registry.backend == .x11 {
				public_slot = staged.len
				generation = 1
				for staged.any(it.id.slot == public_slot) {
					public_slot++
				}
			}
			staged << service_monitor_info_for_slot(candidate, registry.app_instance, public_slot,
				generation, true, sequence)
			seen << true
			continue
		}
		current := staged[record_index]
		generation := if current.available {
			current.id.generation
		} else {
			current.id.generation + 1
		}
		staged[record_index] = service_monitor_info_for_slot(candidate, registry.app_instance,
			current.id.slot, generation, true, sequence)
		seen[record_index] = true
	}
	for index, current in staged {
		if index < seen.len && !seen[index] && current.available {
			staged[index] = service_monitor_info_for_slot(current, registry.app_instance,
				current.id.slot, current.id.generation, false, sequence)
		}
	}
	mut available := []ServiceMonitorInfo{}
	for monitor in staged {
		if monitor.available {
			available << monitor
		}
	}
	registry.monitors = staged
	return available
}

fn (mut registry ServiceRegistry) register_window(id WindowId, config WindowConfig, size WindowSize, mock bool) {
	monitor_ids := if mock && registry.monitors.len > 0 {
		[registry.monitors[0].id]
	} else {
		[]ServiceMonitorId{}
	}
	registry.windows << ServiceWindowRecord{
		id:      id
		owner:   config.owner
		modal:   config.modal
		state:   ServiceWindowState{
			mapping:                     if mock {
				if config.visible {
					ServiceMappingState.mapped
				} else {
					ServiceMappingState.unmapped
				}
			} else {
				ServiceMappingState.unknown
			}
			visibility:                  if mock {
				if config.visible {
					ServiceVisibilityState.visible
				} else {
					ServiceVisibilityState.hidden
				}
			} else {
				ServiceVisibilityState.unknown
			}
			active:                      if mock { .off } else { .unknown }
			focused:                     if mock { .off } else { .unknown }
			minimized:                   if mock { .off } else { .unknown }
			maximized:                   if mock { .off } else { .unknown }
			fullscreen:                  if mock {
				if config.fullscreen { ServiceObservedBool.on } else { ServiceObservedBool.off }
			} else {
				ServiceObservedBool.unknown
			}
			mouse_locked:                if mock { .off } else { .unknown }
			position:                    ServicePosition{}
			monitor_ids:                 monitor_ids
			monitor_membership_observed: mock
		}
		metrics: if mock {
			RenderMetricsSnapshot{
				logical_width:        f32(size.width)
				logical_height:       f32(size.height)
				framebuffer_width:    size.width
				framebuffer_height:   size.height
				dpi_scale:            1.0
				metrics_available:    true
				conversion_available: true
			}
		} else {
			RenderMetricsSnapshot{}
		}
	}
}

fn (registry &ServiceRegistry) child_first_all() ![]WindowId {
	mut order := []WindowId{}
	mut visiting := map[string]bool{}
	mut visited := map[string]bool{}
	for record in registry.windows {
		if record.owner == none {
			registry.append_child_first(record.id, mut visiting, mut visited, mut order)!
		}
	}
	for record in registry.windows {
		registry.append_child_first(record.id, mut visiting, mut visited, mut order)!
	}
	return order
}

fn (mut registry ServiceRegistry) remove_window(id WindowId) ! {
	index := registry.window_index(id)!
	if registry.windows[index].borrow_epochs.len != 0 {
		return error(err_native_borrow_active)
	}
	registry.windows.delete(index)
	mut retained_leases := []ServicePortalLease{cap: registry.portal_leases.len}
	for lease in registry.portal_leases {
		if lease.window != id {
			retained_leases << lease
		}
	}
	registry.portal_leases = retained_leases
}

fn (registry &ServiceRegistry) ensure_no_active_borrows(id WindowId) ! {
	index := registry.window_index(id)!
	if registry.windows[index].borrow_epochs.len != 0 {
		return error(err_native_borrow_active)
	}
}

fn (registry &ServiceRegistry) child_first_order(root WindowId) ![]WindowId {
	registry.window_index(root)!
	mut order := []WindowId{}
	mut visiting := map[string]bool{}
	mut visited := map[string]bool{}
	registry.append_child_first(root, mut visiting, mut visited, mut order)!
	return order
}

fn (registry &ServiceRegistry) append_child_first(id WindowId, mut visiting map[string]bool, mut visited map[string]bool, mut order []WindowId) ! {
	key := id.str()
	if visiting[key] {
		return error(err_owner_relation_invalid)
	}
	if visited[key] {
		return
	}
	registry.window_index(id)!
	visiting[key] = true
	for record in registry.windows {
		if owner := record.owner {
			if owner == id {
				registry.append_child_first(record.id, mut visiting, mut visited, mut order)!
			}
		}
	}
	visiting.delete(key)
	visited[key] = true
	order << id
}

fn (registry &ServiceRegistry) validate_owner(owner ?WindowId) ! {
	if configured := owner {
		registry.window_index(configured)!
		mut current := configured
		mut seen := map[string]bool{}
		for {
			key := current.str()
			if seen[key] {
				return error(err_owner_relation_invalid)
			}
			seen[key] = true
			index := registry.window_index(current)!
			next := registry.windows[index].owner or { break }
			current = next
		}
	}
}

fn (mut registry ServiceRegistry) take_request_id() !ServiceRequestId {
	serial := registry.next_request
	if serial == 0 {
		return error(err_service_request_exhausted)
	}
	registry.next_request = if serial == u64(0xffffffffffffffff) { u64(0) } else { serial + 1 }
	return ServiceRequestId{
		app_instance: registry.app_instance
		serial:       serial
	}
}

fn (mut registry ServiceRegistry) take_readback_id(window WindowId) !ServiceReadbackId {
	request := registry.take_request_id()!
	return ServiceReadbackId{
		app_instance: request.app_instance
		serial:       request.serial
		window:       window
	}
}

fn (mut registry ServiceRegistry) take_borrow_epoch() !u64 {
	epoch := registry.next_borrow_epoch
	if epoch == 0 {
		return error(err_service_request_exhausted)
	}
	registry.next_borrow_epoch = if epoch == u64(0xffffffffffffffff) { u64(0) } else { epoch + 1 }
	return epoch
}
