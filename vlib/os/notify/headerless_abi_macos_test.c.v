module notify

fn C.v_notify_kevent_size() usize
fn C.v_notify_kevent_filter_offset() usize
fn C.v_notify_kevent_data_offset() usize
fn C.v_notify_kevent_udata_offset() usize

fn test_kevent_layout_matches_macos_sdk() {
	assert usize(sizeof(C.kevent)) == C.v_notify_kevent_size()
	assert usize(__offsetof(C.kevent, filter)) == C.v_notify_kevent_filter_offset()
	assert usize(__offsetof(C.kevent, data)) == C.v_notify_kevent_data_offset()
	assert usize(__offsetof(C.kevent, udata)) == C.v_notify_kevent_udata_offset()
}
