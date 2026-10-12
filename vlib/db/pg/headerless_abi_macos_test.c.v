module pg

#flag @VEXEROOT/vlib/db/pg/testdata/headerless_abi.o

fn C.v_pg_notify_size() usize
fn C.v_pg_notify_pid_offset() usize
fn C.v_pg_notify_payload_offset() usize
fn C.v_pg_notify_next_offset() usize

fn test_notify_layout_matches_libpq() {
	assert usize(sizeof(C.PGnotify)) == C.v_pg_notify_size()
	assert usize(__offsetof(C.PGnotify, be_pid)) == C.v_pg_notify_pid_offset()
	assert usize(__offsetof(C.PGnotify, extra)) == C.v_pg_notify_payload_offset()
	assert usize(__offsetof(C.PGnotify, next)) == C.v_pg_notify_next_offset()
}
