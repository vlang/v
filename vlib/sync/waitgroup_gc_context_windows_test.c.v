module sync

#flag @VEXEROOT/vlib/sync/testdata/windows_waitgroup_gc_context.c

fn C.v_sync_test_pending_gc_context() int

fn test_waitgroup_keeps_queued_arguments_visible_to_gc() {
	$if gcboehm ? {
		assert C.v_sync_test_pending_gc_context() == 0
	}
}
