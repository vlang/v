// Tests for `-d prealloc_cleanup` scope destructors.
// Run with:
//   v -prealloc -gc none -d prealloc_cleanup test vlib/builtin/prealloc_cleanup_test.v

struct CleanupLog {
mut:
	calls int
	order [3]u8
}

fn cleanup_a(arg voidptr) {
	unsafe {
		mut log := &CleanupLog(arg)
		log.order[log.calls] = `a`
		log.calls++
	}
}

fn cleanup_b(arg voidptr) {
	unsafe {
		mut log := &CleanupLog(arg)
		log.order[log.calls] = `b`
		log.calls++
	}
}

fn cleanup_c(arg voidptr) {
	unsafe {
		mut log := &CleanupLog(arg)
		log.order[log.calls] = `c`
		log.calls++
	}
}

fn test_prealloc_scope_cleanup_is_lifo_and_once() {
	$if prealloc {
		$if prealloc_cleanup ? {
			mut log := CleanupLog{}
			unsafe {
				scope := prealloc_scope_begin()
				prealloc_scope_on_cleanup(scope, cleanup_a, voidptr(&log))
				prealloc_scope_on_cleanup(scope, cleanup_b, voidptr(&log))
				prealloc_scope_on_cleanup(scope, cleanup_c, voidptr(&log))
				prealloc_scope_end(scope)
			}
			assert log.calls == 3
			assert log.order == [u8(`c`), `b`, `a`]

			// Rewinding invalidates callback arguments; callbacks are dropped.
			mut reused_log := CleanupLog{}
			unsafe {
				reused := prealloc_scope_begin()
				prealloc_scope_on_cleanup(reused, cleanup_a, voidptr(&reused_log))
				prealloc_scope_leave(reused)
				assert prealloc_scope_reenter(reused, 0)
				prealloc_scope_end(reused)
			}
			assert reused_log.calls == 0

			// Abandoning transfers ownership; callbacks are dropped too.
			mut abandoned_log := CleanupLog{}
			unsafe {
				abandoned := prealloc_scope_begin()
				prealloc_scope_on_cleanup(abandoned, cleanup_a, voidptr(&abandoned_log))
				prealloc_scope_abandon(abandoned)
			}
			assert abandoned_log.calls == 0
		}
	}
}
