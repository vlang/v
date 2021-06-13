import os
import os.notify
import time

// make a pipe and return the (read, write) file descriptors
fn make_pipe() ?(int, int) {
	$if linux {
		pipefd := [2]int{}
		if C.pipe(&pipefd[0]) != 0 {
			return error('error $C.errno: ' + os.posix_get_error_msg(C.errno))
		}
		return pipefd[0], pipefd[1]
	}
	return -1, -1
}

fn test_notify_level_trigger() ? {
	// currently only linux is supported
	$if linux {
		mut notifier := notify.new() ?
		reader, writer := make_pipe() ?
		defer {
			os.fd_close(reader)
			os.fd_close(writer)
			notifier.close() or {}
		}
		notifier.add(reader, .read) ?

		os.fd_write(writer, 'foobar')
		mut events := notifier.wait(10 * time.millisecond)
		assert events.len == 1
		assert events[0].fd == reader
		assert events[0].kind.has(.read)
		mut s, mut n := os.fd_read(reader, 3)
		assert n == 3
		assert s == 'foo'

		events = notifier.wait(10 * time.millisecond)
		assert events.len == 1
		assert events[0].fd == reader
		assert events[0].kind.has(.read)
		s, n = os.fd_read(reader, 10)
		assert n == 3
		assert s == 'bar'

		assert notifier.wait(0).len == 0
	}
}

fn test_notify_edge_trigger() ? {
	// currently only linux is supported
	$if linux {
		mut notifier := notify.new() ?
		reader, writer := make_pipe() ?
		defer {
			os.fd_close(reader)
			os.fd_close(writer)
			notifier.close() or {}
		}
		notifier.add(reader, .read, .edge_trigger) ?

		os.fd_write(writer, 'foobar')
		events := notifier.wait(10 * time.millisecond)
		assert events.len == 1
		assert events[0].fd == reader
		assert events[0].kind.has(.read)
		s, n := os.fd_read(reader, 3)
		assert n == 3
		assert s == 'foo'

		assert notifier.wait(0).len == 0
	}
}
