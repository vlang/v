// vtest flaky: true
// vtest retry: 3
import os
import os.notify

// make a pipe and return the (read, write) file descriptors
fn make_pipe() !(int, int) {
	$if linux || macos {
		pipefd := [2]int{}
		if C.pipe(&pipefd[0]) != 0 {
			return error('error ${C.errno}: ' + os.posix_get_error_msg(C.errno))
		}
		return pipefd[0], pipefd[1]
	}
	return -1, -1
}

fn test_level_trigger() {
	// currently only linux and macos are supported
	$if linux || macos {
		mut notifier := notify.new()!
		reader, writer := make_pipe()!
		defer {
			os.fd_close(reader)
			os.fd_close(writer)
			notifier.close() or {}
		}
		notifier.add(reader, .read)!

		os.fd_write(writer, 'foobar')
		mut n := &notifier
		check_read_event(mut n, reader, 'foo')
		check_read_event(mut n, reader, 'bar')

		assert notifier.wait(0).len == 0
	}
}

fn test_edge_trigger() {
	// currently only linux and macos are supported
	$if linux || macos {
		mut notifier := notify.new()!
		reader, writer := make_pipe()!
		defer {
			os.fd_close(reader)
			os.fd_close(writer)
			notifier.close() or {}
		}
		notifier.add(reader, .read, .edge_trigger)!

		mut n := &notifier

		os.fd_write(writer, 'foobar')
		check_read_event(mut n, reader, 'foo')

		$if linux {
			assert notifier.wait(0).len == 0
		}
		$if macos {
			/*
			In the kqueue of macos, EV_CLEAR flag represents a clear event,
			which is mainly used for pipeline and socket class events. When this flag is set,
			kqueue will trigger the corresponding event when the data is readable or writable,
			but it is not guaranteed that the event will only be triggered once.
			Compared to EPOLLET, EV_CLEAR's behavior varies. In epoll, the edge triggered mode only triggers
			an event once when the state changes from unreadable/non writable to readable/writable,
			that is, when the data changes from unreadable to readable,
			or when the data changes from unreadable to writable. In the kqueue of macos,
			EV_CLEAR does not possess this precise edge triggering behavior.
			Therefore, in the kqueue of macos, even if the data is not completely read,
			it is possible to continue triggering read events. This means that if you don't process all the data,
			the next kqueue event notification may still be triggered
			*/

			// notifier.wait(0).len == 1 or 0
		}

		os.fd_write(writer, 'baz')
		// we do not get an event because there is still data
		// to be read
		// assert notifier.wait(0).len == 0
		// TODO: investigage why the above assert suddenly started failing on the latest Ubuntu kernel update:
		// 5.11.0-37-generic #41~20.04.2-Ubuntu SMP Fri Sep 24 09:06:38 UTC 2021 x86_64 x86_64 x86_64 GNU/Linux
	}
}

fn test_one_shot() {
	$if linux || macos {
		mut notifier := notify.new()!
		reader, writer := make_pipe()!
		defer {
			os.fd_close(reader)
			os.fd_close(writer)
			notifier.close() or {}
		}
		notifier.add(reader, .read, .one_shot)!

		mut n := &notifier

		os.fd_write(writer, 'foobar')
		check_read_event(mut n, reader, 'foo')
		os.fd_write(writer, 'baz')

		assert notifier.wait(0).len == 0

		// rearm
		notifier.modify(reader, .read)!
		check_read_event(mut n, reader, 'barbaz')
	}
}

// Kqueue does not support 'hangup' event type.
fn test_hangup() {
	$if linux {
		mut notifier := notify.new()!
		reader, writer := make_pipe()!
		defer {
			os.fd_close(reader)
			notifier.close() or {}
		}
		notifier.add(reader, .hangup)!

		assert notifier.wait(0).len == 0

		// closing on the writer end of the pipe will
		// cause a hangup on the reader end
		os.fd_close(writer)
		events := notifier.wait(0)
		assert events.len == 1
		assert events[0].fd == reader
		assert events[0].kind.has(.hangup)
	}
}

fn test_write() {
	$if linux || macos {
		mut notifier := notify.new()!
		reader, writer := make_pipe()!
		defer {
			os.fd_close(reader)
			os.fd_close(writer)
			notifier.close() or {}
		}

		notifier.add(reader, .write)!
		assert notifier.wait(0).len == 0

		notifier.add(writer, .write)!
		events := notifier.wait(0)
		assert events.len == 1
		assert events[0].fd == writer
		assert events[0].kind.has(.write)
	}
}

fn test_remove() {
	$if linux || macos {
		mut notifier := notify.new()!
		reader, writer := make_pipe()!
		defer {
			os.fd_close(reader)
			os.fd_close(writer)
			notifier.close() or {}
		}

		// level triggered - will keep getting events while
		// there is data to read
		notifier.add(reader, .read)!
		os.fd_write(writer, 'foobar')
		assert notifier.wait(0).len == 1
		assert notifier.wait(0).len == 1

		notifier.remove(reader)!
		assert notifier.wait(0).len == 0
	}
}

fn check_read_event(mut notifier notify.FdNotifier, reader_fd int, expected string) {
	events := notifier.wait(0)
	assert events.len == 1
	assert events[0].fd == reader_fd
	assert events[0].kind.has(.read)
	s, _ := os.fd_read(events[0].fd, expected.len)
	assert s == expected
}
