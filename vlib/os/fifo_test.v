import os
import rand
import time

fn fifo_path() string {
	$if windows {
		return 'v_test_fifo_' + rand.ulid()
	} $else {
		return os.join_path(os.vtmp_dir(), 'v_test_fifo_' + rand.ulid())
	}
}

// connect_client retries until the server endpoint exists (the server thread
// may not have created it yet when the client starts).
fn connect_client(path string) !&os.Fifo {
	deadline := time.ticks() + 5000
	for {
		if mut c := os.fifo_client(path) {
			return c
		} else {
			if time.ticks() > deadline {
				return error('client: timed out waiting for server')
			}
			time.sleep(10 * time.millisecond)
		}
	}
	return error('unreachable')
}

fn test_fifo_roundtrip() {
	path := fifo_path()
	$if !windows {
		defer {
			os.rm(path) or {}
		}
	}
	got_ch := chan string{cap: 1}
	defer {
		got_ch.close()
	}
	spawn fn [path, got_ch] () {
		mut srv := os.fifo_server(path, 0o600) or { panic('server: ${err}') }
		defer {
			srv.close()
		}
		mut buf := []u8{len: 64}
		n := srv.read(mut buf) or { panic('read: ${err}') }
		got_ch <- buf[..n].bytestr()
	}()

	mut cli := connect_client(path) or { panic('client: ${err}') }
	defer {
		cli.close()
	}
	cli.write('ping'.bytes()) or { panic('write: ${err}') }
	got := <-got_ch
	assert got == 'ping'
}

fn test_fifo_missing_client_does_not_create_file() {
	path := fifo_path()
	os.fifo_client(path) or {
		$if !windows {
			assert !os.exists(path)
		}
		return
	}
	assert false, 'a missing endpoint must fail'
}

fn test_fifo_client_rejects_regular_file_without_truncating() {
	$if !windows {
		path := fifo_path()
		os.write_file(path, 'keep this data')!
		defer { os.rm(path) or {} }
		os.fifo_client(path) or {
			assert os.read_file(path)! == 'keep this data'
			return
		}
		assert false, 'a regular file is not a fifo endpoint'
	}
}
