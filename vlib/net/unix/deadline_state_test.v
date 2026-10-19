module unix

import os
import time

fn test_deadline_getters_agree_with_wait_state() ! {
	mut conn := StreamConn{}
	mut listener := StreamListener{}
	for i, deadline in [time.Time{}, time.unix(0), time.unix(-1), time.unix_nanosecond(0, 1),
		time.now().add(time.second)] {
		conn.set_read_deadline(deadline)
		conn.set_write_deadline(deadline)
		listener.set_accept_deadline(deadline)
		if i < 2 {
			if actual := conn.read_deadline() {
				assert false, 'unset read deadline returned ${actual}'
			}
			if actual := conn.write_deadline() {
				assert false, 'unset write deadline returned ${actual}'
			}
			if actual := listener.accept_deadline() {
				assert false, 'unset accept deadline returned ${actual}'
			}
		} else {
			assert conn.read_deadline()! == deadline
			assert conn.write_deadline()! == deadline
			assert listener.accept_deadline()! == deadline
		}
	}
}

fn connect_after_deadline_chunk_delay(path string) {
	time.sleep(50 * time.millisecond)
	mut client := connect_stream(path) or { panic(err) }
	client.close() or { panic(err) }
}

fn test_select_deadline_retries_chunks_and_preserves_unset_sentinels() ! {
	future := time.parse_iso8601('9999-12-31T23:59:59Z')!
	for i, deadline in [time.Time{}, time.unix(0), future] {
		path := os.join_path(os.vtmp_dir(), 'v_unix_chunk_${os.getpid()}_${i}.sock')
		os.rm(path) or {}
		mut listener := listen_stream(path)!
		defer {
			listener.close() or {}
			os.rm(path) or {}
		}
		connector := spawn connect_after_deadline_chunk_delay(path)
		ready := select_deadline_chunked(listener.sock.handle, .read, deadline,
			5 * time.millisecond)!
		connector.wait()
		assert ready
	}
}
