import net
import sync { Mutex }
import time

fn server_thread(mut mtx Mutex) {
	mut buf := []u8{len: 128}
	mut times := 0
	mut listener := net.listen_tcp(.ip, ':8901') or { panic(err) }
	mut server := listener.accept() or { panic(err) }
	server.read_nb(mut buf) or { // nothing can be read yet
		assert err.code() == net.error_ewouldblock
	}
	mtx.@lock() // wait for the master thread write data
	for times < 10 {
		times++
		time.sleep(1 * time.millisecond)
		read_len := server.read_nb(mut buf) or {
			if err.code() == net.error_ewouldblock {
				continue
			} else {
				panic(err)
			}
		}
		if read_len > 0 {
			break
		}
	}
	assert unsafe { tos_clone(&buf[0]) == 'hello' }
}

fn test_non_blocking_read() {
	mut mtx := sync.new_mutex()
	mtx.@lock()
	server := spawn server_thread(mut mtx)
	mut conn := net.dial_tcp('localhost:8901') or { panic(err) }
	conn.write_nb('hello'.bytes()) or { panic(err) }
	mtx.unlock()
	server.wait()
}
