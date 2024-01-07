import net
import sync { Mutex }
import time

fn server_thread(mut server_ready_mtx Mutex, mut client_write_mtx Mutex) {
	mut buf := []u8{len: 128}
	mut times := 0
	mut listener := net.listen_tcp(.ip, ':22444') or { panic(err) }
	server_ready_mtx.unlock()
	mut server := listener.accept() or { panic(err) }
	server.read_nb(mut buf) or { // nothing can be read yet
		assert err.code() == net.error_ewouldblock
	}
	client_write_mtx.@lock() // wait for the client thread write data
	for times < 10 {
		times++
		time.sleep(1 * time.millisecond)
		read_len := server.read_nb(mut buf) or {
			if err.code() == int(net.error_ewouldblock) {
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
	mut server_ready_mtx := sync.new_mutex()
	mut client_write_mtx := sync.new_mutex()
	client_write_mtx.@lock()
	server_ready_mtx.@lock()
	server := spawn server_thread(mut server_ready_mtx, mut client_write_mtx)
	server_ready_mtx.@lock()
	mut conn := net.dial_tcp('127.0.0.1:22444') or { panic(err) }
	conn.write_nb('hello'.bytes()) or { panic(err) }
	client_write_mtx.unlock()
	server.wait()
}
