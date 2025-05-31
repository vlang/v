import net
import time

fn server_thread(c_chan chan int) {
	errors_no_data := [net.err_timed_out.code(), int(net.error_ewouldblock), int(net.error_eagain),
		int(net.error_eintr)]
	mut buf := []u8{len: 128}
	mut times := 0
	mut read_len := 0
	mut listener := net.listen_tcp(.ip, ':22444') or { panic(err) }
	c_chan <- 1
	mut server := listener.accept() or { panic(err) }
	server.set_read_timeout(2 * time.second)
	server.set_blocking(false) or { panic(err) }
	read_len = server.read(mut buf) or { // nothing can be read yet
		assert err.code() in errors_no_data
		-1
	}
	assert read_len == -1 // ensure there is an error with no data
	read_len = server.read(mut buf) or { // nothing can be read yet
		assert err.code() in errors_no_data
		-2
	}
	assert read_len == -2 // ensure there is an error with no data
	c_chan <- 2
	for times < 10 {
		times++
		time.sleep(1 * time.millisecond)
		read_len = server.read(mut buf) or {
			if err.code() in errors_no_data {
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
	mut c_chan := chan int{cap: 1}
	server := spawn server_thread(c_chan)
	_ := <-c_chan // 1
	mut conn := net.dial_tcp('127.0.0.1:22444') or { panic(err) }
	conn.set_blocking(false) or { panic(err) }
	_ := <-c_chan // 2
	conn.write('hello'.bytes()) or { panic(err) }
	server.wait()
}
