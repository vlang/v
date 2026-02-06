// vtest flaky: true
// vtest retry: 3
import os
import log
import time
import net.unix

const tfolder = os.join_path(os.temp_dir(), 'unix_socket_${os.getpid()}')
const socket_path = os.join_path(tfolder, 'v_unix.sock')

fn testsuite_begin() {
	os.mkdir_all(tfolder) or {}
}

fn testsuite_end() {
	os.rmdir_all(tfolder) or {}
}

fn handle_conn(mut c unix.StreamConn) {
	log.warn('    handle_conn ${voidptr(c)} start')
	for {
		mut buf := []u8{len: 100, init: 0}
		log.warn('    handle_conn ${voidptr(c)}, reading data...')
		read := c.read(mut buf) or {
			log.warn('    handle_conn ${voidptr(c)}, reading failed, connection was dropped.')
			return
		}
		log.warn('    handle_conn ${voidptr(c)}, read: ${read} bytes')
		written := c.write(buf[..read]) or {
			log.warn('    handle_conn ${voidptr(c)}, writing failed, connection was dropped.')
			return
		}
		log.warn('    handle_conn ${voidptr(c)}, written: ${written} bytes')
	}
}

fn echo_server(mut l unix.StreamListener) ! {
	log.warn('echo_server l: ${voidptr(l)}, start')
	for {
		log.warn('echo_server l: ${voidptr(l)}, waiting for connections...')
		mut new_conn := l.accept() or { continue }
		log.warn('echo_server l: ${voidptr(l)}, accepted new connection: ${voidptr(new_conn)}, spawning a new thread to handle it...')
		spawn handle_conn(mut new_conn)
		time.sleep(10 * time.millisecond)
	}
}

fn echo() ! {
	log.info('echo: connecting to ${socket_path}...')
	mut c := unix.connect_stream(socket_path)!
	defer {
		log.info('echo: closing ${voidptr(c)} ...')
		c.close() or {}
	}
	data := 'Hello from vlib/net!'
	log.info('echo: writing `${data}` to ${voidptr(c)} ...')
	c.write_string(data)!
	mut buf := []u8{len: 4096}
	read := c.read(mut buf)!
	log.info('echo: read ${read} bytes from ${voidptr(c)} ...')
	assert read == data.len
	for i := 0; i < read; i++ {
		assert buf[i] == data[i]
	}
	log.info('echo: got `${buf.bytestr()}` back from ${voidptr(c)}.')
	return
}

fn test_tcp() {
	log.use_stdout()
	unbuffer_stdout()
	os.rm(socket_path) or {}
	assert os.exists(socket_path) == false
	log.info('${@LOCATION}, setup complete')

	log.info('>>> create listening socket at ${socket_path}...')
	mut l := unix.listen_stream(socket_path) or { panic(err) }
	log.info('>>> listening socket at ${socket_path} is ${voidptr(l)}.')
	println('')

	log.info('>>> spawning server with connection ${voidptr(l)}...')
	spawn echo_server(mut l)
	println('')

	for i in 0 .. 3 {
		log.info('>>> start echo to server, i: ${i}...')
		echo()!
		println('')
	}

	log.info('>>> closing listening socket ${voidptr(l)}...')
	l.close() or {}
	println('')

	// test if socket file is removed/unlinked
	assert os.exists(socket_path) == false

	log.info('${@LOCATION}, done')
}
