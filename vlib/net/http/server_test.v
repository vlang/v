module http

import time

fn test_server_stop() ? {
	mut server := &Server{
		accept_timeout: 1 * time.second,
	}
	t := go server.listen_and_serve()
	time.sleep(250 * time.millisecond)
	mut watch := time.new_stopwatch()
	server.stop()
	assert server.status() == .server_stoped
	assert watch.elapsed() < 100 * time.millisecond
	t.wait() ?
	assert watch.elapsed() < 999 * time.millisecond
}

fn test_server_close() ? {
	mut server := &Server{
		accept_timeout: 1 * time.second
	}
	t := go server.listen_and_serve()
	time.sleep(250 * time.millisecond)
	mut watch := time.new_stopwatch()
	server.close()
	assert server.status() == .server_closed
	assert watch.elapsed() < 100 * time.millisecond
	t.wait() ?
	assert watch.elapsed() < 999 * time.millisecond
}
