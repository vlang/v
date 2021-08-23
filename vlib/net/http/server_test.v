module http

import time

fn test_server_stop() ? {
	server := &Server{
		accept_timeout: 1 * time.second
	}
	t := go server.listen_and_serve()
	time.sleep(250 * time.millisecond)
	mut watch := time.new_stopwatch()
	server.stop()
	assert watch.elapsed() < 100 * time.millisecond
	t.wait() ?
	assert watch.elapsed() < 999 * time.millisecond
}
