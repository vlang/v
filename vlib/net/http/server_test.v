module http

import time

fn test_server_stop() ? {
	server := &Server{
		accept_timeout: 1 * time.second
	}
	mut watch := time.new_stopwatch(auto_start: true)
	t := go server.listen_and_serve()
	server.stop()
	t.wait() ?
	assert watch.elapsed() < 1 * time.second
}
