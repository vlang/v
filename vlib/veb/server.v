module veb

import time

// ShutdownParams configures how graceful shutdown waits for in-flight requests.
@[params]
pub struct ShutdownParams {
pub:
	timeout         time.Duration = time.infinite
	retry_period_ms int           = 10
}

// WaitTillRunningParams configures how long the server waits to report that it is serving.
@[params]
pub struct WaitTillRunningParams {
pub:
	max_retries     int = 100
	retry_period_ms int = 10
}

interface HasInitServer {
mut:
	init_server(server &Server)
}

fn maybe_init_server[A](mut global_app A, server &Server) {
	$if A is HasInitServer {
		global_app.init_server(server)
	}
}
