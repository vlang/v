module veb

import fasthttp

@[heap]
pub struct Server {
	handle            fasthttp.ServerHandle
	lifecycle_control bool
}

fn new_server_with_lifecycle(handle fasthttp.ServerHandle) &Server {
	return &Server{
		handle:            handle
		lifecycle_control: true
	}
}

fn new_server_without_lifecycle() &Server {
	return &Server{}
}

fn (s &Server) ensure_lifecycle_control() ! {
	if !s.lifecycle_control {
		return error('veb server lifecycle control requires `-d new_veb` without SSL')
	}
}

// wait_till_running waits until the server starts accepting requests.
pub fn (s &Server) wait_till_running(params WaitTillRunningParams) !int {
	s.ensure_lifecycle_control()!
	return s.handle.wait_till_running(fasthttp.WaitTillRunningParams{
		max_retries:     params.max_retries
		retry_period_ms: params.retry_period_ms
	})!
}

// shutdown gracefully stops accepting new requests and waits for in-flight requests to finish.
pub fn (s &Server) shutdown(params ShutdownParams) ! {
	s.ensure_lifecycle_control()!
	s.handle.shutdown(fasthttp.ShutdownParams{
		timeout:         params.timeout
		retry_period_ms: params.retry_period_ms
	})!
}
