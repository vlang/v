module veb

@[heap]
pub struct Server {}

fn new_server_without_lifecycle() &Server {
	return &Server{}
}

// wait_till_running waits until the server starts accepting requests.
pub fn (s &Server) wait_till_running(params WaitTillRunningParams) !int {
	_ = params
	return error('veb server lifecycle control requires `-d new_veb` without SSL')
}

// shutdown gracefully stops accepting new requests and waits for in-flight requests to finish.
pub fn (s &Server) shutdown(params ShutdownParams) ! {
	_ = params
	return error('veb server lifecycle control requires `-d new_veb` without SSL')
}
