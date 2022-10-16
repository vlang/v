import net.websocket as ws

pub type RawMessage = ws.Message

pub struct WsTransport {
pub mut:
	ws ws.Client
}

struct WsClient {
pub mut:
	transport Transport
}

pub interface Transport {
	send()
	wait()
}

fn (wst WsTransport) send() {
	println('send is called')
}

fn (wst WsTransport) wait() {
	println('wait is called')
}

pub fn new_ws_client(transport Transport) !WsClient {
	return WsClient{
		transport: transport
	}
}

fn server() ! {
	mut s := ws.new_server(.ip6, 8081, '/')

	s.on_connect(fn (mut s ws.ServerClient) !bool {
		if s.resource_name != '/' {
			return false
		}
		println('Client has connected...')
		return true
	})!

	s.on_message(fn (mut ws ws.Client, msg &RawMessage) ! {
		mut transport := WsTransport{}
		mut ws_client := new_ws_client(transport)!
		_ := ws_client
	})

	s.on_close(fn (mut ws ws.Client, code int, reason string) ! {
		println('client ($ws.id) closed connection')
	})

	s.listen() or { println('error on server listen: $err') }

	unsafe {
		s.free()
	}
}

fn abc() ! {
	server()!
}

fn test_compilation_of_the_example_code_in_issue_15839() {
	assert true
}
