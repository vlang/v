import net
import net.websocket

fn test_websocket_client_default_read_timeout_is_infinite() ! {
	opt := websocket.ClientOpt{}
	assert opt.read_timeout == net.infinite_timeout

	mut client := websocket.new_client('ws://127.0.0.1:12345')!
	assert client.read_timeout == net.infinite_timeout
}
