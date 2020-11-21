import sync
import time
import x.websocket

const (
    default_gateway = 'wss://gateway.discord.gg/?v=8&encoding=json'
)

fn main() {
    mut wg := sync.new_waitgroup()
    
	for {
	mut ws := websocket.new_client(default_gateway)?
		ws.on_open(on_open)
		ws.on_error(on_err)
		ws.on_close(on_close)
		ws.connect()?
		ws.listen() or {
			println('listen: $err')
		}
	}
	
    // wg.add(1)
    // wg.wait()
}

fn on_open(mut ws websocket.Client) ? {
    ws.write_str('send nudes, pls')
}

fn on_err(mut ws websocket.Client, error string) ? {
    println(error)
}

fn on_close(mut ws websocket.Client, code int, reason string) ? {
    println('$code - $reason')
    // reconnect(mut ws)?
}

fn reconnect(mut ws websocket.Client) ? {
    // for ws.state !in [.closed, .closing]{
	// 	return
	// 	// println('ws still open! can not reconnect!')
    //     // ws.close(5000, "Oops, client isn't closed yet") or {}
    // }
    // ws = websocket.new_client(default_gateway)?
    // ws.on_error(on_err)
    // ws.on_close(on_close)
    // ws.connect()?
    // go ws.listen()?
}