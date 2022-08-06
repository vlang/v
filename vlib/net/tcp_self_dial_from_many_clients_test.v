module main

import net
import time

const xport = 15523

struct Context {
mut:
	ok_client_dials   int
	fail_client_dials int
	//
	ok_client_close   int
	fail_client_close int
	//
	ok_server_accepts   int
	fail_server_accepts int
}

fn start_server(schannel chan int, mut ctx Context) {
	eprintln('server: start_server')
	mut tcp_listener := net.listen_tcp(net.AddrFamily.ip, ':$xport') or {
		eprintln('server: start server error $err')
		return
	}
	eprintln('server: server started listening at port :$xport')
	schannel <- 0

	for {
		tcp_con := tcp_listener.accept() or {
			eprintln('server: accept error: $err')
			ctx.fail_server_accepts++
			continue
		}
		ctx.ok_server_accepts++
		eprintln('server: new tcp connection con.sock.handle: $tcp_con.sock.handle')
		continue
	}
}

fn start_client(i int, mut ctx Context) {
	eprintln('client [$i]: start')
	mut tcp_con := net.dial_tcp('127.0.0.1:$xport') or {
		eprintln('client [$i]: net.dial_tcp err $err')
		ctx.fail_client_dials++
		return
	}
	ctx.ok_client_dials++
	eprintln('client [$i]: conn is connected, con.sock.handle: $tcp_con.sock.handle')
	tcp_con.close() or {
		eprintln('client [$i]: close failed, err: $err')
		ctx.fail_client_close++
		return
	}
	ctx.ok_client_close++
}

fn test_tcp_self_dialing() {
	start_time := time.now()
	mut ctx := &Context{}
	mut server_channel := chan int{cap: 1}
	go start_server(server_channel, mut ctx)
	svalue := <-server_channel
	eprintln('>>> server was started: ${svalue}. Starting clients:')
	for i := int(0); i < 20; i++ {
		go start_client(i, mut ctx)
		eprintln('>>> started client $i')
		time.sleep(10 * time.millisecond)
	}
	max_dt := 5 * time.second
	for {
		t := time.now()
		dt := t - start_time
		if dt > max_dt {
			eprintln('>>> exiting after $dt.milliseconds() ms ...')
			dump(ctx)
			if ctx.fail_client_dials > 2 {
				assert false, 'failed $ctx.fail_client_dials client dials'
			}
			exit(0)
		}
		time.sleep(10 * time.millisecond)
	}
}
