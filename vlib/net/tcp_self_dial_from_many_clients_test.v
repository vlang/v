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
	////
	ok_server_accepts   int
	fail_server_accepts int
	//
	ok_server_close   int
	fail_server_close int
	//
	received []int
}

fn elog(msg string) {
	eprintln('${time.now().format_ss_micro()} | ${msg}')
}

fn receive_data(mut con net.TcpConn, shared ctx Context) {
	mut buf := []u8{len: 5}
	for {
		bytes := con.read(mut buf) or { -1 }
		if bytes < 0 {
			break
		}
		if bytes > 0 {
			lock ctx {
				ctx.received << buf[0]
			}
		}
	}
	con.close() or {
		lock ctx {
			ctx.fail_server_close++
		}
		return
	}
	lock ctx {
		ctx.ok_server_close++
	}
}

fn start_server(schannel chan int, shared ctx Context) {
	elog('server: start_server')
	mut tcp_listener := net.listen_tcp(net.AddrFamily.ip, ':${xport}') or {
		elog('server: start server error ${err}')
		return
	}
	elog('server: server started listening at port :${xport}')
	schannel <- 0

	for {
		mut tcp_con := tcp_listener.accept() or {
			elog('server: accept error: ${err}')
			lock ctx {
				ctx.fail_server_accepts++
			}
			continue
		}
		spawn receive_data(mut tcp_con, shared ctx)
		lock ctx {
			ctx.ok_server_accepts++
		}
		elog('server: new tcp connection con.sock.handle: ${tcp_con.sock.handle}')
		continue
	}
}

fn start_client(i int, shared ctx Context) {
	elog('client [${i}]: start')
	mut tcp_con := net.dial_tcp('127.0.0.1:${xport}') or {
		elog('client [${i}]: net.dial_tcp err ${err}')
		lock ctx {
			ctx.fail_client_dials++
		}
		return
	}
	lock ctx {
		ctx.ok_client_dials++
	}
	elog('client [${i}]: conn is connected, con.sock.handle: ${tcp_con.sock.handle}')
	tcp_con.write([u8(i)]) or { elog('client [${i}]: write failed, err: ${err}') }
	time.sleep(1 * time.second)
	elog('client [${i}]: closing connection...')
	tcp_con.close() or {
		elog('client [${i}]: close failed, err: ${err}')
		lock ctx {
			ctx.fail_client_close++
		}
		return
	}
	lock ctx {
		ctx.ok_client_close++
	}
}

fn test_tcp_self_dialing() {
	elog('>>> start')
	start_time := time.now()
	shared ctx := &Context{}
	mut server_channel := chan int{cap: 1}
	spawn start_server(server_channel, shared ctx)
	svalue := <-server_channel
	elog('>>> server was started: ${svalue}. Starting clients:')
	for i := int(0); i < 20; i++ {
		spawn start_client(i, shared ctx)
		elog('>>> started client ${i}')
		// time.sleep(2 * time.millisecond)
	}
	max_dt := 5 * time.second
	for {
		t := time.now()
		dt := t - start_time
		if dt > max_dt {
			elog('>>> exiting after ${dt.milliseconds()} ms ...')
			lock ctx {
				// TODO: fix `dump(ctx)`, when `shared ctx := Type{}`
				final_value_for_ctx := ctx // make a value copy as a temporary workaround. TODO: remove when dump(ctx) works.
				dump(final_value_for_ctx)
				assert ctx.fail_client_dials < 2, 'allowed failed client dials, from ${ctx.ok_server_accepts} connections'
				assert ctx.received.len > ctx.ok_server_accepts / 2, 'at least half the clients sent some data, that was later received by the server'
			}
			elog('>>> goodbye')
			exit(0)
		}
		time.sleep(10 * time.millisecond)
	}
}
