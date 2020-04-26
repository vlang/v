module main

import net.websocket
import eventbus
import time
import readline
import term
import benchmark

const (
	eb = eventbus.new()
)

#flag -I $PWD
#include "utf8.h"
fn C.utf8_validate_str() bool

fn main() {
	// println(sss)
	/*
	for sss in 0..10 {
		mut bm := benchmark.new_benchmark()
		for i in 0..10000 {
			for a, t in tests {
				ss := ws.utf8_validate(t.str, t.len)
				if !ss {
					panic("failed")
				}
				//println("${a}:${ss}")
			}
		}
		bm.stop()
		println( bm.total_message('remarks about the benchmark') )
	}
*/
	mut ws := websocket.new('ws://localhost:9001/getCaseCount')
	// defer { }
	ws.subscriber.subscribe('on_open', on_open)
	ws.subscriber.subscribe('on_message', on_message)
	ws.subscriber.subscribe('on_error', on_error)
	ws.subscriber.subscribe('on_close', on_close)
	// go
	ws.connect()
	ws.read()
	// time.usleep(2000000)
	// go ws.listen()
	// term.erase_clear()
	/*
	text := read_line("[client]:")
	if text == "close" {
		ws.close(1005, "done")
		time.usleep(1000000)
		exit(0)
	}
	ws.write(text, .text_frame)
*/
	/*
	time.usleep(1000000)
	ws.read()
*/
	// ws.close(1005, "done")	//
	// ws.close(1005, "done")
	// read_line("wait")
}

fn read_line(text string) string {
	mut r := readline.Readline{}
	mut output := r.read_line(text + ' ') or {
		panic(err)
	}
	output = output.replace('\n', '')
	if output.len <= 0 {
		return ''
	}
	return output
}

fn on_open(params eventbus.Params) {
	println('websocket opened.')
}

fn on_message(params eventbus.Params) {
	println('Message recieved. Sending it back.')
	typ := params.get_string('type')
	len := params.get_int('len')
	mut ws := params.get_caller(websocket.Client{})
	if typ == 'string' {
		message := params.get_string('payload')
		if ws.uri.ends_with('getCaseCount') {
			num := message.int()
			ws.close(1005, 'done')
			start_tests(mut ws, num)
			return
		}
		// println("Message: " + message)
		ws.write(message.str, len, .text_frame)
	} else {
		println('Binary message.')
		message := params.get_raw('payload')
		ws.write(message, len, .binary_frame)
	}
}

fn start_tests(ws mut websocket.Client, num int) {
	for i := 1; i < num; i++ {
		println('Running test: ' + i.str())
		ws.uri = 'ws://localhost:9001/runCase?case=${i.str()}&agent=vws/1.0a'
		if ws.connect() >= 0 {
			ws.listen()
		}
	}
	println('Done!')
	ws.uri = 'ws://localhost:9001/updateReports?agent=vws/1.0a'
	if ws.connect() >= 0 {
		ws.read()
		ws.close(1000, 'disconnecting...')
	}
	exit(0)
}

fn on_close(params eventbus.Params) {
	println('websocket closed.')
}

fn on_error(params eventbus.Params) {
	println('we have an error.')
}
