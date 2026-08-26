// perf_ws_read.v - benchmarks the websocket payload read path (read_payload).
//
// A local websocket server pushes `frames` large binary messages per round;
// the client reads them back-to-back with read_next_message() and reports
// throughput per round. The first (warmup) round is not counted.
//
// usage: perf_ws_read [total_mib] [frame_kib] [rounds]
//   defaults:            10         1024        3
module main

import net.websocket
import os
import time

const listen_port = 31177

const connect_timeout_ms = 5000

struct BenchState {
mut:
	chunk  []u8 // payload sent for every frame
	frames int  // frames per round
}

// start_server spawns a server that sends the bulk data whenever it
// receives a 'go' text message.
fn start_server(port int, state &BenchState) ! {
	mut s := websocket.new_server(.ip, port, '')
	s.set_ping_interval(3600) // effectively disable pings during the bench
	s.on_message(fn [state] (mut ws websocket.Client, msg &websocket.Message) ! {
		if msg.payload.bytestr() == 'go' {
			for _ in 0 .. state.frames {
				ws.write(state.chunk, .binary_frame) or { break }
			}
		}
	})
	spawn s.listen()
	wait_until_ready(mut s)
}

fn wait_until_ready(mut s websocket.Server) {
	deadline := time.ticks() + connect_timeout_ms
	for s.get_state() != .open {
		if time.ticks() > deadline {
			eprintln('server did not become ready in ${connect_timeout_ms}ms')
			exit(1)
		}
		time.sleep(5 * time.millisecond)
	}
}

fn main() {
	mut total_mib := 10
	mut frame_kib := 1024
	mut rounds := 3
	for i, arg in os.args[1..] {
		v := arg.int()
		if v <= 0 {
			eprintln('ignoring invalid argument "${arg}"')
			continue
		}
		match i {
			0 { total_mib = v }
			1 { frame_kib = v }
			2 { rounds = v }
			else {}
		}
	}
	chunk_len := frame_kib * 1024
	frames := total_mib * 1024 * 1024 / chunk_len
	total_bytes := u64(frames) * u64(chunk_len)

	state := &BenchState{
		chunk:  []u8{len: chunk_len, init: u8((index % 251) + 1)}
		frames: frames
	}
	start_server(listen_port, state)!

	println('websocket payload read benchmark')
	println('  payload/frame : ${frame_kib} KiB')
	println('  frames/round  : ${frames}')
	println('  total/round   : ${total_bytes / (1024 * 1024)} MiB')
	println('  rounds        : ${rounds} (+1 warmup)')
	println('')

	mut client := websocket.new_client('ws://127.0.0.1:${listen_port}')!
	client.connect()!

	mut best := f64(0)
	mut sum := f64(0)
	for r in 0 .. rounds + 1 {
		client.write_string('go')!
		mut sw := time.new_stopwatch()
		mut got := 0
		for got < frames {
			msg := client.read_next_message()!
			match msg.opcode {
				.binary_frame {
					if msg.payload.len != chunk_len {
						eprintln('bad payload size: ${msg.payload.len}, expected ${chunk_len}')
						exit(1)
					}
					got++
				}
				.ping {
					client.pong() or {}
				}
				else {}
			}
		}
		elapsed_ns := f64(u64(sw.elapsed()))
		mibs := f64(total_bytes) / 1048576.0 / (elapsed_ns / 1e9)
		ms := elapsed_ns / 1e6
		if r == 0 {
			println('  warmup : ${ms:9.1f} ms   ${mibs:9.1f} MiB/s   (discarded)')
		} else {
			println('  round ${r} : ${ms:9.1f} ms   ${mibs:9.1f} MiB/s')
			sum += mibs
			if mibs > best {
				best = mibs
			}
		}
	}
	println('')
	println('  best : ${best:9.1f} MiB/s')
	println('  avg  : ${sum / f64(rounds):9.1f} MiB/s')

	client.close(1000, 'done') or {}
	time.sleep(100 * time.millisecond)
}
