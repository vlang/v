// Isolate concurrent first use of json2's field cache from sockets and app state.
module json2_test

import json2

struct ChatMessage {
	id      u64
	from    int
	to      int
	text    string
	sent_at i64
	nonce   string
}

fn encode_message() string {
	return json2.encode(ChatMessage{
		id:      1
		from:    1
		to:      2
		text:    'hello'
		sent_at: 1234567890
		nonce:   'probe'
	}, escape_unicode: true, time_as_unix: true)
}

fn encode_worker(ready chan bool, start chan bool, done chan bool) {
	ready <- true
	_ := <-start
	for _ in 0 .. 100 {
		encoded := encode_message()
		assert encoded == '{"id":1,"from":1,"to":2,"text":"hello","sent_at":1234567890,"nonce":"probe"}'
	}
	done <- true
}

fn test_concurrent_cold_struct_encoding() {
	ready := chan bool{cap: 64}
	start := chan bool{cap: 64}
	done := chan bool{cap: 64}
	for _ in 0 .. 64 {
		spawn encode_worker(ready, start, done)
	}
	for _ in 0 .. 64 {
		_ := <-ready
	}
	for _ in 0 .. 64 {
		start <- true
	}
	for _ in 0 .. 64 {
		_ := <-done
	}
	println('PASS: 6400 JSON encodes across 64 threads')
}
