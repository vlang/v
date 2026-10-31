module json2

struct ColdDecodeCacheMessage {
	id     int
	text   string @[json: 'body']
	active bool
	values []int
}

fn decode_cache_worker(ready chan bool, start chan bool, done chan bool) {
	ready <- true
	_ := <-start
	for _ in 0 .. 100 {
		value := decode[ColdDecodeCacheMessage]('{"id":42,"body":"ready","active":true,"values":[1,2,3]}') or { panic(err) }
		assert value.id == 42
		assert value.text == 'ready'
		assert value.active
		assert value.values == [1, 2, 3]
	}
	done <- true
}

fn test_decode_cache_publishes_complete_metadata_on_concurrent_first_use() {
	ready := chan bool{cap: 32}
	start := chan bool{cap: 32}
	done := chan bool{cap: 32}
	for _ in 0 .. 32 { spawn decode_cache_worker(ready, start, done) }
	for _ in 0 .. 32 { _ := <-ready }
	for _ in 0 .. 32 { start <- true }
	for _ in 0 .. 32 { _ := <-done }
}
