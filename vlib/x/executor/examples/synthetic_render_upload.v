import x.executor

struct TextureUpload {
	id    int
	bytes int
}

fn main() {
	mut render := executor.new(queue_size: 8)!
	uploaded := chan TextureUpload{cap: 3}
	producer_done := chan bool{cap: 1}

	producer := spawn fn [mut render, producer_done, uploaded] () {
		for id in 0 .. 3 {
			bytes := 128 + id
			render.try_post(fn [uploaded, id, bytes] () ! {
				uploaded <- TextureUpload{
					id:    id
					bytes: bytes
				}
			}) or {
				producer_done <- false
				return
			}
		}
		producer_done <- true
	}()

	producer_ok := <-producer_done
	if !producer_ok {
		eprintln('producer could not submit render uploads')
		exit(1)
	}
	drained := render.drain_pending(8)!
	if drained != 3 {
		eprintln('expected 3 render uploads, got ${drained}')
		exit(1)
	}

	mut total_bytes := 0
	for _ in 0 .. 3 {
		total_bytes += (<-uploaded).bytes
	}
	println('uploaded ${drained} synthetic textures, bytes=${total_bytes}')

	render.stop()
	if render.drain_pending(1)! != 0 {
		eprintln('unexpected render job after stop')
		exit(1)
	}
	render.wait()!
	producer.wait()
}
