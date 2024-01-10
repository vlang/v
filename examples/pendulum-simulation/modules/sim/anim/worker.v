module anim

import benchmark
import sim
import sim.img

fn pixels_worker(mut app App) {
	mut bmark := benchmark.new_benchmark()
	for {
		result := <-app.result_chan or { break }
		bmark.step()
		// find the closest magnet
		pixel_color := img.compute_pixel(result)
		app.pixels[result.id] = u32(pixel_color.abgr8())
		bmark.ok()
	}
	bmark.stop()
	println(bmark.total_message(@FN))
}
