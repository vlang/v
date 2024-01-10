module img

import benchmark
import sim

pub fn image_worker(mut writer PPMWriter, result_chan chan &sim.SimResult, settings ImageSettings) {
	width := settings.width
	height := settings.height
	total_pixels := width * height

	// as new pixels come in, write them to the image file
	mut current_index := u64(0)
	mut pixel_buf := []ValidColor{len: total_pixels, init: ValidColor{
		valid: false
	}}

	mut bmark := benchmark.new_benchmark()
	for {
		result := <-result_chan or { break }

		// find the closest magnet
		pixel_buf[result.id].Color = compute_pixel(result)
		pixel_buf[result.id].valid = true

		for current_index < total_pixels && pixel_buf[current_index].valid {
			bmark.step()
			writer.handle_pixel(pixel_buf[current_index].Color) or {
				bmark.fail()
				sim.log(@MOD + '.' + @FN + ': pixel handler failed. Error ${err}')
				break
			}
			bmark.ok()
			current_index++
		}
	}
	bmark.stop()
	println(bmark.total_message(@FN))

	writer.write() or { panic('Could not write image') }
}
