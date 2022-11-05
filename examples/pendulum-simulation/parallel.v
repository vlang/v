module main

import benchmark
import sim
import sim.args as simargs
import sim.img

fn main() {
	args := simargs.parse_args()! as simargs.ParallelArgs

	img_settings := img.image_settings_from_grid(args.grid)

	width := img_settings.width
	height := img_settings.height
	total_pixels := width * height

	request_chan := chan &sim.SimRequest{cap: args.workers}
	result_chan := chan &sim.SimResult{cap: args.workers}

	mut writer := img.ppm_writer_for_fname(args.filename, img_settings)!
	mut image_writer := img.new_image_writer(mut writer, img_settings)

	mut workers := []thread{cap: args.workers}
	mut bmark := benchmark.start()

	defer {
		request_chan.close()
		sim.log('Waiting for workers to finish')
		workers.wait()
		result_chan.close()
		bmark.measure(@FN)
		sim.log('Closing writer file')
		writer.close()
		sim.log('Done!')
	}

	for id in 0 .. args.workers {
		workers << spawn sim.sim_worker(id, request_chan, [result_chan])
	}

	mut x := 0
	mut y := 0
	mut request_index := 0

	for {
		// setup state conditions
		position := sim.vector(
			x: 0.1 * ((f64(x) - 0.5 * f64(width - 1)) / f64(width - 1))
			y: 0.1 * ((f64(y) - 0.5 * f64(height - 1)) / f64(height - 1))
			z: 0.0
		)
		velocity := sim.vector(x: 0, y: 0, z: 0)

		mut state := sim.new_state(
			position: position
			velocity: velocity
		)

		state.satisfy_rope_constraint(args.params)
		request := &sim.SimRequest{
			id: request_index
			state: state
			params: args.params
		}
		select {
			result := <-result_chan {
				image_writer.handle(result) or { break }
			}
			else {
				if request.id == total_pixels {
					continue
				}
				request_chan <- request
				x++
				if x == width {
					x = 0
					y++
					sim.log('y: ${y + 1}')
				}
				request_index++
			}
		}
	}
}
