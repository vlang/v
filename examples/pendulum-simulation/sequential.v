module main

import benchmark
import sim
import sim.args as simargs
import sim.img

fn main() {
	args := simargs.parse_args(sequential: true) ? as simargs.SequentialArgs

	mut bmark := benchmark.start()
	defer {
		bmark.measure(@FN)
	}

	mut writer := img.ppm_writer_for_fname(args.filename, img.image_settings_from_grid(args.grid)) ?
	defer {
		writer.close()
	}

	handle_request := fn [mut writer] (request &sim.SimRequest) ? {
		result := sim.compute_result(request)
		pixel := img.compute_pixel(result)
		return writer.handle_pixel(pixel)
	}

	sim.run(args.params, grid: args.grid, on_request: sim.SimRequestHandler(handle_request))

	writer.write() ?
}
