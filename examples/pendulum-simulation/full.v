module main

import benchmark
import sim
import sim.anim
import sim.args as simargs
import sim.img

fn main() {
	args := simargs.parse_args(extra_workers: 2)? as simargs.ParallelArgs

	img_settings := img.image_settings_from_grid(args.grid)

	mut writer := img.ppm_writer_for_fname(args.filename, img_settings)?

	mut app := anim.new_app(args)
	mut workers := []thread{cap: args.workers + 1}

	mut bmark := benchmark.start()

	img_result_chan := chan &sim.SimResult{cap: args.workers}

	defer {
		image_worker := workers.pop()
		app.request_chan.close()
		sim.log('Waiting for workers to finish')
		workers.wait()
		app.result_chan.close()
		img_result_chan.close()
		sim.log('Waiting for image writer to finish')
		image_worker.wait()
		sim.log('Workers finished!')
		bmark.measure(@FN)
		sim.log('Closing writer file')
		writer.close()
		sim.log('Done!')
	}

	// start a worker on each core
	for id in 0 .. app.args.workers {
		workers << go sim.sim_worker(id, app.request_chan, [app.result_chan, img_result_chan])
	}

	handle_request := fn [app] (request &sim.SimRequest) ? {
		app.request_chan <- request
	}

	workers << go img.image_worker(mut writer, img_result_chan, img_settings)

	go app.gg.run()

	sim.run(app.args.params,
		grid: app.args.grid
		on_request: sim.SimRequestHandler(handle_request)
	)
}
