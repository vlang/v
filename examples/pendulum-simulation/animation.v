module main

import log
import benchmark
import sim
import sim.anim
import sim.args as simargs

fn main() {
	unbuffer_stdout()
	args := simargs.parse_args(extra_workers: 1)! as simargs.ParallelArgs
	mut app := anim.new_app(args)
	mut workers := []thread{cap: args.workers}
	mut bmark := benchmark.start()
	defer {
		app.request_chan.close()
		log.info('Waiting for workers to finish')
		workers.wait()
		app.result_chan.close()
		log.info('Workers finished!')
		bmark.measure(@FN)
		log.info('Done!')
	}
	for id in 0 .. args.workers {
		workers << spawn sim.sim_worker(id, app.request_chan, [app.result_chan])
	}
	handle_request := fn [app] (request &sim.SimRequest) ! {
		app.request_chan <- request
	}
	spawn app.gg.run()
	sim.run(args.params, grid: args.grid, on_request: sim.SimRequestHandler(handle_request))
}
