module main

import v.tests.repl.runner
import log
import benchmark

fn main() {
	mut logger := log.Log{}
	logger.set_level(.debug)
	options := runner.new_options()
	mut bmark := benchmark.new_benchmark()
	for file in options.files {
		bmark.step()
		fres := runner.run_repl_file(options.wd, options.vexec, file) or {
			bmark.fail()
			logger.error(bmark.step_message_fail(err.msg))
			continue
		}
		bmark.ok()
		logger.info(bmark.step_message_ok(fres))
	}
	bmark.stop()
	logger.info(bmark.total_message('total time spent running REPL files'))
}
