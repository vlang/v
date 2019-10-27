module main

import compiler.tests.repl.runner
import log
import benchmark

fn main(){
	logger := &log.Log{log.DEBUG, 'terminal'}
	options := runner.new_options()

	mut bmark := benchmark.new_benchmark()
	for file in options.files {
		bmark.step()
		fres := runner.run_repl_file(options.wd, options.vexec, file) or {
			bmark.fail()
			logger.error( bmark.step_message( err ) )
			continue
		}
		bmark.ok()
		logger.info( bmark.step_message( fres ) )
	}
	bmark.stop()
	logger.info( bmark.total_message('total time spent running REPL files') )
}
