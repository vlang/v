module main

import compiler.tests.repl.runner
import log

fn main(){
	logger := &log.Log{log.DEBUG, 'terminal'}
	options := runner.new_options()
	global_start_time := runner.now()
	for file in options.files {
		stime := runner.now()
		fres := runner.run_repl_file(options.wd, options.vexec, file) or {
			logger.error( runner.tdiff_in_ms(err, stime) )
			continue
		}
		logger.info( runner.tdiff_in_ms(fres, stime) )
	}
	logger.info( runner.tdiff_in_ms('<=== total time spent running REPL files', global_start_time) )
}
