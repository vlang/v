module driver

import os

fn wait_run_process(mut process os.Process) !int {
	process.wait()
	return 0
}
