module test_utils

import os

fn hg_serve(hg_path string, path string) (&os.Process, string) {
	mut p := os.new_process(hg_path)
	p.set_work_folder(path)
	p.set_args(['serve'])
	p.set_redirect_stdio()
	p.run()
	mut addr := ''
	for p.is_alive() {
		line := p.stdout_read()
		if line.contains('listening') {
			addr = line.after('listening at ').before(' ')
			break
		}
	}
	return p, addr
}
