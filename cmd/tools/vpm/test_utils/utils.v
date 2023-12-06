module test_utils

import os
import net

fn hg_serve(hg_path string, path string) (&os.Process, int) {
	mut port := 8000
	for {
		if mut l := net.listen_tcp(.ip6, ':${port}') {
			l.close() or { panic(err) }
			break
		}
		port++
	}
	mut p := os.new_process(hg_path)
	p.set_work_folder(path)
	p.set_args(['serve', '--print-url', '--port', port.str()])
	p.set_redirect_stdio()
	p.run()
	for p.is_alive() {
		if p.stdout_read().contains(':${port}') {
			break
		}
	}
	return p, port
}
