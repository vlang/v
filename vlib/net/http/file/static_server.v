module file

import os
import log
import time
import runtime
import net.http
import net.http.mime

[params]
pub struct StaticServeParams {
pub mut:
	folder         string        = '.' // the folder, that will be used as a base for serving all static resources; If it was /tmp, then: http://localhost:4001/x.txt => /tmp/x.txt
	on             string        = 'localhost:4001' // on which address:port to listen for http requests
	workers        int           = runtime.nr_jobs() // how many worker threads to use for serving the responses, by default it is limited to the number of available cores; can be controlled with setting VJOBS
	shutdown_after time.Duration = time.infinite // after this time has passed, the webserver will gracefully shutdown on its own
}

// serve will start a static files web server.
//
// The most common usage is the following:
//      v -e 'import net.http; http.serve()'
// which will listen for http requests on port 4001 by default, and serve all the files in the current folder.
//
// Another example: `v -e 'import net.http; http.serve(folder: "/tmp")` , same but will serve all files inside the /tmp folder.
// Another example: `v -e 'import net.http; http.serve(folder: "~/Projects", on: ":5002")` , expose all the files inside the ~/Projects folder, on http://localhost:5002/ .
pub fn serve(params StaticServeParams) {
	mut nparams := params
	nparams.folder = os.norm_path(os.real_path(params.folder))
	mut server := &http.Server{
		handler: StaticHttpHandler{
			params: nparams
		}
		addr: params.on
		worker_num: params.workers
	}
	if params.shutdown_after != time.infinite {
		spawn fn (params StaticServeParams, mut server http.Server) {
			log.warn('This file server, will shutdown itself after ${params.shutdown_after}.')
			time.sleep(params.shutdown_after)
			log.warn('Graceful shutdown, because the file server started ${params.shutdown_after} ago.')
			server.stop()
		}(params, mut server)
	}
	log.warn('${@METHOD}, starting...')
	server.listen_and_serve()
	log.warn('${@METHOD}, done.')
}

// implementation details:

struct StaticHttpHandler {
	params StaticServeParams
}

fn (mut h StaticHttpHandler) handle(req http.Request) http.Response {
	mut res := http.new_response(body: '')
	sw := time.new_stopwatch()
	defer {
		log.info('took: ${sw.elapsed().microseconds():6} us, status: ${res.status_code}, size: ${res.body.len:6}, url: ${req.url}')
	}
	requested_file_path := os.norm_path(os.real_path(os.join_path_single(h.params.folder,
		req.url.all_after_first('/'))))
	if !requested_file_path.starts_with(h.params.folder) {
		log.warn('forbidden request; base folder: ${h.params.folder}, requested_file_path: ${requested_file_path}, ')
		res = http.new_response(body: '<h1>forbidden</h1>')
		res.set_status(.forbidden)
		res.header.add(.content_type, 'text/html; charset=utf-8')
		return res
	}
	mut body := ''
	if !os.exists(requested_file_path) {
		res.set_status(.not_found)
		res.body = '<!DOCTYPE html><h1>no such file</h1>'
		return res
	}
	body = os.read_file(requested_file_path) or {
		res.set_status(.not_found)
		''
	}
	mt := mime.get_mime_type(os.file_ext(requested_file_path).all_after_first('.'))
	ct := mime.get_content_type(mt)
	res = http.new_response(body: body)
	res.body = body
	res.header.add(.content_type, ct)
	return res
}
