module file

import os
import log
import time
import runtime
import net.http
import net.http.mime
import net.urllib

@[params]
pub struct StaticServeParams {
pub mut:
	folder         string        = $d('http_folder', '.')              // The folder, that will be used as a base for serving all static resources; If it was /tmp, then: http://localhost:4001/x.txt => /tmp/x.txt . Customize with `-d http_folder=vlib/_docs`.
	index_file     string        = $d('http_index_file', 'index.html') // A request for http://localhost:4001/ will map to `index.html`, if that file is present.
	auto_index     bool          = $d('http_auto_index', true)         // when an index_file is *not* present, a request for http://localhost:4001/ will list automatically all files in the folder.
	on             string        = $d('http_on', 'localhost:4001')     // on which address:port to listen for http requests.
	filter_myexe   bool          = true // whether to filter the name of the static file executable from the automatic folder listings for / . Useful with `v -e 'import net.http.file; file.serve()'`
	workers        int           = runtime.nr_jobs() // how many worker threads to use for serving the responses, by default it is limited to the number of available cores; can be controlled with setting VJOBS
	shutdown_after time.Duration = time.infinite // after this time has passed, the webserver will gracefully shutdown on its own
}

// serve will start a static files web server.
//
// The most common usage is the following: `v -e 'import net.http.file; file.serve()'`
// will listen for http requests on port 4001 by default, and serve all the files in the current folder.
//
// Another example: `v -e 'import net.http.file; file.serve(folder: "/tmp")'`
// will serve all files inside the /tmp folder.
//
// Another example: `v -e 'import net.http.file; file.serve(folder: "~/Projects", on: ":5002")'`
// will expose all the files inside the ~/Projects folder, on http://localhost:5002/ .
pub fn serve(params StaticServeParams) {
	mut nparams := params
	nparams.folder = os.norm_path(os.real_path(params.folder))
	mut server := &http.Server{
		handler:    StaticHttpHandler{
			params: nparams
		}
		addr:       params.on
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

const no_such_file_doc = '<!DOCTYPE html><h1>no such file</h1>'

fn (mut h StaticHttpHandler) handle(req http.Request) http.Response {
	mut res := http.new_response(body: '')
	sw := time.new_stopwatch()
	mut url := urllib.query_unescape(req.url) or {
		log.warn('bad request; url: ${req.url} ')
		res.set_status(.bad_request)
		res.body = '<!DOCTYPE html><h1>url decode fail</h1>'
		res.header.add(.content_type, 'text/html; charset=utf-8')
		return res
	}
	defer {
		log.info('took: ${sw.elapsed().microseconds():6}µs, status: ${res.status_code}, size: ${res.body.len:9}, url: ${url}')
	}
	mut uri_path := url.all_after_first('/').trim_right('/')
	requested_file_path := os.norm_path(os.real_path(os.join_path_single(h.params.folder,
		uri_path)))
	if !requested_file_path.starts_with(h.params.folder) {
		log.warn('forbidden request; base folder: ${h.params.folder}, requested_file_path: ${requested_file_path}, ')
		res.set_status(.forbidden)
		res.body = '<h1>forbidden</h1>'
		res.header.add(.content_type, 'text/html; charset=utf-8')
		return res
	}
	if !os.exists(requested_file_path) {
		res.set_status(.not_found)
		res.body = no_such_file_doc
		res.header.add(.content_type, 'text/html; charset=utf-8')
		return res
	}

	mut body := ''
	mut content_type := 'text/html; charset=utf-8'
	if os.is_dir(requested_file_path) {
		ipath := os.join_path_single(requested_file_path, h.params.index_file)
		if h.params.auto_index {
			if h.params.index_file == '' {
				body = get_folder_index_html(requested_file_path, uri_path, h.params.filter_myexe)
			} else {
				body = os.read_file(ipath) or {
					get_folder_index_html(requested_file_path, uri_path, h.params.filter_myexe)
				}
			}
		} else {
			body = os.read_file(ipath) or {
				res.set_status(.not_found)
				no_such_file_doc
			}
		}
	} else {
		body = os.read_file(requested_file_path) or {
			res.set_status(.not_found)
			'not found'
		}
		mt := mime.get_mime_type(os.file_ext(requested_file_path).all_after_first('.'))
		content_type = mime.get_content_type(mt)
	}
	res.body = body
	res.header.add(.content_type, content_type)
	return res
}
