// Binary Upload/Download Server Example
// Demonstrates []u8 body handling for file uploads and binary responses.
module main

import net.http
import os

struct BinaryHandler {}

fn (h BinaryHandler) handle(req http.ServerRequest) http.ServerResponse {
	match req.path {
		'/upload' {
			if req.method != .post {
				return http.ServerResponse{
					status_code: 405
					body:        'Method Not Allowed'.bytes()
				}
			}
			content_type := req.header.get(.content_type) or { 'application/octet-stream' }
			println('[upload] received ${req.body.len} bytes (${content_type})')

			os.write_file_array('/tmp/uploaded_file', req.body) or {
				return http.ServerResponse{
					status_code: 500
					body:        'Failed to save file: ${err}'.bytes()
				}
			}

			mut header := http.new_header()
			header.add(.content_type, 'application/json')
			return http.ServerResponse{
				status_code: 200
				header:      header
				body:        '{"status":"ok","size":${req.body.len}}'.bytes()
			}
		}
		'/download' {
			data := os.read_bytes('/tmp/uploaded_file') or {
				return http.ServerResponse{
					status_code: 404
					body:        'No file uploaded yet'.bytes()
				}
			}
			mut header := http.new_header()
			header.add(.content_type, 'application/octet-stream')
			header.add(.content_disposition, 'attachment; filename="downloaded_file"')
			return http.ServerResponse{
				status_code: 200
				header:      header
				body:        data
			}
		}
		'/generate' {
			size := 1024 * 1024 // 1 MB of binary data
			mut data := []u8{len: size}
			for i in 0 .. size {
				data[i] = u8(i % 256)
			}
			mut header := http.new_header()
			header.add(.content_type, 'application/octet-stream')
			return http.ServerResponse{
				status_code: 200
				header:      header
				body:        data
			}
		}
		'/' {
			return http.ServerResponse{
				status_code: 200
				body:        'Binary Server\n\nEndpoints:\n  POST /upload   - upload binary file\n  GET  /download - download last uploaded file\n  GET  /generate - download 1MB generated binary\n'.bytes()
			}
		}
		else {
			return http.ServerResponse{
				status_code: 404
				body:        'Not found'.bytes()
			}
		}
	}
}

fn main() {
	mut server := http.Server{
		addr:    ':8080'
		handler: BinaryHandler{}
	}
	server.listen_and_serve()
}
