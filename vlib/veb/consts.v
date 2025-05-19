module veb

import net.http

// max read and write limits in bytes
const max_read = int($d('veb_max_read_bytes', 8192))
const max_write = int($d('veb_max_write_bytes', 2048))

pub const max_http_post_size = $d('veb_max_http_post_size_bytes', 1048576)
pub const default_port = int($d('veb_default_port', 8080))
pub const methods_with_form = [http.Method.post, .put, .patch]

pub const headers_close = http.new_custom_header_from_map({
	'Server': 'veb'
})!

pub const http_302 = http.new_response(
	status: .found
	body:   '302 Found'
	header: headers_close
)

pub const http_400 = http.new_response(
	status: .bad_request
	body:   '400 Bad Request'
	header: http.new_header(
		key:   .content_type
		value: 'text/plain'
	).join(headers_close)
)

pub const http_404 = http.new_response(
	status: .not_found
	body:   '404 Not Found'
	header: http.new_header(
		key:   .content_type
		value: 'text/plain'
	).join(headers_close)
)

pub const http_408 = http.new_response(
	status: .request_timeout
	body:   '408 Request Timeout'
	header: http.new_header(
		key:   .content_type
		value: 'text/plain'
	).join(headers_close)
)

pub const http_413 = http.new_response(
	status: .request_entity_too_large
	body:   '413 Request entity is too large'
	header: http.new_header(
		key:   .content_type
		value: 'text/plain'
	).join(headers_close)
)

pub const http_500 = http.new_response(
	status: .internal_server_error
	body:   '500 Internal Server Error'
	header: http.new_header(
		key:   .content_type
		value: 'text/plain'
	).join(headers_close)
)

pub const mime_types = {
	'.aac':    'audio/aac'
	'.abw':    'application/x-abiword'
	'.arc':    'application/x-freearc'
	'.avi':    'video/x-msvideo'
	'.azw':    'application/vnd.amazon.ebook'
	'.bin':    'application/octet-stream'
	'.bmp':    'image/bmp'
	'.bz':     'application/x-bzip'
	'.bz2':    'application/x-bzip2'
	'.cda':    'application/x-cdf'
	'.csh':    'application/x-csh'
	'.css':    'text/css'
	'.csv':    'text/csv'
	'.doc':    'application/msword'
	'.docx':   'application/vnd.openxmlformats-officedocument.wordprocessingml.document'
	'.eot':    'application/vnd.ms-fontobject'
	'.epub':   'application/epub+zip'
	'.gz':     'application/gzip'
	'.gif':    'image/gif'
	'.htm':    'text/html'
	'.html':   'text/html'
	'.ico':    'image/vnd.microsoft.icon'
	'.ics':    'text/calendar'
	'.jar':    'application/java-archive'
	'.jpeg':   'image/jpeg'
	'.jpg':    'image/jpeg'
	'.js':     'text/javascript'
	'.json':   'application/json'
	'.jsonld': 'application/ld+json'
	'.mid':    'audio/midi audio/x-midi'
	'.midi':   'audio/midi audio/x-midi'
	'.mjs':    'text/javascript'
	'.mp3':    'audio/mpeg'
	'.mp4':    'video/mp4'
	'.mpeg':   'video/mpeg'
	'.mpkg':   'application/vnd.apple.installer+xml'
	'.odp':    'application/vnd.oasis.opendocument.presentation'
	'.ods':    'application/vnd.oasis.opendocument.spreadsheet'
	'.odt':    'application/vnd.oasis.opendocument.text'
	'.oga':    'audio/ogg'
	'.ogv':    'video/ogg'
	'.ogx':    'application/ogg'
	'.opus':   'audio/opus'
	'.otf':    'font/otf'
	'.png':    'image/png'
	'.pdf':    'application/pdf'
	'.php':    'application/x-httpd-php'
	'.ppt':    'application/vnd.ms-powerpoint'
	'.pptx':   'application/vnd.openxmlformats-officedocument.presentationml.presentation'
	'.rar':    'application/vnd.rar'
	'.rtf':    'application/rtf'
	'.scss':   'text/css'
	'.sh':     'application/x-sh'
	'.svg':    'image/svg+xml'
	'.swf':    'application/x-shockwave-flash'
	'.tar':    'application/x-tar'
	'.tif':    'image/tiff'
	'.tiff':   'image/tiff'
	'.ts':     'video/mp2t'
	'.ttf':    'font/ttf'
	'.txt':    'text/plain'
	'.vsd':    'application/vnd.visio'
	'.wasm':   'application/wasm'
	'.wav':    'audio/wav'
	'.weba':   'audio/webm'
	'.webm':   'video/webm'
	'.webp':   'image/webp'
	'.woff':   'font/woff'
	'.woff2':  'font/woff2'
	'.xhtml':  'application/xhtml+xml'
	'.xls':    'application/vnd.ms-excel'
	'.xlsx':   'application/vnd.openxmlformats-officedocument.spreadsheetml.sheet'
	'.xml':    'application/xml'
	'.xul':    'application/vnd.mozilla.xul+xml'
	'.zip':    'application/zip'
	'.3gp':    'video/3gpp'
	'.3g2':    'video/3gpp2'
	'.7z':     'application/x-7z-compressed'
	'.m3u8':   'application/vnd.apple.mpegurl'
	'.vsh':    'text/x-vlang'
	'.v':      'text/x-vlang'
}
