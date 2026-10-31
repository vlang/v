module multipart

import net.http

// Part represents a single part in a multipart message.
pub struct Part {
pub:
	field_name   string
	file_name    string
	content_type string
	data         string
}

// parse parses a multipart body with the given boundary and returns the
// individual parts.
pub fn parse(body string, boundary string) []Part {
	form, files := http.parse_multipart_form(body, boundary)
	mut parts := []Part{}
	for name, val in form {
		parts << Part{
			field_name: name
			data:       val
		}
	}
	for name, fs in files {
		for f in fs {
			parts << Part{
				field_name:   name
				file_name:    f.filename
				content_type: f.content_type
				data:         f.data
			}
		}
	}
	return parts
}
