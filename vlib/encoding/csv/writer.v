// Copyright (c) 2019-2024 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module csv

import strings

struct Writer {
	use_crlf  bool
	delimiter u8
mut:
	sb strings.Builder
}

@[params]
pub struct WriterConfig {
pub:
	use_crlf  bool
	delimiter u8 = `,`
}

// new_writer returns a reference to a Writer
pub fn new_writer(config WriterConfig) &Writer {
	return &Writer{
		sb:        strings.new_builder(200)
		use_crlf:  config.use_crlf
		delimiter: config.delimiter
	}
}

// write writes a single record
pub fn (mut w Writer) write(record []string) !bool {
	if !valid_delim(w.delimiter) {
		return &InvalidDelimiterError{}
	}
	le := if w.use_crlf { '\r\n' } else { '\n' }
	for n, field_ in record {
		mut field := field_
		if n > 0 {
			w.sb.write_string(w.delimiter.ascii_str())
		}
		if !w.field_needs_quotes(field) {
			w.sb.write_string(field)
			continue
		}
		w.sb.write_string('"')
		for field.len > 0 {
			mut i := field.index_any('"\r\n')
			if i < 0 {
				i = field.len
			}
			w.sb.write_string(field[..i])
			field = field[i..]
			if field.len > 0 {
				z := field[0]
				match z {
					`"` { w.sb.write_string('""') }
					`\r`, `\n` { w.sb.write_string(le) }
					else {}
				}
				field = field[1..]
			}
		}
		w.sb.write_string('"')
	}
	w.sb.write_string(le)
	return true
}

// Once we have multi dimensional array
// pub fn (w &Writer) write_all(records [][]string) {
// 	for _, record in records {
// 		w.write(record)
// 	}
// }
fn (w &Writer) field_needs_quotes(field string) bool {
	if field == '' {
		return false
	}
	if field.contains(w.delimiter.ascii_str()) || field.index_any('"\r\n') != -1 {
		return true
	}
	return false
}

// str returns the writer contents
pub fn (mut w Writer) str() string {
	return w.sb.str()
}
