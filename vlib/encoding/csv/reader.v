// Copyright (c) 2019-2022 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module csv

// Once interfaces are further along the idea would be to have something similar to
// go's io.reader & bufio.reader rather than reading the whole file into string, this
// would then satisfy that interface. I designed it this way to be easily adapted.
struct CommentIsDelimiterError {
	Error
}

fn (err CommentIsDelimiterError) msg() string {
	return 'encoding.csv: comment cannot be the same as delimiter'
}

struct InvalidDelimiterError {
	Error
}

fn (err InvalidDelimiterError) msg() string {
	return 'encoding.csv: invalid delimiter'
}

struct EndOfFileError {
	Error
}

fn (err EndOfFileError) msg() string {
	return 'encoding.csv: end of file'
}

struct InvalidLineEndingError {
	Error
}

fn (err InvalidLineEndingError) msg() string {
	return 'encoding.csv: could not find any valid line endings'
}

struct Reader {
	// not used yet
	// has_header        bool
	// headings          []string
	data string
pub mut:
	delimiter         byte
	comment           byte
	is_mac_pre_osx_le bool
	row_pos           int
}

// new_reader initializes a Reader with string data to parse
pub fn new_reader(data string) &Reader {
	return &Reader{
		delimiter: `,`
		comment: `#`
		data: data
	}
}

// read reads a row from the CSV data.
// If successful, the result holds an array of each column's data.
pub fn (mut r Reader) read() ?[]string {
	l := r.read_record() ?
	return l
}

// Once we have multi dimensional array
// pub fn (mut r Reader) read_all() ?[][]string {
// 	mut records := []string{}
// 	for {
// 		record := r.read_record() or {
// 			if err.error == err_eof.error {
// 				return records
// 			} else {
// 				return err
// 			}
// 		}
// 		records << record
// 	}
// 	return records
// }
fn (mut r Reader) read_line() ?string {
	// last record
	if r.row_pos == r.data.len {
		return IError(&EndOfFileError{})
	}
	le := if r.is_mac_pre_osx_le { '\r' } else { '\n' }
	mut i := r.data.index_after(le, r.row_pos)
	if i == -1 {
		if r.row_pos == 0 {
			// check for pre osx mac line endings
			i = r.data.index_after('\r', r.row_pos)
			if i != -1 {
				r.is_mac_pre_osx_le = true
			} else {
				// no valid line endings found
				return IError(&InvalidLineEndingError{})
			}
		} else {
			// No line ending on file
			i = r.data.len - 1
		}
	}
	mut line := r.data[r.row_pos..i]
	r.row_pos = i + 1
	// normalize win line endings (remove extra \r)
	if !r.is_mac_pre_osx_le && (line.len >= 1 && line[line.len - 1] == `\r`) {
		line = line[..line.len - 1]
	}
	return line
}

fn (mut r Reader) read_record() ?[]string {
	if r.delimiter == r.comment {
		return IError(&CommentIsDelimiterError{})
	}
	if !valid_delim(r.delimiter) {
		return IError(&InvalidDelimiterError{})
	}
	mut need_read := true
	mut keep_raw := false
	mut line := ''
	mut fields := []string{}
	mut i := -1
	for {
		if need_read {
			l := r.read_line() ?
			if l.len <= 0 {
				if keep_raw {
					line += '\n'
				}
				continue
			} else if l[0] == r.comment {
				if keep_raw {
					line += '\n' + l
				}
				continue
			} else {
				if keep_raw {
					line += '\n'
				}
				line += l
			}
			need_read = false
			keep_raw = false
		}
		if line.len == 0 || line[0] != `"` { // not quoted
			j := line.index(r.delimiter.ascii_str()) or {
				// last
				fields << line[..line.len]
				break
			}
			i = j
			fields << line[..i]
			line = line[i + 1..]
			continue
		} else { // quoted
			mut need_more := true
			mut has_double_quotes := false
			mut j := 0
			mut n := 1
			for n < line.len {
				if line[n] == `"` {
					if n == line.len - 1 || line[n + 1] != `"` {
						need_more = false
						j = n - 1
						break
					} else {
						has_double_quotes = true
						n++
					}
				}
				n++
			}
			if need_more {
				need_read = true
				keep_raw = true
				continue
			}
			line = line[1..]
			if j + 1 == line.len {
				// last record
				fields << if has_double_quotes { line[..j].replace('""', '"') } else { line[..j] }
				break
			}
			next := line[j + 1]
			if next == r.delimiter {
				fields << if has_double_quotes { line[..j].replace('""', '"') } else { line[..j] }
				if j + 2 == line.len {
					line = ''
				} else {
					line = line[j + 2..]
				}
				continue
			}
		}
		if i <= -1 && fields.len == 0 {
			return IError(&InvalidDelimiterError{})
		}
	}
	return fields
}

fn valid_delim(b byte) bool {
	return b != 0 && b != `"` && b != `\r` && b != `\n`
}
