// Copyright (c) 2019-2020 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module csv

// Once interfaces are further along the idea would be to have something similar to
// go's io.reader & bufio.reader rather than reading the whole file into string, this
// would then satisfy that interface. I designed it this way to be easily adapted.
const (
	err_comment_is_delim = error('encoding.csv: comment cannot be the same as delimiter')
	err_invalid_delim    = error('encoding.csv: invalid delimiter')
	err_eof              = error('encoding.csv: end of file')
	err_invalid_le       = error('encoding.csv: could not find any valid line endings')
)

struct Reader {
	// not used yet
	// has_header        bool
	// headings          []string
	data              string
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
// 			if error(err).error == err_eof.error {
// 				return records
// 			} else {
// 				return error(err)
// 			}
// 		}
// 		records << record
// 	}
// 	return records
// }
fn (mut r Reader) read_line() ?string {
	// last record
	if r.row_pos == r.data.len {
		return err_eof
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
				return err_invalid_le
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
		return err_comment_is_delim
	}
	if !valid_delim(r.delimiter) {
		return err_invalid_delim
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
		if line[0] != `"` { // not quoted
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
			j := line[1..].index('"') or {
				need_read = true
				keep_raw = true
				continue
			}
			line = line[1..]
			if j + 1 == line.len {
				// last record
				fields << line[..j]
				break
			}
			next := line[j + 1]
			if next == r.delimiter {
				fields << line[..j]
				line = line[j..]
				continue
			}
			line = line[1..]
		}
		if i <= -1 && fields.len == 0 {
			return err_invalid_delim
		}
	}
	return fields
}

fn valid_delim(b byte) bool {
	return b != 0 && b != `"` && b != `\r` && b != `\n`
}
