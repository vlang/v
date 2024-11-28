/*
csv serial reader 1.0 alpha

Copyright (c) 2023 Dario Deledda. All rights reserved.
Use of this source code is governed by an MIT license
that can be found in the LICENSE file.

Known limitations:
*/
module csv

import os

@[params]
pub struct SequentialReaderConfig {
pub:
	scr_buf      voidptr // pointer to the buffer of data
	scr_buf_len  i64     // if > 0 use the RAM pointed by scr_buf as source of data
	file_path    string
	start_index  i64
	end_index    i64    = -1
	mem_buf_size int    = 1024 * 64 // default buffer size 64KByte
	separator    u8     = `,`
	comment      u8     = `#` // every line that start with the comment char is ignored
	default_cell string = '*' // return this string if out of the csv boundaries
	empty_cell   string // return this string if empty cell
	end_line_len int = endline_cr_len // size of the endline rune
	quote        u8  = `"`            // double quote is the standard quote char
}

pub struct SequentialReader {
pub mut:
	index i64

	f              os.File
	f_len          i64
	is_bom_present bool

	start_index i64
	end_index   i64 = -1

	end_line      u8  = `\n`
	end_line_len  int = endline_cr_len // size of the endline rune \n = 1, \r\n = 2
	separator     u8  = `,`            // comma is the default separator
	separator_len int = 1              // size of the separator rune
	quote         u8  = `"`            // double quote is the standard quote char

	comment u8 = `#` // every line that start with the quote char is ignored

	default_cell string = '*' // return this string if out of the csv boundaries
	empty_cell   string = '#' // retunrn this if empty cell
	// ram buffer
	mem_buf_type  u32     // buffer type 0=File,1=RAM
	mem_buf       voidptr // buffer used to load chars from file
	mem_buf_size  i64     // size of the buffer
	mem_buf_start i64 = -1 // start index in the file of the read buffer
	mem_buf_end   i64 = -1 // end index in the file of the read buffer

	ch_buf []u8 = []u8{cap: 1024}
	// error management
	row_count i64
	col_count i64
}

// csv_sequential_reader creates a sequential csv reader
pub fn csv_sequential_reader(cfg SequentialReaderConfig) !&SequentialReader {
	mut cr := &SequentialReader{}

	cr.start_index = cfg.start_index
	cr.end_index = cfg.end_index

	// reading from a RAM buffer
	if cfg.scr_buf != 0 && cfg.scr_buf_len > 0 {
		cr.mem_buf_type = ram_csv // RAM buffer
		cr.mem_buf = cfg.scr_buf
		cr.mem_buf_size = cfg.scr_buf_len
		if cfg.end_index == -1 {
			cr.end_index = cfg.scr_buf_len
		}

		// check if BOM header is in the memory buffer
		unsafe {
			if *&u8(cr.mem_buf) == 0xEF && *(&u8(cr.mem_buf) + 1) == 0xBB
				&& *(&u8(cr.mem_buf) + 2) == 0xBF {
				cr.is_bom_present = true
				cr.index += 3 // skip the BOM
				cr.start_index += 3 // skip the BOM
			}
		}
		cr.mem_buf_start = 0
		cr.mem_buf_end = cr.mem_buf_size

		// check if is a file source
	} else if cfg.file_path.len > 0 {
		if !os.exists(cfg.file_path) {
			return error('ERROR: file ${cfg.file_path} not found!')
		}
		cr.mem_buf_type = file_csv // File buffer
		// allocate the memory
		unsafe {
			cr.mem_buf = malloc(cfg.mem_buf_size)
			cr.mem_buf_size = cfg.mem_buf_size
		}
		cr.f = os.open_file(cfg.file_path, 'rb')!

		cr.f.seek(0, .end)!
		cr.f_len = cr.f.tell()!

		cr.f.seek(cfg.start_index, .start)!
		cr.index = cr.f.tell()!

		if cfg.end_index == -1 {
			cr.end_index = cr.f_len
		}

		// check if BOM header is in the file
		if cr.index == 0 {
			if cr.f.read_into_ptr(cr.mem_buf, 4)! == 4 {
				unsafe {
					if *&u8(cr.mem_buf) == 0xEF && *(&u8(cr.mem_buf) + 1) == 0xBB
						&& *(&u8(cr.mem_buf) + 2) == 0xBF {
						cr.is_bom_present = true
						cr.index += 3 // skip the BOM
						cr.start_index += 3 // skip the BOM
					}
				}
			}
			cr.f.seek(cfg.start_index, .start)!
		}
	}

	cr.default_cell = cfg.default_cell
	cr.empty_cell = cfg.empty_cell
	cr.end_line_len = cfg.end_line_len
	cr.separator = cfg.separator
	cr.comment = cfg.comment
	cr.quote = cfg.quote

	return cr
}

// dispose_csv_reader release the resources used by the csv_reader
pub fn (mut cr SequentialReader) dispose_csv_reader() {
	if cr.mem_buf_type == ram_csv {
		// do nothing, ram buffer is static
	} else if cr.mem_buf_type == file_csv {
		// file close
		if cr.f.is_opened {
			cr.f.close()
		}

		// free the allocated memory
		if cr.mem_buf_size > 0 {
			unsafe {
				free(cr.mem_buf)
			}
			cr.mem_buf = unsafe { nil }
			cr.mem_buf_size = 0
		}
	}
}

// has_data return the bytes available for future readings
pub fn (mut cr SequentialReader) has_data() i64 {
	return cr.end_index - cr.start_index
}

fn (mut cr SequentialReader) fill_buffer(index i64) ! {
	if cr.mem_buf_type == ram_csv {
		// for now do nothing if ram buffer
	} else {
		cr.f.seek(index, .start)!
		// IMPORTANT: add 64 bit support in vlib!!
		read_bytes_count := cr.f.read_into_ptr(cr.mem_buf, int(cr.mem_buf_size))!
		cr.mem_buf_start = index
		cr.mem_buf_end = index + read_bytes_count
	}
}

enum SequentialReadingState as u16 {
	comment
	quote
	after_quote
	cell
	newline
}

// get_next_row get the next row from the CSV file as a string array
pub fn (mut cr SequentialReader) get_next_row() ![]string {
	mut row_res := []string{}
	// clear the cell buffer
	cr.ch_buf.clear()
	mut i := cr.start_index
	mut state := SequentialReadingState.cell

	p := &u8(cr.mem_buf)
	for i < cr.end_index {
		if i < cr.mem_buf_start || i >= cr.mem_buf_end {
			cr.fill_buffer(i)!
		}
		unsafe {
			ch := *(p + i - cr.mem_buf_start)

			if state == .cell {
				if ch == cr.separator {
					// must be optimized
					cr.ch_buf << 0
					row_res << if (cr.ch_buf.len - 1) == 0 {
						cr.empty_cell
					} else {
						(tos(cr.ch_buf.data, cr.ch_buf.len - 1).clone())
					}
					cr.ch_buf.clear()
				} else if cr.ch_buf.len == 0 && ch == cr.comment && row_res.len == 0 {
					state = .comment
				} else if ch == cr.quote {
					state = .quote
					cr.ch_buf.clear()
					cr.col_count++
					i++
					continue
				} else if ch == cr.end_line {
					cr.row_count++
					cr.col_count = 0

					// skip empty rows
					if !(row_res.len == 0 && cr.ch_buf.len < 1) {
						cr.ch_buf << 0
						row_res << if (cr.ch_buf.len - 1) == 0 {
							cr.empty_cell
						} else {
							(tos(cr.ch_buf.data, cr.ch_buf.len - 1).clone())
						}
						i += cr.end_line_len - 1
						break
					}
				} else if ch == `\r` && cr.end_line_len == 2 {
					// skip CR
				} else { // normal char inside a cell
					cr.ch_buf << ch
				}
			}

			if state == .comment {
				if cr.ch_buf.len > 0 {
					// must be optimized
					cr.ch_buf << 0
					row_res << if (cr.ch_buf.len - 1) == 0 {
						cr.empty_cell
					} else {
						(tos(cr.ch_buf.data, cr.ch_buf.len - 1).clone())
					}
					cr.ch_buf.clear()
				} else if ch == cr.end_line {
					state = .cell
				}
			}

			if state == .quote {
				if ch == cr.quote {
					// must be optimized
					cr.ch_buf << 0
					row_res << if (cr.ch_buf.len - 1) == 0 {
						cr.empty_cell
					} else {
						(tos(cr.ch_buf.data, cr.ch_buf.len - 1).clone())
					}
					cr.ch_buf.clear()

					state = .after_quote
					cr.col_count++
					i++
					continue
				} else if ch == cr.end_line {
					return error('ERROR: quote not closed at row ${cr.row_count} after column ${cr.col_count}!')
				} else { // normal char inside a quote inside a cell
					cr.ch_buf << ch
				}
			}

			if state == .after_quote {
				if ch == cr.separator {
					state = .cell
				} else if ch == cr.end_line {
					cr.row_count++
					cr.col_count = 0
					cr.ch_buf.clear()
					i += cr.end_line_len - 1
					break
				}
			}
		}
		cr.col_count++
		i++
	}
	cr.start_index = i
	return row_res
}
