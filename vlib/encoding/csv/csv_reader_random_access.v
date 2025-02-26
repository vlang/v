/*
csv random access reader 1.0 alpha

Copyright (c) 2023 Dario Deledda. All rights reserved.
Use of this source code is governed by an MIT license
that can be found in the LICENSE file.

Known limitations:
- no stream reading
*/
module csv

import os

/******************************************************************************
*
* Consts
*
******************************************************************************/
// endline lengths
pub const endline_cr_len = 1
pub const endline_crlf_len = 2

// Type of read buffer
pub const ram_csv = 1
pub const file_csv = 0

/******************************************************************************
*
* Structs
*
******************************************************************************/
pub enum ColumType {
	string = 0
	int    = 1
	f32    = 2
}

pub struct HeaderItem {
pub mut:
	label  string
	column int
	htype  ColumType = .string
}

@[heap]
pub struct RandomAccessReader {
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
	quote_remove  bool // if true clear the cell from the quotes
	comment       u8 = `#` // every line that start with the quote char is ignored

	default_cell string = '*' // return this string if out of the csv boundaries
	empty_cell   string = '#' // retunrn this if empty cell
	// ram buffer
	mem_buf_type  u32     // buffer type 0=File,1=RAM
	mem_buf       voidptr // buffer used to load chars from file
	mem_buf_size  i64     // size of the buffer
	mem_buf_start i64 = -1 // start index in the file of the read buffer
	mem_buf_end   i64 = -1 // end index in the file of the read buffer
	// csv map for quick access
	create_map_csv bool = true // flag to enable the csv map creation
	csv_map        [][]i64
	// header
	header_row  int = -1 // row index of the header in the csv_map
	header_list []HeaderItem   // list of the header item
	header_map  map[string]int // map from header label to column index
}

@[params]
pub struct RandomAccessReaderConfig {
pub:
	scr_buf        voidptr // pointer to the buffer of data
	scr_buf_len    i64     // if > 0 use the RAM pointed from scr_buf as source of data
	file_path      string
	start_index    i64
	end_index      i64    = -1
	mem_buf_size   int    = 1024 * 64 // default buffer size 64KByte
	separator      u8     = `,`
	comment        u8     = `#` // every line that start with the quote char is ignored
	default_cell   string = '*' // return this string if out of the csv boundaries
	empty_cell     string // return this string if empty cell
	end_line_len   int = endline_cr_len // size of the endline rune
	quote          u8  = `"`            // double quote is the standard quote char
	quote_remove   bool // if true clear the cell from the quotes
	create_map_csv bool = true // if true make the map of the csv file
}

/******************************************************************************
*
* Init, dispose, fill buffer
*
******************************************************************************/

// csv_reader_from_string create a csv reader from a string
pub fn csv_reader_from_string(in_str string) !&RandomAccessReader {
	return csv_reader(RandomAccessReaderConfig{ scr_buf: in_str.str, scr_buf_len: in_str.len })!
}

// csv_reader create a random access csv reader
pub fn csv_reader(cfg RandomAccessReaderConfig) !&RandomAccessReader {
	mut cr := &RandomAccessReader{}

	cr.start_index = cfg.start_index
	cr.end_index = cfg.end_index

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
			}
		}
	}
	// check if is a file source
	else if cfg.file_path.len > 0 {
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
	cr.quote_remove = cfg.quote_remove
	cr.quote = cfg.quote

	cr.create_map_csv = cfg.create_map_csv
	if cr.create_map_csv {
		cr.map_csv()!
	}

	return cr
}

// dispose_csv_reader release the resources used by the csv_reader
pub fn (mut cr RandomAccessReader) dispose_csv_reader() {
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

fn (mut cr RandomAccessReader) fill_buffer(i i64) !i64 {
	// use ram
	if cr.mem_buf_type == ram_csv {
		// do nothing, ram buffer are static for now
		cr.mem_buf_start = i
		cr.mem_buf_end = cr.mem_buf_size
		read_bytes_count := cr.mem_buf_end - cr.mem_buf_start
		// println("fill_buffer RAM: ${i} read_bytes_count: ${read_bytes_count} mem_buf_start: ${cr.mem_buf_start} mem_buf_end: ${cr.mem_buf_end}")
		return i64(read_bytes_count)
		// use file
	} else if cr.mem_buf_type == file_csv {
		cr.start_index = i
		cr.f.seek(cr.start_index, .start)!
		// IMPORTANT: add 64 bit support in vlib!!
		read_bytes_count := cr.f.read_into_ptr(cr.mem_buf, int(cr.mem_buf_size))!
		cr.mem_buf_start = i
		cr.mem_buf_end = i + read_bytes_count
		// println("fill_buffer FILE: ${i} read_bytes_count: ${read_bytes_count} mem_buf_start: ${cr.mem_buf_start} mem_buf_end: ${cr.mem_buf_end}")
		return i64(read_bytes_count)
	}
	return i64(-1)
}

// copy_configuration copies the configuration from another csv RandomAccessReader
// this function is a helper for using the RandomAccessReader in multi threaded applications
// pay attention to the free process
pub fn (mut cr RandomAccessReader) copy_configuration(src_cr RandomAccessReader) {
	cr.header_row = src_cr.header_row
	unsafe {
		cr.header_list = &src_cr.header_list
		cr.header_map = &src_cr.header_map
		cr.csv_map = &src_cr.csv_map
	}
}

/******************************************************************************
*
* Csv mapper, mapped reader
*
******************************************************************************/
// map_csv create an index of whole csv file to consent random access to every cell in the file
pub fn (mut cr RandomAccessReader) map_csv() ! {
	mut count := 0
	mut i := i64(0)
	mut capture_flag := true
	mut drop_row := false
	mut quote_flag := false // true if we are parsing inside a quote

	// if File return to the start of the file
	if cr.mem_buf_type == file_csv {
		cr.f.seek(cr.start_index, .start)!
	}

	unsafe {
		p := &u8(cr.mem_buf)
		cr.csv_map << []i64{}
		cr.csv_map[0] << if cr.is_bom_present { 3 } else { 0 } // skip the BOM data

		// mut counter := i64(0)
		for i < cr.end_index {
			read_bytes_count := cr.fill_buffer(i)!

			// DEBUG print
			// perc := f32(counter) / f32(cr.end_index) * 100.0
			// println("${perc:.2f}")

			// println("${i:-12d} of ${cr.f_len:-12d} readed: ${read_bytes_count}")
			mut p1 := p
			mut i1 := i64(0)
			for i1 < read_bytes_count {
				// println("loop char: ${*&u8(p1):c}")
				// manage quote char
				if *p1 == cr.quote {
					quote_flag = !quote_flag
					p1++
					i1++
				}
				else if // manage comment line
				 !quote_flag && *p1 == cr.comment && cr.csv_map[cr.csv_map.len - 1].len <= 1 {
					drop_row = true
					p1++
					i1++
					// println("drop_row: ${cr.csv_map.len - 1}")
				}
				else if // capture separator
				 !quote_flag && capture_flag && *p1 == cr.separator && !drop_row {
					cr.csv_map[cr.csv_map.len - 1] << (i + i1)

					p1 += cr.separator_len
					i1 += cr.separator_len
				}
				else if // capture end line
				 *p1 == cr.end_line {
					if quote_flag {
						error_col := cr.csv_map[cr.csv_map.len - 1].last() - cr.csv_map[cr.csv_map.len - 1].first()
						return error('ERROR: quote not closed at row ${count} after column ${error_col}!')
					}
					count++

					cr.csv_map[cr.csv_map.len - 1] << (i + i1) - (cr.end_line_len - 1)
					p1 += cr.end_line_len
					i1 += cr.end_line_len

					if drop_row == true {
						cr.csv_map[cr.csv_map.len - 1].clear()
						drop_row = false
					} else {
						// skip empty rows
						if cr.csv_map[cr.csv_map.len - 1].len == 2
							&& cr.csv_map[cr.csv_map.len - 1][0] == cr.csv_map[cr.csv_map.len - 1][1] {
							// recycle the row
							cr.csv_map[cr.csv_map.len - 1].clear()
						} else {
							// it all ok, insert a new row
							cr.csv_map << []i64{cap: cr.csv_map[cr.csv_map.len - 1].len}
						}
					}

					cr.csv_map[cr.csv_map.len - 1] << (i + i1) - (cr.end_line_len - 1)

					p1 -= (cr.end_line_len - 1)
					i1 -= (cr.end_line_len - 1)

					// DEBUG checks
					// r := &u8(cr.mem_buf) + (i + i1) - (cr.end_line_len - 1)
					// r := p1
					// println("char: ${*r:c}")
				} else {
					p1++
					i1++
				}
			}
			i += read_bytes_count
			// counter += i1
		}
	}
	// remove last row if it is not a valid one
	if cr.csv_map[cr.csv_map.len - 1].len < 2 {
		cr.csv_map.delete(cr.csv_map.len - 1)
	}

	// if File return to the start of the file
	if cr.mem_buf_type == file_csv {
		cr.f.seek(cr.start_index, .start)!
	}

	// println("map_csv Done! ${count}")
}

// get_row get a row from the CSV file as a string array
pub fn (mut cr RandomAccessReader) get_row(y int) ![]string {
	mut h := []string{}
	if cr.csv_map.len > 1 {
		for x in 0 .. (cr.csv_map[y].len - 1) {
			h << cr.get_cell(x: x, y: y)!
		}
	}
	return h
}

@[params]
pub struct GetCellConfig {
pub:
	x int
	y int
}

// get_cell read a single cel nd return a string
pub fn (mut cr RandomAccessReader) get_cell(cfg GetCellConfig) !string {
	if cfg.y < cr.csv_map.len && cfg.x < (cr.csv_map[cfg.y].len - 1) {
		mut start := cr.csv_map[cfg.y][cfg.x]
		mut end := cr.csv_map[cfg.y][cfg.x + 1]

		if cfg.x > 0 {
			start++
		}

		mut len := end - start
		// println("len calc: ${len}")
		if len <= 0 {
			return cr.empty_cell
		}

		// fill the buffer if needed
		if !(start >= cr.mem_buf_start && end < cr.mem_buf_end) {
			cr.fill_buffer(start)!
		}
		unsafe {
			// execute this section only if we need to remove the quotes
			if cr.quote_remove {
				// println("[${start},${end}] len:${len}")
				// remove front quote and spaces
				mut tmp_p := &u8(cr.mem_buf) + start - cr.start_index
				for start < end {
					if *tmp_p == cr.quote {
						start++
						break
					}
					start++
					tmp_p++
				}
				// println("after start quote filtering [${start},${end}] len:${len}")
				// remove back quote and spaces
				tmp_p = &u8(cr.mem_buf) + end - cr.start_index
				for end > start {
					if *tmp_p == cr.quote {
						break
					}
					tmp_p--
					end--
				}
				// println("after end quote filtering [${start},${end}] len:${len}")

				len = end - start
				// println("len calc2: ${len}")
				if len <= 0 {
					return cr.empty_cell
				}
				// println("[${start},${end}] len:${len}")
			}

			// create the string from the buffer
			mut tmp_mem := malloc_noscan(isize(len + 1))
			/*
			defer {
				free(tmp_mem)
			}
			*/
			mem_start := &u8(cr.mem_buf) + start - cr.start_index
			vmemcpy(tmp_mem, mem_start, isize(len))
			tmp_mem[len] = 0 // 0 for C string compatibility
			ret_str := tos(tmp_mem, int(len))
			return ret_str
		}
	}
	return cr.default_cell
}

pub type CellValue = f32 | int | string

// get_cellt read a single cell and return a sum type CellValue
pub fn (mut cr RandomAccessReader) get_cellt(cfg GetCellConfig) !CellValue {
	if cr.header_row >= 0 && cfg.x < cr.header_list.len {
		h := cr.header_list[cfg.x]
		res := cr.get_cell(cfg)!
		match h.htype {
			.int {
				return res.trim_space().int()
			}
			.string {
				return res
			}
			.f32 {
				return res.trim_space().f32()
			}
		}
	}
	return cr.get_cell(cfg)!
}

/******************************************************************************
*
* Header management
*
******************************************************************************/
@[params]
pub struct GetHeaderConf {
pub:
	header_row int // row where to inspect the header
}

// build_header_dict infer the header, it use the first available row in not row number is passesd
// it try to infer the type of column using the first available row after the header
// By default all the column are of the string type
pub fn (mut cr RandomAccessReader) build_header_dict(cfg GetHeaderConf) ! {
	if cr.csv_map.len > 1 && cfg.header_row >= 0 && cfg.header_row < cr.csv_map.len {
		cr.header_row = cfg.header_row
		for col in 0 .. (cr.csv_map[cfg.header_row].len - 1) {
			// fill the base struct
			label := cr.get_cell(x: col, y: cfg.header_row)!
			mut h := HeaderItem{
				label:  label
				column: col
				htype:  .string
			}

			// try to infer the type if we haev at least one more row
			if cfg.header_row + 1 < cr.csv_map.len {
				x := cr.get_cell(x: col, y: cfg.header_row + 1)!.trim_space().to_lower()
				mut sign_c := int(0)
				mut int_c := int(0)
				mut float_c := int(0)
				mut alpha_c := int(0)
				mut htype := ColumType.string
				// raw extimation fo the type
				for c in x {
					if c in [`+`, `-`] {
						sign_c++
						continue
					}
					if c >= `0` && c <= `9` {
						int_c++
						continue
					}
					if c == `.` {
						float_c++
						continue
					}
					if c in [`e`, `E`] && (float_c > 0 || int_c > 0) {
						float_c++
						continue
					}
					alpha_c++
					break
				}

				// if no alpha_c can be and int or a float
				if alpha_c == 0 {
					if float_c > 0 {
						htype = .f32
					} else {
						htype = .int
					}
				}
				h.htype = htype
			}

			cr.header_list << h
			cr.header_map[label] = col
		}
	}
}

/******************************************************************************
*
* Utility function
*
******************************************************************************/
// rows_count count the rows in the csv between start_index and end_index
pub fn (mut cr RandomAccessReader) rows_count() !i64 {
	mut count := i64(0)
	mut i := i64(0)

	if cr.mem_buf_type == file_csv {
		cr.f.seek(cr.start_index, .start)!
	}
	unsafe {
		p := &u8(cr.mem_buf)
		for i < cr.end_index {
			read_bytes_count := cr.fill_buffer(i)!
			// println("${i:-12d} of ${cr.f_len:-12d} readed: ${read_bytes_count}")
			mut p1 := p
			mut i1 := 0
			for i1 < read_bytes_count {
				if *p1 == cr.end_line {
					count++
				}
				p1++
				i1++
			}
			i += read_bytes_count
		}
	}
	if cr.mem_buf_type == file_csv {
		cr.f.seek(cr.start_index, .start)!
	}
	// println("rows_count Done!")
	return count
}
