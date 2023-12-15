/*
csv reader 1.0 alpha

Copyright (c) 2023 Dario Deledda. All rights reserved.
Use of this source code is governed by an MIT license
that can be found in the LICENSE file.

This file contains tests

Known limitations:
*/
import encoding.csv
import os

/******************************************************************************
*
* Test Data
*
******************************************************************************/
// dataset 1
const txt1 = '

#
# pippo
#
a,b,c,d,e,f,g
0,dario,.2,3.2e-2,4,"pero5",6
# first comment, test @# again
1,2,3,4,5,6,7
2,3,4,5,6,7,8
3,4,5,6,7,8,9

a,"b,c,d",0,#,3,"pippo"

# last comment
'

const target_header_list = [
	csv.HeaderItem{
		label: 'a'
		column: 0
		htype: .int
	},
	csv.HeaderItem{
		label: 'b'
		column: 1
		htype: .string
	},
	csv.HeaderItem{
		label: 'c'
		column: 2
		htype: .f32
	},
	csv.HeaderItem{
		label: 'd'
		column: 3
		htype: .f32
	},
	csv.HeaderItem{
		label: 'e'
		column: 4
		htype: .int
	},
	csv.HeaderItem{
		label: 'f'
		column: 5
		htype: .string
	},
	csv.HeaderItem{
		label: 'g'
		column: 6
		htype: .int
	},
]

const target_data = [
	['a', 'b', 'c', 'd', 'e', 'f', 'g'],
	['0', 'dario', '.2', '3.2e-2', '4', '"pero5"', '6'],
	['1', '2', '3', '4', '5', '6', '7'],
	['2', '3', '4', '5', '6', '7', '8'],
	['3', '4', '5', '6', '7', '8', '9'],
	['a', '"b,c,d"', '0', '#', '3', '"pippo"'], // 6 columns for test purpose
]

// dataset 2 crlf string from windows
const txt2 = '

#
# pippo
#
a,b,c,d,e,f,g
0,dario,.2,3.2e-2,4,"pero5",6
# first comment, test @# again
1,2,3,4,5,6,7
2,3,4,5,6,7,8
3,4,5,6,7,8,9

a,"b,c,d",,#,3,"pippo"

# last comment
'

// dataset 3/4
const txt3 = 'a,b,c,d\r\n0,1,2,3\r\n4,5,6,7\r\n'
const txt4 = 'a,b,c,d\n0,1,2,3\n4,5,6,7\n'
/******************************************************************************
*
* Test Sequential Functions
*
******************************************************************************/
fn test_csv_sequential() {
	mut csvr := csv.csv_sequential_reader(scr_buf: txt1.str, scr_buf_len: txt1.len)!
	mut data := [][]string{}
	for csvr.has_data() > 1 {
		data << csvr.get_next_row()!
	}
	csvr.dispose_csv_reader()
	assert data[0][0] == 'a', 'test_csv_sequential1 reading failed!'
	// there is a final empty row in txt1
	assert data[data.len - 2][0] == 'a', 'test_csv_sequential2 reading failed!'
	assert data[data.len - 2][1] == 'b,c,d', 'test_csv_sequential3 reading failed!'

	csvr = csv.csv_sequential_reader(scr_buf: txt2.str, scr_buf_len: txt2.len)!
	csvr.empty_cell = '####'
	data = [][]string{}
	for csvr.has_data() > 1 {
		data << csvr.get_next_row()!
	}
	csvr.dispose_csv_reader()
	assert data[data.len - 2][2] == '####', 'test_csv_sequential4 reading failed!'
	assert data[data.len - 2][5] == 'pippo', 'test_csv_sequential5 reading failed!'

	// create a temp file to test csv parsing from file
	file_path_str := os.join_path(os.temp_dir(), 'test_csv.csv')
	// println("file_path_str: ${file_path_str}")

	// test Windows confguration
	mut tmp_txt1 := txt1.replace('\n', '\r\n')

	mut f := os.open_file(file_path_str, 'wb')!
	unsafe {
		f.write_ptr(tmp_txt1.str, tmp_txt1.len)
	}
	// f.write_string(tmp_txt1)!
	f.close()

	csvr = csv.csv_sequential_reader(
		file_path: file_path_str
		mem_buf_size: 64
		end_line_len: csv.endline_crlf_len
	)!
	data = [][]string{}
	for csvr.has_data() > 1 {
		data << csvr.get_next_row()!
	}
	csvr.dispose_csv_reader()

	assert data[0][0] == 'a', 'test_csv_sequential1 reading failed!'
	// there is a final empty row in txt1
	assert data[data.len - 2][0] == 'a', 'test_csv_sequential2 reading failed!'
	assert data[data.len - 2][1] == 'b,c,d', 'test_csv_sequential3 reading failed!'

	// remove the temp file
	os.rm(file_path_str)!
}

/******************************************************************************
*
* Test Random Access Functions
*
******************************************************************************/
fn perform_test(mut csvr csv.RandomAccessReader) ! {
	csvr.build_header_dict(csv.GetHeaderConf{})!

	// test the Header reader
	// println("csvr.header_list: ${csvr.header_list}")
	assert csvr.header_list == target_header_list, 'header_list not matched!'

	/*	
	println("--------------------------------")
	for x in csvr.csv_map#[..5] {
		println(x.len)
		println(x)
	}
	println("--------------------------------")
	*/

	// test the data reading
	mut data := [][]string{len: csvr.csv_map.len}
	for x in 0 .. csvr.csv_map.len {
		data[x] = csvr.get_row(x)!
		// if x % 10000 == 0 {
		//	println("#${x:-6d}")
		//}
	}

	/*	
	// debug print
	println("---------------")
	for x in 0..csvr.csv_map.len {
		println(csvr.get_row(x)!)
	}
	*/

	// test if we have the same amount of data rows
	assert data.len == csvr.csv_map.len, 'data len not equal'

	// test the data retriever
	for row_count, row in target_data {
		// println("${data[row_count]} ${row}")
		assert data[row_count] == row, ''
	}

	// test lfcr cr
	assert csvr.get_cell(x: 6, y: 4)! == '9'

	// test the get cell behaviour
	assert csvr.get_cell(x: csvr.header_map['b'], y: 1)! == 'dario', 'get_cell failed 1'
	assert csvr.get_cell(x: csvr.header_map['g'], y: 5)! == csvr.default_cell, 'get_cell out of data failed 2'
	assert csvr.get_cellt(x: 0, y: 1)! == csv.CellValue(0), 'get_cellt [int] failed'
	assert csvr.get_cellt(x: 1, y: 1)! == csv.CellValue('dario'), 'get_cellt [string] failed'
	assert csvr.get_cellt(x: 2, y: 1)! == csv.CellValue(f32(.2)), 'get_cell [f32] failed'

	// test the filter quote flag
	csvr.quote_remove = true
	assert csvr.get_cell(x: 1, y: 5)! == 'b,c,d', 'get_cell filer quote flag failed'
}

fn perform_test2(mut csvr csv.RandomAccessReader) ! {
	csvr.build_header_dict(csv.GetHeaderConf{})!
	// test the empty cells
	assert csvr.get_cell(x: csvr.header_map['c'], y: 5)! == csvr.empty_cell, 'get_cell empty_cell failed 2'
}

fn perform_test3(mut csvr csv.RandomAccessReader) ! {
	csvr.build_header_dict(csv.GetHeaderConf{})!
	/*
	// debug print
	println("---------------")
	for x in 0..csvr.csv_map.len {
		println(csvr.get_row(x)!)
	}
	*/
	assert csvr.get_cell(x: csvr.header_map['d'], y: 2)! == '7', 'test \n \r\n failed'
}

fn test_csv_string() {
	// test the csv parsing from RAM
	mut csvr := csv.csv_reader_from_string(txt1)!
	perform_test(mut csvr)!
	csvr.dispose_csv_reader()

	// create a temp file to test csv parsing from file
	file_path_str := os.join_path(os.temp_dir(), 'test_csv.csv')
	// println("file_path_str: ${file_path_str}")

	// test Windows confguration
	mut tmp_txt1 := txt1.replace('\n', '\r\n')

	mut f := os.open_file(file_path_str, 'wb')!
	unsafe {
		f.write_ptr(tmp_txt1.str, tmp_txt1.len)
	}
	// f.write_string(tmp_txt1)!
	f.close()

	// parse the temp file
	csvr = csv.csv_reader(
		file_path: file_path_str
		mem_buf_size: 64
		end_line_len: csv.endline_crlf_len
	)!
	perform_test(mut csvr)!
	csvr.dispose_csv_reader()

	// remove the temp file
	os.rm(file_path_str)!

	csvr = csv.csv_reader_from_string(txt2)!
	perform_test2(mut csvr)!
	csvr.dispose_csv_reader()

	// test crlf endline
	csvr = csv.csv_reader(
		scr_buf: txt3.str
		scr_buf_len: txt3.len
		end_line_len: csv.endline_crlf_len
	)!
	perform_test3(mut csvr)!
	csvr.dispose_csv_reader()

	// test cr endline
	csvr = csv.csv_reader(scr_buf: txt4.str, scr_buf_len: txt4.len, end_line_len: csv.endline_cr_len)!
	perform_test3(mut csvr)!
	csvr.dispose_csv_reader()
}

// Debug code
fn main() {
	test_csv_string()
}
