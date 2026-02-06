# CSV Reader
There are two CSV readers in this module:

* Random Access reader
* Sequential reader
 
# Sequential CSV reader
The sequential reader read the file row by row using only the memory needed for readings.
Here is a very simple example of usage:

```v
import encoding.csv

fn main() {
	mut csvr := csv.csv_sequential_reader(file_path: 'big2.csv', end_line_len: csv.endline_crlf_len)!
	for csvr.has_data() > 1 {
		println(csvr.get_next_row()!)
	}
	csvr.dispose_csv_reader()
}
```
This is the simplest way to use it to read csv files in sequential mode,
with default configuration every cell is read as `string`.
The function `get_row()` is used to read a single row, and it returns an array of `string`.

## Reading from different sources `csv_sequential_reader`
The CSV Sequential Reader can read from files, and memory buffers.

### read from a file
```v ignore
csv.csv_sequential_reader(file_path:file_path)
```
### read from a memory buffer
```v ignore
csv.csv_sequential_reader(scr_buf:voidptr(buffer_ptr),  scr_buf_len: buffer_len)
```
When you call `csv.csv_sequential_reader` a `SequentialReader` struct is initialized passing 
a `SequentialReaderConfig` struct as a parameter.
Using these structs, it is possible to change the behavior of the CSV Reader.

## The `SequentialReaderConfig` struct
The config struct is as follows:
```v ignore
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
	quote        u8  = `"` // double quote is the standard quote char
}
```
|Field|Description|
|------------|--------------|
|`scr_buf`, `scr_buf_len`|If `scr_buf_len > 0` the reader will use the  `scr_buf` pointer as the base address of the data to parse and  `scr_buf_len` as the length of the buffer itself|
|`file_path`| if `scr_buf_len == 0` the reader will try to open the `file_path`  file|
|`start_index`,`end_index`| **Internal usage for now**|
|`mem_buf_size`|memory allocated for the reading operations on the file, more memory more speed|
|`separator`|char used as cell separator in the CSV file, default is comma|
|`comment`|every line that start with the comment char is ignored|
|`default_cell`|return this string if the query coordinates are out of the csv boundaries|
|`empty_cell`|return this string if the query coordinates are on an empty cell|
|`end_line_len`|size of the endline, `endline_cr_len=1`,`endline_crlf_len=2`|
|`quote`|quote char for the cells|


# Random Access CSV Reader
The Random Access CSV file reader indexes the file before reading the data.
This indexing operation permits access to every cell of the CSV file in random order.
Here is a very simple example of usage:

```v
import encoding.csv

const txt = '
a,b,c
0,1,2
3,4,5
'

fn main() {
	mut csvr := csv.csv_reader_from_string(txt)!
	// scan all rows, csvr.csv_map.len contain the valid
	// rows number in the CSV file.
	for row_index in 0 .. csvr.csv_map.len {
		row := csvr.get_row(row_index)!
		println(row)
	}
	csvr.dispose_csv_reader()
}
```
will give the following output:
```
['a', 'b', 'c']
['0', '1', '2']
['3', '4', '5']
```
This is the simplest way to use it to read csv files in a random access mode, 
with default configuration every cell is read as `string`.
The function `get_row()` is used to read a single row, and it returns an array of `string`.

## Reading from different sources `csv_reader`
The CSV Random access Reader can read from files, strings, memory buffers.
### read from a file
```v ignore
csv.csv_reader(file_path:file_path)
```
### read from a string 
```v ignore
csv.csv_reader_from_string(string_with_the_csv)
```
*Note: csv_reader_from_string is "syntax sugar" for buffered reading*
### read from a memory buffer
```v ignore
csv.csv_reader(scr_buf:voidptr(buffer_ptr),  scr_buf_len: buffer_len)
```
When you call `csv.csv_reader` a `RandomAccessReader` struct is initialized passing 
a `RandomAccessReaderConfig` struct as a parameter.
Using these structs, it is possible to change the behavior of the CSV Reader.

## The `RandomAccessReaderConfig` struct
The config struct is as follows:
```v ignore
pub struct RandomAccessReaderConfig {
pub:
	scr_buf      voidptr // pointer to the buffer of data
	scr_buf_len  i64     // if > 0 use the RAM pointed from scr_buf as source of data
	file_path    string
	start_index  i64
	end_index    i64    = -1
	mem_buf_size int    = 1024 * 64 // default buffer size 64KByte
	separator    u8     = `,`
	comment      u8     = `#` // every line that start with the comment char is ignored
	default_cell string = '*' // return this string if out of the csv boundaries
	empty_cell   string // return this string if empty cell
	end_line_len int = csv.endline_cr_len // size of the endline rune
	quote        u8  = `"` // double quote is the standard quote char
	quote_remove bool   // if true clear the cell from the quotes
}
```
|Field|Description|
|------------|--------------|
|`scr_buf`, `scr_buf_len`|If `scr_buf_len > 0` the reader will use the  `scr_buf` pointer as the base address of the data to parse and  `scr_buf_len` as the length of the buffer itself|
|`file_path`| if `scr_buf_len == 0` the reader will try to open the `file_path`  file|
|`start_index`,`end_index`| **Internal usage for now**|
|`mem_buf_size`|memory allocated for the reading operations on the file, more memory more speed|
|`separator`|char used as cell separator in the CSV file, default is comma|
|`comment`|every line that start with the comment char is ignored
|`default_cell`|return this string if the query coordinates are out of the csv boundaries|
|`empty_cell`|return this string if the query coordinates are on an empty cell|
|`end_line_len`|size of the endline, `endline_cr_len=1`,`endline_crlf_len=2`|
|`quote`|quote char for the cells|
|`quote_remove`| if **true** try to remove the quotes from each cell, use only in quoted csv files|

## Random Access reading
The main feature of this module is to allow random access to CSV file cells, 
such as this example with custom configuration:
```v
import encoding.csv

const txt = '
a,b,c
0,1,2
3,,5
'

fn main() {
	mut csvr := csv.csv_reader_from_string(txt)!

	// we are directly setting these params in the `RandomAccessReader` struct
	csvr.default_cell = '*'
	csvr.empty_cell = 'EMPTY'

	// read a single cell at the row 1 column 0
	println('[0,1] => ${csvr.get_cell(x: 0, y: 1)!}')
	// try a non existing cell. it will return `default_cell` string
	println('[0,4] => ${csvr.get_cell(x: 0, y: 4)!}')
	// try an empty string. it will return `empty_cell` string
	println('[1,2] => ${csvr.get_cell(x: 1, y: 2)!}')
	csvr.dispose_csv_reader()
}
```
Output:
```
[0,1] => 0
[0,4] => *
[1,2] => EMPTY
```
The function `csvr.get_cell()` allows reading a single cell as a `string`.

## Using the header
This example reads the CSV file header (if present) to simplify 
some read operations on the CSV file.
```v
import encoding.csv

const txt = '
a,b,c
0,pippo,1.2
1,pero,2.3
'

fn main() {
	mut csvr := csv.csv_reader_from_string(txt)!

	// try to create the header, it need the header and
	// at least one row of data
	csvr.build_header_dict(csv.GetHeaderConf{})!
	println('Header: ${csvr.header_list}')

	// simple cell read usign the header map
	println(csvr.get_cell(x: csvr.header_map['b'], y: 1)!)

	// get typed value instead of a string
	println(csvr.get_cellt(x: csvr.header_map['b'], y: 2)! as string)

	csvr.dispose_csv_reader()
}
```
Output:
```
Header: [csv.HeaderItem{
    label: 'a'
    column: 0
    htype: int
}, csv.HeaderItem{
    label: 'b'
    column: 1
    htype: string
}, csv.HeaderItem{
    label: 'c'
    column: 2
    htype: f32
}]
pippo
pero
```
In this example we have seen:
* how to generate the header with `build_header_dict(csv.GetHeaderConf{})`
* how to read a cell as a string with `get_cell(x:csvr.header_map['b'], y:1)`
* how to read a cell as a sum type `CellValue` using `get_cellt(x:csvr.header_map['b'], y:2)`

The sum type `CellValue` is defined as the following:
``` 
type CellValue = int | f32 | string
```
## Read columns
In this example we read two columns from a big file from disk:
```v
import encoding.csv

fn main() {
	file_path := 'big2.csv'
	mut csvr := csv.csv_reader(
		file_path:    file_path                     // path to the file CSV
		mem_buf_size: 1024 * 1024 * 64              // we set 64MByte of buffer for this file
		end_line_len: csv.endline_crlf_len // we are using a windows text file
	)!
	// The data will be saved in this array
	mut data := [][]string{len: csvr.csv_map.len}
	for row_index in 1 .. csvr.csv_map.len {
		// get single cells
		data[row_index] << csvr.get_cell(x: 2, y: row_index)!
		data[row_index] << csvr.get_cell(x: 3, y: row_index)!
	}

	csvr.dispose_csv_reader()
}
```
This style of reading is faster then reading an entire row then extracting the needed data.

## Comments and blank lines
This example shows how to handle blank rows and comment rows in the file:
```v
import encoding.csv

const txt = '
# this is a comment line
a,b,c



0,1,2

3,4,5
# another comment
'

fn main() {
	mut csvr := csv.csv_reader(
		scr_buf:     txt.str
		scr_buf_len: txt.len
		comment:     `#` // line starting with # will be ignored
	)!
	// scan all rows, csvr.csv_map.len contain the valid
	// rows number in the CSV file.
	for row_index in 0 .. csvr.csv_map.len {
		row := csvr.get_row(row_index)!
		println(row)
	}
	csvr.dispose_csv_reader()
}
```
Output:
```
['a', 'b', 'c']
['0', '1', '2']
['3', '4', '5']
```
## Quote char
This example shows how to handle quoted cells:
```v
import encoding.csv

const txt = "
# comment line
'a' , 'b', 'c'
'1' , '2', '3'
'4' ,'5', 'a,b,c', 'e'
"

fn main() {
	mut csvr := csv.csv_reader(
		scr_buf:      txt.str // string pointer
		scr_buf_len:  txt.len // string length
		comment:      `#`     // line starting with # will be ignored
		quote:        `'`     // char used for quotes
		quote_remove: true    // remove quotes from the cells
	)!

	// scan all rows, csvr.csv_map.len contain the valid
	// rows number in the CSV file.
	for row_index in 0 .. csvr.csv_map.len {
		row := csvr.get_row(row_index)!
		println(row)
	}
	csvr.dispose_csv_reader()
}
```
Output:
```
['a', 'b', 'c']
['1', '2', '3']
['4', '5', 'a,b,c', 'e']
```
## Performance
This module was tested with CSV files up to 4 GBs with 4 million rows