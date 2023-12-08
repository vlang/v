# CSV Reader
This module is a Random Access CSV file reader, it perform a file indexing before the readings.
This indexing operation permit to access to every cell of the CSV file in a random order.
Here a very simple example of usage:

```v
import encoding.csv
const txt := 
'
a,b,c
0,1,2
3,4,5
'
fn main() {
    mut csvr := csv.csv_reader_from_string(txt)!
    // scan all rows, csvr.csv_map.len contain the valid
    // rows number in the CSV file.
    for row_index in 0..csvr.csv_map.len {
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
This is the simplest way to use it to read csv files, with default configuration every cell is read as `string`.
To read a single row it is used the function `get_row()` that return an array of `string`.

## Reading from different sources `csv_reader`
The CSV Reader can read from files, strings, memory buffers.
### read from a file
```v
csv.csv_reader(file_path:file_path)
```
### read from a string 
```v
csv.csv_reader_from_string(string_with_the_csv)
```
*Note: csv_reader_from_string is a syntactic sugar for memory buffer reading*
### read from a memory buffer
```v
csv.csv_reader(scr_buf:voidptr(buffer_ptr),  scr_buf_len: buffer_len)
```
When you call `csv.csv_reader` a `CsvReader` struct is initialized passing a `CsvReaderConfig` as a parameter.
Working on these structs it is possible to change the beahivour of the CSV Reader.

## The `CsvReaderConfig` struct
The config struct is the following:
```v
struct CsvReaderConfig {
	scr_buf      voidptr // pointer to the buffer of data 
	scr_buf_len  i64     // if > 0 use the RAM pointed from scr_buf as source of data
	file_path    string
	start_index  i64
	end_index    i64 = -1
	mem_buf_size int = 1024 * 64  // default buffer size 64KByte
	separator    u8= `,`
	default_cell string = "*" // return this string if out of the csv boundaries
	empty_cell   string       // return this string if empty cell
	end_line_len int  = endline_cr_len  // size of the endline rune 
}
```
|Field|Description|
|------------|--------------|
|`scr_buf`, `scr_buf_len`|If `scr_buf_len > 0` the reader will use the  `scr_buf` pointer as the base address of the data to parse and  `scr_buf_len` as the length of the buffer itself|
|`file_path`| if `scr_buf_len == 0` the reader will try to open the `file_path`  file|
| `start_index`,`end_index`| **Internal usage for now**|
| `mem_buf_size`|memory allocated for the reading operations on the file, more memory more speed|
|`separator`|char used as cell separator in the CSV file, default is comma|
|`default_cell`|return this string if the query coordinates are out of the csv boundaries|
|`empty_cell`|return this string if the query coordinates are on an empty cell|
|`end_line_len`|size of the endline, `endline_cr_len=1`,`endline_crlf_len=2`|

## Random Access reading
The main feature of this module is allow random access to the CSV file cells, let see an example with some custom configurations:
```v
import encoding.csv
const txt := 
'
a,b,c
0,1,2
3,,5
'
fn main() {
    mut csvr := csv.csv_reader_from_string(txt)!
	
	// we are setting directly in the `CsvReader` struct these params
    csvr.default_cell = "*"
    csvr.empty_cell = "EMPTY"

    // read a single cell at the row 1 column 0
    println("[0,1] => ${csvr.get_cell(x:0, y:1)!}")
    // try a non existing cell. it will return `default_cell` string
    println("[0,4] => ${csvr.get_cell(x:0, y:4)!}")
    // try an empty string. it will return `empty_cell` string
    println("[1,2] => ${csvr.get_cell(x:1, y:2)!}")
    csvr.dispose_csv_reader()
}
```
Output:
```
[0,1] => 0
[0,4] => *
[1,2] => EMPTY
```
The function `csvr.get_cell()` allow to pick a single cell as a `string`.

## Using the header
This module allow to use the header of the CSV (if any is present) to simplify some read operation on the CSV file.
```v
import encoding.csv
const txt := 
'
import encoding.csv
const txt := 
'
a,b,c
0,pippo,1.2
1,pero,2.3
'
fn main() {
    mut csvr := csv.csv_reader_from_string(txt)!

    // try to create the header, it need the header and 
    // at least one row of data
    csvr.build_header_dict(csv.GetHeaderConf{})!
    println("Header: ${csvr.header_list}")

    // simple cell read usign the header map
    println(csvr.get_cell(x:csvr.header_map['b'], y:1)!)

    // get typed value instead of a string
    println(csvr.get_cellt(x:csvr.header_map['b'], y:2)! as string )

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
```v 
type CellValue = int | f32 | string
```
## Read columns
In this example we read two columns from a big file from disk:
```v
import encoding.csv
fn main() {
    file_path := 'big2.csv'
    mut csvr := csv.csv_reader(
        file_path:file_path, // path to the file CSV
        mem_buf_size:1024*1024*64, // we set 64MByte of buffer for this file
        end_line_len:csv.endline_crlf_len // we are using a windows text file
    )!
    // The data will be saved in this array
    mut data := [][]string{len:csvr.csv_map.len}
    for row_index in 1..csvr.csv_map.len {
        // get single cells
        data[row_index] << csvr.get_cell(x:2, y:row_index)!
        data[row_index] << csvr.get_cell(x:3, y:row_index)!
    }
    
    csvr.dispose_csv_reader()
}
```
This type of reading if faster then read an entire row and then extract the needed data.

## Comments and blank lines
This module can manage blank rows in the file and comment rows:
```v
import encoding.csv
const txt := 
'
# this is a comment line
a,b,c



0,1,2

3,4,5
# another comment
'
fn main() {
    mut csvr := csv.csv_reader(
        scr_buf:txt.str,  
        scr_buf_len: txt.len, 
        comment: `#`  // line starting with # will be ignored
    )!
    // scan all rows, csvr.csv_map.len contain the valid
    // rows number in the CSV file.
    for row_index in 0..csvr.csv_map.len {
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
This module support quote char inside the cells, the following example show the usage:
```v
import encoding.csv
const txt := 
"
# comment line
'a' , 'b', 'c'
'1' , '2', '3'
'4' ,'5', 'a,b,c', 'e'
"
fn main() {
    mut csvr := csv.csv_reader(
        scr_buf:txt.str,       // string pointer
        scr_buf_len: txt.len,  // string length
        comment: `#`,          // line starting with # will be ignored
        quote: `'`,            // char used for quotes
        quote_remove: true     // remove quotes from the cells
    )!
    
    // scan all rows, csvr.csv_map.len contain the valid
    // rows number in the CSV file.
    for row_index in 0..csvr.csv_map.len {
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
This module was tested with CSV files up to 4GByte with 4 milions of rows

